"""03b_creator_centralities.py

Recompute each creator's (approximate) current-flow betweenness centrality
within its longitudinal public, from the per-cluster shared-audience edge
lists produced by 03a_make_creator_projection.py (cluster_graph_<grp>.json).

Replaces get_creator_centralities.py. Differences from that script:

1. Edge weights. In current-flow betweenness the edge weight is a
   CONDUCTANCE (networkx builds the graph Laplacian from it): a heavier edge
   carries more current. The original script used weight = 1 / shared
   audience, so creators with a LARGE shared audience were WEAKLY connected,
   the opposite of the intended meaning. The default here is
   weight = shared audience size (--weighting shared_count).
   --weighting reciprocal reproduces the original definition, for comparison.

2. Sampling. networkx's approximate_current_flow_betweenness_centrality with
   its default epsilon=0.5 averages over only k = ceil(4 ln n) random
   source/target pairs (~31-33 per cluster here), and its inner loop is
   pure Python over every edge, which is slow on these near-complete graphs.
   This script implements the same estimator (same sampling scheme,
   normalization, and grounding) vectorized in numpy, so a much larger k is
   affordable. k = ceil((c*/epsilon)^2 ln n), as in networkx; the default
   epsilon=0.1 gives k ~ 770 pairs for a 2,200-node cluster.

3. Reproducibility: a fixed seed (--seed).

4. Output: only the two columns 02_results.R uses (id, approx_rw_centr), so
   the merge there is unchanged. The metadata join against
   clean_kenny_full_influencer_data.parquet is dropped (02_results.R gets all
   metadata from infl_w_tweet_count.parquet).

As in the original, edges with zero shared audience are dropped (they carry
no current). networkx requires a connected graph; if a cluster's graph is
not connected, centrality is computed on its largest connected component and
creators outside it get no value (reported in the log).

Usage (from final_code/, with the uv venv):
    .venv/bin/python 03b_creator_centralities.py \
        --graph-dir /path/to/cluster_graphs \
        --influencers ../influencers_1kfol_total5.csv \
        --out data/creators_with_centralities_shared_count.parquet

Then set CENTRALITY_FILE at the top of 02_results.R to the new file and rerun.
"""

import argparse
import glob
import json
import math
import os
import sys
import time
from concurrent.futures import ProcessPoolExecutor, as_completed

# Clusters run in parallel worker processes (--workers), so keep each
# process's BLAS single-threaded to avoid oversubscribing the cores. Must be
# set before numpy is imported (workers re-import this module).
for _v in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
           "VECLIB_MAXIMUM_THREADS"):
    os.environ.setdefault(_v, "1")

import numpy as np
import pandas as pd
import scipy.linalg
import scipy.sparse.csgraph


def load_cluster_graph(fname, weighting):
    """Return (node_ids, dense weight matrix W) for one cluster JSON.

    The JSON is a list of [creator_a, creator_b, shared_audience_size] for
    every pair of creators in the cluster.
    """
    edges = np.asarray(json.load(open(fname)), dtype=np.int64)
    if edges.size == 0:
        return np.array([], dtype=np.int64), np.zeros((0, 0))
    edges = edges[edges[:, 2] > 0]  # drop zero-weight edges, as the original
    ids, idx = np.unique(edges[:, :2], return_inverse=True)
    idx = idx.reshape(-1, 2)
    if weighting == "shared_count":
        w = edges[:, 2].astype(float)
    elif weighting == "reciprocal":
        w = 1.0 / edges[:, 2].astype(float)
    else:
        raise ValueError(weighting)
    n = len(ids)
    W = np.zeros((n, n))
    W[idx[:, 0], idx[:, 1]] = w
    W[idx[:, 1], idx[:, 0]] = w
    return ids, W


def approx_cfb(W, epsilon, rng, batch=256):
    """Approximate current-flow betweenness, normalized.

    Same estimator as networkx.approximate_current_flow_betweenness_centrality
    (networkx 3.x): sample k ordered pairs (s, t) uniformly without
    replacement within a pair, inject unit current at s and extract at t,
    solve for node potentials p, and add to each node v not in {s, t} its
    throughput  sum_u W[v,u] |p_v - p_u|  scaled by c*/(2k).
    Returns (centrality array, k).
    """
    n = W.shape[0]
    if n < 3:
        return np.zeros(n), 0
    nb = (n - 1.0) * (n - 2.0)
    cstar = n * (n - 1) / nb
    k = int(math.ceil((cstar / epsilon) ** 2 * math.log(n)))

    # Grounded Laplacian (node 0 removed); potential differences, which are
    # all the estimator uses, do not depend on which node is grounded.
    L = np.diag(W.sum(axis=1)) - W
    chol = scipy.linalg.cho_factor(L[1:, 1:])

    # Sample pairs: s uniform, t uniform among the other n-1 nodes.
    s = rng.integers(0, n, size=k)
    t = rng.integers(0, n - 1, size=k)
    t = t + (t >= s)

    bet = np.zeros(n)
    for start in range(0, k, batch):
        ss, tt = s[start:start + batch], t[start:start + batch]
        m = len(ss)
        B = np.zeros((n, m))
        B[ss, np.arange(m)] += 1.0
        B[tt, np.arange(m)] -= 1.0
        P = np.zeros((n, m))
        P[1:] = scipy.linalg.cho_solve(chol, B[1:])
        for j in range(m):
            p = P[:, j]
            thru = (W * np.abs(p[:, None] - p[None, :])).sum(axis=1)
            thru[ss[j]] = 0.0
            thru[tt[j]] = 0.0
            bet += thru
    return bet * cstar / (2 * k), k


def process_cluster(fname, weighting, epsilon, seed):
    """Compute centralities for one cluster file (runs in a worker process).

    Each cluster gets its own RNG seeded by (seed, cluster id), so results do
    not depend on the number of workers or the order clusters finish in.
    """
    grp = os.path.basename(fname)[len("cluster_graph_"):-len(".json")]
    ids, W = load_cluster_graph(fname, weighting)
    n_all = len(ids)
    if n_all == 0:
        return grp, None, "empty graph, skipped"
    note = ""
    n_comp, labels = scipy.sparse.csgraph.connected_components(W > 0, directed=False)
    if n_comp > 1:
        keep = labels == np.bincount(labels).argmax()
        note = (f"; {n_comp} components, used largest "
                f"({keep.sum()} of {n_all} nodes)")
        ids, W = ids[keep], W[np.ix_(keep, keep)]
    rng = np.random.default_rng([seed, int(grp)])
    cent, k = approx_cfb(W, epsilon, rng)
    return grp, pd.DataFrame({"id": ids, "approx_rw_centr": cent}), f"n={len(ids)}, k={k}{note}"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--graph-dir", required=True,
                    help="directory containing cluster_graph_<grp>.json files")
    ap.add_argument("--influencers", default="../influencers_1kfol_total5.csv",
                    help="creator list (id, grp) used to check every cluster/creator is covered")
    ap.add_argument("--weighting", choices=["shared_count", "reciprocal"],
                    default="shared_count")
    ap.add_argument("--epsilon", type=float, default=0.1)
    ap.add_argument("--seed", type=int, default=779)
    ap.add_argument("--workers", type=int, default=max(1, (os.cpu_count() or 2) - 2),
                    help="parallel worker processes (default: CPU count - 2). Each "
                         "needs up to ~1.5 GB RAM while loading the largest clusters")
    ap.add_argument("--out", default="data/creators_with_centralities_shared_count.parquet")
    args = ap.parse_args()

    files = sorted(glob.glob(os.path.join(args.graph_dir, "cluster_graph_*.json")))
    if not files:
        sys.exit(f"No cluster_graph_*.json files in {args.graph_dir}")

    infl = pd.read_csv(args.influencers, usecols=["id", "grp"])
    expected_grps = set(infl["grp"].astype(str))
    found_grps = {os.path.basename(f)[len("cluster_graph_"):-len(".json")] for f in files}
    missing = sorted(expected_grps - found_grps, key=lambda g: int(g) if g.isdigit() else g)
    if missing:
        print(f"WARNING: no graph file for {len(missing)} cluster(s): {missing}. "
              f"Their creators will have no centrality.", flush=True)

    # Largest files first, so the slowest clusters don't start last
    files.sort(key=os.path.getsize, reverse=True)
    rows = []
    t0 = time.time()
    with ProcessPoolExecutor(max_workers=args.workers) as pool:
        futs = [pool.submit(process_cluster, f, args.weighting, args.epsilon, args.seed)
                for f in files]
        for i, fut in enumerate(as_completed(futs), 1):
            grp, df, msg = fut.result()
            if df is not None:
                rows.append(df)
            print(f"[{i}/{len(files)}] grp {grp}: {msg}, "
                  f"elapsed {time.time() - t0:.0f}s", flush=True)

    out = pd.concat(rows, ignore_index=True).sort_values("id", ignore_index=True)
    dup = out["id"].duplicated().sum()
    if dup:
        print(f"WARNING: {dup} creator ids appear in more than one cluster", flush=True)
    covered = infl["id"].isin(out["id"]).mean()
    print(f"Creators in --influencers with a centrality: {covered:.1%}", flush=True)
    out.to_parquet(args.out, index=False)
    print(f"Wrote {len(out)} rows to {args.out}", flush=True)


if __name__ == "__main__":
    main()
