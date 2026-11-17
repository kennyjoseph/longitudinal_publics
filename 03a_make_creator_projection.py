"""03a_make_creator_projection.py

Builds, for each longitudinal public (cluster), the weighted creator-creator
"shared audience" graph used for betweenness centrality in RQ2c: for every
pair of creators in the same cluster, the number of accounts that follow
both. Writes one file per cluster, <out-dir>/cluster_graph_<grp>.json, a
list of [creator_a, creator_b, shared_audience_size] for every pair
(including pairs with zero shared audience). 03b_creator_centralities.py
reads these files.

Inputs (not distributed - contain Twitter user IDs):
  --creators  CSV with columns id, grp: the creators in each cluster
  --follows   parquet follow edge list with columns source_id (follower)
              and target_id (followed account)

Usage:
    python 03a_make_creator_projection.py \
        --creators influencers_1kfol_total5.csv \
        --follows friends_2020.parquet \
        --out-dir cluster_graphs

Requires polars and tqdm. Note that the number of pairs grows quadratically
with cluster size (~4M pairs for a 2,800-creator cluster), so this is slow.
"""

import argparse
import itertools
import json
import os

import polars as pl
import tqdm


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--creators", required=True,
                    help="CSV with columns id, grp (creator id, cluster)")
    ap.add_argument("--follows", required=True,
                    help="parquet edge list with columns source_id, target_id")
    ap.add_argument("--out-dir", default="cluster_graphs")
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    df_infl = pl.read_csv(args.creators)
    df_foll = pl.read_parquet(args.follows, columns=["source_id", "target_id"])

    # Audience (set of followers) of every creator
    creators = set(df_infl["id"].unique().to_list())
    audiences = {
        row[0]: set(row[1])
        for row in (df_foll.filter(pl.col("target_id").is_in(creators))
                    .group_by("target_id").agg(pl.col("source_id"))
                    .iter_rows())
    }
    missing = creators - audiences.keys()
    if missing:
        print(f"{len(missing)} creators have no followers in --follows; "
              f"their shared audience with everyone is 0")

    for (grp,), df_cl in tqdm.tqdm(df_infl.group_by("grp")):
        ids_in_cl = df_cl["id"].to_list()
        pr_list = []
        for node1, node2 in itertools.combinations(ids_in_cl, 2):
            shared = len(audiences.get(node1, set()) & audiences.get(node2, set()))
            pr_list.append((node1, node2, shared))
        with open(os.path.join(args.out_dir, f"cluster_graph_{grp}.json"), "w") as f:
            json.dump(pr_list, f)


if __name__ == "__main__":
    main()
