# 01c_generate_usr_to_grp_cnt.py
#
# Step 3 of the clustering pipeline: aggregate the panelist -> top-influencer
# edge list into per-panelist, per-cluster follow counts. This produces
# user_to_group_cnt.csv, the panelist x cluster table from which the
# cluster-level audience statistics (data/clust_stats.parquet) are computed.
#
# PySpark is used only because the edge list is large (~220M rows); the
# computation itself is a single join + group-by count.
#
# Inputs (not distributed - contain raw Twitter user IDs):
#   - adjacency_mat.csv: panelist -> followed-influencer edge list
#     (columns source, target, attr), produced by 01_runvsp.R
#   - influencers.csv: space-separated (uid, grp) file mapping each top
#     influencer to their cluster, produced by 01_runvsp.R
#
# Output:
#   - user_to_group_cnt.csv: (source, grp, count) = number of accounts in
#     cluster `grp` that panelist `source` follows (moved to data/ for the
#     analysis code)

import pandas as pd
from pyspark.sql import SparkSession

# ---------------- Parameters ----------------
N_CORES = 16
DRIVER_MEMORY_GB = 50
MAX_RESULT_GB = 10

ADJACENCY_FILE = "adjacency_mat.csv"
INFLUENCER_FILE = "influencers.csv"
INTERMEDIATE_PARQUET = "user_grp_cnt"
OUTPUT_FILE = "user_to_group_cnt.csv"
# --------------------------------------------

spark = (
    SparkSession
    .builder
    .master(f"local[{N_CORES}]")
    .config("spark.driver.memory", f"{DRIVER_MEMORY_GB}g")
    .config("spark.driver.maxResultSize", f"{MAX_RESULT_GB}g")
    .getOrCreate()
)

adj = spark.read.csv(ADJACENCY_FILE, header=True)
top = spark.read.csv(INFLUENCER_FILE, header=True, sep=" ")

# Attach each followed influencer's cluster, then count follows per
# (panelist, cluster) pair
adj2 = adj.join(top, adj.target == top.uid)
adj2.groupby(["source", "grp"]).count().write.parquet(
    INTERMEDIATE_PARQUET, mode="overwrite"
)

# Collapse the parquet shards into a single CSV for the R code
df = pd.read_parquet(INTERMEDIATE_PARQUET + "/")
df.to_csv(OUTPUT_FILE, index=False)
