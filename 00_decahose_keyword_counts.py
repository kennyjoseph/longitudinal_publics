# 00_decahose_keyword_counts.py
#
# Data collection, run first: pulls the raw tweet data from the decahose and
# counts, for each top influencer and day, how many of their tweets match
# the issue-public keyword lists. This produces the file shipped as
# data/tweet_counts.parquet, used by 02_results.R to build the event-window
# series and the per-account pol/blm/total counts. (The join to the
# influencer set uses influencers.csv from 01_runvsp.R.)
#
# Retweets and original tweets are processed separately (pure retweets vs.
# tweets that are neither retweets, quotes, nor replies), and for each the
# per-(date, cluster, account) counts are computed for:
#   - total: all tweets
#   - pol:   tweets matching the electoral keyword list (Mukerjee et al. and
#            Shugars et al.)
#   - blm:   tweets matching the BLM keyword list (Shugars et al.)
# (A COVID list from Shugars et al. was also run historically but is not
# used in the paper.)
#
# Inputs:
#   - keyword_lists/political_keywords.csv, keyword_lists/blm_keywords.tsv:
#     one keyword per line (first tab-separated field); included in this
#     repository
#   - influencers.csv: space-separated (uid, grp) top-influencer file,
#     produced by 01_runvsp.R (not distributed - contains user IDs)
#   - DECAHOSE_GLOB: the Twitter decahose (10% sample), 2020, as
#     xz-compressed JSON. Access to the decahose is licensed; not
#     distributable
#   - SCHEMA_FILE: a Spark schema (StructType JSON) for the decahose tweets
#
# Output:
#   - data/tweet_counts.parquet: (date, grp, userid, count, regex_type,
#     is_rt) - not distributed as-is (contains user IDs)
#
# This runs on a Spark cluster with HDFS; paths below must be adapted to
# your environment.

import json

import pandas as pd
import pyspark.sql.functions as F
import pyspark.sql.types as T
from pyspark.sql import SparkSession

# ---------------- Parameters ----------------
DECAHOSE_GLOB = "twitter_decahose/2020-*-*.xz"  # HDFS path to decahose JSON
SCHEMA_FILE = "decahose_schema.json"            # Spark schema for the tweets
INFLUENCER_FILE = "influencers.csv"
OUTPUT_DIR = "agg_res_following"                # intermediate parquet dirs
FINAL_OUTPUT = "data/tweet_counts.parquet"
# Reading .xz-compressed JSON requires the hadoop-xz codec jar
XZ_JAR_PATH = "file:///path/to/hadoop-xz-1.5-SNAPSHOT.jar"
# --------------------------------------------


def start_spark(name, debug=False):
    builder = SparkSession.builder.appName(name)

    # Exclude nodes on which tasks are failing; helps long jobs survive
    # single-node storage/disk issues
    builder = (
        builder
        .config("spark.excludeOnFailure.enabled", "true")
        .config("spark.excludeOnFailure.killExcludedExecutors", "true")
        .config("spark.excludeOnFailure.application.fetchFailure.enabled", "true")
        .config("spark.sql.legacy.timeParserPolicy", "LEGACY")
        .config("spark.jars", XZ_JAR_PATH)
        .config("spark.driver.extraClassPath", XZ_JAR_PATH)
        .config("spark.executor.extraClassPath", XZ_JAR_PATH)
        .config("io.compression.codecs", "io.sensesecure.hadoop.xz.XZCodec")
    )

    if debug:
        builder = builder.master("local[5]")

    return builder.getOrCreate()


def get_keyword_regex(filename):
    # one keyword per line (first tab-separated field); #/@ prefixes are
    # stripped so hashtag/mention variants of a term also match
    words_list = []
    with open(filename, "r") as f:
        for line in f.readlines():
            words_list.append(
                line.split("\t")[0].strip().replace("#", "").replace("@", "")
            )
    joined = "|".join(words_list)
    return rf"\b({joined})\b"


def run_all_regex(tweet_dataframe, suffix="", output_dir=OUTPUT_DIR):
    # total counts, then counts of keyword matches, per (date, cluster, account)
    tweet_dataframe.groupBy(["date", "grp", "userid"]).count() \
        .write.parquet(f"{output_dir}/total{suffix}")

    for type_regex, name_regex in [(pol_reg, "pol"),
                                   (blm_reg, "blm")]:
        matches = tweet_dataframe.where(F.lower(F.col("all_text")).rlike(type_regex))
        matches = matches.groupBy(["date", "grp", "userid"]).count()
        matches.write.parquet(f"{output_dir}/{name_regex}{suffix}")


spark = start_spark("decahose_keyword_counts")
schm = T.StructType.fromJson(json.load(open(SCHEMA_FILE)))

pol_reg = get_keyword_regex("keyword_lists/political_keywords.csv")
blm_reg = get_keyword_regex("keyword_lists/blm_keywords.tsv")

# load influencer account list w/ cluster labels
infl_accts = spark.read.csv(INFLUENCER_FILE, header=True, sep=" ")

# load decahose data for 2020 and parse tweet dates
df0 = spark.read.json(DECAHOSE_GLOB, schema=schm)
df0 = df0.withColumn("ts_parsed",
                     F.to_timestamp(F.col("created_at"),
                                    "EEE MMM dd HH:mm:ss ZZZZZ yyyy"))
df0 = df0.withColumn("date",
                     F.date_format(F.date_trunc("day", df0.ts_parsed),
                                   "yyyy-MM-dd HH:mm:ss"))

########## Retweets ##########
# pure retweets only (not quotes or replies), joined to the influencer set;
# the RT's text lives on the retweeted status (full_text when extended)
df_all_infl_rts = infl_accts.join(
    df0.filter(
        ~(F.isnull(F.col("retweeted_status.id_str")))
        & F.isnull(F.col("quoted_status.id_str"))
        & F.isnull(F.col("in_reply_to_user_id"))
    ),
    infl_accts.uid == df0.user.id)

df_all_infl_rts = df_all_infl_rts.selectExpr(
    "id_str",
    "user.id_str as userid",
    "retweeted_status.text as rt_text",
    "retweeted_status.extended_tweet.full_text as extended_text",
    "date",
    "grp",
)
df_all_infl_rts = df_all_infl_rts.withColumn(
    "all_text", F.coalesce(df_all_infl_rts.rt_text,
                           df_all_infl_rts.extended_text))

run_all_regex(df_all_infl_rts)

########## Original tweets ##########
# neither retweet, quote, nor reply
df_all_infl_orig = infl_accts.join(
    df0.filter(
        F.isnull(F.col("retweeted_status.id_str"))
        & F.isnull(F.col("quoted_status.id_str"))
        & F.isnull(F.col("in_reply_to_user_id"))
    ),
    infl_accts.uid == df0.user.id)

df_all_infl_orig = df_all_infl_orig.selectExpr(
    "id_str",
    "user.id_str as userid",
    "extended_tweet.full_text as extended_text",
    "date",
    "text",
    "grp",
)
df_all_infl_orig = df_all_infl_orig.withColumn(
    "all_text", F.coalesce(df_all_infl_orig.text,
                           df_all_infl_orig.extended_text))

run_all_regex(df_all_infl_orig, suffix="_orig")

spark.stop()

########## Combine and write out ##########
# (if OUTPUT_DIR is on HDFS, copy the parquet directories to local disk first)
dfs = []
for fil in ["pol", "blm", "total"]:
    df = pd.read_parquet(f"{OUTPUT_DIR}/{fil}")
    df["regex_type"] = fil
    df["is_rt"] = True
    dfs.append(df)

    df = pd.read_parquet(f"{OUTPUT_DIR}/{fil}_orig")
    df["regex_type"] = fil
    df["is_rt"] = False
    dfs.append(df)

df = pd.concat(dfs)
df.to_parquet(FINAL_OUTPUT, index=False)
