# Replication code

Replication code for the paper's follower-clustering ("flocks") pipeline and
downstream analyses.

**A note on data**: the underlying data is sensitive — it links Twitter
accounts to a voter file and contains raw Twitter user IDs, which we cannot
release. Consequently, the raw inputs to this pipeline (the follow matrix,
panel/voter files, and any intermediate file containing user IDs) are **not**
distributed; see `.gitignore` for the full list of withheld files. The code
is provided so the full procedure is transparent and reproducible for anyone
with equivalent data.

**To reproduce the paper's results, run `02_results.R`.** It requires only
the five files in `data/` described under "Data files" below — not the raw
panel/voter data. Figures are written to `img/`.

## Pipeline overview

`00_decahose_keyword_counts.py` collects the tweet data; the `01*` scripts
create and validate the clustering; `02_results.R` generates the results in
the paper.

```
00_decahose_keyword_counts.py  [+ keyword_lists/]
   └─> data/tweet_counts.parquet (per-influencer daily keyword-match counts)
01_runvsp.R
   └─> per-k cluster assignments (y_<k>.csv, z_<k>.csv),
       data/y_150_wval.csv, data/z_150.csv,
       data/total_panel_follows.csv,
       influencers.csv, adjacency_mat.csv
01b_evalvsp_and_gen_annotation_data.R
   └─> AMI-based selection of k=150; annotation spreadsheet
       └─> manual annotation ─> cluster label files
01c_generate_usr_to_grp_cnt.py
   └─> data/user_to_group_cnt.csv (panelist × cluster follow counts)
(aggregation of the above with the voter file ─> the preloaded files in data/)
02_results.R
   └─> figures (img/) and results in the paper
```

## The data collection and clustering scripts

### `00_decahose_keyword_counts.py`

Collects the raw tweet data; run first. Counts each top influencer's tweets
that overlap with the issue publics over the 2020 Twitter decahose (10%
sample; licensed, not distributable) on a Spark cluster: joins tweets to
the top-influencer set (`influencers.csv`, from `01_runvsp.R`), separates
pure retweets from original tweets (excluding quotes and replies), and
counts per (date, cluster, account) the tweets matching each keyword list
plus totals. Produces `data/tweet_counts.parquet`. Cluster-specific paths
(decahose location, schema file, xz codec jar) are parameters at the top of
the script.

The keyword lists are public, in `keyword_lists/`:

- `political_keywords.csv` — electoral/political keywords, accounts, and
  hashtags (from Mukerjee et al. and Shugars et al.)
- `blm_keywords.tsv` — Black Lives Matter movement keywords (from
  Shugars et al.)

### `01_runvsp.R`

Runs VSP (vintage sparse PCA with varimax rotation; the
[`vsp`](https://github.com/RoheLab/vsp) R package) on the sparse
panelist × followed-account matrix (1,373,158 panelists × 68,572,969
accounts).

1. Records each panelist's total follow count (`data/total_panel_follows.csv`).
2. Prunes the matrix to accounts followed by ≥25 panelists, then to
   panelists following ≥10 remaining accounts.
3. For each k ∈ {50, 75, 100, 125, 150, 175, 200, 225}: fits `vsp`, and
   assigns each followed account ("influencer") and each panelist to the
   cluster with their largest varimax loading. Writes per-k assignment files
   (`y_<k>.csv` with the loading strength `grp_val`, and `z_<k>.csv`).
4. Copies the chosen-k (k=150) files to `data/y_150_wval.csv` and
   `data/z_150.csv`, the paths the analysis loaders in `util.R` read.
5. Keeps the 3,500 most strongly loading accounts per cluster (a cutoff
   justified by the elite-coverage analysis reported in the paper) and
   writes their (uid, cluster) mapping (`influencers.csv`) and the
   panelist → top-influencer edge list (`adjacency_mat.csv`) for `01c`.

### `01b_evalvsp_and_gen_annotation_data.R`

Two purposes:

1. **Validating the choice of k**: computes pairwise AMI/ARI/NMI between the
   cluster assignments from every pair of k settings. k = 150 sits at/near
   the maximum mean agreement (AMI ≈ 0.727), i.e. it is the most stable
   solution (`img/ami_by_k.pdf`).
2. **Annotation data**: for each of the 150 clusters, exports the 50
   highest-loading accounts with profile metadata and a profile link.
   Annotators used this spreadsheet to produce the manual cluster labels.

### `01c_generate_usr_to_grp_cnt.py`

Joins the ~220M-row edge list (`adjacency_mat.csv`) with the top-influencer
set (`influencers.csv`) and counts, for each panelist, how many accounts
they follow in each cluster. The output (`data/user_to_group_cnt.csv`) is
the panelist × cluster attention table from which the cluster-level audience
statistics were computed. PySpark is used only for scale; the logic is a
single join + group-by count.

## Analysis

- **`02_results.R` — generates the results in the paper.** Runs entirely
  from the pre-computed files in `data/`; where a preloaded file was derived
  from withheld data, the derivation is documented in comments in this
  script. Figures go to `img/`, and regression tables are printed to the
  console as LaTeX.
- `util.R` — shared helpers: data loaders, `cs2` scaling, the event-window
  aggregation (`get_data_from_date`), and the RQ2b plotting helpers.

## Data files

### Required by `02_results.R` (in `data/`)

All five are at the influencer (public-account) level or the cluster level —
none contain panelist-level records.

- **`infl_w_tweet_count.parquet`** — one row per top influencer (the
  3,500-per-cluster set): cluster assignment (`grp`, `Label`,
  `narrow_label`, high-level type `meta`), profile fields (`verified`,
  `followers_count`, …), and counts of the account's tweets matching the
  electoral (`pol`) and BLM (`blm`) issue-public keyword regexes plus all
  tweets (`total`). Derived from the clustering output (`y_150_wval.csv`),
  `tweet_counts.parquet`, and the manual cluster labels.
- **`clust_stats.parquet`** — one row per cluster: audience demographics
  (mean age, % male, % Democrat/Republican/Independent, % white/Black,
  total attention `n`/`log_n`), computed as attention-weighted means over
  panelists. The exact derivation (which requires the withheld
  `user_to_group_cnt.csv` and voter file) is preserved as a comment at the
  top of the RQ2a section of `02_results.R`.
- **`tweet_counts.parquet`** — per account, per day, per keyword-regex type
  (`pol`, `blm`, `total`), split by retweet/original: number of tweets.
  Produced by `00_decahose_keyword_counts.py` from the decahose and the
  `keyword_lists/` files. Used to build the weekly event-window series
  around George Floyd's murder and the election.
- **`kenny_influencer_data_new_age_info.parquet`** — per-influencer audience
  demographics: distributions of age, race, party, and gender among the
  panelists following each account (`influencer_id`).
- **`creators_with_centralities_no_zero_weight_edges.parquet`** — per
  influencer (`id`): approximate random-walk betweenness centrality
  (`approx_rw_centr`) in the follow network.

Note: these files still contain Twitter account IDs (of the public
influencer accounts, not panelists). They are gitignored pending a decision
on anonymizing IDs before public release.

### Not required (moved to `data/ignore/`, all gitignored)

Inputs/intermediates for the 01 pipeline and the data loaders in `util.R`
only:

- Panelist/voter data (never distributable): `TSmart-cleaner-Oct2017-rawFormat.tsv`
  (voter file), `panel_7_7_20.tsv` (panel profiles), `z_150.csv` (panelist
  cluster assignments), `user_to_group_cnt.csv` (panelist × cluster follow
  counts), `total_panel_follows.csv`, `user_ids_in_vsp.txt`.
- Influencer-side pipeline intermediates: `y_150_wval.csv`, `top_infl.csv`,
  `new_bios_decahose.parquet` (updated bios from the decahose).
- Auxiliary/label files: `cluster_labels.csv` (manual cluster annotations
  read by `get_cluster_names()` in `util.R`, only needed when rebuilding
  from raw data), `top_level_label_map.tsv`, `clust_stats.csv` (superseded
  by the parquet), `curating_actors.csv` (elite list used in earlier
  validation analyses).

## Requirements

- **R** — clustering pipeline: `vsp`, `adjHelpR`, `Matrix`, `data.table`,
  `arrow`, `bit64`, `aricode`, `stringr`, `Hmisc`, `ggplot2`, `dplyr`;
  analysis (`02_results.R`/`util.R`): `tidyverse`, `lme4`, `lmerTest`,
  `fixest`, `performance`, `marginaleffects`, `patchwork`, `binom`,
  `sandwich`, `lmtest`, `modelsummary`, `xtable`, `GGally`, `ggpubr`,
  `ggrepel`, `ggridges`, `mgcv`, `scales`, `glue`, `plyr`, `lubridate`
- **Python** ≥ 3.9: `pyspark`, `pandas`, `pyarrow`
