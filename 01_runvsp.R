# 01_runvsp.R
#
# Step 1 of the clustering pipeline: run VSP (vintage sparse PCA with a
# varimax rotation; Rohe & Zeng 2023, the `vsp` R package) on the
# panelist -> followed-account sparse matrix, for a range of settings of k.
#
# Inputs (not distributed; contain raw Twitter user IDs):
#   - MATRIX_FILE:  sparse 0/1 follow matrix in MatrixMarket format,
#                   rows = panelists (N=1,373,158), cols = followed accounts
#                   (N=68,572,969)
#   - ROW_IDS_FILE: Twitter user ID for each row of the matrix (one per line)
#   - COL_IDS_FILE: Twitter user ID for each column of the matrix
#   - data/user_deets/part-*parquet: profile metadata (username, bio, etc.)
#     for followed accounts, used to make the influencer files interpretable
#
# Outputs:
#   - data/total_panel_follows.csv: total accounts followed per panelist
#   - <DATASET_NAME>/vsp_<k>.rdata: the fitted vsp object for each k
#   - <DATASET_NAME>/y_<k>.csv: per followed account ("influencer"), the
#     cluster assignment (grp = argmax varimax loading), the strength of that
#     assignment (grp_val = max loading), and profile metadata
#   - <DATASET_NAME>/z_<k>.csv: per panelist, the cluster assignment
#   - data/y_150_wval.csv, data/z_150.csv: copies of the chosen-k (k=150,
#     see 01b) files, at the paths the analysis code (util.R) reads
#   - top_users.csv, influencers.csv, adjacency_mat.csv: the
#     top-3,500-per-cluster influencer set and the panelist -> top-influencer
#     edge list, consumed by 01c_generate_usr_to_grp_cnt.py

options(warn = -1)
library(arrow)
library(data.table)
library(stringr)
library(bit64)
library(Matrix)
library(vsp)
library(adjHelpR)
options(arrow.skip_nul = TRUE)

###############################################
############ Parameters #######################
###############################################

DATASET_NAME <- "sept2020"
MATRIX_FILE  <- "/data/kenny/full_follow_matrices/friends_sep2020/friends_sep2020_mat.mtx"
COL_IDS_FILE <- "/data/kenny/full_follow_matrices/friends_sep2020/friends_sep2020_mat_colnames.txt"
ROW_IDS_FILE <- "/data/kenny/full_follow_matrices/friends_sep2020/friends_sep2020_mat_rownames.txt"

# Settings of k (number of clusters) to sweep; 01b compares these to pick one
CLUST_LEN <- c(50, 75, 100, 125, 150, 175, 200, 225)
# The k selected via the AMI analysis in 01b and used in all analyses
KVAL_FINAL <- 150

# An account must be followed by at least this many panelists to be kept
MIN_FOLLOWERS_IN_PANEL <- 25
# A panelist must follow at least this many kept accounts to be kept
MIN_FOLLOWS_BY_PANELIST <- 10

# Number of top influencers (by loading) retained per cluster; this cutoff
# is justified by the elite-coverage analysis reported in the paper
TOP_INFL_PER_CLUSTER <- 3500

# Column names of the varimax loading matrices are y001..y<k> / z001..z<k>
gen_vals <- function(k, y_or_z) {
  paste0(y_or_z, str_pad(1:k, floor(log10(k)) + 1, pad = "0"))
}

###############################################
###### Load matrix + profile metadata #########
###############################################

user_data <- rbindlist(lapply(Sys.glob("data/user_deets/part-*parquet"), read_parquet))
user_data[, sn_lower := tolower(username)]

df <- Matrix::readMM(MATRIX_FILE)

# Record each panelist's total follow count *before* pruning; used
# downstream to compute what share of a panelist's follows are influencers
row_ids <- fread(ROW_IDS_FILE)
n_fols_tot <- rowSums(df)
tot_fol <- data.table(uid = row_ids$V1, fols = n_fols_tot)
write.csv(tot_fol, "data/total_panel_follows.csv", row.names = F)

###############################################
###### Prune the matrix #######################
###############################################

# Keep accounts followed by >= MIN_FOLLOWERS_IN_PANEL panelists...
cs <- colSums(df)
cs_ind <- which(cs >= MIN_FOLLOWERS_IN_PANEL)
df_min <- df[1:nrow(df), cs_ind]

# ...then panelists following >= MIN_FOLLOWS_BY_PANELIST of those accounts
rs <- rowSums(df_min)
rs_ind <- which(rs >= MIN_FOLLOWS_BY_PANELIST)
df_min <- df_min[rs_ind, 1:ncol(df_min)]

# Subset the ID lookups to match the pruned matrix
col_ids <- fread(COL_IDS_FILE)
col_ids <- col_ids$V1[cs_ind]

row_ids <- fread(ROW_IDS_FILE)
row_ids <- row_ids$V1[rs_ind]

###############################################
###### Run VSP for each setting of k ##########
###############################################

for (kval in CLUST_LEN) {
  # Fit VSP; save the full model so 01b can reuse the loading matrices
  fa <- vsp(df_min, rank = kval, center = F)
  save(fa, file = paste0(DATASET_NAME, "/vsp_", kval, ".rdata"))

  # Influencer (column) side: assign each followed account to the cluster
  # with its largest varimax loading (grp), and keep that loading (grp_val)
  # as a measure of how strongly the account belongs to the cluster
  y <- data.table(get_varimax_y(fa))
  y$grp <- unname(apply(y[, gen_vals(kval, "y"), with = F], 1, which.max))
  y$grp_val <- unname(apply(y[, gen_vals(kval, "y"), with = F], 1, max))
  y$id <- as.character(col_ids)
  ymg <- merge(y, user_data, by = "id", all.x = T)
  ymg$kval <- kval
  # Drop the k raw loading columns; keep assignment + metadata
  write.csv(ymg[, -gen_vals(kval, "y"), with = F],
            paste0(DATASET_NAME, "/y_", kval, ".csv"),
            row.names = F)

  # Panelist (row) side: same argmax assignment on the z loadings
  z <- data.table(get_varimax_z(fa))
  z$grp <- unname(apply(z[, gen_vals(kval, "z"), with = F], 1, which.max))
  z$id <- as.character(row_ids)
  write.csv(z[, -gen_vals(kval, "z"), with = F],
            paste0(DATASET_NAME, "/z_", kval, ".csv"), row.names = F)
}

# Diagnostics, if you want them:
# vsp::plot_mixing_matrix(fa)
# screeplot(fa)
# plot_ipr_pairs(fa)

###############################################
###### Export files for the chosen k ##########
###############################################

# Copy the chosen-k outputs to the paths the analysis code (util.R) reads
file.copy(paste0(DATASET_NAME, "/y_", KVAL_FINAL, ".csv"),
          "data/y_150_wval.csv", overwrite = T)
file.copy(paste0(DATASET_NAME, "/z_", KVAL_FINAL, ".csv"),
          "data/z_150.csv", overwrite = T)

###############################################
###### Edge list for 01c ######################
###############################################

# Keep the TOP_INFL_PER_CLUSTER most strongly loading accounts per cluster
# and write out the panelist -> top-influencer edge list. 01c aggregates this
# into per-panelist, per-cluster follow counts.
ymg <- fread("data/y_150_wval.csv")
top_users <- ymg[order(-grp_val)][, .SD[1:min(nrow(.SD), TOP_INFL_PER_CLUSTER)], by = grp]
z <- which(col_ids %in% top_users$id)
col_top_df <- data.table(idv = col_ids[z], target = 1:length(z))
ymg[, idv := as.integer64(id)]
col_top_df <- merge(col_top_df, ymg[, .(idv, grp)], by = "idv")
write.csv(col_top_df, "top_users.csv", row.names = F)

# Space-separated (uid, grp) file for the top influencers, read by 01c
write.table(top_users[, .(uid = id, grp)], "influencers.csv",
            row.names = F, quote = F)

df_top <- df_min[1:nrow(df_min), z]
adj <- adj2el(df_top)
write.csv(adj, "adjacency_mat.csv", row.names = F)
