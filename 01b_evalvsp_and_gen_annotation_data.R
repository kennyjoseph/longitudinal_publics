# 01b_evalvsp_and_gen_annotation_data.R
#
# Step 2 of the clustering pipeline, with two goals:
#
#   1. Validate the clustering / pick k. For every pair of k settings run in
#      01_runvsp.R, compute the agreement (AMI/ARI/NMI) between the two
#      cluster assignments of the followed accounts. A k whose solution
#      agrees highly with neighboring solutions is stable; this analysis is
#      what selected k = 150 (mean AMI ~ 0.727, the/near the maximum).
#
#   2. Generate the manual-annotation spreadsheet: for each of the k = 150
#      clusters, the 50 accounts with the highest loading on that cluster,
#      plus profile metadata and a link to the account. Annotators used this
#      to give each cluster the labels shipped in data/cluster_labels.csv.
#
# Inputs (produced by 01_runvsp.R; not distributed - contain user IDs):
#   - <DATASET_NAME>/y_<k>.csv for each k
#   - <DATASET_NAME>/vsp_<KVAL>.rdata (full loading matrix for ranking)
#
# Outputs:
#   - img/ami_by_k.pdf: mean pairwise AMI per setting of k
#   - data/data_to_annotate_<DATASET_NAME>_<KVAL>.csv (not distributed -
#     contains account URLs/profiles)

library(data.table)
library(ggplot2)
library(bit64)
library(dplyr)
library(aricode)
library(stringr)
library(Hmisc)
library(vsp)

###############################################
############ Parameters #######################
###############################################

DATASET_NAME <- "sept2020"
# The k chosen based on the AMI analysis below
KVAL <- 150
# Number of top accounts per cluster to include in the annotation file
N_TO_ANNOTATE <- 50

gen_vals <- function(k, y_or_z) {
  paste0(y_or_z, str_pad(1:k, floor(log10(k)) + 1, pad = "0"))
}

###############################################
###### Load per-k cluster assignments #########
###############################################

y_res <- list()
kvals <- c()
for (fil in Sys.glob(paste0(DATASET_NAME, "/y_*.csv"))) {
  print(fil)
  y_dat <- fread(fil)
  kval <- as.integer(sub(".csv", "", sub(paste0(DATASET_NAME, "/y_"), "", fil)))
  y_res[[kval]] <- y_dat
  kvals <- c(kvals, kval)
}

###############################################
###### Pairwise agreement across k ############
###############################################

# For each pair of k settings, merge the two assignments on account ID and
# compute adjusted/normalized mutual information and adjusted Rand index
ami_dat <- data.frame()
for (i in 1:(length(kvals) - 1)) {
  r1 <- y_res[[kvals[i]]][, .(id, grp)]
  r1[, id := as.integer64(id)]
  print(i)
  for (j in (i + 1):length(kvals)) {
    r2 <- y_res[[kvals[j]]][, .(id, grp)]
    r2[, id := as.integer64(id)]
    mg <- merge(r1, r2, by = "id")
    # Store the pair in both directions so per-k averages are easy to take
    ami_dat <- rbind(ami_dat, data.frame(i = kvals[i], j = kvals[j],
                                         ami = AMI(mg$grp.x, mg$grp.y),
                                         ari = ARI(mg$grp.x, mg$grp.y),
                                         nmi = NMI(mg$grp.x, mg$grp.y)))
    ami_dat <- rbind(ami_dat, data.frame(i = kvals[j], j = kvals[i],
                                         ami = AMI(mg$grp.x, mg$grp.y),
                                         ari = ARI(mg$grp.x, mg$grp.y),
                                         nmi = NMI(mg$grp.x, mg$grp.y)))
  }
}

# Mean AMI (with bootstrapped 66% CI) of each k against all other settings
ami_dat <- data.table(ami_dat)
res <- ami_dat[, as.list(smean.cl.boot(ami, conf.int = .66)), by = i]
print(res)
pl <- ggplot(res, aes(factor(i), Mean, ymin = Lower, ymax = Upper)) +
  geom_pointrange() +
  ylab("Adjusted Mutual Information") +
  xlab("Setting for k in VSP")
dir.create("img", showWarnings = FALSE)
ggsave("img/ami_by_k.pdf", pl, h = 5, w = 5)

###############################################
###### Annotation data for the chosen k #######
###############################################

# Reload the full vsp model to get the raw loading matrix (the y_<k>.csv
# files drop the per-cluster loading columns), and pair it back with the
# assignment + profile metadata
load(paste0(DATASET_NAME, "/vsp_", KVAL, ".rdata"))
my_y <- fread(paste0(DATASET_NAME, "/y_", KVAL, ".csv"))
y <- data.table(get_varimax_y(fa))
y <- y[, gen_vals(KVAL, "y"), with = F]
y_res <- cbind(my_y, y)

# For each cluster, take the N_TO_ANNOTATE accounts with the highest loading
# on that cluster and keep the profile fields annotators need. The Twitter
# ID is only used to construct the profile link, then dropped.
res <- data.table()
for (grp_num in 1:KVAL) {
  grp_str <- paste0("y", str_pad(grp_num, width = 3, "left", "0"))
  dat <- y_res[grp == grp_num][order(-get(grp_str))][1:N_TO_ANNOTATE][
    , .(id, username, description, expanded_url,
        pinned_tweet_text, followers_count, following_count,
        tweet_count, verified, location)]
  dat$url <- paste0("https://twitter.com/intent/user?user_id=", dat$id)
  dat$id <- NULL
  dat$k <- grp_num
  res <- rbind(res, dat)
}

write.csv(res, paste0("data/data_to_annotate_", DATASET_NAME, "_", KVAL, ".csv"),
          row.names = F)
