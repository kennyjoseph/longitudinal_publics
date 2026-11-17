# 01d_creator_selection_figures.R
#
# Appendix figures on how creators (the most influential accounts in each
# longitudinal public) were selected, and how much of panelists' following
# they account for. Ported from the original analysis script (02_analysis.R
# in the repo root), with axis labels updated to the paper's terminology
# ("longitudinal public" instead of "flock", "creators" instead of
# "influencers").
#
#   1. img/flocksize_set.pdf: for N = 500, 1000, ..., 7000 creators per
#      longitudinal public, the share of elite accounts from two external
#      datasets (Wojcieszak et al., Mukerjee et al.) that are included. This analysis motivated the cutoff of N = 3500.
#
#   2. img/density_of_influencer_follows.png: distribution, across panelists,
#      of the share of all accounts they follow that are creators (red line =
#      median).
#
# Inputs (not distributed - contain user IDs; see README):
#   - DATA_DIR/y_150_wval.csv: cluster assignment + loading for every account
#   - DATA_DIR/new_bios_decahose.parquet: updated profiles (screen names)
#   - DATA_DIR/user_to_group_cnt.csv: per-panelist follows of each cluster (01c)
#   - DATA_DIR/total_panel_follows.csv: total accounts followed per panelist (01_runvsp.R)
#   - Wojcieszak et al. and Mukerjee et al. elite lists, downloaded from GitHub

library(arrow)
library(bit64)
library(data.table)
library(ggplot2)
library(scales)
theme_set(theme_bw(16))
options(arrow.skip_nul = TRUE)

DATA_DIR <- "data/ignore"

###############################################
############ Creator data #####################
###############################################

# Same as get_influencer_data() in util.R, minus the cluster-label merge:
# replace profiles with the newer decahose bios where we have them, so
# screen names match the external elite lists
infl_clust <- fread(file.path(DATA_DIR, "y_150_wval.csv"))
new_bios <- data.table(read_parquet(file.path(DATA_DIR, "new_bios_decahose.parquet")))
setnames(new_bios, c("uid", "screen_name"), c("id", "username"))
new_bios[, id := as.integer64(id)]
new_bios <- merge(new_bios[, .(id, username)], infl_clust[, .(id, grp, grp_val)])
infl_clust <- rbind(infl_clust[!id %in% new_bios$id, .(id, username, grp, grp_val)],
                    new_bios)
infl_clust[, sn_lower := tolower(username)]

# Rank of each account within its longitudinal public, by loading
setorder(infl_clust, -grp_val)
infl_clust[, rank_in_grp := rowid(grp)]

###############################################
############ External elite lists #############
###############################################

political_woj <- fread("https://raw.githubusercontent.com/CasAndreu/ingroup_filtering/master/data/actors-ideo-scores.csv")
celebs_woj <- fread("https://gist.githubusercontent.com/mbejda/9c3353780270e7298763/raw/1bfc4810db4240d85947e6aef85fcae71f475493/Top-1000-Celebrity-Twitter-Accounts.csv")
celebs_woj[, sn_lower := tolower(twitter)]

elites_kokil <- fread("https://raw.githubusercontent.com/wrahool/twitter-landscape/master/data/elite_classification.csv")
elites_kokil[, handle := tolower(handle)]
kokil_partisan_scores <- fread("https://raw.githubusercontent.com/wrahool/twitter-landscape/master/data/all_relevance_ideologies.csv")
kokil_partisan_scores[, userhandle := tolower(userhandle)]
elites_kokil <- merge(elites_kokil, kokil_partisan_scores, by.x = "handle", by.y = "userhandle")
elites_kokil[, kokil_political := grepl("(political|hard news|media outlet)",
                                        paste(sector1, sector2, sector3))]

###############################################
#### Figure: coverage by creators per LP ######
###############################################

z <- rbindlist(lapply(seq(500, 7000, by = 500), function(i) {
  top_infl <- infl_clust[rank_in_grp <= i]
  data.table(i = i,
             pol_woj = mean(political_woj$actor_id %in% top_infl$id),
             celeb_woj = mean(celebs_woj$sn_lower %in% top_infl$sn_lower),
             kokil = mean(elites_kokil$handle %in% top_infl$sn_lower),
             kokil_pol = mean(elites_kokil[kokil_political == TRUE]$handle %in% top_infl$sn_lower))
}))
print(z)

mlt_z <- melt(z, id = "i")
mlt_z[, Paper := ifelse(grepl("woj", variable), "Wojcieszak et al.", "Mukerjee et al.")]
lab_map <- c(pol_woj = "Political Accounts", celeb_woj = "Celebrities",
             kokil = "All Elites",
             kokil_pol = "Political Accounts")
mlt_z[, label := factor(lab_map[as.character(variable)], levels = unique(lab_map))]

pl <- ggplot(mlt_z, aes(i, value, color = label)) +
  geom_point(size = 2) + geom_line() +
  geom_vline(xintercept = 3500, linetype = "dotted") +
  geom_hline(yintercept = .8, linetype = "dotted") +
  scale_y_continuous("% of Elite Accounts Included\nin a Longitudinal Public", labels = percent) +
  scale_x_continuous("Number of Influential Accounts per Longitudinal Public",
                     breaks = c(1000, 3500, 6000), labels = comma) +
  scale_color_discrete("Dataset") +
  facet_wrap(~Paper) +
  theme(legend.position = "bottom")
dir.create("img", showWarnings = FALSE)
ggsave("img/flocksize_set.pdf", pl, h = 5, w = 8)

###############################################
#### Figure: share of follows that are creators
###############################################

net <- fread(file.path(DATA_DIR, "user_to_group_cnt.csv"))
tot_follows <- fread(file.path(DATA_DIR, "total_panel_follows.csv"))
tp <- merge(net[, .(tot_infl_cnt = sum(count)), by = source],
            tot_follows, by.x = "source", by.y = "uid")
tp[, prop_creator := tot_infl_cnt / fols]
cat("Panelists:", nrow(tp), " median share of follows that are creators:",
    median(tp$prop_creator), "\n")

pl <- ggplot(tp, aes(prop_creator)) + geom_density() +
  scale_x_continuous("Percent of all Follows that are of Creators", labels = percent) +
  geom_vline(xintercept = median(tp$prop_creator), color = "red")
ggsave("img/density_of_influencer_follows.png", pl, h = 5, w = 7, dpi = 300)
