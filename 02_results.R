# 02_results.R
#
# Generates all results in the paper from pre-computed ("preloaded") data
# files. This is the script to run for replication: it does not require the
# withheld raw data (voter file, panel follow data, or per-panelist cluster
# counts) - the objects derived from those are shipped as parquet files in
# data/, and comments below document how each was constructed.
#
# Inputs (all in data/; see the README for full descriptions):
#   - infl_w_tweet_count.parquet: the top-3,500 influencers per cluster
#     (by varimax loading, from the VSP clustering built by the 01_*
#     scripts), with manually annotated cluster labels and per-account
#     counts of tweets overlapping the BLM/electoral issue publics
#   - clust_stats.parquet: cluster-level audience demographics (derivation
#     shown, commented out, at the top of RQ2a below)
#   - tweet_counts.parquet: per-account, per-day counts of tweets matching
#     each keyword regex type (pol / blm / total)
#   - kenny_influencer_data_new_age_info.parquet: per-influencer audience
#     demographics (age, race, party, gender), aggregated from the panel
#   - creators_with_centralities_shared_count.parquet: per-creator
#     approximate current-flow betweenness centrality within its longitudinal
#     public's shared-audience graph (03a/03b scripts; CENTRALITY_FILE below)
#   - rq2b_boot_curves.rds (optional): cached bootstrap for img/rq2b.pdf; if
#     present and it matches the fitted model, the ~4.5 h bootstrap is skipped
#
# Outputs: img/aud_diff.pdf, img/aud_diff_political.pdf,
# img/rq2a_time_raw.pdf, img/rq2a_time_model.pdf, img/rq2.pdf,
# img/rq2a_audience.pdf, img/rq2b.pdf; table_F2.tex and table_G3.tex; other
# regression tables printed to the console as LaTeX.
#
# Terminology used in comments below:
#   - LP  = longitudinal public (a flock/cluster; `meta` is its high-level
#     type: National Politics / Local Politics-News / Others)
#   - IP  = issue public (BLM or electoral, identified by keyword regexes;
#     `regex_type` / `variable` / `issuepublic` in the data)

source("util.r")
library(xtable)

# Creator betweenness centralities used in RQ2c (paper numbering).
# Original file: edge weight = 1/shared audience, networkx default
#   epsilon=0.5 (~32 sampled pairs per cluster), no seed.
# Recomputed file (03b_creator_centralities.py): edge weight = shared audience
#   (current-flow weights are conductances), epsilon=0.1, seed 779.
# CENTRALITY_FILE <- "data/creators_with_centralities_no_zero_weight_edges.parquet"
CENTRALITY_FILE <- "data/creators_with_centralities_shared_count.parquet"

# All figures are written here
dir.create("img", showWarnings = FALSE)
#########################################
#########################################
################### RQ1 #################
#########################################
#########################################

# Top influencers with issue-public tweet counts and cluster labels.
# `pol`, `blm`, `total` are counts of the account's tweets that matched the
# electoral regex, the BLM regex, and all tweets, respectively.
infl_w_tweet_count <- data.table(read_parquet("data/infl_w_tweet_count.parquet"))

# Table 1: which clusters (Label) fall under each narrow/meta label pair
xtable(infl_w_tweet_count[,paste(unique(Label), collapse=", "), by=.(narrow_label,meta)][order(-V1)])

#########################################
#########################################
################### RQ2 #################
#########################################
#########################################

#########################################
############### RQ2a
#########################################

######## Compute cluster stats
# The block below is how clust_stats.parquet was generated.
# It requires the withheld panel data: user_to_group_cnt.csv (per-panelist,
# per-cluster follow counts from 01c) and the voter-file demographics.
# Each cluster's audience statistics are means over panelists, weighted by
# the share of the panelist's influencer follows that go to that cluster.
#
# net <- fread("data/user_to_group_cnt.csv")
# net <- merge(net, clust_names,by.x="grp",by.y="k")
# net_tot <- length(unique(net$source))
# net[, prop := count/sum(.SD$count),by=source]
# panel[, party := ifelse(tsmart_partisan_score < 35, "Republican",
#                         ifelse(tsmart_partisan_score < 65, "Independent",
#                                "Democrat"))]
# net_panel <- merge(net, panel[,.(twProfileID,voterbase_age,voterbase_gender,
#                                  party,voterbase_race)],by.x="source",by.y="twProfileID")
# clust_stats <- net_panel[,list(n=sum(prop),
#                                age=weighted.mean(voterbase_age[!is.na(voterbase_age) & voterbase_age < 75],
#                                                  prop[!is.na(voterbase_age) & voterbase_age < 75]),
#                                male=weighted.mean(voterbase_gender=="Male",prop),
#                                dem=weighted.mean(party=="Democrat" ,prop),
#                                rep=weighted.mean(party=="Republican" ,prop),
#                                indep=weighted.mean(party=="Independent",prop),
#                                white=weighted.mean(voterbase_race[!voterbase_race%in%c("Uncoded","Other")] =="Caucasian",
#                                                    prop[!voterbase_race%in%c("Uncoded","Other")]),
#                                black=weighted.mean(voterbase_race[!voterbase_race%in%c("Uncoded","Other")] =="African-American",
#                                                    prop[!voterbase_race%in%c("Uncoded","Other")]),
#                                white_lib=weighted.mean(voterbase_race[!voterbase_race%in%c("Uncoded","Other")] =="Caucasian" &
#                                                          party[!voterbase_race%in%c("Uncoded","Other")]=="Democrat",
#                                                        prop[!voterbase_race%in%c("Uncoded","Other")])
# ),
# by=grp]
# clust_stats[, log_n := log(n)]
# Above is how i generate "clust_stats"
clust_stats <- data.table(read_parquet("data/clust_stats.parquet"))

######## Count tweets per day for clusters, counting only the top influencers
tw_cnt_raw <- data.table(read_parquet("./data/tweet_counts.parquet"))
tw_cnt_raw[, date := ymd_hms(date)]
tw_cnt_raw <- tw_cnt_raw[date >= ymd("2020-05-10") ]
tw_cnt <- tw_cnt_raw[userid %in% infl_w_tweet_count$id,
                     sum(count),
                     by=.(regex_type,grp,date)]

# Aggregate into weekly bins relative to each focal event (2 weeks before
# through 6 weeks after; see get_data_from_date in util.R), producing
# per-cluster counts of issue-public tweets (polcnt) and all tweets (total)
rq2a <- rbind(get_data_from_date(tw_cnt,"2020-05-26","George Floyd"),
              get_data_from_date(tw_cnt,"2020-11-03","Election")
)
rq2a[, grp := as.integer(grp)]

rq2a <- merge(rq2a,
              infl_w_tweet_count[,.(grp,Label,narrow_label,meta)][,.SD[1],by=grp])

# How many clusters get dropped by the filter below? (reported in the paper)
length(unique(rq2a[grepl("Hispanic|Venezuelan|Non-US" ,Label)]$grp))

length(unique(rq2a[narrow_label == "No Clear Pattern"]$grp))
unique(rq2a[grepl("Hispanic|Venezuelan|Non-US" ,Label) | narrow_label == "No Clear Pattern"]$Label)

# Drop non-English, non-publics (former bad for KW search and skews regression)
rq2a <- rq2a[!(grepl("Hispanic|Venezuelan|Non-US" ,Label) | narrow_label == "No Clear Pattern")]

rq2a <- merge(rq2a, clust_stats)
rq2a[, par_diff := rep-dem]




p_data <- melt(rq2a, id="meta",c("age","par_diff","male"))
p_data[, variable := factor(variable,
                            labels=c("Mean Age",
                                     "% Rep. - % Dem.",
                                     "% Male"))]
plt <- ggplot(p_data, aes(value,meta))+
  geom_density_ridges()+
  facet_grid(.~variable,scales="free_x")+
  ylab("Longitudinal Public Type")+
  xlab("")
ggsave("img/aud_diff.pdf",plt,h=4,w=10)

# Same figure, with separate densities within each LP type for LPs that
# carry a "Political" label at all (any of Metalabel1-4) vs. those that
# don't. Only Local Politics/News has both kinds (6 political, 18 not);
# all National Politics LPs are political and no Others LPs are.
# One row per LP, so each LP counts once.
pol_lab <- infl_w_tweet_count[, .SD[1], by = grp][, .(grp, Political)]
p_data_pol <- melt(merge(unique(rq2a[, .(grp, meta, age, par_diff, male)]), pol_lab,
                         by = "grp"),
                   id = c("grp", "meta", "Political"), c("age", "par_diff", "male"))
p_data_pol[, variable := factor(variable,
                                labels=c("Mean Age",
                                         "% Rep. - % Dem.",
                                         "% Male"))]
p_data_pol[, Political := factor(ifelse(Political, "Labeled Political",
                                        "Not Labeled Political"),
                                 levels = c("Labeled Political", "Not Labeled Political"))]
plt_pol <- ggplot(p_data_pol, aes(value, meta, fill = Political))+
  # vertical line in each density = that group's median
  stat_density_ridges(geom = "density_ridges", alpha = .5,
                      quantile_lines = TRUE, quantiles = 2)+
  # dotted reference line at parity (0) in the partisanship panel only
  geom_vline(data = data.frame(variable = factor("% Rep. - % Dem.",
                                                 levels = levels(p_data_pol$variable)),
                               x = 0),
             aes(xintercept = x), linetype = "dotted")+
  facet_grid(.~variable,scales="free_x")+
  scale_fill_discrete(name = NULL)+
  ylab("Longitudinal Public Type")+
  xlab("")+
  theme(legend.position = "bottom")
ggsave("img/aud_diff_political.pdf",plt_pol,h=4.5,w=10)
########### Temporal heterogeneity

# Overall (pooled) weekly proportions with Agresti-Coull CIs, for the
# cluster-level spaghetti plot (panel A of rq2a_time.pdf)
library(binom)
j <- rq2a[, as.list(binom.agresti.coull(sum(polcnt),sum(total))),
          by=.(t,event,regex_type)]
j[, talk := ifelse(regex_type =="blm",
                   "BLM Public",
                   "Electoral Public")]
rq2a[, talk := ifelse(regex_type =="blm",
                      "BLM Public",
                      "Electoral Public")]
basic_plt <- ggplot()+
  geom_line(data=rq2a, aes(t,polcnt/total,
                           group=grp,
                           color=meta),alpha=.7)+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_point(size=1.2) +
  geom_line(data=j, aes(t,mean))+
  geom_line(data=j, aes(t,mean,group=1))+
  geom_vline(xintercept = '0',
             color='black',
             linetype='dashed')+
  facet_grid(event~talk,scales="free_y") +

  scale_y_continuous("% Overlap w/ Public",
                     labels=percent)+
  xlab("Time in Weeks Relative to Event")+
  scale_color_discrete(name=
                         "Type\nof LP")+
  theme(legend.position='bottom')

# Base model - need all time x public x event intersections.
# Mixed models with a cluster random intercept; model comparison shows the
# full three-way interaction (base_model_5) is needed.
base_model_1 <- lmer(polcnt/total~regex_type + (1|grp),data=rq2a)
base_model_2 <- lmer(polcnt/total~t+regex_type+event + (1|grp),data=rq2a)
base_model_3 <- lmer(polcnt/total~t*event+regex_type*t+ (1|grp),data=rq2a)
base_model_5 <- lmer(polcnt/total~t*regex_type*event+ (1|grp),
                     data=rq2a)
anova(base_model_1,
      base_model_2,
      base_model_3,base_model_5)
compare_performance(base_model_1,
                    base_model_2,
                    base_model_3,base_model_5)
# Quick visual fit checks
rq2a[, pred_null := predict(base_model_1)]
rq2a[, pred_base := predict(base_model_5)]
rq2a[, v:=polcnt/total]
ggplot(rq2a, aes(v,pred_null))+geom_point(alpha=.2)
ggplot(rq2a, aes(v,pred_base,color=meta))+
  geom_point(alpha=.2)+
  facet_wrap(~regex_type)

## Adding in high-level types
# Does the type of longitudinal public (meta) moderate the temporal
# response? Again the full interaction model (meta_5) wins.
meta_1 <- lmer(polcnt/total~t*regex_type*event+meta+(1 | grp),
               REML=T,
               data=rq2a)
meta_2 <- lmer(polcnt/total~t*regex_type*event+regex_type*meta+(1 | grp),
               REML=T,
               data=rq2a)
meta_3 <- lmer(polcnt/total~t*regex_type*event+regex_type*meta*t+(1 | grp),
               REML=T,
               data=rq2a)
meta_5 <- lmer(polcnt/total~t*event*regex_type*meta+(1 | grp),
               REML=T,
               data=rq2a)
compare_performance(base_model_5,
                    meta_1,
                    meta_2,
                    meta_3,meta_5)
anova(base_model_5,
      meta_1,
      meta_2,
      meta_3,meta_5)
# Marginal predictions from the winning model (population level, re.form=NA)
z <- data.table(predictions(
  meta_5,
  newdata = datagrid(grp = NA,
                     t = unique(rq2a$t),
                     regex_type=unique(rq2a$regex_type),
                     event=unique(rq2a$event),
                     meta=unique(rq2a$meta)),
  re.form = NA))

z[, talk := ifelse(regex_type =="blm",
                   "BLM Public",
                   "Electoral Public")]
# Dashed reference segments extending the pre-event level across the plot
m <- z[t %in% c("-2","-1")]
m$end <- "+6"
p4 <-ggplot(z, aes(t,estimate, ymin=conf.low,
                   ymax=conf.high,group=talk,color=talk))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_point(size=1.2) +
  geom_linerange()+
  geom_line() +
  geom_vline(xintercept = '0',
             color='black',
             linetype='dashed')+
  facet_grid(event~meta,scales='free_y') +

  scale_y_continuous("% Overlap w/ Public",
                     labels=percent)+
  xlab("Time in Weeks Relative to Event")+
  scale_color_discrete(name=
                         "Public")+geom_segment(data=m,aes(
                           y=ifelse(event=="Election",
                                    conf.low,conf.high),
                           yend = ifelse(event=="Election",
                                         conf.low,conf.high),
                           x=t,
                           xend=end),linetype='3313',alpha=.8) +
  theme(legend.position='bottom')
p4
theme_set(theme_bw(14))
# Saved as two separate figures (formerly panels A and B of rq2a_time.pdf):
#   rq2a_time_raw.pdf: observed per-LP overlap over time (black = mean)
#   rq2a_time_model.pdf: marginal predictions from meta_5 by LP type
ggsave("img/rq2a_time_raw.pdf", basic_plt + theme_bw(14) +
         theme(axis.text.x=element_text(angle=45,hjust=1),
               legend.position='bottom'),
       h=5,w=6.5)
ggsave("img/rq2a_time_model.pdf", p4 + theme_bw(14) +
         theme(axis.text.x=element_text(angle=45,hjust=1),
               legend.position='bottom'),
       h=5,w=8.5)




############## Now look @ demographics #######################

# Collapse to one row per cluster: total overlap with each issue public
rq2a_2 <- infl_w_tweet_count[, list(pol=sum(pol),blm=sum(blm),total=sum(total)),
                             by=.(grp,Label,meta,narrow_label)]
rq2a_2 <- rq2a_2[!(grepl("Hispanic|Venezuelan|Non-US" ,Label) | narrow_label == "No Clear Pattern")]
# Scatter of BLM vs electoral overlap per cluster (panel A of rq2a_audience)
summ <- ggplot(rq2a_2,
               aes(pol/total,blm/total,color=meta,label=Label))+
  geom_point() +
  geom_text_repel() + stat_smooth(method='lm',color=1,group=1)+ stat_cor(group=1,color=1) +
  theme(legend.position='inside',legend.position.inside = c(.25,.75))+
  scale_x_continuous("% Overlap w/ Electoral Issue Public",labels=percent) +
  scale_y_continuous("% Overlap w/ BLM Issue Public",labels=percent) +
  scale_color_discrete(name="Longitudinal Public Type")
ggsave("img/rq2.pdf",summ,h=6,w=11)

#########################################
############### Paper RQ2b: heterogeneity ACROSS longitudinal publics
#########################################
# NOTE on naming: this block is RQ2b in the paper (Section 3.3.4 / 4.3,
# Appendix F, Table F2). The section labelled "RQ2b" further below is RQ2c
# in the paper.
### Adding in demographics
# Long format: one row per cluster x issue public, with the cluster's
# audience demographics attached
rq2a_2 <- merge(melt(rq2a_2,measure=c("pol","blm")),clust_stats,by="grp")
setnames(rq2a_2, c("variable","value"), c("issuepublic","polcnt"))

rq2a_2[, par_diff := rep-dem]
rq2a_2[, v := polcnt/total]
# Binomial GLMs weighted by total tweet count: does each audience
# characteristic predict overlap with the issue publics, beyond LP type?
base <- glm(v~issuepublic*meta,
            data=rq2a_2,
            family="binomial",
            weights=total)
m1 <- update(base, .~.+(age*issuepublic))
m2 <- update(base,.~.+poly(par_diff,2)*issuepublic)
m3 <- update(base, .~.+male*issuepublic)
m4 <- update(base, .~.+log_n*issuepublic)
anova(base,m1)
anova(base,m2)
anova(base,m3)
anova(base,m4)

full <- update(base, .~.+(age+poly(par_diff,2)+male+log_n)*issuepublic)

# Base vs. full model test (reported in Section 4.3). The full model adds 10
# parameters, so the likelihood-ratio test is chi^2(10), not chi^2(1).
anova(base, full, test="Chisq")
# The data are heavily overdispersed relative to the binomial (Pearson
# dispersion ~4,900), which the LR chi^2 test ignores. The quasibinomial
# F-test rescales by the estimated dispersion and is the test to report.
sum(residuals(full, "pearson")^2) / df.residual(full)
anova(update(base, family="quasibinomial"),
      update(full, family="quasibinomial"),
      test="F")

library(sandwich)
library(lmtest)
# Compute clustered SEs (clustered by 'group')
cl_vcov <- vcovCL(full, cluster = ~ grp)

# Cluster-robust coefficient tests
coeftest(full, vcov = cl_vcov)

compare_performance(base,m1,m2,m3,m4,full)
# Population-averaged (G-computation) curves for age and partisanship
# (panels B/C): each focal value is set counterfactually for *all* flocks,
# holding every flock's other characteristics fixed, and the predictions are
# averaged. This is the estimand behind in-text claims of the form "on
# average, overlap rises from X% to Y% across the observed range" (an
# at-means grid would instead describe a single synthetic modal-type flock).
# CIs are delta-method with cluster-robust (by grp) vcov.
ageplt <- plot_predictions(full,
                           by = c("age","issuepublic"),
                           newdata = datagrid(age = seq(min(rq2a_2$age),
                                                        max(rq2a_2$age),
                                                        length.out = 25),
                                              grid_type = "counterfactual"),
                           vcov = ~grp)+
  my_plot_style(ylim=c(NA,NA)) + xlab("Avg. Audience Age")
pardiff_plt <- plot_predictions(full,
                                by = c("par_diff","issuepublic"),
                                newdata = datagrid(par_diff = seq(min(rq2a_2$par_diff),
                                                                  max(rq2a_2$par_diff),
                                                                  length.out = 25),
                                                   grid_type = "counterfactual"),
                                vcov = ~grp)+
  xlab("Audience % Rep. - % Dem.")+
  my_plot_style(ylim=c(NA,NA))

# Numbers for the in-text claim: average predicted overlap at the lowest vs
# highest observed average audience age
avg_predictions(full,
                variables = list(age = range(rq2a_2$age)),
                by = c("age","issuepublic"),
                vcov = ~grp)



theme_set(theme_bw(13))
ggsave("img/rq2a_audience.pdf",summ + (ageplt/pardiff_plt) +
         plot_layout(widths = c(2,1.2))&
         plot_annotation(tag_levels = 'A'),h=7,w=11)

# Regression table for the appendix (Table F2). Binomial GLMs weighted by
# total tweets; 95% CIs from cluster-robust SEs by longitudinal public.
# There are no time/event terms or random effects in these models.
# Written to table_F2.tex (paste into the paper) and printed.
library(modelsummary)
tab_f2 <- modelsummary(models=list("Base"=base,"Full"=full),
             output='latex_tabular',
             se.below = F,
             estimate="{estimate}{stars}",
             statistic="[{conf.low}, {conf.high}]",
             stars=c("*"=.05, "**"=.001),vcov=~grp)
writeLines(as.character(tab_f2), "table_F2.tex")
tab_f2

#################################################
################# RQ2b (code) = RQ2c (paper) #################
#################################################
# Heterogeneity WITHIN longitudinal publics: this is RQ2c in the paper
# (Section 3.3.5 / 4.4, Appendix E.2 and G, Table G3). Object names keep
# the original "rq2b" prefix.

# Account-level analysis: which influencer characteristics predict overlap
# with the issue publics? Restrict to accounts with enough tweets and
# followers for the outcome/predictors to be meaningful, and apply the same
# cluster filter as RQ2a.
rq2b <- infl_w_tweet_count[total >= 5 & followers_count >= 1000]
rq2b <- rq2b[!(grepl("Hispanic|Venezuelan|Non-US" ,Label) |
                 narrow_label == "No Clear Pattern")]

# Per-influencer *audience* demographics: distributions of age, race, party,
# and gender among the panelists who follow each account
basic_demographics <- data.table(
  read_parquet("data/kenny_influencer_data_new_age_info.parquet")
)[,c("influencer_id",
     "age_avg",
     "source_id_count" ,
     "age_std",
     "age_median",
     "race_Asian",
     "race_African-American",
     "race_None",
     "race_Unknown",
     "race_Caucasian",
     "race_Hispanic",
     "race_Other",
     "party_Independent",
     "party_Democrat",
     "party_Republican",
     "gender_Male",
     "gender_Female",
     "gender_None",
     "gender_Unknown"),with=F]
rq2b <- merge(rq2b, basic_demographics, by.x="id",by.y="influencer_id")

# Approximate random-walk betweenness centrality in the follow network
cent <- data.table(read_parquet(CENTRALITY_FILE))
rq2b <- merge(rq2b, cent, by="id")


# Long format: one row per influencer x issue public
rq2b <- melt(rq2b, id=c("id",
                        "meta",
                        "grp",
                        "verified",
                        "description",
                        "username",
                        "name",
                        "followers_count",
                        "following_count",
                        "Label",
                        "total",
                        "age_avg",
                        "gender_Male",
                        "gender_Female",
                        "party_Independent",
                        "party_Democrat",
                        "party_Republican",
                        "approx_rw_centr",
                        "source_id_count"),
             measure=c("pol","blm"))

# create difference variables
rq2b[, par_diff := party_Republican-party_Democrat]
rq2b[, male := gender_Male - gender_Female]
rq2b[, pop := log((followers_count+1)/(following_count+1))]

# drop NAs
rq2b <- rq2b[!is.na(age_avg) & !is.na(par_diff) & !is.na(male)]

# compute contextual values: each predictor is centered within cluster and
# scaled by 2 SDs (cs2, in util.R), so effects are relative to the
# influencer's own flock
rq2b[, cent_pardiff := cs2(par_diff - mean(par_diff)),by=grp]
rq2b[, cent_age :=  cs2(age_avg - mean(age_avg)),by=grp]
rq2b[, cent_male :=  cs2(male - mean(male)),by=grp]
rq2b[, cent_fol :=  cs2(log(followers_count)- mean(log(followers_count))),by=grp]
rq2b[, cent_pop :=  cs2(pop - mean(pop)),by=grp]
rq2b[, cent_bet :=  cs2(approx_rw_centr),by=grp]


#### Modeling
# Binomial fixed-effects GLMs (fixest::feglm) with issue-public and cluster
# fixed effects, weighted by total tweets. Predictors are added in blocks
# (status, then audience age / partisanship / gender, then centrality), with
# compare_performance() guiding what to keep.

# start with a base model for comparison, FEs for variable and group
# no need to interact those, given what we see about their correlation.
base <- feglm(value/total ~ 1 | variable+grp,
              data=rq2b,
              family="binomial",
              weights=~total)

# add status variables (mvsw = stepwise over all subsets of these terms)
m1 <- feglm(value/total ~ mvsw(verified,cent_fol,cent_pop) | variable+grp,
            data=rq2b,
            family="binomial",
            combine.quick=FALSE,
            weights=~total)
etable(m1)
# seems like verified and cent values add unique significance, keep both
# [but not both the centered measures, they're too correlated. Pop seems slightly better]

# effects by type / issue?
m2a <- feglm(value/total ~ (verified + cent_pop) | variable+grp,
             data=rq2b,
             family="binomial",
             combine.quick=FALSE,
             weights=~total)
m2b <- update(m2a, .~(verified+cent_pop)*meta | .)
m2c <- update(m2a, .~(verified+cent_pop)*variable | .)
m2d <- update(m2a, .~(verified+cent_pop)*variable*meta | .)
compare_performance(m2a,m2b,m2c,m2d)

#Public really doesn't have an interesting impact here, but type does. keep it.

age_mod1 <- feglm(value/total ~(verified+cent_pop)*variable*meta+
                    cent_age
                  | variable+grp,
                  data=rq2b,
                  family="binomial",
                  combine.quick=F,
                  weights=~total)
age_mod2 <- update(age_mod1, .~.+variable*cent_age)
age_mod3 <- update(age_mod1, .~.+meta*cent_age)
age_mod4 <- update(age_mod1, .~.+meta*cent_age+variable*cent_age)
age_mod5 <- update(age_mod1, .~.+meta*cent_age*variable)
compare_performance(
  m2d,
  age_mod1,
  age_mod2,
  age_mod3,
  age_mod4,
  age_mod5
)

# Audience partisanship enters quadratically (both very left- and very
# right-leaning audiences can be high-overlap)
pardiff_mod5 <- update(age_mod5, .~.+meta*poly(cent_pardiff,2)*variable)
compare_performance(
  m2d,
  age_mod5,
  pardiff_mod5
)

male_mod5 <- update(pardiff_mod5, .~.+meta*cent_male*variable)
compare_performance(
  m2d,
  age_mod5,
  pardiff_mod5,
  male_mod5
)

full_model <- update(male_mod5, .~.+meta*cent_bet*variable)
compare_performance(
  base,
  m2d,
  age_mod5,
  pardiff_mod5,
  male_mod5,
  full_model
)

# Check the predictors aren't overly collinear
cor(rq2b[,.(cent_pop,cent_pardiff,cent_age,cent_male,approx_rw_centr)])


# Prediction plots for each predictor in the full model (rq2b.pdf panels;
# boot_avg_curves / plot_rq2b / RQ2B_LABELS are in util.R)
#ggplot(rq2b, aes_string("cent_male", color="meta"))+geom_density()

# One flock-cluster bootstrap shared by all six panels: each replicate
# refits the model once and computes the five continuous curves plus the
# verified predictions from that refit. All panels are population-averaged
# (G-computation) with flock-bootstrap percentile CIs. This is the slow
# step (~R model refits); lower R or n_avg to iterate.
# Replicates run in parallel (forked workers; set BOOT_CORES <- 1 on
# Windows). The L'Ecuyer RNG gives each replicate its own reproducible
# stream for a given BOOT_CORES.
#
# The bootstrap output is cached in BOOT_CACHE, so the figure can be
# restyled/resized without rerunning it. The cache stores the full model's
# coefficients and is only reused if they match the current fit (i.e. the
# data, centrality file, and specification are unchanged); otherwise the
# bootstrap reruns and overwrites it. Delete the file to force a rerun.
BOOT_CACHE <- "data/rq2b_boot_curves.rds"
BOOT_CORES <- max(1, parallel::detectCores() - 2)
cache_ok <- FALSE
if (file.exists(BOOT_CACHE)) {
  boot_cache <- readRDS(BOOT_CACHE)
  cache_ok <- isTRUE(all.equal(boot_cache$coef, coef(full_model)))
  if (!cache_ok) message("Bootstrap cache is from a different model fit; rerunning")
}
if (cache_ok) {
  rq2b_curves <- boot_cache$curves
} else {
  RNGkind("L'Ecuyer-CMRG")
  set.seed(779)
  rq2b_curves <- boot_avg_curves(full_model, rq2b,
                                 c("cent_pop","cent_pardiff","cent_age",
                                   "cent_male","cent_bet"),
                                 discrete_vars = "verified",
                                 R = 1000, cores = BOOT_CORES)
  RNGkind("default")
  saveRDS(list(coef = coef(full_model), curves = rq2b_curves,
               R = 1000, cores = BOOT_CORES, seed = 779),
          BOOT_CACHE)
}

# Verified panel: population-averaged predictions by verified x meta x
# issue public - same estimand as the curve panels, discrete x-axis
verified <- ggplot(rq2b_curves[focal == "verified" & meta !="National\nPolitics"],
                   aes(factor(value), estimate, ymin=conf.low, ymax=conf.high,
                       color=meta))+
  geom_pointrange(position=position_dodge(width=.4))+
  facet_wrap(~variable,scales="free_y",
             labeller =
               as_labeller(RQ2B_LABELS))+
  scale_x_discrete(name="Verified",labels=c("No","Yes"))+
  scale_color_discrete(name="Longitudinal Public Type")+
  scale_y_continuous(name= "",labels=percent) +
  labs(fill = NULL, color = NULL, linetype = NULL)+
  theme(legend.position='none')

pop_plt <- plot_rq2b(rq2b_curves,"cent_pop","Log(Follower/Friends)","B")
pardiff_plt <- plot_rq2b(rq2b_curves,"cent_pardiff","Avg. Audience % Rep. - % Dem.","C")
age_plt <- plot_rq2b(rq2b_curves,"cent_age","Avg. Audience Age","D")
male_plt <- plot_rq2b(rq2b_curves,"cent_male","Avg. Audience % Male","E")
cent_plt <- plot_rq2b(rq2b_curves,"cent_bet","Approx. Bet. Centr.","F")
# Two columns x three rows (panels A-F read left to right, top to bottom),
# with the shared legend at the bottom
theme_set(theme_bw(14))
ggsave("img/rq2b.pdf",
       verified + guides(color = 'none') +
         pop_plt + pardiff_plt + age_plt + male_plt + cent_plt +
         plot_layout(ncol = 2, guides = 'collect') +
         plot_annotation(tag_levels = 'A') &
         theme(legend.position = 'bottom'),
       h=11,w=10)

# Fit diagnostics: predicted vs observed for base and full models
rq2b[, pred_full := predict(full_model)]
rq2b[, pred_base := predict(base)]
rq2b[, true := value/total]

library(GGally)
ggpairs(sample_n(rq2b[,.(true,pred_full,pred_base)],10000))


# Regression table for the appendix (Table G3).
# vcov = ~grp: cluster-robust SEs by longitudinal public (141 clusters).
# Without it, fixest clusters on the FIRST fixed effect (`variable`, the
# issue public), i.e. on only 2 clusters, which gives far too narrow CIs.
# Clustering by grp also absorbs the substantial overdispersion (Pearson
# dispersion ~11).
# Observations = creator x issue-public rows (2 per creator), after the
# >=5 tweets / >=1000 followers filter, the LP filter, and dropping
# creators with missing audience demographics.
# Fit stats: adjusted pseudo R2 (the "adjusted R2" in Section 4.4), within
# pseudo R2 (the "11.2% of within-public variation"), squared correlation.
nrow(rq2b); uniqueN(rq2b$id)
fitstat(full_model, ~ apr2 + wpr2 + sq.cor + rmse)
# Written to table_G3.tex (paste into the paper) and printed.
# NOTE: par_diff / cent_pardiff = % Republican - % Democrat, so the label is
# Rep-Dem (the earlier "Prop.Dem-Prop.Rep" label had the sign backwards).
g3_dict <- c("value/total"="Proportion of Discourse Overlap",
             "variable"="Issue Public Type",
             "grp"="Longitudinal Public ID",
             "cent_male"="Prop.Male-Prop.Female",
             "cent_bet"="Approx.Bet.Cent.",
             "cent_age"="Avg.Age",
             "poly(cent_pardiff,2)1"="Prop.Rep-Prop.Dem",
             "poly(cent_pardiff,2)2"="(Prop.Rep-Prop.Dem)$^2$",
             "cent_pop"="Log(followers/following)",
             "variableblm"="BLM IP",
             "variablepol"="Electoral IP",
             "metaNational\nPolitics"="Nat.Politics LP",
             "metaOthers"="Other LP",
             "verifiedTRUE"="IsVerified")
etable(base,full_model,tex=T,
       file = "table_G3.tex", replace = TRUE,
       vcov = ~grp,
       fitstat = ~ n + apr2 + wpr2 + sq.cor + bic,
       se.below = F,
       signif.code=c("**"=.001, "*"=.05),
       coefstat="confint",
       dict=g3_dict)
etable(base,full_model,tex=T,
       vcov = ~grp,
       fitstat = ~ n + apr2 + wpr2 + sq.cor + bic,
       se.below = F,
       signif.code=c("**"=.001, "*"=.05),
       coefstat="confint",
       dict=g3_dict)


#### Robustness of model selection (Appendix E.3)
# The stepwise selection above used compare_performance(), i.e. AIC/BIC on
# the binomial likelihood. That likelihood ignores overdispersion (Pearson
# dispersion ~11), so we re-check every selection step two ways:
#   (1) QAIC/QBIC: -2 logLik divided by the estimated dispersion c_hat
#   (2) Wald F-tests of the added interaction terms, cluster-robust by
#       longitudinal public (denominator df = #LPs - 1)
# Result: QAIC/QBIC choose the same model at every step, and the Wald tests
# support every retained interaction EXCEPT the betweenness x issue public
# x LP-type 3-way terms (p ~ .17 with the recomputed shared-count
# centralities; p ~ .23 with the original file). The model is kept as is;
# the paper notes that exception.
n_lp <- uniqueN(rq2b$grp)
c_hat <- sum(resid(full_model, "pearson")^2) /
  (nobs(full_model) - length(coef(full_model)) - n_lp - 1)
c_hat

# Main-effect-only alternatives for the steps that went straight to the
# 3-way interaction
par_main  <- update(age_mod5, .~.+poly(cent_pardiff,2))
male_main <- update(pardiff_mod5, .~.+cent_male)
bet_main  <- update(male_mod5, .~.+cent_bet)

q_ic <- function(m) {
  k <- length(coef(m)); ll <- as.numeric(logLik(m))
  c(k = k, QAIC = -2*ll/c_hat + 2*k, QBIC = -2*ll/c_hat + log(nobs(m))*k)
}
sel_mods <- list(m2a=m2a, m2b=m2b, m2c=m2c, m2d=m2d,
                 age_mod1=age_mod1, age_mod2=age_mod2, age_mod3=age_mod3,
                 age_mod4=age_mod4, age_mod5=age_mod5,
                 par_main=par_main, pardiff_mod5=pardiff_mod5,
                 male_main=male_main, male_mod5=male_mod5,
                 bet_main=bet_main, full_model=full_model)
round(t(sapply(sel_mods, q_ic)), 1)

# Joint cluster-robust Wald F-test of all coefficients whose names match
# `pat` (perl regex; (?s) because the LP level "National\nPolitics" contains
# a newline)
wald_grp <- function(m, pat, label) {
  s <- summary(m, cluster = ~grp)
  b <- coef(s); V <- vcov(s)
  i <- grep(pat, names(b), perl = TRUE)
  stat <- as.numeric(t(b[i]) %*% solve(V[i, i]) %*% b[i])
  data.table(test = label, k = length(i), F = stat/length(i),
             p = pf(stat/length(i), length(i), n_lp - 1, lower.tail = FALSE))
}
any_int <- function(x) paste0("(?s)(?=.*:)(?=.*(", x, "))")
three_way <- function(x) paste0("(?s)(?=.*variableblm)(?=.*meta)(?=.*(", x, "))")
rbind(
  wald_grp(m2b, "(?s)^(verifiedTRUE|cent_pop):meta", "status x LP type (m2b vs m2a)"),
  wald_grp(m2c, "^(verifiedTRUE|cent_pop):variable", "status x IP (m2c vs m2a)"),
  wald_grp(m2d, "^(verifiedTRUE|cent_pop):", "all status interactions (m2d vs m2a)"),
  wald_grp(m2d, three_way("verifiedTRUE|cent_pop"), "status 3-way"),
  wald_grp(age_mod5, any_int("cent_age"), "all age interactions (age_mod5 vs age_mod1)"),
  wald_grp(age_mod5, three_way("cent_age"), "age 3-way (age_mod5 vs age_mod4)"),
  wald_grp(pardiff_mod5, any_int("cent_pardiff"), "all partisanship interactions"),
  wald_grp(pardiff_mod5, three_way("cent_pardiff"), "partisanship 3-way"),
  wald_grp(male_mod5, any_int("cent_male"), "all % male interactions"),
  wald_grp(male_mod5, three_way("cent_male"), "% male 3-way"),
  wald_grp(full_model, any_int("cent_bet"), "all betweenness interactions"),
  wald_grp(full_model, three_way("cent_bet"), "betweenness 3-way")
)

#### National Politics slopes (Section 4.4 text; not shown in Figure 6)
# Slope of each creator variable for National Politics LPs, per issue public:
# sum of the main effect and its interactions, with cluster-robust SEs.
np_slopes <- function(m, x) {
  s <- summary(m, cluster = ~grp); b <- coef(s); V <- vcov(s)
  np <- "metaNational\nPolitics"
  terms <- list(Electoral = c(x, paste0(x, ":", np)),
                BLM = c(x, paste0(x, ":", np), paste0(x, ":variableblm"),
                        paste0(x, ":variableblm:", np)))
  rbindlist(lapply(names(terms), function(ip) {
    tt <- terms[[ip]]
    # fixest may order terms differently; match each by its parts
    idx <- sapply(tt, function(t) {
      parts <- strsplit(t, ":", fixed = TRUE)[[1]]
      hit <- which(sapply(strsplit(names(b), ":", fixed = TRUE),
                          function(n) setequal(n, parts)))
      stopifnot(length(hit) == 1); hit
    })
    w <- rep(1, length(idx))
    est <- sum(b[idx]); se <- sqrt(as.numeric(t(w) %*% V[idx, idx] %*% w))
    data.table(var = x, issue_public = ip, est = est, se = se,
               p = 2*pnorm(-abs(est/se)))
  }))
}
rbindlist(lapply(c("verifiedTRUE", "cent_pop", "cent_male"),
                 function(x) np_slopes(full_model, x)))

# Robustness check: same full model but with the raw (uncentered) audience
# variables added, to confirm the contextual effects aren't artifacts of
# the within-cluster centering
all_vars_model <- update(male_mod5, .~.+meta*cent_bet*variable+
                           meta*variable*(poly(par_diff,2)+
                                            age_avg+
                                            male+
                                            pop+approx_rw_centr))
