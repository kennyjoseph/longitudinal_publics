library(arrow)
library(bit64)
library(data.table)
library(dplyr)
library(ggplot2)
#library(ggpubfigs)
library(ggpubr)
library(ggrepel)
library(ggridges)
library(glue)
library(Hmisc)
library(lubridate)
library(marginaleffects)
library(plyr)
library(scales)
library(tidyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(fixest)
library(performance)
library(mgcv)
library(patchwork)
theme_set(theme_bw(20))
options(arrow.skip_nul = TRUE)


cs2 <- function(x, na.rm = TRUE) {
  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  (x - m) / (2 * s)
}



get_panel <- function(){
  panel_dem <- fread("data/TSmart-cleaner-Oct2017-rawFormat.tsv")
  panel_info <- read_simple_user_info("data/panel_7_7_20.tsv")
  panel_clust <- fread("data/z_150.csv")

  panel_clust <- merge(panel_clust,
                          get_cluster_names(),
                          by.x="grp",by.y="k", all.x=T)
  panel <- merge(panel_dem, panel_clust, 
                      by.x="twProfileID",by.y="id")
  panel <- merge(panel, panel_info, by.x="twProfileID",by.y="uid")
  return(panel)
}

get_influencer_data <- function(){
  infl_clust <- fread("data/y_150_wval.csv")

  ## This is to incorporate new bios from the decahose
  ## Just updating bios that we didn't otherwise have in the data
  new_bios <- data.table(read_parquet("data/new_bios_decahose.parquet"))
  setnames(new_bios, "uid","id")
  setnames(new_bios, "screen_name","username")
  setnames(new_bios, "url","expanded_url")
  new_bios[, id := as.integer64(id)]
  new_bios[, created_at := strptime(created_at, 
                                    "%a, %d %b %Y %H:%M:%S %z", 
                                    tz = "GMT")]
  new_bios <- merge(new_bios, infl_clust[,.(id,grp,grp_val)])

  infl_clust_min <- infl_clust[!id %in% new_bios$id]
  infl_clust <- rbind(infl_clust_min,new_bios,fill=T)

  infl_clust <- merge(infl_clust,clust_names,by.x="grp",by.y="k")
  infl_clust[ , sn_lower := tolower(username)]
  return(infl_clust)
}

get_cluster_names <- function(){
  # Load cluster info
  clust_names <- fread("data/cluster_labels.csv",sep="\t")
  clust_names[, filt_meta := ifelse(Metalabel1 %in% 
                                      c("Local","Political","Entertainment",
                                        "Sports","Business","Celeb/Influencer"),
                                    Metalabel1,"Other")]
  clust_names[, filt_meta := ifelse(filt_meta %in% c("Entertainment",
                                                    "Celeb/Influencer"),
                                    "Entertain/Influencer",
                                    filt_meta)]
  clust_names[, filt_meta := ifelse(filt_meta %in% c("Local",
                                                    "News"),
                                    "(Local) News",
                                    filt_meta)]
  clust_names[, filt_meta := factor(filt_meta,
                                    levels=c("Political","(Local) News",
                                            "Sports","Entertain/Influencer",
                                            "Business","Other"))]
  return(clust_names)

}

compute_logodds <- function(c1,c2,a1=1,a2=1){
  c1_s = c1 + a1
  c2_s = c2 + a2
  d <- (log( c1_s / (sum(c1) + a1*length(c1)- c1_s )) - 
          log( c2_s / (sum(c2) + a2*length(c2)- c2_s )))
  sigma = (1./c1_s) + (1./c2_s)
  val = d/sqrt(sigma)
  return(data.frame(delta=d,sigma=sigma,val=val))
}

read_simple_user_info <- function(inFile){
  
  r = readBin(inFile, raw(), file.info(inFile)$size)
  r[r==as.raw(0)] = as.raw(0x20)
  tfile = tempfile(fileext=".txt")
  writeBin(r, tfile)
  rm(r)
  inFile = tfile
  
  return(fread(inFile, sep="\t",col.names =c("uid",
                                             'name',
                                             "screen_name",
                                             'url',
                                             'protected',
                                             'location',
                                             'description',
                                             "followers_count",
                                             "friends_count",
                                             "created_at",
                                             "utc_offset",
                                             'time_zone',
                                             "statuses_count",
                                             "lang",
                                             "status_created_at",
                                             'status_coordinates',
                                             "status_lang",
                                             "profile_image_url_https","verified")))
}



get_data_from_date <- function(d,date_cut_str,name_date){
  date_cut = ymd(date_cut_str)
  ct <- d[date >  date_cut-22 & 
            date < date_cut+7*7]
  ct[, t := ifelse(date < date_cut-14,"pre2",
       ifelse(date < date_cut-7,"pre1",
              ifelse(date >= date_cut+42,"post6",
              ifelse(date >= date_cut+35,"post5",
              ifelse(date >= date_cut+28,"post4",
                   ifelse(date >= date_cut+21,"post3",
                          ifelse(date >= date_cut+14,"post2",
                                 ifelse(date >= date_cut+7,"post","during"))))))))]
  
  k <- ct[, list(cnt=sum(V1)), 
          by=.(grp,t,regex_type)]
  
  
  k[, t := factor(t, levels=c("pre2","pre1","during","post","post2",
                              "post3","post4","post5","post6"),
                   labels=c(
                     "-2",
                     "-1",
                     "0",
                            "+1",
                            "+2",
                            "+3",
                     "+4",
                     "+5",
                     "+6"))]
  k <- merge(k[regex_type != "total"], k[regex_type == "total",.(grp,t,cnt)], by=c("grp","t"))
  k$event <- name_date
  setnames(k, c("cnt.x","cnt.y"), c("polcnt","total"))
  return(k)
}



my_plot_style <- function(ylab="% Tweets Overlapping with Public ",
                          ylim=c(-.15,.65)) {
  list(
    scale_color_manual(name="Public",
                       values=c(blm="orange",
                                pol="purple"),limits=c("blm","pol"),
                       labels=c("BLM","Electoral")),
    scale_fill_manual(name="Public",
                      values=c(blm="orange",
                               pol="purple"),limits=c("blm","pol"),
                      labels=c("BLM","Electoral")),
    scale_y_continuous(name=ylab, label=percent,limits=ylim) 
  )
}

RQ2B_LABELS <- c(pol="Electoral",blm="BLM")

# Generic flock-cluster bootstrap.
#
# For each replicate: flocks (grp) are resampled with replacement and
# relabeled (so a flock drawn twice acts as two distinct clusters), the model
# is refit on the resample, and FUN(model, data) is evaluated on the refit.
# FUN must return a data.table with an `estimate` column plus any number of
# id columns; percentile CIs are computed per unique id-column combination.
# Point estimates come from the original model/data. Resamples that fail to
# refit (e.g. a scarce level dropped) are skipped and counted.
#
# The model must have been fit with weights as a formula (weights=~total) so
# that update(model, data=...) resolves weights from the resampled data.
boot_flock <- function(model, dat, FUN, R = 1000){
  point <- FUN(model, dat)
  id_cols <- setdiff(names(point), "estimate")

  last_err <- NULL
  boots <- rbindlist(lapply(seq_len(R), function(i){
    gs <- sample(unique(dat$grp), replace = TRUE)
    bd <- rbindlist(lapply(seq_along(gs), function(j){
      x <- copy(dat[grp == gs[j]])
      x[, grp := j]
      x
    }))
    bm <- try(update(model, data = bd), silent = TRUE)
    if (inherits(bm, "try-error")){
      last_err <<- conditionMessage(attr(bm, "condition"))
      return(NULL)
    }
    res <- try(FUN(bm, bd), silent = TRUE)
    if (inherits(res, "try-error")){
      last_err <<- conditionMessage(attr(res, "condition"))
      return(NULL)
    }
    res[, rep := i]
  }))
  if (nrow(boots) == 0)
    stop("all ", R, " replicates failed; last error was:\n  ", last_err,
         "\n(a common cause: the model was fit with a weights *vector* - ",
         "refit it with weights=~total so update() works on resampled data)")
  n_ok <- length(unique(boots$rep))
  if (n_ok < R) message(R - n_ok, " of ", R,
                        " replicates failed and were skipped; last error: ",
                        last_err)

  cis <- boots[, .(conf.low  = quantile(estimate, .025),
                   conf.high = quantile(estimate, .975)),
               by = id_cols]
  merge(point, cis, by = id_cols)
}

# Flock-cluster bootstrap for population-averaged (G-computation) prediction
# curves, for any set of continuous predictors in the model.
#
# Every variable's curve is computed from the same refit per replicate -
# refitting is the expensive step, so all variables share it. Curves are
# counterfactual averages: each grid value is set for all rows (holding
# everything else about each influencer fixed) and predictions are averaged
# by (value, meta, variable). Grid points are fixed quantiles of the
# *original* data so replicates line up.
#
# Args (beyond boot_flock's):
#   varnames - character vector of continuous predictors to sweep
#   discrete_vars - optional discrete predictors (e.g. verified); swept over
#              their unique observed values instead of a quantile grid, but
#              otherwise treated exactly like the continuous curves:
#              population-averaged predictions by (value, meta, variable),
#              from the same refits. Logical values appear in `value` as 0/1
#   grid_n   - number of grid points per variable
#   n_avg    - rows used as the averaging population within each replicate;
#              subsampling trades a little Monte Carlo noise for a large
#              speedup (Inf = use all rows)
#   grid_probs - quantile range the grid spans (trims extreme tails)
boot_avg_curves <- function(model, dat, varnames,
                            discrete_vars = NULL,
                            R = 1000, grid_n = 20, n_avg = 20000,
                            grid_probs = c(.005, .995)){
  grids <- lapply(varnames, function(v)
    unname(quantile(dat[[v]], seq(grid_probs[1], grid_probs[2],
                                  length.out = grid_n))))
  names(grids) <- varnames

  FUN <- function(m, d){
    avg_pop <- if (nrow(d) > n_avg) d[sample(.N, n_avg)] else d
    curves <- rbindlist(lapply(varnames, function(v){
      est <- avg_predictions(m,
                             variables = grids[v],
                             by = c(v, "meta", "variable"),
                             newdata = avg_pop,
                             vcov = FALSE)
      out <- data.table(est)[, .(value = get(v), meta, variable, estimate)]
      out[, focal := v]
      out
    }))
    if (is.null(discrete_vars)) return(curves)
    discretes <- rbindlist(lapply(discrete_vars, function(v){
      vals <- sort(unique(d[[v]]))
      est <- avg_predictions(m,
                             variables = setNames(list(vals), v),
                             by = c(v, "meta", "variable"),
                             newdata = avg_pop,
                             vcov = FALSE)
      out <- data.table(est)[, .(value = as.numeric(get(v)),
                                 meta, variable, estimate)]
      out[, focal := v]
      out
    }))
    rbindlist(list(curves, discretes))
  }

  boot_flock(model, dat, FUN, R = R)
}

# Plot one variable's curve from a boot_avg_curves() result
plot_rq2b <- function(boot_res,
                      varname,
                      xlab_name,
                      title_letter = NULL){
  z <- boot_res[focal == varname]

  ggplot(z, aes(x=value,y=estimate,ymin=conf.low,ymax=conf.high,
                color=meta,fill=meta))+geom_line() +
    geom_ribbon(alpha=.2,linewidth = 0)+
    facet_wrap(~variable,scales="free_y",
               labeller =
                 as_labeller(RQ2B_LABELS))+
    scale_x_continuous(name=xlab_name)+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    scale_color_discrete(name="Longitudinal\nPublic\nType")+
    scale_fill_discrete(name="Longitudinal\nPublic\nType")+
    scale_y_continuous(name="",
                       labels=percent)
}


