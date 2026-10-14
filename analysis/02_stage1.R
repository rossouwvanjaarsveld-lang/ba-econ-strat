# =============================================================================
#  analysis/02_stage1.R -- Stage 1: macro -> sector satellite models
#    volumes (BA900 sector totals, 2008Q3-2026Q2) and pricing, credit and
#    other P&L ratios (BA120/BA100, 2008Q3-2025Q2)
# =============================================================================
source("analysis/00_config.R")
P <- readRDS(out_path("panels.rds"))
pan <- P$s1pan

fit_args <- list(sample_from = CFG$sample_from, max_k = CFG$max_k, lags = CFG$lags,
                 h = CFG$h_oos, min_train = CFG$min_train)
vol <- do.call(fit_stage1, c(list(pan, stage1_targets(), controls = c("covid_q2", "covid_q3")), fit_args))
pnl <- do.call(fit_stage1, c(list(pan, stage1_pnl_targets(), controls = c("covid_q2", "covid_q3")), fit_args))
sats <- c(vol, pnl)

summ <- stage1_summary(sats)
summ$family <- vapply(sats[summ$target], function(s) s$family %||% "", "")
bma <- do.call(rbind, lapply(names(sats), function(nm) {
  b <- satellite_bma_summary(sats[[nm]]); if (!nrow(b)) return(NULL); b$target <- nm; b }))
fitted <- do.call(rbind, lapply(names(sats), function(nm) {
  f <- sats[[nm]]$fit; data.frame(target = nm, date = f$dates, actual = f$y, fitted = f$fitted) }))

guard(all(is.finite(summ$rmse_oos)), "a satellite has no out-of-sample evaluation")
saveRDS(list(sats = sats, summary = summ, bma = bma, fitted = fitted, args = fit_args),
        out_path("stage1.rds"))
ngfb_log("stage 1: %d satellites, median OOS skill vs AR %.1f%%", nrow(summ), 100 * stats::median(summ$skill))
