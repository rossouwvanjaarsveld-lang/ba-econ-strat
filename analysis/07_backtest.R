# =============================================================================
#  analysis/07_backtest.R -- conditional (macro-known) pseudo-out-of-sample test
#
#  The question a scenario tool must answer is: IF the macro path were known,
#  would the satellites have tracked the sector? From two origins -- the eve
#  of COVID (2019Q4) and the eve of the 2022-23 hiking cycle (2021Q4) -- every
#  Stage 1 model is re-estimated on data up to the origin only, then projected
#  eight quarters along the REALISED macro path and compared with outcomes and
#  with a no-change benchmark. Ratio targets are projected with and without
#  the intercept correction, which is how the correction was chosen, and with
#  three ways of using the specification search: the BIC-weighted model
#  average (used in the scenarios), an equal-weighted combination of the five
#  best specifications by out-of-sample RMSE, and the single best one.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
pan <- P$s1pan
origins <- as.Date(c("2019-12-31", "2021-12-31"))
H <- 8L

bt <- list()
for (k in seq_along(origins)) {
  o <- origins[k]
  tr <- pan[pan$date <= o, ]
  args <- S1$args; args$min_train <- min(args$min_train, 24L)
  v <- do.call(fit_stage1, c(list(tr, stage1_targets(), controls = c("covid_q2", "covid_q3")), args))
  p <- do.call(fit_stage1, c(list(tr, stage1_pnl_targets(), controls = c("covid_q2", "covid_q3")), args))
  sats <- c(v, p)
  fut <- pan[pan$date > o, ][seq_len(H), ]
  drv <- unique(unlist(lapply(sats, function(s) c(s$spec$d, unlist(lapply(s$bma$specs, `[[`, "d")),
                                                  unlist(lapply(s$comb$specs, `[[`, "d"))))))
  path <- locf_tail(fut, drv)
  pr0 <- project_satellites(sats, tr, path, ic_window = 0L)
  pic <- function(mode, method = "bma") project_satellites(sats, tr, path, method = method, ic_window = CFG$ic_window,
                                                           ic_families = CFG$ic_families, ic_mode = mode)
  pr4 <- pic(CFG$ic_mode)
  prl <- pic("level"); pre <- pic("equation")
  prc <- pic(CFG$ic_mode, "comb")
  prb <- pic(CFG$ic_mode, "best")
  for (nm in names(sats)) {
    tg <- sats[[nm]]$target
    last <- utils::tail(tr[[tg]][is.finite(tr[[tg]])], 1)
    bt[[length(bt) + 1L]] <- data.frame(origin = o, target = nm, family = sats[[nm]]$family %||% "",
                                        date = fut$date, h = seq_len(H), actual = fut[[tg]],
                                        model = pr0[[tg]], model_ic = pr4[[tg]], model_ic_level = prl[[tg]],
                                        model_ic_equation = pre[[tg]], model_comb = prc[[tg]],
                                        model_best = prb[[tg]], naive = last)
  }
}
bt <- do.call(rbind, bt)
rmse <- function(a, b) { ok <- is.finite(a) & is.finite(b); sqrt(mean((a[ok] - b[ok])^2)) }
acc <- do.call(rbind, lapply(split(bt, list(bt$origin, bt$target), drop = TRUE), function(z) {
  data.frame(origin = z$origin[1], target = z$target[1], family = z$family[1],
             n = sum(is.finite(z$actual) & is.finite(z$model)),
             rmse_model = rmse(z$actual, z$model), rmse_model_ic = rmse(z$actual, z$model_ic),
             rmse_ic_level = rmse(z$actual, z$model_ic_level), rmse_ic_equation = rmse(z$actual, z$model_ic_equation),
             rmse_comb = rmse(z$actual, z$model_comb), rmse_best = rmse(z$actual, z$model_best),
             rmse_naive = rmse(z$actual, z$naive))
}))
acc$skill <- 1 - acc$rmse_model / acc$rmse_naive
acc$skill_ic <- 1 - acc$rmse_model_ic / acc$rmse_naive
acc$skill_used <- ifelse(acc$family %in% CFG$ic_families, acc$skill_ic, acc$skill)
acc$skill_comb <- 1 - acc$rmse_comb / acc$rmse_naive
acc$skill_best <- 1 - acc$rmse_best / acc$rmse_naive
acc$skill_ic_level <- 1 - acc$rmse_ic_level / acc$rmse_naive
acc$skill_ic_equation <- 1 - acc$rmse_ic_equation / acc$rmse_naive
saveRDS(list(path = bt, accuracy = acc, origins = origins, horizon = H), out_path("backtest.rds"))
ngfb_log("backtest: model beats no-change in %d of %d target-origins (ratio targets with intercept correction)",
         sum(acc$skill_used > 0, na.rm = TRUE), sum(is.finite(acc$skill_used)))
ngfb_log("backtest: median skill BIC-BMA %.1f%%, OOS top-5 combination %.1f%%, single best %.1f%%",
         100 * stats::median(acc$skill_used), 100 * stats::median(acc$skill_comb),
         100 * stats::median(acc$skill_best))
