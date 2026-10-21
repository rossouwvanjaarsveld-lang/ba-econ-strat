# =============================================================================
#  analysis/09_monthly.R -- monthly frequency on the perfect-foresight footing
#
#  The macro path is taken as known (perfect foresight): no model here
#  forecasts or nowcasts a macro variable. The question is which frequency
#  recovers the macro -> balance-sheet relationships better.
#
#  1. The monthly macro panel: monthly SARB and FRED series; quarterly real
#     GDP, unemployment and house prices disaggregated to months by the
#     Kalman filter up to their last published quarter only. The GDP
#     indicators are chosen in sample (AIC of the disaggregation model); the
#     same quarters without indicators give the smooth monthly path a
#     quarterly scenario implies.
#  2. Monthly sector targets: BA900 (every month), BA100 and BA120.
#  3. Monthly satellites: the same registry, signs and search as the
#     quarterly ones, lags of 0-12 months, selection on 36-month conditional
#     simulations; and the same satellites fed only the smooth GDP path.
#  4. One scenario back-test: from each quarter-end origin every model is
#     re-estimated and simulated twelve quarters along the REALISED macro
#     path, scored on the same quarterly outcomes: quarterly and monthly
#     ARDL averages, polynomial distributed lags, Bayesian shrinkage and,
#     as the no-macro reference, the AR(1).
#  (Sign restrictions, lag profiles and dynamic multipliers follow in
#  09b_dynamics.R.)
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))

## ---- 1. monthly macro panel --------------------------------------------------------
kbp  <- read_kbp_cache(file.path(CFG$raw, "macro"))
fred <- read_fred_cache(file.path(CFG$raw, "fred"))
mm <- build_macro_panel_monthly(kbp, fred, from = "2005-01-01")
td <- attr(mm, "disaggregation")
guard(identical(td$gdp$indicators, c("coinc", "mfg", "retail")), "unexpected GDP indicators")
## the quarterly sums of the monthly GDP path reproduce the published quarters
gq <- fred[fred$name == "gdp_real", c("date", "value")]
gm <- td$gdp_monthly
guard(max(gm$date) == as.Date(sprintf("%s-%02d-01", format(max(gq$date), "%Y"),
                                      (as.integer(format(max(gq$date), "%m")) - 1L) %/% 3L * 3L + 3L)),
      "the monthly GDP path runs beyond the last published quarter")
gm$q <- as.Date(sprintf("%s-%02d-01", format(gm$date, "%Y"), (as.integer(format(gm$date, "%m")) - 1L) %/% 3L * 3L + 1L))
agg <- stats::aggregate(list(sum = gm$gdp_m, n = gm$gdp_m), list(q = gm$q), function(x) c(sum(x), length(x)))
agg <- data.frame(q = agg$q, sum = agg$sum[, 1], n = agg$sum[, 2])
agg <- merge(agg[agg$n == 3, ], data.frame(q = month_start(gq$date), value = gq$value), by = "q")
guard(max(abs(agg$sum / agg$value - 1)) < 1e-10, "monthly GDP does not add up to the quarters")
## which indicators? chosen in sample: the disaggregation model's AIC on the
## published quarters (no out-of-sample nowcast: nothing is forecast)
ks <- function(code) { s <- kbp[kbp$series == code, c("date", "value")]; s$date <- month_start(s$date); s }
q_last <- max(gq$date[is.finite(gq$value)])
to_m <- as.Date(sprintf("%s-%02d-01", format(q_last, "%Y"), (as.integer(format(q_last, "%m")) - 1L) %/% 3L * 3L + 3L))
tdm <- seq(as.Date("2000-01-01"), to_m, by = "month")
ind <- data.frame(date = tdm)
codes <- c(coinc = "DIFN002A", mfg = "MPR0000B", retail = "MAN5008F")
for (nm in names(codes)) { s0 <- ks(codes[[nm]]); ind[[nm]] <- s0$value[match(tdm, s0$date)] }
td_cfg <- unlist(lapply(list(character(0), "coinc", c("mfg", "retail"), c("coinc", "mfg", "retail")), function(st)
  lapply(c("chow-lin", "fernandez", "litterman"), function(md) list(set = st, model = md))), recursive = FALSE)
qq <- gq[is.finite(gq$value) & gq$date >= as.Date("2000-01-01") & gq$date <= q_last, ]
qq <- qq[order(qq$date), ]
td_fit <- do.call(rbind, lapply(td_cfg, function(cf) {
  X <- if (length(cf$set)) log(as.matrix(ind[, cf$set, drop = FALSE])) else NULL
  f <- td_disaggregate(log(qq$value / 3), as.Date("2000-01-01"), X = X, nm = 3L * nrow(qq),
                       conversion = "average", model = cf$model)
  ## how much of the quarterly variation the aggregated indicators explain
  ## (growth rates, R^2 of the GLS fit's systematic part)
  fq <- colMeans(matrix(drop(cbind(1, X) %*% f$beta), 3))
  r2 <- if (length(cf$set)) stats::cor(diff(fq), diff(log(qq$value / 3)))^2 else 0
  data.frame(indicators = if (length(cf$set)) paste(cf$set, collapse = " + ") else "none", model = cf$model,
             quarters = nrow(qq), aic = f$aic, loglik = f$loglik, phi = f$phi, r2_growth = r2,
             stringsAsFactors = FALSE)
}))
td_fit$d_aic <- td_fit$aic - min(td_fit$aic)
chosen <- td_fit$indicators == paste(td$gdp$indicators, collapse = " + ") & td_fit$model == td$gdp$model
guard(sum(chosen) == 1 && td_fit$d_aic[chosen] == 0, "the production GDP disaggregation is not the in-sample (AIC) choice")

## ---- 2. monthly targets ------------------------------------------------------------
## P&L (pricing, credit, fees, costs): BA120 is monthly; BA100 stocks are
## monthly after the monthly pull, otherwise interpolated between quarter
## ends. Volumes: BA900 every month after the monthly pull, otherwise the
## SARB's monthly credit statistics (all monetary institutions, households
## and companies), mapped to the six lending products -- compared with
## quarterly versions of the same series.
f9 <- file.path(CFG$raw, "sarb/BA900m_long.rds"); f1 <- file.path(CFG$raw, "sarb/BA100m_long.rds")
vol_source <- if (file.exists(f9)) "BA900" else "KBP"
b100_m <- if (file.exists(f1)) readRDS(f1) else readRDS(file.path(CFG$raw, "sarb/BA100_long.rds"))
spnl_m <- sector_pnl_panel_monthly(b100_m, readRDS(file.path(CFG$raw, "sarb/BA120_long.rds")))
pnl_source <- if (attr(spnl_m, "interpolated_stocks")) "BA120 monthly, BA100 interpolated" else "BA120 and BA100 monthly"
## monthly rates against the quarterly rates of the production panel. The
## quarterly panel divides a quarter's interest by the average of its two
## quarter-end balances; the monthly panel divides each month's interest by
## that month's average balance. With monthly balance sheets the two differ
## when balances move within the quarter (most in 2008-09 and 2020); with
## interpolated ones they agree almost exactly.
def_gap <- do.call(rbind, lapply(c("c_current", "c_savings", "c_call", "c_fixed_notice", "c_ncd", "y_homeloans",
                                   "y_vaf", "y_cards", "clr", "nir_assets", "opex_assets"), function(v) {
  if (!v %in% names(spnl_m) || !v %in% names(P$spnl)) return(NULL)
  chk <- spnl_m; chk$q <- quarter_end(chk$date)
  chk$w <- if (grepl("^(c_|y_)", v)) 4 * chk$days / 365 else 1 / 3
  chk <- stats::aggregate(list(m = chk[[v]] * chk$w), list(date = chk$q), function(x) if (sum(is.finite(x)) == 3) sum(x) else NA)
  chk <- merge(chk, P$spnl[, c("date", v)], by = "date"); chk <- chk[is.finite(chk$m) & is.finite(chk[[v]]), ]
  g <- chk$m - chk[[v]]
  data.frame(target = v, n = nrow(chk), level = mean(chk[[v]]), mean_abs_gap = mean(abs(g)),
             max_abs_gap = max(abs(g)), max_date = chk$date[which.max(abs(g))], stringsAsFactors = FALSE)
}))
pnl_gap <- def_gap$max_abs_gap[def_gap$target == "c_savings"]
guard(def_gap$mean_abs_gap[def_gap$target == "c_savings"] < 0.05 && pnl_gap < 0.15,
      "monthly savings-deposit costs do not average to the quarterly ones (max gap %.3f)", pnl_gap)
guard(all(def_gap$mean_abs_gap < 0.15 * pmax(abs(def_gap$level), 1)), "a monthly P&L ratio is far from its quarterly definition")
ctl_m <- c(sprintf("sm%02d", 2:12), "covid_q2", "covid_q3")
months <- seq(as.Date("2005-01-01"), month_start(CFG$date0), by = "month")
if (vol_source == "BA900") {
  lm9 <- ba900_lines(readRDS(f9))
  guard(nrow(lm9$guard) == 0, "monthly BA900 description guard failed")
  gp_m <- add_composites(group_panel(add_composites(bank_panel_monthly(lm9$lines))))
  vol_m <- stage1_panel_monthly(gp_m, mm, NULL, end = month_start(CFG$date0))
  vol_m <- vol_m[, c("date", grep("^g_", names(vol_m), value = TRUE))]
  pan_qv <- P$s1pan
  chk <- merge(stats::aggregate(list(m = vol_m$g_mortgages_res), list(date = quarter_end(vol_m$date)),
                                function(x) if (length(x) == 3) mean(x) else NA),
               pan_qv[, c("date", "g_mortgages_res")], by = "date")
  chk <- chk[is.finite(chk$m) & is.finite(chk$g_mortgages_res), ]
  guard(max(abs(chk$m - chk$g_mortgages_res)) < 1e-8, "monthly and quarterly BA900 growth disagree")
  vol_targets <- stage1_targets()
  sats_qv <- S1$sats[names(vol_targets)]
} else {
  sumk <- function(cds) Reduce(`+`, lapply(cds, function(cd) { s <- ks(cd); s$value[match(months, s$date)] }))
  kmap <- list(mortgages_res = "MON0232A", mortgages_com = "MON0236A",
               vaf = c("BAT1431M", "BAT1481M", "BATT142M", "BATT143M"), cards = c("MON0235A", "MON0239A"),
               personal_other = "MON0234A", corp_all = c("MON0238A", "MON0233A", "MON0237A"))
  lv <- data.frame(date = months)
  for (p in names(kmap)) lv[[p]] <- sumk(kmap[[p]])
  vol_m <- data.frame(date = months)
  for (p in names(kmap)) vol_m[[paste0("g_", p)]] <- 1200 * c(NA, diff(log(lv[[p]])))
  lq <- lv[is_quarter_end_month(lv$date), ]
  pan_qv <- data.frame(date = quarter_end(lq$date))
  for (p in names(kmap)) pan_qv[[paste0("g_", p)]] <- 400 * c(NA, diff(log(lq[[p]])))
  pan_qv <- merge(pan_qv, P$s1pan[, setdiff(names(P$s1pan), names(pan_qv)[-1])], by = "date")
  vol_targets <- stage1_targets()[paste0("g_", names(kmap))]
  sats_qv <- do.call(fit_stage1, c(list(pan_qv, vol_targets, controls = c("covid_q2", "covid_q3")), S1$args))
}
pan_m <- stage1_panel_monthly(data.frame(date = months, entity = "Sector"), mm, spnl_m,
                              end = month_start(CFG$date0))
pan_m <- merge(pan_m[, setdiff(names(pan_m), grep("^g_", names(pan_m), value = TRUE))], vol_m, by = "date", all.x = TRUE)
pan_m <- pan_m[order(pan_m$date), ]
pnl_targets <- stage1_pnl_targets()
targets <- c(vol_targets, pnl_targets)
targets <- targets[names(targets) %in% names(pan_m)]
## the quarterly counterpart of every target: its panel and its satellite
pan_q_of <- function(nm) if (nm %in% names(vol_targets)) pan_qv else P$s1pan
sats_q <- c(sats_qv, S1$sats[names(pnl_targets)])

## ---- 3. monthly satellites -----------------------------------------------------------
m_args <- list(sample_from = as.Date("2008-07-01"), max_k = CFG$max_k, lags = CFG$m_lags,
               h = CFG$m_h_oos, min_train = CFG$m_min_train, origin_step = 3L, oos_top = CFG$m_oos_top)
sats_m <- do.call(fit_stage1, c(list(pan_m, targets, controls = ctl_m), m_args))
guard(length(sats_m) == length(targets), "a monthly satellite failed")
## the same search on the smooth (indicator-free) monthly GDP path -- what a
## monthly model would be fed in a scenario whose GDP path is quarterly
uses_gdp <- names(targets)[vapply(targets, function(t) any(c("gdp_yy", "gdp_neg") %in% t$candidates), TRUE)]
pan_ms <- pan_m
pan_ms$gdp_yy <- pan_ms$gdp_yy_smooth; pan_ms$gdp_neg <- pmin(pan_ms$gdp_yy, 0)
sats_ms <- do.call(fit_stage1, c(list(pan_ms, targets[uses_gdp], controls = ctl_m), m_args))

## ---- 4. scenario back-test on common quarterly outcomes ---------------------------
## twelve quarters (the scenario horizon) along the realised macro path, from
## every quarter-end origin with a complete window
H <- CFG$h_oos
blk <- function(h) ifelse(h <= 4, "h1_4", ifelse(h <= 8, "h5_8", "h9_12"))
race <- list(); errs <- list(); errs_own <- list()
for (nm in names(targets)) {
  sq <- sats_q[[nm]]; sm <- sats_m[[nm]]; tg <- targets[[nm]]
  pq <- satellite_panel(sq, pan_q_of(nm)); pm <- satellite_panel(sm, pan_m)
  okq <- which(is.finite(pq[[nm]]))
  oq <- okq[okq >= okq[CFG$min_train] & okq <= max(okq) - H]
  od <- pq$date[oq]
  om <- match(month_start(od), pm$date)
  keep <- is.finite(om); oq <- oq[keep]; om <- om[keep]; od <- od[keep]
  act <- data.frame(date = pq$date, value = pq[[nm]])
  grp <- tg$groups %||% driver_groups()
  ar_q <- list(ardl_design(pq, nm, list(d = character(0), l = integer(0)), TRUE, sq$controls))
  ar_m <- list(ardl_design(pm, nm, list(d = character(0), l = integer(0)), TRUE, sm$controls))
  pdl_q <- pdl_search(pq, nm, tg$candidates, tg$signs, grp, CFG$max_k, L = 4L, degree = 2L,
                      sample_from = S1$args$sample_from, controls = sq$controls, required = tg$required %||% character(0))
  pdl_m <- pdl_search(pm, nm, tg$candidates, tg$signs, grp, CFG$max_k, L = CFG$m_pdl_L, degree = 2L,
                      sample_from = m_args$sample_from, controls = sm$controls, required = tg$required %||% character(0))
  des_pdl <- function(pd, pan, ctl) lapply(pd$specs, function(sp) pdl_design(pan, nm, sp$d, sp$L, sp$degree, TRUE, ctl))
  ## interest rates are day-count annualised monthly: map them back to the
  ## quarterly definition with day weights; volumes and ratios average
  dw <- if (grepl("^(c_|s_)", nm)) day_weights(pm$date[om], 3L * H) else NULL
  m2q <- function(Fm) months_to_quarters(Fm, dw)
  ms_fc <- NULL
  if (nm %in% uses_gdp && !is.null(sats_ms[[nm]])) {
    pms <- satellite_panel(sats_ms[[nm]], pan_ms)
    ms_fc <- m2q(oos_forecasts(satellite_designs(sats_ms[[nm]], pms, 20L), om, 3L * H, min_train = CFG$m_min_train))
  }
  Fq <- list(
    q_ardl = oos_forecasts(satellite_designs(sq, pq, 20L), oq, H),
    q_pdl = if (length(pdl_q$specs)) oos_forecasts(des_pdl(pdl_q, pq, sq$controls), oq, H) else NULL,
    q_shrink = shrinkage_oos(pq, nm, tg$candidates, CFG$lags, oq, H, TRUE, sq$controls),
    q_ar1 = oos_forecasts(ar_q, oq, H),
    m_ardl = m2q(oos_forecasts(satellite_designs(sm, pm, 20L), om, 3L * H, min_train = CFG$m_min_train)),
    m_ardl_smooth = ms_fc,
    m_pdl = if (length(pdl_m$specs)) m2q(oos_forecasts(des_pdl(pdl_m, pm, sm$controls), om, 3L * H,
                                                       min_train = CFG$m_min_train)) else NULL,
    m_shrink = m2q(shrinkage_oos(pm, nm, tg$candidates, CFG$m_lags, om, 3L * H, TRUE, sm$controls,
                                 min_train = CFG$m_min_train)),
    m_ar1 = m2q(oos_forecasts(ar_m, om, 3L * H, min_train = CFG$m_min_train)))
  Fq <- Filter(Negate(is.null), Fq)
  E <- lapply(names(Fq), function(k) { e <- quarterly_errors(Fq[[k]], od, act); e$model <- k; e })
  E <- do.call(rbind, E)
  ## common (origin, h) pairs only -- the smooth-GDP variant exists for the
  ## GDP-driven targets and is compared on those pairs separately
  core_models <- setdiff(names(Fq), "m_ardl_smooth")
  key <- paste(E$origin, E$h)
  common <- names(which(table(key[E$model %in% core_models]) == length(core_models)))
  E <- E[key %in% common, ]
  E$target <- nm
  errs[[nm]] <- E
  ## robustness: the monthly models scored against their OWN definition of
  ## the quarterly outcome (the quarter's months, day-weighted for interest
  ## rates) -- identical to the production definition for volumes
  if (!nm %in% names(vol_targets)) {
    wq <- if (grepl("^(c_|s_)", nm)) 4 * days_in_month(pm$date) / 365 else rep(1 / 3, nrow(pm))
    aq <- stats::aggregate(list(value = pm[[nm]] * wq), list(date = quarter_end(pm$date)),
                           function(x) if (length(x) == 3L && all(is.finite(x))) sum(x) else NA_real_)
    act_own <- aq[is.finite(aq$value), ]
    Eo <- do.call(rbind, lapply(intersect(c("m_ardl", "m_pdl", "m_shrink", "m_ar1"), names(Fq)), function(k) {
      e <- quarterly_errors(Fq[[k]], od, act_own); e$model <- k; e }))
    Eo <- Eo[paste(Eo$origin, Eo$h) %in% common, ]
    Eo$target <- nm
    errs_own[[nm]] <- rbind(E[E$model %in% grep("^q_", names(Fq), value = TRUE), ], Eo)
  }
  r <- stats::aggregate(list(rmse = E$error^2), list(model = E$model, block = blk(E$h)), function(x) sqrt(mean(x)))
  r_all <- stats::aggregate(list(rmse = E$error^2), list(model = E$model), function(x) sqrt(mean(x)))
  r <- rbind(r, data.frame(model = r_all$model, block = "h1_12", rmse = r_all$rmse))
  r$target <- nm; r$family <- sq$family %||% "volume"
  r$n_pdl_q <- length(pdl_q$specs); r$n_pdl_m <- length(pdl_m$specs)
  race[[nm]] <- r
}
race <- do.call(rbind, race)
errs <- do.call(rbind, errs)
## volumes: the error in the LEVEL of the book after twelve quarters (percent),
## the sum of the annualised growth errors over the window divided by four
lvl <- errs[errs$target %in% names(vol_targets), ]
lvl12 <- stats::aggregate(list(level_err = lvl$error / 4), list(target = lvl$target, model = lvl$model, origin = lvl$origin),
                          function(x) if (length(x) == H) sum(x) else NA_real_)
lvl12 <- lvl12[is.finite(lvl12$level_err), ]
level_rmse <- stats::aggregate(list(rmse_level12 = lvl12$level_err^2), list(target = lvl12$target, model = lvl12$model),
                               function(x) sqrt(mean(x)))
## Diebold-Mariano: monthly ARDL against quarterly ARDL, the twelve-quarter
## path error per origin (overlapping windows: HAC lag of twelve quarters)
dm_of <- function(E, mdl_q = "q_ardl", mdl_m = "m_ardl") do.call(rbind, lapply(unique(E$target), function(nm) {
  e <- E[E$target == nm & E$model %in% c(mdl_q, mdl_m), ]
  if (!all(c(mdl_q, mdl_m) %in% e$model)) return(NULL)
  a <- stats::aggregate(list(e2 = e$error^2), list(origin = e$origin, model = e$model), mean)
  w <- stats::reshape(a, idvar = "origin", timevar = "model", direction = "wide")
  w <- w[stats::complete.cases(w), ]
  if (nrow(w) < 8L) return(NULL)
  d <- dm_test(sqrt(w[[paste0("e2.", mdl_q)]]), sqrt(w[[paste0("e2.", mdl_m)]]), lag = H)
  data.frame(target = nm, dm_stat = d[["stat"]], p_value = d[["p"]], mspe_ratio = d[["mspe_ratio"]], n = nrow(w))
}))
dm <- dm_of(errs)
## the smooth-GDP monthly models against the indicator-GDP ones (GDP-driven targets)
dm_smooth <- dm_of(errs[errs$model %in% c("m_ardl", "m_ardl_smooth"), ], "m_ardl", "m_ardl_smooth")
## the same comparison with the monthly models on their own definition (P&L targets)
errs_own <- do.call(rbind, errs_own)
race_own <- stats::aggregate(list(rmse = errs_own$error^2),
                             list(model = errs_own$model, target = errs_own$target), function(x) sqrt(mean(x)))
race_own$family <- vapply(race_own$target, function(t) S1$sats[[t]]$family %||% "volume", "")
dm_own <- dm_of(errs_own)

saveRDS(list(vol_source = vol_source, pnl_source = pnl_source, pnl_gap = pnl_gap, def_gap = def_gap,
             race_own = race_own, dm_own = dm_own, macro_m = mm, td_fit = td_fit, horizon = H,
             level_rmse = level_rmse, dm_smooth = dm_smooth, sats_ms = sats_ms, uses_gdp = uses_gdp,
             disaggregation = td, panel_m = pan_m,
             panel_qv = pan_qv, vol_targets = names(vol_targets), sats_m = sats_m,
             sats_qv = if (vol_source == "KBP") sats_qv else NULL, summary_m = stage1_summary(sats_m),
             race = race, errors = errs, dm = dm, m_args = m_args),
        out_path("monthly.rds"))
rq <- with(race[race$block == "h1_12", ], tapply(rmse, list(target, model), mean))
ngfb_log("monthly (volumes %s; P&L %s): %d monthly satellites; median RMSE ratio monthly/quarterly ARDL over 12 quarters %.2f",
         vol_source, pnl_source, length(sats_m), stats::median(rq[, "m_ardl"] / rq[, "q_ardl"]))
