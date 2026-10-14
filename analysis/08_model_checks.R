# =============================================================================
#  analysis/08_model_checks.R -- is the ARDL satellite the right tool?
#
#  Every check is run on the footing the satellites are used on: dynamic
#  forecasts conditional on the realised macro path, re-estimated at each
#  expanding-window origin (pseudo-out-of-sample, horizons 1-4), and at the
#  two back-test origins over eight quarters.
#
#    1. Do the selected satellites beat the AR(1) by more than chance?
#       (Clark-West tests; the AR(1) is nested in every satellite)
#    2. Are deposit costs tied to the repo rate in the long run, or is the
#       levels regression spurious? (Pesaran-Shin-Smith bounds test)
#    3. Do hikes and cuts pass through differently? (repo split into partial
#       sums of increases and decreases)
#    4. Are credit losses convex in a downturn? (log credit-loss ratio; a
#       kink in GDP growth below zero)
#    5. Does the intercept correction help at every origin, not just at the
#       two back-test origins? (Diebold-Mariano)
#    6. BIC-weighted model averaging against an out-of-sample combination
#       (from the back-test, which re-runs the whole search at each origin)
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
BT <- readRDS(out_path("backtest.rds"))
pan <- P$s1pan[order(P$s1pan$date), ]
guard(all(c("repo_pos", "repo_neg", "gdp_neg") %in% names(pan)), "the macro panel lacks the asymmetric drivers")
pan <- asym_decompose(pan, "d_repo", "split")
h <- CFG$h_oos; mt <- CFG$min_train
bt_origins <- BT$origins; HB <- BT$horizon
ic_of <- function(s) if ((s$family %||% "") %in% CFG$ic_families) CFG$ic_window else 0L
rmse <- function(e) sqrt(mean(e^2))

## ---- 1. satellites against the AR(1): Clark-West --------------------------
cw <- do.call(rbind, lapply(names(S1$sats), function(nm) {
  s <- S1$sats[[nm]]; pz <- satellite_panel(s, pan)
  cmp <- spec_compare(pz, s$target, list(drivers = character(0), lags = integer(0)),
                      list(drivers = s$spec$d, lags = s$spec$l), h = h, min_train = mt,
                      controls = s$controls)
  data.frame(target = nm, family = s$family, spec = paste(sprintf("%s[L%d]", s$spec$d, s$spec$l), collapse = " + "),
             rmse_ar = cmp$rmse_a, rmse_sat = cmp$rmse_b, skill = 1 - cmp$ratio,
             cw1 = cmp$cw1[["stat"]], p1 = cmp$cw1[["p"]], cwh = cmp$cwh[["stat"]], ph = cmp$cwh[["p"]],
             origins = cmp$n_origins, stringsAsFactors = FALSE)
}))
guard(all(is.finite(cw$p1)), "Clark-West test failed for a satellite")

## ---- 2. deposit costs and the repo rate: bounds test ----------------------
dep <- c("c_current", "c_savings", "c_call", "c_fixed_notice", "c_ncd")
## symmetric (repo) and asymmetric (partial sums of increases and decreases)
bounds <- do.call(rbind, lapply(dep, function(nm) {
  s <- S1$sats[[nm]]; pz <- satellite_panel(s, pan)
  ok <- is.finite(pz[[nm]]) & is.finite(pz$repo)
  ctl <- intersect(s$impulses, names(pz))
  cm <- if (length(ctl)) as.matrix(pz[ok, ctl, drop = FALSE]) else NULL
  b <- bounds_test(pz[[nm]][ok], pz$repo[ok], p = 1L, controls = cm, nsim = 4000L, seed = CFG$seed)
  ba <- bounds_test(pz[[nm]][ok], cbind(pz$repo_pos[ok], pz$repo_neg[ok]), p = 1L, controls = cm,
                    nsim = 4000L, seed = CFG$seed)
  data.frame(target = nm, F = b$F, n = b$n, i0_95 = b$bounds["95%", "I0"], i1_95 = b$bounds["95%", "I1"],
             i1_99 = b$bounds["99%", "I1"], decision = b$decision, speed = b$speed, long_run = b$long_run,
             F_asym = ba$F, i0_95_asym = ba$bounds["95%", "I0"], i1_95_asym = ba$bounds["95%", "I1"],
             decision_asym = ba$decision, lr_up = ba$long_run[1], lr_down = ba$long_run[2])
}))

## ---- 3. deposit-cost dynamics and asymmetric pass-through ------------------
## Three registries for the five deposit costs (see stage1_pnl_targets()):
##   restricted : the original -- the repo level OR its change
##   ardl11     : the level AND the change together (impact and long run differ)
##   asym       : ardl11 with the level split into partial sums of hikes and cuts
## Each is fitted on the full sample (pseudo-out-of-sample skill of the
## selected specification) and re-fitted at the two back-test origins (the
## model-averaged projection over eight quarters, as the scenarios use it).
## The loan spreads over prime get the symmetric and asymmetric registries
## too (the change in repo split into rises and falls).
rmse2 <- function(a, b) { ok <- is.finite(a) & is.finite(b); sqrt(mean((a[ok] - b[ok])^2)) }
fit_reg <- function(r, data, args = S1$args)
  do.call(fit_stage1, c(list(data, r, controls = c("covid_q2", "covid_q3")), args))
spreads <- c("s_homeloans", "s_commercial", "s_vaf", "s_cards", "s_overdrafts", "s_term_loans")
pricing_t <- c(dep, spreads)
regs <- list(restricted = stage1_pnl_targets(character(0), "restricted")[dep],
             ardl11 = stage1_pnl_targets(character(0), "ardl11")[pricing_t],
             asym = stage1_pnl_targets(pricing_t, "ardl11")[pricing_t])
reg_fit <- lapply(regs, fit_reg, data = P$s1pan)
reg_bt <- lapply(regs, function(r) do.call(rbind, lapply(bt_origins, function(o) {
  tr <- P$s1pan[P$s1pan$date <= o, ]
  a <- S1$args; a$min_train <- min(a$min_train, 24L)
  sats <- fit_reg(r, tr, a)
  fut <- P$s1pan[P$s1pan$date > o, ][seq_len(HB), ]
  drv <- unique(unlist(lapply(sats, function(s) c(s$spec$d, unlist(lapply(s$bma$specs, `[[`, "d"))))))
  pr <- project_satellites(sats, tr, locf_tail(fut, drv), ic_window = CFG$ic_window, ic_mode = CFG$ic_mode)
  data.frame(origin = o, target = names(sats),
             rmse = vapply(names(sats), function(nm) rmse2(fut[[nm]], pr[[nm]]), 0))
})))
dyn <- do.call(rbind, lapply(names(regs), function(k) do.call(rbind, lapply(names(regs[[k]]), function(nm) {
  s <- reg_fit[[k]][[nm]]; b <- reg_bt[[k]]
  data.frame(registry = k, target = nm, spec = paste(sprintf("%s[L%d]", s$spec$d, s$spec$l), collapse = " + "),
             n_specs = s$n_specs, n_valid = s$n_valid, rmse_oos = s$oos$rmse, skill = s$skill,
             bt_2019 = b$rmse[b$target == nm & b$origin == bt_origins[1]],
             bt_2021 = b$rmse[b$target == nm & b$origin == bt_origins[2]], stringsAsFactors = FALSE)
}))))
dyn$bt_mean <- (dyn$bt_2019 + dyn$bt_2021) / 2
dw <- function(k, v) stats::setNames(dyn[[v]][dyn$registry == k], dyn$target[dyn$registry == k])
## rule 1: the ARDL(1,1) form replaces the restricted one if it improves the
## eight-quarter back-test for most deposit costs without worsening the median
## pseudo-out-of-sample RMSE
ardl11_better <- mean(dw("ardl11", "bt_mean")[dep] < dw("restricted", "bt_mean")[dep]) > 0.5 &&
  stats::median(dw("ardl11", "rmse_oos")[dep] / dw("restricted", "rmse_oos")[dep]) <= 1
guard(ardl11_better == identical(eval(formals(stage1_pnl_targets)$deposit_dynamics)[1], "ardl11"),
      "the deposit-dynamics default in stage1_pnl_targets() disagrees with the evidence")

## asymmetry, nested test: start from each symmetric ARDL(1,1) satellite and
## split its repo term into hikes and cuts (deposit costs); for the loan
## spreads over prime, split the change in repo into its positive and
## negative parts
sats_sym <- S1$sats; sats_sym[pricing_t] <- reg_fit$ardl11[pricing_t]
asym_targets <- pricing_t
asym <- do.call(rbind, lapply(asym_targets, function(nm) {
  s <- sats_sym[[nm]]; pz <- satellite_panel(s, pan)
  d <- s$spec$d; l <- s$spec$l
  v <- if ("repo" %in% d) "repo" else if ("d_repo" %in% d) "d_repo" else NA_character_
  if (is.na(v)) return(NULL)
  j <- match(v, d)
  d2 <- c(d[-j], paste0(v, c("_pos", "_neg"))); l2 <- c(l[-j], rep(l[j], 2))
  dz <- baeconstrat:::.sat_design(pz, nm, d2, l2, TRUE, s$controls)
  f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
  bp <- sprintf("%s_pos_L%d", v, l[j]); bn <- sprintf("%s_neg_L%d", v, l[j])
  w <- wald_equal(f, bp, bn)
  ic <- ic_of(s)
  cmp <- spec_compare(pz, nm, list(drivers = d, lags = l, ic_k = ic), list(drivers = d2, lags = l2, ic_k = ic),
                      h = h, min_train = mt, controls = s$controls)
  btr <- function(dd, ll) {
    z <- spec_oos(pz, nm, dd, ll, h = HB, min_train = mt, controls = s$controls, ic_k = ic,
                  origin_dates = bt_origins)
    rmse(z$actual - z$fc)
  }
  rho <- f$coef[["ar1"]]
  data.frame(target = nm, base_spec = paste(sprintf("%s[L%d]", d, l), collapse = " + "), driver = v, lag = l[j],
             beta_up = f$coef[[bp]], beta_down = f$coef[[bn]],
             lr_up = f$coef[[bp]] / (1 - rho), lr_down = f$coef[[bn]] / (1 - rho), p_sym = w[["p"]],
             rmse_sym = cmp$rmse_a, rmse_asym = cmp$rmse_b, oos_ratio = cmp$ratio,
             cw_p = cmp$cwh[["p"]], bt_sym = btr(d, l), bt_asym = btr(d2, l2), stringsAsFactors = FALSE)
}))
## adoption rule, the same for every refinement and fixed before looking at
## the scenario results: a refinement enters the core only if, for the target
## concerned, it improves BOTH the pseudo-out-of-sample RMSE of the selected
## specification and the eight-quarter back-test of the model-averaged
## projection (the model as the scenarios use it). For asymmetry the nested
## in-sample test must also reject symmetry (HAC Wald, 10%).
reg_cmp <- merge(dyn[dyn$registry == "ardl11", c("target", "rmse_oos", "bt_mean")],
                 dyn[dyn$registry == "asym", c("target", "rmse_oos", "bt_mean")],
                 by = "target", suffixes = c("_sym", "_asym"))
asym <- merge(asym, reg_cmp, by = "target", all.x = TRUE, sort = FALSE)
asym <- asym[match(asym_targets, asym$target), ]
asym$adopt <- asym$p_sym < 0.10 & asym$rmse_oos_asym < asym$rmse_oos_sym & asym$bt_mean_asym < asym$bt_mean_sym
asym$adopt[is.na(asym$adopt)] <- FALSE
guard(setequal(asym$target[asym$adopt], eval(formals(stage1_pnl_targets)$asymmetric)),
      "the asymmetric deposit costs in stage1_pnl_targets() disagree with the adoption rule: %s",
      paste(asym$target[asym$adopt], collapse = ", "))

## ---- 4. convex credit losses -----------------------------------------------
## (a) registry level, as the scenarios would use it: the credit-loss search
## with GDP growth below zero as an extra candidate driver ("kink"), against
## the linear registry
clr_reg <- list(linear = stage1_pnl_targets()["clr"], kink = stage1_pnl_targets(convex_credit = TRUE)["clr"])
clr_fit <- lapply(clr_reg, fit_reg, data = P$s1pan)
clr_bt <- vapply(clr_reg, function(r) vapply(bt_origins, function(o) {
  tr <- P$s1pan[P$s1pan$date <= o, ]
  a <- S1$args; a$min_train <- min(a$min_train, 24L)
  sats <- fit_reg(r, tr, a)
  fut <- P$s1pan[P$s1pan$date > o, ][seq_len(HB), ]
  drv <- unique(unlist(lapply(sats, function(s) c(s$spec$d, unlist(lapply(s$bma$specs, `[[`, "d"))))))
  pr <- project_satellites(sats, tr, locf_tail(fut, drv), ic_window = CFG$ic_window, ic_mode = CFG$ic_mode)
  rmse2(fut$clr, pr$clr)
}, 0), numeric(length(bt_origins)))
convex_reg <- data.frame(registry = names(clr_reg),
                         spec = vapply(clr_fit, function(z) paste(sprintf("%s[L%d]", z$clr$spec$d, z$clr$spec$l), collapse = " + "), ""),
                         rmse_oos = vapply(clr_fit, function(z) z$clr$oos$rmse, 0),
                         pip_kink = vapply(clr_fit, function(z) { b <- satellite_bma_summary(z$clr)
                           if ("gdp_neg" %in% b$driver) b$pip[b$driver == "gdp_neg"] else 0 }, 0),
                         bt_2019 = clr_bt[1, ], bt_2021 = clr_bt[2, ], stringsAsFactors = FALSE)
convex_adopt <- convex_reg$rmse_oos[2] < convex_reg$rmse_oos[1] &&
  (convex_reg$bt_2019[2] + convex_reg$bt_2021[2]) < (convex_reg$bt_2019[1] + convex_reg$bt_2021[1])
guard(convex_adopt == isTRUE(eval(formals(stage1_pnl_targets)$convex_credit)),
      "the convex-credit default in stage1_pnl_targets() disagrees with the adoption rule")

## (b) specification level, on the two leading members of the linear model
## average: a kink in GDP growth below zero and a log credit-loss ratio
sc <- S1$sats$clr; pzc <- satellite_panel(sc, pan)
ord <- order(-sc$bma$weights)
lead_specs <- sc$bma$specs[ord[1:2]]
conv <- do.call(rbind, lapply(seq_along(lead_specs), function(i) {
  sp <- lead_specs[[i]]
  vars <- list(linear = list(drivers = sp$d, lags = sp$l),
               kink = list(drivers = c(sp$d, "gdp_neg"), lags = c(sp$l, sp$l[match("gdp_yy", sp$d)])),
               log = list(drivers = sp$d, lags = sp$l, transform = "log"),
               log_kink = list(drivers = c(sp$d, "gdp_neg"), lags = c(sp$l, sp$l[match("gdp_yy", sp$d)]),
                               transform = "log"))
  do.call(rbind, lapply(names(vars), function(k) {
    v <- vars[[k]]; v$ic_k <- CFG$ic_window
    z <- spec_oos(pzc, "clr", v$drivers, v$lags, h = h, min_train = mt, controls = sc$controls,
                  transform = v$transform %||% "none", ic_k = v$ic_k)
    zb <- spec_oos(pzc, "clr", v$drivers, v$lags, h = HB, min_train = mt, controls = sc$controls,
                   transform = v$transform %||% "none", ic_k = v$ic_k, origin_dates = bt_origins)
    pz2 <- pzc; if (identical(v$transform, "log")) pz2$clr <- ifelse(pz2$clr > 0, log(pz2$clr), NA)
    dz <- baeconstrat:::.sat_design(pz2, "clr", v$drivers, v$lags, TRUE, sc$controls)
    f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
    kn <- grep("^gdp_neg_", names(f$coef), value = TRUE)
    gn <- grep("^gdp_yy_", names(f$coef), value = TRUE)
    data.frame(base = paste(sprintf("%s[L%d]", sp$d, sp$l), collapse = " + "), variant = k,
               b_gdp = f$coef[[gn]], b_kink = if (length(kn)) f$coef[[kn]] else NA_real_,
               t_kink = if (length(kn)) f$t[[kn]] else NA_real_, rho = f$coef[["ar1"]],
               rmse_oos = rmse(z$actual - z$fc),
               rmse_bt_2019 = rmse((zb$actual - zb$fc)[zb$origin == bt_origins[1]]),
               rmse_bt_2021 = rmse((zb$actual - zb$fc)[zb$origin == bt_origins[2]]),
               stringsAsFactors = FALSE)
  }))
}))

## ---- 5. the intercept correction at every origin ---------------------------
## no correction against the mean residual of the last four quarters entered
## as a level shift of the projected path, or in the equation's intercept
## (where it accumulates through the AR term to ic / (1 - rho))
icx <- do.call(rbind, lapply(names(S1$sats), function(nm) {
  s <- S1$sats[[nm]]
  if (!(s$family %in% CFG$ic_families)) return(NULL)
  pz <- satellite_panel(s, pan)
  sp <- list(drivers = s$spec$d, lags = s$spec$l)
  cl <- spec_compare(pz, s$target, c(sp, ic_k = 0L), c(sp, ic_k = CFG$ic_window, ic_mode = "level"),
                     h = h, min_train = mt, controls = s$controls)
  ce <- spec_compare(pz, s$target, c(sp, ic_k = 0L), c(sp, ic_k = CFG$ic_window, ic_mode = "equation"),
                     h = h, min_train = mt, controls = s$controls)
  data.frame(target = nm, family = s$family, rho = s$fit$coef[["ar1"]], rmse_no_ic = cl$rmse_a,
             rmse_level = cl$rmse_b, rmse_equation = ce$rmse_b, ratio = cl$ratio, ratio_equation = ce$ratio,
             dm = cl$dm[["stat"]], p = cl$dm[["p"]], dm_equation = ce$dm[["stat"]], p_equation = ce$dm[["p"]],
             stringsAsFactors = FALSE)
}))
ic_level_better <- stats::median(icx$ratio) <= stats::median(icx$ratio_equation) &&
  stats::median(BT$accuracy$skill_ic_level[BT$accuracy$family %in% CFG$ic_families]) >=
  stats::median(BT$accuracy$skill_ic_equation[BT$accuracy$family %in% CFG$ic_families])
guard(ic_level_better == identical(CFG$ic_mode, "level"),
      "the intercept-correction mode in 00_config.R disagrees with the evidence")

## ---- pass-through of the repo rate to the deposit costs (production models) ----
pass <- do.call(rbind, lapply(dep, function(nm) do.call(rbind, lapply(c(hike = 1, cut = -1), function(st) {
  z <- satellite_pass_through(S1$sats[[nm]], pan, step = st, n = 40L)
  data.frame(target = nm, move = if (st > 0) "hike" else "cut", quarter = z$quarter, response = z$response)
}))))

## ---- 6. forecast combination (from the back-test) ---------------------------
acc <- BT$accuracy
comb <- stats::aggregate(cbind(skill_used, skill_comb, skill_best) ~ family, acc, stats::median)
comb_all <- c(bma = stats::median(acc$skill_used), comb = stats::median(acc$skill_comb),
              best = stats::median(acc$skill_best),
              comb_wins = mean(acc$rmse_comb < ifelse(acc$family %in% CFG$ic_families, acc$rmse_model_ic,
                                                      acc$rmse_model)))

saveRDS(list(cw = cw, bounds = bounds, pass_through = pass, dynamics = dyn, ardl11_better = ardl11_better, asym = asym,
             convex = conv, convex_reg = convex_reg, convex_adopt = convex_adopt, ic = icx, ic_level_better = ic_level_better, comb = comb,
             comb_all = comb_all), out_path("model_checks.rds"))
ngfb_log("model checks: %d of %d satellites beat the AR(1) at 10%% (Clark-West, h = %d); symmetry rejected at 10%% for %d of %d pricing targets",
         sum(cw$ph < 0.10), nrow(cw), h, sum(asym$p_sym < 0.10), nrow(asym))
