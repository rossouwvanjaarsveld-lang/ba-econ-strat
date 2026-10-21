# =============================================================================
#  analysis/08_model_checks.R -- is the ARDL satellite the right tool, and
#                                which asymmetries belong in it?
#
#  Every check is run on the footing the satellites are used on, with the
#  macro path taken as KNOWN (perfect foresight -- no macro variable is ever
#  forecast): models are re-estimated on data up to an origin and simulated
#  twelve quarters (the scenario horizon) along the REALISED macro path.
#  Three yardsticks:
#    (a) in-sample evidence (HAC Wald or t tests on the leading specification);
#    (b) the scenario back-test of the model average at every expanding-
#        window origin (model space from the full-sample search, coefficients
#        and BIC weights re-estimated at each origin);
#    (c) the two-origin back-test (2019Q4, 2021Q4) that re-runs the whole
#        search on data up to each origin (no look-ahead in the model space).
#
#    1. Does knowing the macro path help? Each satellite against the AR(1)
#       (Clark-West at the twelve-quarter horizon; the AR(1) has no macro
#       channel, so this is a test of the relationship, not of forecasting)
#    2. Are deposit costs tied to the repo rate in the long run? (bounds test)
#    3. Deposit-cost dynamics; do hikes and cuts pass through differently?
#    4. Do loan and deposit VOLUMES respond differently to rate rises and falls?
#    5. Contraction kinks: are credit losses -- and volumes -- more sensitive
#       to GDP growth when the economy contracts?
#    6. The intercept correction at every origin
#    7. Model averaging against selection and combination (from the back-test)
#
#  Adoption rule, the same for every refinement and fixed before the
#  scenario results are looked at: a refinement enters the production
#  registry only if (a) the in-sample test supports it at 10%, (b) it lowers
#  the scenario back-test error and (c) it lowers the two-origin back-test
#  error. The script fails if the registry defaults disagree with the rule.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
BT <- readRDS(out_path("backtest.rds"))
pan <- P$s1pan[order(P$s1pan$date), ]
guard(all(c("repo_pos", "repo_neg", "gdp_neg", "d4_repo_up", "d4_repo_dn") %in% names(pan)),
      "the macro panel lacks the asymmetric drivers")
pan <- asym_decompose(pan, "d_repo", "split")
h <- CFG$h_oos; mt <- CFG$min_train
bt_origins <- BT$origins; HB <- BT$horizon
guard(h == 12L && HB == 12L, "the checks are written for twelve-quarter conditional simulations")
ic_of <- function(s) if ((s$family %||% "") %in% CFG$ic_families) CFG$ic_window else 0L
rmse <- function(e) sqrt(mean(e^2))
rmse2 <- function(a, b) { ok <- is.finite(a) & is.finite(b); sqrt(mean((a[ok] - b[ok])^2)) }
fit_reg <- function(r, data, args = S1$args)
  do.call(fit_stage1, c(list(data, r, controls = c("covid_q2", "covid_q3")), args))
## (b) the scenario back-test of each satellite's model average
sbt <- function(sats) vapply(names(sats), function(nm) {
  s <- sats[[nm]]; e <- scenario_backtest(s, satellite_panel(s, pan), h, mt, ic_of(s)); rmse(e$error) }, 0)
## (c) the two-origin back-test, re-running the whole search
bt2 <- function(r) do.call(rbind, lapply(bt_origins, function(o) {
  tr <- P$s1pan[P$s1pan$date <= o, ]
  a <- S1$args; a$min_train <- min(a$min_train, 24L)
  sats <- fit_reg(r, tr, a)
  fut <- P$s1pan[P$s1pan$date > o, ][seq_len(HB), ]
  pr <- project_satellites(sats, tr, fut, ic_window = CFG$ic_window, ic_families = CFG$ic_families,
                           ic_mode = CFG$ic_mode)
  data.frame(origin = o, target = names(sats),
             rmse = vapply(names(sats), function(nm) rmse2(fut[[sats[[nm]]$target]], pr[[sats[[nm]]$target]]), 0))
}))
bt_mean <- function(b) tapply(b$rmse, b$target, mean)
## (a) nested in-sample test: split one driver of a specification into its
## rises and falls (same lag) and test equality with a HAC Wald test
split_of <- c(repo = "repo_pos|repo_neg", d_repo = "d_repo_pos|d_repo_neg", d4_repo = "d4_repo_up|d4_repo_dn")
nested_split <- function(s, pz) {
  d <- s$spec$d; l <- s$spec$l
  v <- intersect(c("repo", "d4_repo", "d_repo"), d)[1]
  if (is.na(v)) return(c(p = NA_real_, up = NA_real_, down = NA_real_, rho = NA_real_, lag = NA_real_))
  j <- match(v, d); parts <- strsplit(split_of[[v]], "|", fixed = TRUE)[[1]]
  d2 <- c(d[-j], parts); l2 <- c(l[-j], rep(l[j], 2))
  dz <- baeconstrat:::.sat_design(pz, s$target, d2, l2, TRUE, s$controls)
  f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
  bp <- sprintf("%s_L%d", parts[1], l[j]); bn <- sprintf("%s_L%d", parts[2], l[j])
  w <- wald_equal(f, bp, bn)
  c(p = w[["p"]], up = f$coef[[bp]], down = f$coef[[bn]], rho = f$coef[["ar1"]], lag = l[j])
}

## ---- 1. does knowing the macro path help? Clark-West against the AR(1) -----
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
guard(all(is.finite(cw$ph)), "Clark-West test failed for a satellite")

## ---- 2. deposit costs and the repo rate: bounds test ----------------------
dep <- c("c_current", "c_savings", "c_call", "c_fixed_notice", "c_ncd")
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
## restricted : the repo level OR its change
## ardl11     : the level AND the change together (impact and long run differ)
## asym       : ardl11 with the level (deposit costs) or the change (loan
##              spreads over prime) split into rises and falls
spreads <- c("s_homeloans", "s_commercial", "s_vaf", "s_cards", "s_overdrafts", "s_term_loans")
pricing_t <- c(dep, spreads)
regs <- list(restricted = stage1_pnl_targets(character(0), "restricted")[dep],
             ardl11 = stage1_pnl_targets(character(0), "ardl11")[pricing_t],
             asym = stage1_pnl_targets(pricing_t, "ardl11")[pricing_t])
reg_fit <- lapply(regs, fit_reg, data = P$s1pan)
reg_sbt <- lapply(reg_fit, sbt)
reg_bt <- lapply(regs, bt2)
dyn <- do.call(rbind, lapply(names(regs), function(k) do.call(rbind, lapply(names(regs[[k]]), function(nm) {
  s <- reg_fit[[k]][[nm]]; b <- reg_bt[[k]]
  data.frame(registry = k, target = nm, spec = paste(sprintf("%s[L%d]", s$spec$d, s$spec$l), collapse = " + "),
             n_specs = s$n_specs, n_valid = s$n_valid, rmse_sbt = reg_sbt[[k]][[nm]],
             bt_2019 = b$rmse[b$target == nm & b$origin == bt_origins[1]],
             bt_2021 = b$rmse[b$target == nm & b$origin == bt_origins[2]], stringsAsFactors = FALSE)
}))))
dyn$bt_mean <- (dyn$bt_2019 + dyn$bt_2021) / 2
dw <- function(k, v) stats::setNames(dyn[[v]][dyn$registry == k], dyn$target[dyn$registry == k])
## the ARDL(1,1) form replaces the restricted one if it improves the two-origin
## back-test for most deposit costs without worsening the median scenario
## back-test error
ardl11_better <- mean(dw("ardl11", "bt_mean")[dep] < dw("restricted", "bt_mean")[dep]) > 0.5 &&
  stats::median(dw("ardl11", "rmse_sbt")[dep] / dw("restricted", "rmse_sbt")[dep]) <= 1
## asymmetry: nested test on each symmetric ARDL(1,1) satellite
sats_sym <- S1$sats; sats_sym[pricing_t] <- reg_fit$ardl11[pricing_t]
asym <- do.call(rbind, lapply(pricing_t, function(nm) {
  s <- sats_sym[[nm]]; ns <- nested_split(s, satellite_panel(s, pan))
  data.frame(target = nm, base_spec = paste(sprintf("%s[L%d]", s$spec$d, s$spec$l), collapse = " + "),
             lag = ns[["lag"]], beta_up = ns[["up"]], beta_down = ns[["down"]],
             lr_up = ns[["up"]] / (1 - ns[["rho"]]), lr_down = ns[["down"]] / (1 - ns[["rho"]]),
             p_sym = ns[["p"]], stringsAsFactors = FALSE)
}))
asym$sbt_sym <- dw("ardl11", "rmse_sbt")[asym$target]; asym$sbt_asym <- dw("asym", "rmse_sbt")[asym$target]
asym$bt_sym <- dw("ardl11", "bt_mean")[asym$target]; asym$bt_asym <- dw("asym", "bt_mean")[asym$target]
asym$adopt <- is.finite(asym$p_sym) & asym$p_sym < 0.10 & asym$sbt_asym < asym$sbt_sym & asym$bt_asym < asym$bt_sym

## ---- 4. asymmetric VOLUME responses to rate rises and falls -----------------
vol_t <- names(stage1_targets())
## each refinement is tested against the plain registry (no volume asymmetry,
## no kink), whatever the production defaults are
vregs <- list(sym = stage1_targets(asymmetric = character(0), kink = character(0)),
              asym = stage1_targets(asymmetric = vol_t, kink = character(0)))
vfit <- lapply(vregs, fit_reg, data = P$s1pan)
vsbt <- lapply(vfit, sbt)
vbt <- lapply(vregs, bt2)
vol_asym <- do.call(rbind, lapply(vol_t, function(nm) {
  s <- vfit$sym[[nm]]; ns <- nested_split(s, satellite_panel(s, pan))
  sa <- vfit$asym[[nm]]; ba <- satellite_bma_summary(sa)
  pip <- function(v) if (v %in% ba$driver) ba$pip[ba$driver == v] else 0
  data.frame(target = nm, base_spec = paste(sprintf("%s[L%d]", s$spec$d, s$spec$l), collapse = " + "),
             asym_spec = paste(sprintf("%s[L%d]", sa$spec$d, sa$spec$l), collapse = " + "),
             beta_up = ns[["up"]], beta_down = ns[["down"]], p_sym = ns[["p"]],
             pip_up = pip("d4_repo_up"), pip_down = pip("d4_repo_dn"),
             sbt_sym = vsbt$sym[[nm]], sbt_asym = vsbt$asym[[nm]],
             bt_sym = bt_mean(vbt$sym)[[nm]], bt_asym = bt_mean(vbt$asym)[[nm]], stringsAsFactors = FALSE)
}))
vol_asym$adopt <- is.finite(vol_asym$p_sym) & vol_asym$p_sym < 0.10 & vol_asym$sbt_asym < vol_asym$sbt_sym &
  vol_asym$bt_asym < vol_asym$bt_sym

## ---- 5. contraction kinks: credit losses and volumes ------------------------
## the registry with GDP growth below zero as an extra candidate, against the
## linear registry; in-sample evidence: |t| of the kink in the leading
## specification that contains it
kink_t <- function(s) {
  if (is.null(s$bma)) return(NA_real_)
  i <- which(vapply(s$bma$specs, function(sp) "gdp_neg" %in% sp$d, TRUE))
  if (!length(i)) return(NA_real_)
  i <- i[which.max(s$bma$weights[i])]; sp <- s$bma$specs[[i]]
  pz <- satellite_panel(s, pan)
  dz <- baeconstrat:::.sat_design(pz, s$target, sp$d, sp$l, TRUE, s$controls)
  f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
  f$t[[grep("^gdp_neg_", names(f$coef), value = TRUE)]]
}
kregs <- list(linear = c(stage1_targets(asymmetric = character(0), kink = character(0)), stage1_pnl_targets(convex_credit = FALSE)["clr"]),
              kink = c(stage1_targets(asymmetric = character(0), kink = vol_t), stage1_pnl_targets(convex_credit = TRUE)["clr"]))
kregs <- lapply(kregs, function(r) r[c(vol_t, "clr")])
kfit <- list(linear = c(vfit$sym, fit_reg(kregs$linear["clr"], P$s1pan)), kink = fit_reg(kregs$kink, P$s1pan))
ksbt <- lapply(kfit, function(f) sbt(f[c(vol_t, "clr")]))
kbt <- list(linear = rbind(vbt$sym, bt2(kregs$linear["clr"])), kink = bt2(kregs$kink))
kinks <- do.call(rbind, lapply(c(vol_t, "clr"), function(nm) {
  sk <- kfit$kink[[nm]]; bk <- satellite_bma_summary(sk)
  data.frame(target = nm, has_kink = "gdp_neg" %in% sk$candidates,
             spec_kink = paste(sprintf("%s[L%d]", sk$spec$d, sk$spec$l), collapse = " + "),
             pip_kink = if ("gdp_neg" %in% bk$driver) bk$pip[bk$driver == "gdp_neg"] else 0,
             t_kink = kink_t(sk), sbt_linear = ksbt$linear[[nm]], sbt_kink = ksbt$kink[[nm]],
             bt_linear = bt_mean(kbt$linear)[[nm]], bt_kink = bt_mean(kbt$kink)[[nm]], stringsAsFactors = FALSE)
}))
kinks$adopt <- kinks$has_kink & is.finite(kinks$t_kink) & abs(kinks$t_kink) >= stats::qnorm(0.95) &
  kinks$sbt_kink < kinks$sbt_linear & kinks$bt_kink < kinks$bt_linear
convex_adopt <- kinks$adopt[kinks$target == "clr"]

## the two leading members of the linear credit-loss average, with a kink and
## in logs (specification level, for the report)
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
    v <- vars[[k]]
    z <- spec_oos(pzc, "clr", v$drivers, v$lags, h = h, min_train = mt, controls = sc$controls,
                  transform = v$transform %||% "none", ic_k = CFG$ic_window)
    pz2 <- pzc; if (identical(v$transform, "log")) pz2$clr <- ifelse(pz2$clr > 0, log(pz2$clr), NA)
    dz <- baeconstrat:::.sat_design(pz2, "clr", v$drivers, v$lags, TRUE, sc$controls)
    f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
    kn <- grep("^gdp_neg_", names(f$coef), value = TRUE)
    gn <- grep("^gdp_yy_", names(f$coef), value = TRUE)
    data.frame(base = paste(sprintf("%s[L%d]", sp$d, sp$l), collapse = " + "), variant = k,
               b_gdp = if (length(gn)) f$coef[[gn]] else NA_real_, b_kink = if (length(kn)) f$coef[[kn]] else NA_real_,
               t_kink = if (length(kn)) f$t[[kn]] else NA_real_, rho = f$coef[["ar1"]],
               rmse_sbt = rmse(z$actual - z$fc), stringsAsFactors = FALSE)
  }))
}))

## ---- the registry defaults must agree with the adoption rule ----------------
rules <- list(
  deposit_dynamics = ardl11_better == identical(eval(formals(stage1_pnl_targets)$deposit_dynamics)[1], "ardl11"),
  pricing_asymmetry = setequal(asym$target[asym$adopt], eval(formals(stage1_pnl_targets)$asymmetric)),
  convex_credit = isTRUE(convex_adopt) == isTRUE(eval(formals(stage1_pnl_targets)$convex_credit)),
  volume_asymmetry = setequal(vol_asym$target[vol_asym$adopt], eval(formals(stage1_targets)$asymmetric)),
  volume_kink = setequal(kinks$target[kinks$adopt & kinks$target != "clr"], eval(formals(stage1_targets)$kink)))
if (!isTRUE(as.logical(Sys.getenv("NGFB_DISCOVERY", "FALSE"))))
  for (k in names(rules)) guard(rules[[k]], "the registry default for %s disagrees with the adoption rule", k)

## ---- 6. the intercept correction at every origin ---------------------------
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
## the level shift is the incumbent: the equation form (which compounds
## through the AR term) replaces it only if it is better on BOTH tests
eq_better_oos <- stats::median(icx$ratio_equation) < stats::median(icx$ratio)
eq_better_bt <- stats::median(BT$accuracy$skill_ic_equation[BT$accuracy$family %in% CFG$ic_families]) >
  stats::median(BT$accuracy$skill_ic_level[BT$accuracy$family %in% CFG$ic_families])
ic_level_better <- !(eq_better_oos && eq_better_bt)
if (!isTRUE(as.logical(Sys.getenv("NGFB_DISCOVERY", "FALSE"))))
  guard(ic_level_better == identical(CFG$ic_mode, "level"),
        "the intercept-correction mode in 00_config.R disagrees with the evidence")

## ---- pass-through of the repo rate to the deposit costs (production models) ----
pass <- do.call(rbind, lapply(dep, function(nm) do.call(rbind, lapply(c(hike = 1, cut = -1), function(st) {
  z <- satellite_pass_through(S1$sats[[nm]], pan, step = st, n = 40L)
  data.frame(target = nm, move = if (st > 0) "hike" else "cut", quarter = z$quarter, response = z$response)
}))))

## ---- 7. model averaging, selection and combination (from the back-test) ----
acc <- BT$accuracy
comb <- stats::aggregate(cbind(skill_used, skill_comb, skill_best) ~ family, acc, stats::median)
comb_all <- c(bma = stats::median(acc$skill_used), comb = stats::median(acc$skill_comb),
              best = stats::median(acc$skill_best),
              comb_wins = mean(acc$rmse_comb < ifelse(acc$family %in% CFG$ic_families, acc$rmse_model_ic,
                                                      acc$rmse_model)))

saveRDS(list(cw = cw, bounds = bounds, pass_through = pass, dynamics = dyn, ardl11_better = ardl11_better, asym = asym,
             vol_asym = vol_asym, kinks = kinks, convex = conv, convex_adopt = convex_adopt, rules = rules,
             ic = icx, ic_level_better = ic_level_better, ic_tests = c(eq_better_oos = eq_better_oos, eq_better_bt = eq_better_bt),
             comb = comb, comb_all = comb_all, horizon = h),
        out_path("model_checks.rds"))
ngfb_log("model checks (12-quarter conditional simulations): %d of %d satellites beat the AR(1) at 10%% (Clark-West); adopted -- pricing asymmetry: %s; volume asymmetry: %s; kinks: %s",
         sum(cw$ph < 0.10), nrow(cw), paste(asym$target[asym$adopt], collapse = ", "),
         paste(vol_asym$target[vol_asym$adopt], collapse = ", "), paste(kinks$target[kinks$adopt], collapse = ", "))
