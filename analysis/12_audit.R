# =============================================================================
#  analysis/12_audit.R -- are the numbers right?
#
#  1. The swap curve. Actual ZAR swap rates (rbond.co.za) against the proxy
#     inside the government curve that priced the structural hedge up to the
#     previous version; the term spreads the scenarios now use and the
#     relationships tested; how the 5-year bond-swap spread moves with the
#     curve (the risk factor of an asset swap); the hedge book's fixed rate;
#     what the correction does to NGFB.
#  2. NII sensitivity to rates. A parallel 100 bp move on the static balance
#     sheet (prices only, volumes, credit and fees as in the baseline) for
#     NGFB with and without its hedge and for the sector, against a published
#     South African bank's disclosure; the hedge ratio at which NGFB's NII is
#     rate-neutral.
#  3. Jump-off reconciliation. How much of the gap between the sector
#     engine's first year and BA120 a structural hedge at real swap rates
#     would explain.
#  4. Funding spreads. Is there a macro relationship for the cost of
#     wholesale funding beyond the repricing lag? If not, a funding shock is
#     an overlay, and the overlay is sized.
#  5. JIBAR -> ZARONIA. The engine's floating benchmark switches from 3-month
#     JIBAR to compounded ZARONIA + the 16.19 bp credit adjustment spread
#     after JIBAR's last fixing (31 December 2026): what that does to the
#     level of the benchmark, the basis it freezes, NGFB's exposure to the
#     benchmark, and the risk that borrowers negotiate the spread away.
#
#  Every model is used as a scenario uses it: the macro path is known
#  (perfect foresight); nothing here forecasts a macro variable.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
S2 <- readRDS(out_path("stage2.rds"))
N  <- readRDS(out_path("ngfb.rds"))
SC <- readRDS(out_path("scenarios.rds"))
hist <- P$macro[P$macro$date <= CFG$date0, ]
ea <- SC$eng_args
sec_ebs <- engine_balance_sheet(SC$sector_bs, N$chars_sector, N$pricing)
nw <- function(f, lag) { V <- hac_vcov(stats::model.matrix(f), stats::resid(f), lag); coef(f) / sqrt(diag(V)) }  # HAC t
run_with <- function(inp, ebs = N$engine_bs, hedge = SC$hedge0, ...) {
  a <- utils::modifyList(ea, list(...))
  do.call(run_engine, c(list(bs = ebs, growth = inp$growth, rates = inp$rates, credit = inp$credit,
                             ratios = inp$ratios, hedge = hedge, line_rates = inp$line_rates), a))
}

## ---- 1. the swap curve ------------------------------------------------------------
## (a) the previous proxy against actual 5-year swaps, quarter by quarter
q <- P$macro[is.finite(P$macro$swap5y) & is.finite(P$macro$jibar3m), ]
q$proxy5 <- q$jibar3m + 0.55 * (q$gov10y - q$jibar3m)
proxy_vs_actual <- data.frame(date = q$date, jibar3m = q$jibar3m, gov10y = q$gov10y, gov5y = q$gov5y,
                              swap5y = q$swap5y, proxy = q$proxy5, error = q$proxy5 - q$swap5y)
guard(all(proxy_vs_actual$error > 0), "the government-curve proxy should overstate the 5-year swap in every quarter")
proxy_err <- c(mean = mean(proxy_vs_actual$error), min = min(proxy_vs_actual$error),
               max = max(proxy_vs_actual$error), rmse = sqrt(mean(proxy_vs_actual$error^2)))

## (b) term spreads used by the scenarios (quarterly, to the jump-off)
spreads <- swap_term_spreads(hist)
guard(identical(spreads$col, names(swap_tenors())), "a swap tenor is missing from the term spreads")
guard(all(diff(spreads$spread[spreads$tenor >= 2]) > -0.1) && spreads$spread[spreads$tenor == 10] > 0.5,
      "the swap curve's term spreads are not upward sloping from two years")

## (c) relationships tested on monthly averages (HAC, Newey-West 6 lags):
##     the government curve's slope, and the coming year's policy path (known
##     under perfect foresight), as drivers of each tenor's spread over JIBAR
mo <- read_rbond_cache(file.path(CFG$raw, "rbond"), freq = "month")
mo <- mo[mo$date >= as.Date("2023-10-01") & mo$date <= as.Date("2026-09-01"), ]
rq <- utils::read.csv(file.path(CFG$raw, "rbond", "REPO.csv"))
rq <- stats::aggregate(list(repo = rq$yield_pct), list(date = month_start(as.Date(rq$date))), mean)
mo$repo <- rq$repo[match(mo$date, rq$date)]
mo$fwd12 <- vapply(seq_len(nrow(mo)), function(i) {
  w <- rq$repo[rq$date > mo$date[i] & rq$date <= seq(mo$date[i], by = "12 months", length.out = 2)[2]]
  if (length(w) < 12) NA_real_ else mean(w) - mo$repo[i]
}, 0)
mo$gslope <- mo$gov10y_rb - mo$jibar3m_rb
rel <- do.call(rbind, lapply(names(swap_tenors()), function(k) {
  s <- mo[[k]] - mo$jibar3m_rb
  f1 <- stats::lm(s ~ mo$gslope); f2 <- stats::lm(s ~ mo$fwd12)
  data.frame(col = k, tenor = swap_tenors()[[k]], n = length(s), mean = mean(s), sd = stats::sd(s),
             b_slope = coef(f1)[2], t_slope = nw(f1, 6)[2], r2_slope = summary(f1)$r.squared,
             b_path = coef(f2)[2], t_path = nw(f2, 6)[2], r2_path = summary(f2)$r.squared,
             n_path = sum(is.finite(mo$fwd12)), stringsAsFactors = FALSE)
}))
rownames(rel) <- NULL
## the claims in the text: neither driver is significant at the 3- to 5-year
## points where the hedge is struck
guard(all(abs(rel$t_slope[rel$tenor %in% c(3, 5)]) < 1.645) && all(abs(rel$t_path[rel$tenor %in% c(3, 5)]) < 1.645),
      "a driver is significant for the 3- or 5-year swap spread: revisit the constant-spread rule")
gov_spread <- do.call(rbind, lapply(list(c("swap3y", "gov3y", 3), c("swap5y", "gov5y", 5), c("swap10y", "gov10y_rb", 10)),
  function(p) { d <- mo[[p[1]]] - mo[[p[2]]]
    data.frame(tenor = as.numeric(p[3]), mean = mean(d), min = min(d), max = max(d)) }))
guard(all(gov_spread$max < 0), "a swap traded above the government bond of its tenor")
basis <- mo$jibar3m_rb - mo$zaronia
jibar_zaronia <- c(mean = mean(basis), sd = stats::sd(basis), min = min(basis), max = max(basis))

## (c2) the 5-year bond-swap spread, the risk factor of an asset-swapped bond
##      (treasury.R). Both curves price the policy path, so a policy-driven
##      move should leave the spread alone, and the spread takes a share of
##      the rest of the long bond's move (fiscal premium, global yields).
##      beta: the 10-year yield's response to the benchmark, on three-year
##      changes since 2005 (the scenario horizon; overlapping, HAC t with 12
##      lags) and on quarterly changes; kappa: the share of the 10-year's
##      remaining move the 5-year bond-swap spread takes, on monthly levels
##      since the swap data start (HAC t), against month-on-month changes
q5 <- hist[is.finite(hist$gov10y) & is.finite(hist$jibar3m) & hist$date >= as.Date("2005-01-01"), ]
fb12 <- stats::lm(dg ~ dj, data.frame(dg = diff(q5$gov10y, 12), dj = diff(q5$jibar3m, 12)))
fb1 <- stats::lm(dg ~ dj, data.frame(dg = diff(q5$gov10y), dj = diff(q5$jibar3m)))
beta_bs <- unname(coef(fb12)[2])
mo$bs5 <- mo$gov5y - mo$swap5y
mo$xs <- mo$gov10y_rb - beta_bs * mo$jibar3m_rb
fk <- stats::lm(bs5 ~ xs, mo)
fk1 <- stats::lm(d_bs ~ d_xs, data.frame(d_bs = diff(mo$bs5), d_xs = diff(mo$xs)))
bs_model <- list(beta = beta_bs, beta_t = unname(nw(fb12, 12)[2]), beta_q = unname(coef(fb1)[2]),
                 beta_q_t = unname(nw(fb1, 4)[2]), n_beta = nrow(q5),
                 kappa = unname(coef(fk)[2]), kappa_t = unname(nw(fk, 6)[2]), kappa_r2 = summary(fk)$r.squared,
                 kappa_d1 = unname(coef(fk1)[2]), kappa_d1_t = unname(nw(fk1, 6)[2]), n_kappa = nrow(mo),
                 bs_mean = mean(mo$bs5), bs_min = min(mo$bs5), bs_max = max(mo$bs5),
                 from = min(mo$date), to = max(mo$date),
                 fit = data.frame(date = mo$date, bs5 = mo$bs5, gov10y = mo$gov10y_rb, jibar3m = mo$jibar3m_rb,
                                  fitted = unname(stats::fitted(fk))))
guard(bs_model$beta > 0 && bs_model$beta < 0.5 && bs_model$kappa > 0.1 && bs_model$kappa < 0.6 &&
      bs_model$kappa_t > 2 && bs_model$kappa_d1 < bs_model$kappa,
      "the bond-swap spread model is outside the range the text describes")

## (d) the hedge book's fixed rate: actual swaps from 2023Q4, back-cast
##     before; two alternative back-casts for the uncertainty
fx <- SC$hedge_fixed0
bq <- fx$quarters
ov <- hist[is.finite(hist$swap5y) & is.finite(hist$gov5y), ]
fr <- stats::lm(swap5y ~ gov5y + jibar3m, ov)
alt_reg <- bq$rate
bc <- bq$source == "back-cast"
alt_reg[bc] <- stats::predict(fr, hist[match(bq$date[bc], hist$date), ])
recent_sp <- mean(utils::tail(ov$swap5y - ov$gov5y, 4))
alt_recent <- bq$rate; alt_recent[bc] <- hist$gov5y[match(bq$date[bc], hist$date)] + recent_sp
fixed0 <- c(core = fx$fixed0, regression = mean(alt_reg), recent_spread = mean(alt_recent), proxy = fx$proxy)
guard(fx$n_actual >= 10 && fx$n_backcast <= 10, "the book's fixed rate leans on too many back-cast quarters")
guard(all(fixed0[c("core", "regression", "recent_spread")] < fixed0[["proxy"]] - 0.5),
      "every back-cast should put the book's rate well below the proxy")

## (e) what the correction does to NGFB, scenario by scenario
corr <- do.call(rbind, lapply(names(SC$results), function(k) {
  core <- SC$results[[k]]$ngfb; old <- SC$what_changed[[k]]
  data.frame(scenario = k, roe_v4 = core$summary$roe, roe_proxy = old$roe,
             ni_v4 = core$summary$net_income, ni_proxy = old$net_income,
             nim_v4 = core$summary$nim, nim_proxy = old$nim,
             hedge_pl_v4 = sum(core$run$quarterly$hedge_pl),
             roe_unhedged = SC$results[[k]]$ngfb_unhedged$summary$roe, stringsAsFactors = FALSE)
}))
corr$d_roe <- corr$roe_v4 - corr$roe_proxy
guard(all(corr$d_roe < 0), "the swap correction should lower NGFB's ROE in every scenario")

## ---- 2. NII sensitivity to rates (static balance sheet, prices only) --------------
base <- SC$base
b0 <- SC$results$baseline
nii_y1 <- function(r) sum(r$quarterly$nii[1:4])
static <- function(shift, ebs, hedge, base_inp) {
  s <- scenario_apply(base, list(repo = rep(shift, CFG$horizon)), hist, name = "shock")
  proj <- project_satellites(S1$sats, P$s1pan, s, ic_window = CFG$ic_window, ic_families = CFG$ic_families,
                             ic_mode = CFG$ic_mode)
  inp <- engine_inputs(proj, s, scenario_regimes(s, hist), share_params = NULL, mean_g = S2$mean_g,
                       pricing = N$pricing)
  st <- base_inp; st$rates <- inp$rates; st$line_rates <- inp$line_rates   # prices move, nothing else
  run_with(st, ebs = ebs, hedge = hedge)
}
sens_one <- function(label, ebs, hedge, base_inp) {
  r0 <- run_with(base_inp, ebs = ebs, hedge = hedge)
  up <- static(1, ebs, hedge, base_inp); dn <- static(-1, ebs, hedge, base_inp)
  data.frame(entity = label, nii_y1 = nii_y1(r0),
             up_y1 = nii_y1(up) - nii_y1(r0), dn_y1 = nii_y1(dn) - nii_y1(r0),
             up_3y = sum(up$quarterly$nii) - sum(r0$quarterly$nii), stringsAsFactors = FALSE)
}
nii_sens <- rbind(sens_one("NGFB with its hedge", N$engine_bs, SC$hedge0, b0$ngfb$inputs),
                  sens_one("NGFB without a hedge", N$engine_bs, NULL, b0$ngfb$inputs),
                  sens_one("Sector (unhedged)", sec_ebs, NULL, b0$sector$inputs))
nii_sens$up_pct <- nii_sens$up_y1 / nii_sens$nii_y1
nii_sens$dn_pct <- nii_sens$dn_y1 / nii_sens$nii_y1
## the hedge ratio at which NGFB's first-year NII does not move with a
## parallel 100 bp rise (linear in the notional: each rand of receive-fixed
## notional loses 1% a year when JIBAR rises 1pp)
unh <- nii_sens[nii_sens$entity == "NGFB without a hedge", ]
neutral_ratio <- unh$up_y1 / (0.01 * N$nmd0)
## Published benchmark: Nedbank Group, 2025 annual results (results booklet,
## net margin analysis): "NII sensitivity for 1% change in interest rates:
## R1.5bn (Endowment hedging implemented to date: R14bn)"; NII R42 878m.
disclosure <- c(sens_bn = 1.5, nii_bn = 42.878, hedge_bn = 14)
guard(nii_sens$up_pct[nii_sens$entity == "NGFB without a hedge"] > 0, "unhedged NGFB should be asset-sensitive")
guard(neutral_ratio > 0 && neutral_ratio < ngfb_canonical()$hedge_ratio,
      "the rate-neutral hedge ratio should lie below the canonical ratio")

## ---- 3. jump-off reconciliation with a sector structural hedge ---------------------
sp <- P$spnl; w4 <- sp$date %in% utils::tail(sp$date[is.finite(sp$nii)], 4)
nii_act <- sum(sp$nii[w4])
sec_nmd <- sum(sec_ebs$balance[sec_ebs$line %in% c("beh_operational", "beh_notice", "beh_savings")])
sec_hedge <- list(notional = ngfb_canonical()$hedge_ratio * sec_nmd, tenor_q = 20L, fixed0 = fx$fixed0)
r_sec <- run_with(b0$sector$inputs, ebs = sec_ebs, hedge = NULL)
r_sec_h <- run_with(b0$sector$inputs, ebs = sec_ebs, hedge = sec_hedge)
recon <- data.frame(item = c("BA120, last four quarters", "Engine, sector unhedged", "Engine, sector with a hedge at real swap rates"),
                    nii = c(nii_act, nii_y1(r_sec), nii_y1(r_sec_h)),
                    nim = c(NA, mean(r_sec$quarterly$nim[1:4]), mean(r_sec_h$quarterly$nim[1:4])))
ta_ratio <- mean(r_sec$quarterly$total_assets[1:4]) / mean(sp$total_assets[w4])
recon$nii_scaled <- recon$nii / c(1, ta_ratio, ta_ratio)   # on BA120's balance-sheet size
gap_share <- (recon$nii_scaled[3] - recon$nii_scaled[2]) / (recon$nii_scaled[1] - recon$nii_scaled[2])

## ---- 4. funding spreads ------------------------------------------------------------
z <- P$s1pan; z$s_ncd <- z$c_ncd - z$jibar3m
z <- z[is.finite(z$s_ncd) & is.finite(z$gdp_yy) & is.finite(z$d_repo), ]
z$s_l1 <- lag_vec(z$s_ncd, 1L); z$d_repo_l1 <- lag_vec(z$d_repo, 1L)
cands <- c("gdp_yy", "d4_unemp", "gdp_neg", "slope", "d_gov10y")
fund <- do.call(rbind, lapply(c("none", cands), function(v) {
  fm <- stats::as.formula(paste("s_ncd ~ s_l1 + d_repo + d_repo_l1", if (v != "none") paste("+", v) else ""))
  f <- stats::lm(fm, z); tt <- nw(f, 4)
  data.frame(driver = v, coef = if (v == "none") NA else coef(f)[[v]], t = if (v == "none") NA else tt[[v]],
             d_repo = coef(f)[["d_repo"]], t_d_repo = tt[["d_repo"]], bic = stats::BIC(f), n = stats::nobs(f),
             stringsAsFactors = FALSE)
}))
guard(all(abs(fund$t[fund$driver != "none"]) < 1.645), "a macro driver of the NCD spread is significant: adopt it")
guard(fund$t_d_repo[fund$driver == "none"] < -1.645, "the repricing lag should be significant")
fund_hist <- z[, c("date", "s_ncd", "d_repo", "gdp_yy")]
## the overlay: wholesale funding (and the NCD share of term deposits) 100 bp
## dearer for the whole horizon, by scenario
fund_overlay <- do.call(rbind, lapply(names(SC$results), function(k) {
  inp <- SC$results[[k]]$ngfb$inputs; r0 <- SC$results[[k]]$ngfb$run
  lr <- inp$line_rates; lr$wholesale_other <- lr$wholesale_other + 1; lr$beh_term <- lr$beh_term + 0.2
  inp$line_rates <- lr
  r1 <- run_with(inp)
  data.frame(scenario = k, d_roe = mean(r1$quarterly$roe) - mean(r0$quarterly$roe),
             d_ni = sum(r1$quarterly$net_income) - sum(r0$quarterly$net_income))
}))

## ---- 5. JIBAR -> ZARONIA --------------------------------------------------------------
## (a) the switch: at a given policy rate, ZARONIA + CAS less JIBAR (both
##     spreads over the policy rate measured on the latest eight quarters)
jt <- jibar_transition()
bsp <- benchmark_spreads(hist)
switch_level <- bsp[["zaronia"]] + bsp[["cas"]] - bsp[["jibar"]]
guard(abs(switch_level) < 0.10, "ZARONIA + CAS should sit within 10 bp of JIBAR at a given policy rate")
bl <- SC$results$baseline$scenario
guard(all(abs(bl$float3m[bl$date > jt$last_fixing] - bl$zaronia[bl$date > jt$last_fixing] - jt$cas) < 1e-12),
      "the baseline does not switch to ZARONIA + CAS after the last fixing")
n_post <- sum(bl$date > jt$last_fixing)
## what the switch does: each scenario re-run with JIBAR kept as the benchmark
## to the end of the horizon (its spread over the policy rate, the swap curve
## and every model input that depends on it)
keep_jibar <- function(s) {
  s$float3m <- s$repo + bsp[["jibar"]]; s$tbill <- s$float3m - 0.15; s$slope <- s$gov10y - s$float3m
  add_swap_curve(s, hist)
}
switch_effect <- do.call(rbind, lapply(names(SC$results), function(k) {
  s <- keep_jibar(SC$results[[k]]$scenario)
  proj <- project_satellites(S1$sats, P$s1pan, s, ic_window = CFG$ic_window, ic_families = CFG$ic_families,
                             ic_mode = CFG$ic_mode)
  inp <- engine_inputs(proj, s, scenario_regimes(s, hist), share_params = N$share_params, mean_g = S2$mean_g,
                       pricing = N$pricing)
  r <- run_with(inp); r0 <- SC$results[[k]]$ngfb$run
  data.frame(scenario = k, d_roe = mean(r0$quarterly$roe) - mean(r$quarterly$roe),
             d_nii = sum(r0$quarterly$nii) - sum(r$quarterly$nii), stringsAsFactors = FALSE)
}))
guard(all(abs(switch_effect$d_roe) < 0.0010), "the switch to ZARONIA + CAS should move ROE by less than 10 bp")
## the basis the fixed spread replaces: monthly averages of daily fixings
## (above), and the longer quarterly history of 3-month JIBAR over the
## policy rate, whose stress widenings no longer reach the benchmark
jr <- P$macro[is.finite(P$macro$jibar3m) & is.finite(P$macro$repo) & P$macro$date <= CFG$date0, c("date", "jibar3m", "repo")]
jr$spread <- jr$jibar3m - jr$repo
jr <- jr[jr$date >= CFG$sample_from, ]
jibar_repo <- list(hist = jr, median = stats::median(jr$spread), p95 = unname(stats::quantile(jr$spread, 0.95)),
                   max = max(jr$spread), date_max = jr$date[which.max(jr$spread)])
## (b) NGFB's exposure to the benchmark: floating-rate assets, floating
##     funding and the floating leg of the structural hedge
eb <- N$engine_bs
fl_a <- eb$line[eb$ref == "float3m" & eb$side == "asset"]
fl_l <- eb$line[eb$ref == "float3m" & eb$side != "asset"]
guard(length(fl_a) >= 2 && length(fl_l) >= 1, "the engine balance sheet has no lines on the floating benchmark")
benchmark_exposure <- c(assets = sum(eb$balance[eb$line %in% fl_a]), funding = sum(eb$balance[eb$line %in% fl_l]),
                        hedge = SC$hedge0$notional)
benchmark_exposure[["net"]] <- benchmark_exposure[["assets"]] - benchmark_exposure[["funding"]] - benchmark_exposure[["hedge"]]
## what 10 bp of basis would have done to annual NII while the benchmark was
## JIBAR (the jump-off book; the fixed spread takes it to zero from 2027)
basis_10bp <- 0.001 * benchmark_exposure[["net"]]
## (c) the spread at risk: transitioned floating-rate assets re-papered at
##     ZARONIA plus the old margin, without the 16.19 bp, from 2027Q1 (an
##     upper bound: every floating asset, the whole spread); and the same on
##     floating funding as well (the symmetric case). The hedge keeps it:
##     converted swaps pay ZARONIA + 16.19 bp on the floating leg.
cas_risk <- do.call(rbind, lapply(names(SC$results), function(k) {
  inp <- SC$results[[k]]$ngfb$inputs; r0 <- SC$results[[k]]$ngfb$run
  pst <- inp$line_rates$date > jt$last_fixing
  do.call(rbind, lapply(c("assets", "assets_and_funding"), function(case) {
    lr <- inp$line_rates
    for (ln in if (case == "assets") fl_a else c(fl_a, fl_l)) lr[[ln]][pst] <- lr[[ln]][pst] - jt$cas
    i2 <- inp; i2$line_rates <- lr; r1 <- run_with(i2)
    data.frame(scenario = k, case = case, d_roe = mean(r1$quarterly$roe) - mean(r0$quarterly$roe),
               d_nii = sum(r1$quarterly$nii) - sum(r0$quarterly$nii),
               d_ni = sum(r1$quarterly$net_income) - sum(r0$quarterly$net_income), stringsAsFactors = FALSE)
  }))
}))
guard(all(cas_risk$d_roe[cas_risk$case == "assets"] < 0), "losing the spread on assets should cost ROE")

saveRDS(list(proxy_vs_actual = proxy_vs_actual, proxy_err = proxy_err, spreads = spreads, relationships = rel,
             gov_spread = gov_spread, monthly = mo, jibar_zaronia = jibar_zaronia, bs_model = bs_model, fixed0 = fixed0,
             fixed0_quarters = bq, fixed0_reg = coef(fr), correction = corr, nii_sens = nii_sens,
             neutral_ratio = neutral_ratio, disclosure = disclosure, recon = recon, gap_share = gap_share,
             sec_hedge = sec_hedge, funding = fund, funding_hist = fund_hist, fund_overlay = fund_overlay,
             transition = list(facts = jt, spreads = bsp, switch_level = switch_level, n_post = n_post,
                               switch_effect = switch_effect,
                               jibar_repo = jibar_repo, exposure = benchmark_exposure, basis_10bp = basis_10bp,
                               lines_assets = fl_a, lines_funding = fl_l, cas_risk = cas_risk)),
        out_path("audit.rds"))
ngfb_log("audit: book fixed rate %.2f%% (proxy %.2f%%); NGFB baseline ROE %.1f%% (proxy %.1f%%); rate-neutral hedge ratio %.0f%%",
         fixed0[["core"]], fixed0[["proxy"]], 100 * corr$roe_v4[1], 100 * corr$roe_proxy[1], 100 * neutral_ratio)
