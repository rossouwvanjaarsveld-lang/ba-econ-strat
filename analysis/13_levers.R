# =============================================================================
#  analysis/13_levers.R -- more levers that tie in with the macro view
#
#  Each lever runs through the full engine in every scenario (and in the
#  downside-with-cuts variant), against NGFB's plan:
#   1. Hedge ratio, 0-80% of non-maturity deposits, changed now with swaps
#      at the 5-year rate: receive-fixed to add, pay-fixed to offset. The
#      existing book's locked-in carry is kept either way (an offset, or an
#      unwind under hedge accounting, leaves it in NII); what changes is the
#      exposure from here on.
#   2. Hedge reinvestment tenor: maturing tranches reinvested at 3, 5, 7 or
#      10 years, at the scenario's swap rate for that tenor.
#   3. A hedge of equity: half of equity received fixed for 5 years, struck
#      now.
#   4. HQLA duration: a quarter of the earning HQLA in 10-year government
#      bonds bought at the jump-off instead of Treasury bills. The carry runs
#      through NII; if the bonds are held at fair value through OCI, the
#      mark-to-market along the scenario's 10-year yield runs through CET1.
#   5. Capital: payout ratios of 30-70%, and a distribution at the jump-off
#      of capital above the 12.5% CET1 target.
#   6. Deposit pricing: passing 10% less of each policy-rate rise and 10%
#      more of each cut through to non-maturity deposit rates than the
#      sector does (the volume response is not modelled: an upper bound).
#   7. Asset-swapped government bonds: a quarter of the earning HQLA in
#      5-year government bonds bought at the jump-off with a pay-fixed 5-year
#      swap against them. Earns the bond-swap spread (the fiscal premium) and
#      the bills' discount to the floating benchmark, without the duration of
#      lever 4; the spread's mark-to-market runs through CET1 (treasury.R).
#      The spread moves with the part of the 10-year yield's move the policy
#      rate does not explain (estimated in 12_audit.R).
#   8. Liquidity buffer: Treasury bills released from the HQLA buffer to
#      repay wholesale funding until the LCR proxy is 180%, 170% or 160% at
#      the jump-off (against about 190% in the plan and 150% published for the
#      sector; 160% repays nearly all of NGFB's wholesale funding, the limit
#      of this use of the bills). Saves the wholesale spread over bills;
#      costs liquidity coverage.
#  The robust choice weighs the scenarios with the judgemental probabilities
#  (CFG$scenario_weights); the downside-with-cuts variant is reported but
#  carries no weight. All macro paths are known (perfect foresight).
# =============================================================================
source("analysis/00_config.R")
N  <- readRDS(out_path("ngfb.rds"))
SC <- readRDS(out_path("scenarios.rds"))
A  <- readRDS(out_path("audit.rds"))
ea <- SC$eng_args
w  <- CFG$scenario_weights
scen_inputs <- c(lapply(SC$results, function(r) r$ngfb$inputs), list(downside_cuts = SC$variants$downside_cuts$ngfb$inputs))
core_runs <- c(lapply(SC$results, function(r) r$ngfb$run), list(downside_cuts = SC$variants$downside_cuts$ngfb$run))
nmd0 <- N$nmd0; eq0 <- sum(N$engine_bs$balance[N$engine_bs$side == "equity"])
h0 <- SC$hedge0
ht <- c(h0, list(new_ref = "swap5y", new_tenor_q = 20L))        # tranche form of the plan's hedge
## positions are taken at the jump-off, at the market's prices then: the
## jump-off quarter's 5-year swap rate and 10-year government yield (not the
## first projected quarter's, which already carries the scenario's shock)
P <- readRDS(out_path("panels.rds"))
jo <- P$macro[P$macro$date == CFG$date0, ]
swap5_jo <- jo$swap5y; gov10_jo <- jo$gov10y
guard(length(swap5_jo) == 1 && is.finite(swap5_jo) && is.finite(gov10_jo), "no market rates at the jump-off")

run_lever <- function(k, hedge = ht, ebs = N$engine_bs, line_rates = NULL, ...) {
  inp <- scen_inputs[[k]]
  a <- utils::modifyList(ea, list(...))
  do.call(run_engine, c(list(bs = ebs, growth = inp$growth, rates = inp$rates, credit = inp$credit,
                             ratios = inp$ratios, hedge = hedge, line_rates = line_rates %||% inp$line_rates), a))
}
metrics <- function(r) {
  s <- summarise_run(r)
  data.frame(roe = s$roe, roe_y1 = s$roe_y1, ni = s$net_income, nii = sum(r$quarterly$nii),
             nim = s$nim, cet1_end = s$cet1_end, cet1_min = s$cet1_min)
}
## the tranche form reproduces the plan exactly in every scenario
for (k in names(scen_inputs))
  guard(max(abs(run_lever(k)$quarterly$roe - core_runs[[k]]$quarterly$roe)) < 1e-12,
        "the tranche hedge does not reproduce the plan in %s", k)
core <- do.call(rbind, lapply(names(scen_inputs), function(k) cbind(scenario = k, metrics(core_runs[[k]]))))
lever_rows <- function(lever, setting, runs) do.call(rbind, lapply(names(runs), function(k) {
  m <- metrics(runs[[k]]); c0 <- core[core$scenario == k, ]
  data.frame(lever = lever, setting = setting, scenario = k, m, d_roe = m$roe - c0$roe, d_ni = m$ni - c0$ni,
             d_nii = m$nii - c0$nii, d_cet1_end = m$cet1_end - c0$cet1_end, stringsAsFactors = FALSE)
}))
all_runs <- function(...) { r <- lapply(names(scen_inputs), function(k) run_lever(k, ...)); names(r) <- names(scen_inputs); r }

## ---- 1. hedge ratio ---------------------------------------------------------------
ratios <- seq(0, 0.80, by = 0.05)
hedge_at <- function(r) c(ht, list(extra_notional = r * nmd0 - h0$notional, extra_tenor_q = 20L, extra_rate = swap5_jo))
ratio_tab <- do.call(rbind, lapply(ratios, function(r) lever_rows("Hedge ratio", sprintf("%.0f%%", 100 * r),
                                                                    all_runs(hedge = hedge_at(r)))))
ratio_tab$ratio <- as.numeric(sub("%", "", ratio_tab$setting)) / 100

## ---- 2. reinvestment tenor ------------------------------------------------------------
tenors <- c(swap3y = 12L, swap5y = 20L, swap7y = 28L, swap10y = 40L)
tenor_tab <- do.call(rbind, lapply(names(tenors), function(ref)
  lever_rows("Reinvestment tenor", sprintf("%d years", tenors[[ref]] / 4L),
             all_runs(hedge = utils::modifyList(ht, list(new_ref = ref, new_tenor_q = tenors[[ref]]))))))
## the risk that comes with it: the hedge book's PV01 at the end of the
## horizon (rand per basis point), from the deterministic tranche schedule
annuity <- function(m, y = 7.5) if (m <= 0) 0 else sum(0.25 / (1 + y / 400)^seq_len(round(4 * m)))
book_pv01 <- function(tenor_q, nq = CFG$horizon, notional = h0$notional, n0 = h0$tenor_q) {
  tr <- notional / n0
  old <- seq_len(n0) - nq; old <- old[old > 0]                       # original tranches still alive
  new <- tenor_q - (nq - seq_len(min(nq, n0)))                         # reinvested tranches' remaining life
  sum(vapply(c(old, new) / 4, annuity, 0)) * tr * 1e-4
}
pv01 <- data.frame(setting = sprintf("%d years", tenors / 4L), tenor_q = tenors,
                   pv01 = vapply(tenors, book_pv01, 0))
pv01$vs_plan <- pv01$pv01 / pv01$pv01[pv01$tenor_q == 20L] - 1

## ---- 3. a hedge of equity --------------------------------------------------------------
eq_tab <- lever_rows("Equity hedge", "Half of equity, 5 years",
                     all_runs(hedge = c(ht, list(extra_notional = 0.5 * eq0, extra_tenor_q = 20L, extra_rate = swap5_jo))))

## ---- 4. HQLA duration ---------------------------------------------------------------------
theta <- 0.25
hq <- N$pricing$hqla; sc_hq <- hq$scale %||% 1
md <- function(m, y) if (m <= 0) 0 else { cf <- c(rep(y / 2, round(2 * m) - 1), 100 + y / 2)
  t <- seq_along(cf) / 2; d <- 1 / (1 + y / 200)^(2 * t); sum(t * cf * d) / sum(cf * d) / (1 + y / 200) }
hqla_runs <- list(); hqla_mtm <- list()
for (k in names(scen_inputs)) {
  inp <- scen_inputs[[k]]; r0 <- core_runs[[k]]
  y0 <- gov10_jo
  B <- theta * sc_hq * N$engine_bs$balance[N$engine_bs$line == "hqla"]   # rand bought at the jump-off
  bal <- r0$balances["hqla", ]; avg <- (bal[-1] + bal[-length(bal)]) / 2
  lr <- inp$line_rates
  lr$hqla <- lr$hqla + B * (y0 - inp$rates$tbill) / avg                   # the bond carry over bills (pp on the line)
  hqla_runs[[k]] <- run_lever(k, line_rates = lr)
  ## fair value through OCI: price change along the scenario's 10-year yield
  ## (modified duration of the bond's remaining life), after tax
  yt <- inp$rates$gov10y; rem <- 10 - seq_along(yt) / 4
  dv <- -vapply(seq_along(yt), function(i) md(rem[i], y0), 0) * (yt - y0) / 100 * B * (1 - ea$tax)
  rwa <- hqla_runs[[k]]$quarterly$rwa
  hqla_mtm[[k]] <- data.frame(date = inp$rates$date, gov10y = yt, oci = dv, d_cet1 = dv / rwa)
}
hqla_tab <- lever_rows("HQLA duration", "A quarter of HQLA in 10-year bonds", hqla_runs)
hqla_tab$d_cet1_oci_end <- vapply(hqla_tab$scenario, function(k) utils::tail(hqla_mtm[[k]]$d_cet1, 1), 0)
hqla_tab$d_cet1_oci_min <- vapply(hqla_tab$scenario, function(k) min(hqla_mtm[[k]]$d_cet1), 0)
hqla_tab$gov10y_change <- vapply(hqla_tab$scenario, function(k) {
  g <- hqla_mtm[[k]]$gov10y; utils::tail(g, 1) - g[1] }, 0)

## ---- 5. capital ----------------------------------------------------------------------------
payout_tab <- do.call(rbind, lapply(c(0.3, 0.5, 0.7), function(p)
  lever_rows("Payout", sprintf("%.0f%% of earnings", 100 * p), all_runs(payout = p))))
eb <- N$engine_bs; is_a <- eb$side == "asset"
rwa0 <- sum(eb$balance[is_a] * eb$rw[is_a]) + ea$rwa_other * sum(eb$balance[is_a])
cet1_0 <- ea$cet1_share * eq0 / rwa0
excess <- max(0, (cet1_0 - CFG$cet1_target) * rwa0 / ea$cet1_share)
eb2 <- N$engine_bs
eb2$balance[eb2$side == "equity"] <- eb2$balance[eb2$side == "equity"] * (1 - excess / eq0)
eb2$balance[eb2$line == ea$plug] <- eb2$balance[eb2$line == ea$plug] + excess
dist_tab <- lever_rows("Distribution", "Capital above a 12.5% CET1 returned at the jump-off", all_runs(ebs = eb2))

## ---- 6. deposit pricing ------------------------------------------------------------------
nmd_lines <- c("beh_operational", "beh_notice", "beh_savings")
repo0 <- P$macro$repo[P$macro$date == CFG$date0]
guard(length(repo0) == 1 && is.finite(repo0), "no policy rate at the jump-off")
dep_runs <- lapply(names(scen_inputs), function(k) {
  inp <- scen_inputs[[k]]; lr <- inp$line_rates
  dr <- inp$rates$repo - repo0                       # change from the jump-off quarter
  for (ln in nmd_lines) lr[[ln]] <- lr[[ln]] - 0.10 * abs(dr)
  run_lever(k, line_rates = lr)
})
names(dep_runs) <- names(scen_inputs)
dep_tab <- lever_rows("Deposit pricing", "10% less of rises, 10% more of cuts", dep_runs)

## ---- 7. asset-swapped government bonds -----------------------------------------------
H0 <- sc_hq * N$engine_bs$balance[N$engine_bs$line == "hqla"]            # earning HQLA at the jump-off
jo_as <- list(gov10y = gov10_jo, float3m = jo$jibar3m, gov5y = jo$gov5y, bs = jo$gov5y - jo$swap5y)
guard(all(is.finite(unlist(jo_as))) && jo_as$bs > 0, "no bond-swap spread at the jump-off")
bsm <- A$bs_model
as_book <- list(); as_runs <- list()
for (k in names(scen_inputs)) {
  inp <- scen_inputs[[k]]
  bb <- bond_book(rep(theta, CFG$horizon), H0, inp$rates, jo_as, type = "asset_swap", tax = ea$tax,
                  spread = asset_swap_spread(inp$rates, jo_as, bsm$beta, bsm$kappa))
  as_book[[k]] <- bb
  as_runs[[k]] <- run_lever(k, overlay = books_overlay(inp$rates$date, list(bb)))
}
as_tab <- lever_rows("Asset swap", "A quarter of HQLA in asset-swapped 5-year bonds", as_runs)
as_tab$d_cet1_oci_min <- vapply(as_tab$scenario, function(k) min(as_book[[k]]$oci / as_runs[[k]]$quarterly$rwa), 0)
as_tab$d_cet1_oci_end <- vapply(as_tab$scenario, function(k) utils::tail(as_book[[k]]$oci / as_runs[[k]]$quarterly$rwa, 1), 0)
as_tab$spread_end <- vapply(as_tab$scenario, function(k) utils::tail(as_book[[k]]$spread, 1), 0)
as_tab$spread_max <- vapply(as_tab$scenario, function(k) max(as_book[[k]]$spread), 0)
as_spv01 <- as_book$baseline$spv01[1]
## a 100 bp widening of the spread at the jump-off, after tax, as a share of
## CET1 (the credit-spread risk in the banking book the episodes may not show)
eb0 <- N$engine_bs; ia <- eb0$side == "asset"
rwa_jo <- sum(eb0$balance[ia] * eb0$rw[ia]) + ea$rwa_other * sum(eb0$balance[ia])
as_shock100 <- -100 * as_spv01 * (1 - ea$tax) / rwa_jo
guard(all(as_tab$d_roe > 0), "the asset swap should pay in every scenario")

## ---- 8. the size of the liquidity buffer ------------------------------------------------------
lcr_targets <- c(1.80, 1.70, 1.60)
lcr0 <- liquidity_ratios(N$engine_bs$balance, N$engine_bs)[["lcr"]]
rel_at <- vapply(lcr_targets, function(tg) buffer_release_for_lcr(N$engine_bs, tg, scale = sc_hq), 0)
buf_tab <- do.call(rbind, lapply(seq_along(lcr_targets), function(i) {
  runs <- all_runs(buffer = list(release = rel_at[i], scale = sc_hq))
  for (k in names(runs)) guard(all(runs[[k]]$quarterly$plug_balance > 0), "the buffer release exhausts wholesale funding")
  d <- lever_rows("Liquidity buffer", sprintf("LCR %.0f%% at the jump-off", 100 * lcr_targets[i]), runs)
  d$release <- rel_at[i] * N$engine_bs$balance[N$engine_bs$line == "hqla"]
  d$lcr_min <- vapply(d$scenario, function(k) min(runs[[k]]$quarterly$lcr), 0)
  d$nsfr_min <- vapply(d$scenario, function(k) min(runs[[k]]$quarterly$nsfr), 0)
  d
}))
guard(all(buf_tab$d_roe > 0) && all(buf_tab$lcr_min > CFG$lcr_min), "a smaller buffer pays and keeps the LCR above the floor")
plan_liq <- data.frame(scenario = names(core_runs), lcr_min = vapply(core_runs, function(r) min(r$quarterly$lcr), 0),
                       nsfr_min = vapply(core_runs, function(r) min(r$quarterly$nsfr), 0),
                       lcr_q1 = vapply(core_runs, function(r) r$quarterly$lcr[1], 0))
## what each rand released saves, from first principles: the wholesale rate
## less the bill rate (pp, the baseline's average)
bl_r <- scen_inputs$baseline
buf_spread <- mean(bl_r$line_rates$wholesale_other - bl_r$rates$tbill)

## ---- 9. the levers that pay in every scenario, together ---------------------------------
## ten-year reinvestment, a quarter of HQLA in ten-year bonds and the capital
## above the CET1 target returned (deposit pricing is left out: its volume
## cost is not modelled)
combo_runs <- lapply(names(scen_inputs), function(k) {
  inp <- scen_inputs[[k]]; r0 <- core_runs[[k]]; y0 <- gov10_jo
  B <- theta * sc_hq * N$engine_bs$balance[N$engine_bs$line == "hqla"]
  bal <- r0$balances["hqla", ]; avg <- (bal[-1] + bal[-length(bal)]) / 2
  lr <- inp$line_rates; lr$hqla <- lr$hqla + B * (y0 - inp$rates$tbill) / avg
  run_lever(k, hedge = utils::modifyList(ht, list(new_ref = "swap10y", new_tenor_q = 40L)), ebs = eb2, line_rates = lr)
})
names(combo_runs) <- names(scen_inputs)
combo_tab <- lever_rows("Combined", "Ten-year reinvestment, HQLA duration and the distribution", combo_runs)
combo_tab$d_cet1_oci_min <- hqla_tab$d_cet1_oci_min[match(combo_tab$scenario, hqla_tab$scenario)]
## the CET1 trough with the bonds at fair value through OCI (a lower bound:
## the OCI trough and the CET1 trough need not fall in the same quarter)
combo_tab$cet1_min_fvoci <- combo_tab$cet1_min + combo_tab$d_cet1_oci_min
guard(all(combo_tab$cet1_min > CFG$cet1_min), "the combined levers breach the CET1 floor before any mark-to-market")

## ---- summary: robust and scenario-contingent choices -------------------------------------------
levers <- rbind(ratio_tab[, setdiff(names(ratio_tab), "ratio")], tenor_tab, eq_tab,
                hqla_tab[, names(tenor_tab)], as_tab[, names(tenor_tab)], buf_tab[, names(tenor_tab)],
                payout_tab, dist_tab, dep_tab, combo_tab[, names(tenor_tab)])
wsum <- function(d, v) sum(vapply(names(w), function(k) w[[k]] * d[[v]][d$scenario == k], 0))
summ <- do.call(rbind, lapply(split(levers, list(levers$lever, levers$setting), drop = TRUE), function(d)
  data.frame(lever = d$lever[1], setting = d$setting[1],
             d_roe_weighted = wsum(d, "d_roe"), d_ni_weighted = wsum(d, "d_ni"),
             d_roe_baseline = d$d_roe[d$scenario == "baseline"], d_roe_upside = d$d_roe[d$scenario == "upside"],
             d_roe_downside = d$d_roe[d$scenario == "downside"], d_roe_cuts = d$d_roe[d$scenario == "downside_cuts"],
             d_roe_worst = min(d$d_roe[d$scenario %in% names(w)]),
             cet1_min = min(d$cet1_min[d$scenario %in% names(w)]), stringsAsFactors = FALSE)))
rownames(summ) <- NULL
## the hedge ratio: the probability-weighted best, the most stable (smallest
## ROE range across the weighted scenarios) and the best in each scenario
rr <- do.call(rbind, lapply(split(ratio_tab, ratio_tab$ratio), function(d) data.frame(
  ratio = d$ratio[1], weighted = sum(vapply(names(w), function(k) w[[k]] * d$roe[d$scenario == k], 0)),
  range = diff(range(d$roe[d$scenario %in% names(w)])), worst = min(d$roe[d$scenario %in% names(w)]),
  baseline = d$roe[d$scenario == "baseline"], upside = d$roe[d$scenario == "upside"],
  downside = d$roe[d$scenario == "downside"], cuts = d$roe[d$scenario == "downside_cuts"])))
ratio_best <- c(weighted = rr$ratio[which.max(rr$weighted)], stable = rr$ratio[which.min(rr$range)],
                worst_case = rr$ratio[which.max(rr$worst)],
                baseline = rr$ratio[which.max(rr$baseline)], upside = rr$ratio[which.max(rr$upside)],
                downside = rr$ratio[which.max(rr$downside)], cuts = rr$ratio[which.max(rr$cuts)])
guard(all(abs(ratio_tab$d_roe[abs(ratio_tab$ratio - 0.50) < 1e-9 & ratio_tab$scenario == "baseline"]) < 0.002),
      "a 50% hedge should sit next to the plan's 49.6%")

saveRDS(list(levers = levers, summary = summ, core = core, ratio_curve = rr, ratio_best = ratio_best, hqla = hqla_tab,
             asset_swap = as_tab, as_jo = jo_as, as_spv01 = as_spv01, as_shock100 = as_shock100,
             as_spread_paths = lapply(as_book, `[[`, "spread"), bs_model = bsm[setdiff(names(bsm), "fit")],
             buffer = buf_tab, lcr_targets = lcr_targets, lcr0 = lcr0, release_share = rel_at, plan_liq = plan_liq,
             buf_spread = buf_spread, H0 = H0,
             swap5_jo = swap5_jo, gov10_jo = gov10_jo,
             combo = combo_tab,
             pv01 = pv01, hqla_mtm = hqla_mtm, theta = theta, excess = excess, cet1_0 = cet1_0,
             eq0 = eq0, nmd0 = nmd0, weights = w), out_path("levers.rds"))
best <- summ[which.max(summ$d_roe_weighted), ]
ngfb_log("levers: %d lever settings x %d scenarios; best probability-weighted: %s (%s) %+.0f bps of ROE",
         nrow(summ), length(scen_inputs), best$lever, best$setting, 1e4 * best$d_roe_weighted)
