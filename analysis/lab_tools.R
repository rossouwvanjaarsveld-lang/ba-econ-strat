# =============================================================================
#  analysis/lab_tools.R -- the strategies of the strategy lab and of the
#                          risk-budgeted optimiser
#
#  Shared by 14_strategy_lab.R and 15_risk_budget.R, so that the optimiser
#  runs exactly the strategies the lab tests. Sourced with `local = TRUE` by a
#  script that has already defined:
#    CFG, N (ngfb.rds), SC (scenarios.rds), A (audit.rds),
#    hist  the macro history to the jump-off,
#    hj    the same with the scenario's junction values for quarters not yet
#          published (pf_history),
#    ea = SC$eng_args, h0 = SC$hedge0, nmd0 = N$nmd0, n = CFG$horizon.
#  Defines the engine call `run()`, the hedge, bond-book, buffer, payout and
#  growth strategies, the signals the rules read, and the metrics.
# =============================================================================

## ---- the engine under a strategy -------------------------------------------------------
## books: bond books held in HQLA (treasury.R); release: the share of the HQLA
## line released from the buffer each quarter (Treasury bills repaying
## wholesale funding)
run <- function(inp, hedge = ht, payout = ea$payout, line_rates = inp$line_rates, growth = inp$growth,
                rates = inp$rates, books = list(), release = NULL) {
  a <- ea; a$payout <- payout
  r <- do.call(run_engine, c(list(bs = N$engine_bs, growth = growth, rates = rates, credit = inp$credit,
                                  ratios = inp$ratios, hedge = hedge, line_rates = line_rates,
                                  overlay = if (length(books)) books_overlay(inp$rates$date, books),
                                  buffer = if (!is.null(release)) list(release = release, scale = sc_hq)), a))
  ## quarters in which retained earnings and deposit growth have repaid all
  ## wholesale funding: the engine then holds the surplus in HQLA, which
  ## undoes part of a release
  r$plug_zero <- sum(r$quarterly$plug_balance <= 1e-6)
  r$books <- books
  r
}
ht <- c(h0, list(new_ref = "swap5y", new_tenor_q = 20L))         # the plan's hedge, tranche form

## ---- the structural hedge ------------------------------------------------------------------
jo <- hist[hist$date == CFG$date0, ]
jo_swap <- c(swap3y = jo$swap3y, swap5y = jo$swap5y, swap7y = jo$swap5y + 0.4 * (jo$swap10y - jo$swap5y),
             swap10y = jo$swap10y)
guard(all(is.finite(jo_swap)), "no swap rates at the jump-off")
tenors <- c(swap3y = 12L, swap5y = 20L, swap7y = 28L, swap10y = 40L)
plan_ratio <- h0$notional / nmd0
## a hedge ratio set now -- receive- or pay-fixed 5-year swaps at today's rate,
## laddered like the existing book -- with maturing tranches reinvested at `ref`
hedge_static <- function(ratio, ref = "swap5y") {
  h <- utils::modifyList(ht, list(new_ref = ref, new_tenor_q = tenors[[ref]]))
  x <- ratio * nmd0 - h0$notional
  if (abs(x) > 1) h <- c(h, list(extra_notional = x, extra_tenor_q = 20L, extra_rate = jo_swap[["swap5y"]]))
  h
}
## any ratio policy (a static ratio, or "R1") with any reinvestment policy (a
## tenor such as "swap10y", or "R2"); `rp` holds the rules' signals
hedge_spec <- function(ratio, tenor, rp = NULL) {
  r_rule <- identical(ratio, "R1"); t_rule <- identical(tenor, "R2")
  h <- hedge_static(if (r_rule) rp$ratio[1] else ratio, if (t_rule) "swap5y" else tenor)
  if (r_rule) h$target_path <- rp$ratio * nmd0
  if (t_rule) { h$new_ref <- rp$tenor; h$new_tenor_q <- unname(tenors[rp$tenor]) }
  h
}
hedge_rule <- function(rp, use_ratio = TRUE, use_tenor = FALSE)
  hedge_spec(if (use_ratio) "R1" else plan_ratio, if (use_tenor) "R2" else "swap5y", rp)
ratios <- sort(unique(round(c(0, 0.2, A$neutral_ratio, plan_ratio, 0.65, 0.8), 4)))

## ---- the liquidity book --------------------------------------------------------------------
## bond books: share[t] = share of the jump-off earning HQLA held in the book
## after quarter t's trade; bought at the jump-off's prices in quarter 1 and
## at the quarter's after; cut pro rata, realising the mark-to-market
## (treasury.R). The carry over Treasury bills runs through NII, realised
## gains through profit, the unrealised mark-to-market through the OCI
## reserve and CET1, and the whole mark-to-market through the HQLA stock at
## market value.
hq_rule <- N$pricing$hqla; sc_hq <- hq_rule$scale %||% 1
H0 <- sc_hq * N$engine_bs$balance[N$engine_bs$line == "hqla"]       # earning HQLA at the jump-off
## the jump-off's market: 10-year yield and benchmark for the duration book,
## the 5-year bond and the bond-swap spread for the asset swap
jo_mk <- list(gov10y = jo$gov10y, float3m = jo$jibar3m, gov5y = jo$gov5y, bs = jo$gov5y - jo$swap5y)
guard(all(is.finite(unlist(jo_mk))) && jo_mk$bs > 0, "no bond or swap rates at the jump-off")
## the bond-swap spread along a path: kappa of the 10-year yield's move net
## of beta times the benchmark's (estimated in 12_audit.R)
bsm <- A$bs_model
as_spread <- function(rates) asset_swap_spread(rates, jo_mk, beta = bsm$beta, kappa = bsm$kappa)
dur_book <- function(share, inp) bond_book(share, H0, inp$rates, jo_mk, "duration", tax = ea$tax)
## balance-sheet carry: asset-swapped 5-year government bonds held outside the
## buffer, encumbered in repo and funded at the rate of NGFB's existing repo
## funding (the policy rate plus its calibrated spread); `size` in rand
## ('000), one value per quarter
eb_rf <- N$engine_bs[N$engine_bs$line == "repo_funding", ]
guard(nrow(eb_rf) == 1 && eb_rf$ref == "repo" && is.finite(eb_rf$spread), "no repo funding line to price the carry book")
repo_sp <- 100 * eb_rf$spread                                         # pp over the policy rate
carry_book <- function(size, inp) bond_book(rep_len(size, nrow(inp$rates)) / H0, H0, inp$rates, jo_mk, "asset_swap",
                                            tax = ea$tax, spread = as_spread(inp$rates),
                                            funding = inp$rates$repo + repo_sp, in_hqla = FALSE)
carry_max <- 1e8                                                      # R100bn, the largest carry book tested
as_book <- function(share, inp) bond_book(share, H0, inp$rates, jo_mk, "asset_swap", tax = ea$tax,
                                          spread = as_spread(inp$rates))
## the buffer: the share of the HQLA line released (Treasury bills repaying
## wholesale funding) for a given LCR proxy at the jump-off
lcr_jo <- liquidity_ratios(N$engine_bs$balance, N$engine_bs)[["lcr"]]
rel_lcr <- function(tg) as.numeric(buffer_release_for_lcr(N$engine_bs, tg, scale = sc_hq))
rel_170 <- rel_lcr(1.70); rel_185 <- rel_lcr(1.85)
guard(lcr_jo > 1.85 && rel_170 > rel_185 && rel_185 > 0, "the buffer releases are not ordered")
## no more than 90% of the jump-off's earning HQLA leaves Treasury bills (for
## bonds or to repay funding): the rest stays in bills for day-to-day
## liquidity management
hq_cap <- 0.9

## ---- swap rates that price a share `phi` of the coming path ---------------------------------
## each tenor moves by phi x (average benchmark over the swap's life - today's
## benchmark), along the path and at its last value beyond the horizon
anticipate <- function(rates, phi) {
  if (phi == 0) return(rates)
  f <- rates$float3m
  ten <- c(swap1y = 1, swap2y = 2, swap3y = 3, swap5y = 5, swap7y = 7, swap10y = 10)
  for (k in intersect(names(ten), names(rates))) {
    L <- 4L * ten[[k]]
    fwd <- vapply(seq_along(f), function(t) mean(c(f, rep(utils::tail(f, 1), L))[t + seq_len(L)]), 0)
    rates[[k]] <- rates[[k]] + phi * (fwd - f)
  }
  rates
}

## ---- the signals each rule sees -----------------------------------------------------------------
## the previous quarter's value (the jump-off's in quarter 1)
lag1 <- function(x, x_jo) c(x_jo, utils::head(x, -1L))
repo_jo <- hj$repo[hj$date == CFG$date0]
slope_jo <- hj$gov10y[hj$date == CFG$date0] - hj$jibar3m[hj$date == CFG$date0]
gdp_jo <- hj$gdp_yy[hj$date == CFG$date0]
d4u_jo <- hj$unemp[hj$date == CFG$date0] - hj$unemp[match(seq(CFG$date0, by = "-12 months", length.out = 2)[2], hj$date)]
d4u_jo <- if (length(d4u_jo) && is.finite(d4u_jo)) d4u_jo else 0
zar_jo <- hj$zar_yy[hj$date == CFG$date0]
## the government curve's slope over the models' sample
sl_hist <- hist$gov10y - hist$jibar3m
sl_hist <- sl_hist[hist$date >= CFG$sample_from & is.finite(sl_hist)]
slope_med <- stats::median(sl_hist)
r_star <- CFG$anchors$repo
guard(all(is.finite(c(repo_jo, slope_jo, gdp_jo, slope_med, zar_jo))), "a rule has no jump-off signal")

## R1 ratio, R2 tenor, R3 duration share (theta), R4 good times, R5 tilt, R6
## asset-swap share (alpha), R7 stress and the release; and the books scaled
## so that, with the release, they stay within the HQLA cap (all seven)
rule_paths <- function(sc, inp) {
  rp <- lag1(sc$repo, repo_jo); sl <- lag1(sc$gov10y - sc$float3m, slope_jo)
  gd <- lag1(sc$gdp_yy, gdp_jo); du <- lag1(sc$d4_unemp, d4u_jo); zr <- lag1(sc$zar_yy, zar_jo)
  bs <- lag1(as_spread(inp$rates), jo_mk$bs)
  stress <- gd < 0 | zr > 15
  out <- list(ratio = pmin(pmax(plan_ratio + 0.10 * (rp - r_star), 0.20), 0.80),
              tenor = ifelse(rp >= r_star + 0.5, "swap10y", ifelse(rp <= r_star - 0.5, "swap3y", "swap5y")),
              theta = pmin(pmax(0.25 + 0.125 * (sl - slope_med), 0), 0.50),
              good = gd > 0,
              tilt = ifelse(du < 0 & gd > 1, 5, ifelse(du > 1 | gd < 0, -5, 0)),
              alpha = pmin(pmax(0.5 * (bs - 0.5), 0), 0.50),
              stress = stress, release = ifelse(stress, rel_185, rel_170), spread_seen = bs,
              carry = carry_max * pmin(pmax(bs - 0.5, 0), 1))
  out[c("theta_all", "alpha_all")] <- fit_cap(out$theta, out$alpha, out$release)
  out
}
## scale two book shares pro rata so that, with the release, no more than the
## cap of earning HQLA leaves bills
fit_cap <- function(theta, alpha, release) {
  room <- pmax(hq_cap - release / sc_hq, 0); used <- theta + alpha
  k <- ifelse(used > room, room / pmax(used, 1e-12), 1)
  list(theta * k, alpha * k)
}
payout_rule <- function(good) function(t, s) if (good[t] && s$cet1 > CFG$cet1_target) 0.7 else 0.3
unsecured <- c("cards", "personal_other")
tilt_growth <- function(inp, tilt) { g <- inp$growth; for (ln in unsecured) g[[ln]] <- g[[ln]] * exp(tilt / 400); g }

## ---- metrics -----------------------------------------------------------------------------------
## CET1 at fair value: the engine's ratio (realised gains are in earnings)
## plus the books' OCI reserve. Rate risk added: the PV01 of the receive-
## fixed hedge book and of the duration book at the end of the horizon (rand
## per basis point), and what a 200 bps parallel rise would take off their
## value as a share of CET1 -- the IRRBB outlier test's yardstick (relative to
## the plan; the deposits' own behavioural duration is the same under every
## strategy). Spread risk: the books' PV01 to the bond-swap spread.
oci_reserve <- function(r) {
  bk <- r$books
  if (length(bk)) Reduce(`+`, lapply(bk, `[[`, "oci_unreal")) else rep(0, nrow(r$quarterly))
}
met <- function(r) {
  q <- r$quarterly; bk <- r$books
  pv01 <- utils::tail(q$hedge_pv01, 1) + sum(vapply(bk, function(b) utils::tail(b$pv01, 1), 0))
  spv01 <- sum(vapply(bk, function(b) utils::tail(b$spv01, 1), 0))
  cet1 <- utils::tail(ea$cet1_share * q$equity, 1)
  data.frame(roe = mean(q$roe), nii = sum(q$nii), ni = sum(q$net_income), cet1_min = min(q$cet1_ratio),
             cet1_min_fv = min(q$cet1_ratio + oci_reserve(r) / q$rwa), cet1_end = utils::tail(q$cet1_ratio, 1),
             pv01_end = pv01, eve200 = 200 * pv01 / cet1, spv01_end = spv01, dividends = sum(q$dividends),
             gains = sum(q$overlay_gains), hedge_mean = mean(q$hedge_notional) / nmd0,
             lcr_min = min(q$lcr), nsfr_min = min(q$nsfr), plug_zero = r$plug_zero)
}
