# =============================================================================
#  analysis/05_scenarios.R -- scenarios -> sector and NGFB projections,
#                             and NGFB's macro sensitivities
#
#  Three scenarios (baseline, upside, downside), each run for NGFB with its
#  structural hedge, for NGFB without it, and for the BA900 sector through
#  the same engine. Model-risk variants of the scenarios: the downside with
#  the opposite policy response (the SARB cuts), credit losses that are
#  convex in a contraction, and no intercept correction on credit losses.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
S2 <- readRDS(out_path("stage2.rds"))
N  <- readRDS(out_path("ngfb.rds"))
cal <- N$calibration

hist <- P$macro[P$macro$date <= CFG$date0, ]
base <- scenario_baseline(hist, n = CFG$horizon, anchors = CFG$anchors,
                          repo_path = CFG$repo_path, paths = list(cpi_yy = CFG$cpi_path))
lib  <- scenario_library(base, hist)
guard(identical(names(lib), c("baseline", "upside", "downside")), "unexpected scenario library")
guard(identical(names(CFG$scenario_weights), names(lib)) && abs(sum(CFG$scenario_weights) - 1) < 1e-12,
      "scenario weights do not match the library")

## structural hedge on the jump-off book: canonical ratio of the NMD base,
## 5-year boxcar; the existing book's fixed rate is the average 5y swap proxy
## (JIBAR + 0.55 x curve slope) over the last 20 quarters
h20 <- utils::tail(hist[is.finite(hist$slope), ], 20L)
hedge0 <- list(notional = ngfb_canonical()$hedge_ratio * N$nmd0, tenor_q = 20L,
               fixed0 = mean(h20$jibar3m + 0.55 * h20$slope))
eng_args <- list(plug = "wholesale_other", tax = cal$tax, payout = CFG$payout,
                 cet1_share = cal$cet1_share, rwa_other = cal$rwa_other)

## sector jump-off book (BA900 total at the jump-off), for the industry view
sec_bal <- sector_balance_sheet(P$gp, CFG$date0)
sec_ebs <- engine_balance_sheet(sec_bal, N$chars_sector, N$pricing)

project <- function(scen, sats = S1$sats, ic_families = CFG$ic_families)
  project_satellites(sats, P$s1pan, scen, ic_window = CFG$ic_window, ic_families = ic_families,
                     ic_mode = CFG$ic_mode)
run_one <- function(scen, ebs = N$engine_bs, share = N$share_params, hedge = hedge0,
                    overlays = list(), proj = NULL) {
  if (is.null(proj)) proj <- project(scen)
  reg <- scenario_regimes(scen, hist)
  inp <- engine_inputs(proj, scen, reg, share_params = share, mean_g = S2$mean_g,
                       drift = FALSE, pricing = N$pricing, overlays = overlays)
  run <- do.call(run_engine, c(list(bs = ebs, growth = inp$growth, rates = inp$rates,
                                    credit = inp$credit, ratios = inp$ratios, hedge = hedge,
                                    line_rates = inp$line_rates), eng_args))
  list(proj = proj, inputs = inp, run = run, summary = summarise_run(run), regimes = reg)
}
run_all3 <- function(s, proj = NULL) {
  ng <- run_one(s, proj = proj)
  sc <- run_one(s, ebs = sec_ebs, share = NULL, hedge = NULL, proj = ng$proj)
  ## NGFB without its structural hedge: separates what the hedge does from
  ## what NGFB's balance-sheet mix does when it is compared with the sector
  nh <- run_one(s, hedge = NULL, proj = ng$proj)
  list(scenario = s, ngfb = ng, sector = sc, ngfb_unhedged = nh)
}

res <- lapply(lib, run_all3)
names(res) <- names(lib)
## every run balances: assets = liabilities + equity in every quarter
for (k in names(res)) for (e in c("ngfb", "sector", "ngfb_unhedged")) {
  r <- res[[k]][[e]]$run; side <- if (e == "sector") sec_ebs$side else N$engine_bs$side
  gap <- colSums(r$balances[side == "asset", , drop = FALSE]) - colSums(r$balances[side != "asset", , drop = FALSE])
  guard(max(abs(gap) / colSums(r$balances[side == "asset", , drop = FALSE])) < 1e-9,
        "%s / %s run does not balance", k, e)
}

## ---- model-risk variants ------------------------------------------------------
## (1) the downside with the opposite policy response: the same real shock,
##     but the SARB cuts (a demand-led recession, as in 2009 and 2020)
cuts <- scenario_downside_cuts(base, hist)
variants <- list(downside_cuts = run_all3(cuts))
## (2) credit losses convex in a contraction: the credit-loss satellite
##     re-searched with GDP growth below zero as a candidate driver
clr_convex <- do.call(fit_stage1, c(list(P$s1pan, stage1_pnl_targets(convex_credit = TRUE)["clr"],
                                         controls = c("covid_q2", "covid_q3")), S1$args))
sats_convex <- S1$sats; sats_convex["clr"] <- clr_convex
convex <- lapply(lib, function(s) run_one(s, proj = project(s, sats_convex))$summary)
## (3) no intercept correction on the credit-loss ratio
no_ic_credit <- lapply(lib, function(s)
  run_one(s, proj = project(s, ic_families = setdiff(CFG$ic_families, "credit")))$summary)
model_risk <- do.call(rbind, lapply(names(lib), function(k) data.frame(
  scenario = k, core = res[[k]]$ngfb$summary$roe, core_ni = res[[k]]$ngfb$summary$net_income,
  convex = convex[[k]]$roe, convex_ni = convex[[k]]$net_income, convex_clr = convex[[k]]$clr,
  core_clr = res[[k]]$ngfb$summary$clr,
  no_ic_credit = no_ic_credit[[k]]$roe, no_ic_credit_ni = no_ic_credit[[k]]$net_income,
  no_ic_credit_clr = no_ic_credit[[k]]$clr)))

## (4) what changed from the previous version of this analysis: the baseline
##     with the original deposit-cost models (repo level OR its change) and the
##     intercept correction in the equation, then with the refined deposit
##     models but the old correction, then the core
chg <- union(c("c_current", "c_savings", "c_call", "c_fixed_notice", "c_ncd"),
             eval(formals(stage1_pnl_targets)$asymmetric))
prev_fit <- do.call(fit_stage1, c(list(P$s1pan, stage1_pnl_targets(character(0), "restricted")[chg],
                                       controls = c("covid_q2", "covid_q3")), S1$args))
sats_prev <- S1$sats; sats_prev[chg] <- prev_fit
proj_eq <- function(sats) project_satellites(sats, P$s1pan, lib$baseline, ic_window = CFG$ic_window,
                                             ic_families = CFG$ic_families, ic_mode = "equation")
what_changed <- list(previous = run_one(lib$baseline, proj = proj_eq(sats_prev)),
                     refined_equation_ic = run_one(lib$baseline, proj = proj_eq(S1$sats)))
what_changed <- lapply(what_changed, function(z) list(summary = z$summary, proj = z$proj))

## ---- macro sensitivities around the baseline --------------------------------
n <- CFG$horizon
shocks <- list(
  repo_up_100   = list(repo = rep(1, n)),
  repo_down_100 = list(repo = rep(-1, n)),
  gdp_down_1    = list(gdp_yy = rep(-1, n)),
  unemp_up_2    = list(unemp = rep(2, n)),
  hpi_down_10   = list(hpi_yy = c(rep(-10, 4), rep(0, n - 4))),
  zar_weaker_10 = list(zar_yy = c(rep(10, 4), rep(0, n - 4))),
  gov10y_up_100 = list(gov10y = rep(1, n)),
  cpi_up_1      = list(cpi_yy = rep(1, n)))
b0 <- res$baseline$ngfb
runner <- function(ebs, hedge) function(inp)
  do.call(run_engine, c(list(bs = ebs, growth = inp$growth, rates = inp$rates, credit = inp$credit,
                             ratios = inp$ratios, hedge = hedge, line_rates = inp$line_rates), eng_args))
ngfb_runner <- runner(N$engine_bs, hedge0)
channels <- function(sh) {
  s <- scenario_apply(base, sh, hist, name = "shock")
  full <- run_one(s)
  a <- channel_attribution(b0$inputs, full$inputs, ngfb_runner)
  r <- a[a$metric == "roe", ]
  data.frame(total = r$total, volume = r$volume, pricing = r$pricing, credit = r$credit,
             fees_costs = r$fees_costs, interaction = r$interaction,
             roe_y1 = full$summary$roe_y1 - b0$summary$roe_y1,
             nim = full$summary$nim - b0$summary$nim, clr = full$summary$clr - b0$summary$clr,
             cet1_end = full$summary$cet1_end - b0$summary$cet1_end,
             nim_eq = a$total[a$metric == "nim_eq"], net_income = a$total[a$metric == "net_income"])
}
sens <- do.call(rbind, lapply(names(shocks), function(k) cbind(shock = k, channels(shocks[[k]]))))

## the same sensitivities without the structural hedge (what the hedge buys)
nh_runner <- runner(N$engine_bs, NULL)
sens_nohedge <- do.call(rbind, lapply(names(shocks), function(k) {
  s <- scenario_apply(base, shocks[[k]], hist, name = "shock")
  full <- run_one(s, hedge = NULL)
  a <- channel_attribution(res$baseline$ngfb_unhedged$inputs, full$inputs, nh_runner)
  data.frame(shock = k, total = a$total[a$metric == "roe"], pricing = a$pricing[a$metric == "roe"])
}))

## each scenario (and the downside-with-cuts variant) against the baseline,
## by channel, for NGFB and the sector
sec_runner <- runner(sec_ebs, NULL)
alt <- c(res[setdiff(names(res), "baseline")], variants)
scen_attr <- do.call(rbind, lapply(names(alt), function(k) rbind(
  cbind(scenario = k, entity = "NGFB",
        channel_attribution(b0$inputs, alt[[k]]$ngfb$inputs, ngfb_runner)),
  cbind(scenario = k, entity = "Sector",
        channel_attribution(res$baseline$sector$inputs, alt[[k]]$sector$inputs, sec_runner)))))
chk <- scen_attr[scen_attr$metric == "roe" & scen_attr$entity == "NGFB", ]
guard(max(abs(chk$alt - vapply(chk$scenario, function(k) alt[[k]]$ngfb$summary$roe, 0))) < 1e-10,
      "channel attribution does not reproduce the scenario runs")
chk <- scen_attr[scen_attr$metric == "net_income" & scen_attr$entity == "NGFB", ]
guard(max(abs(chk$alt - vapply(chk$scenario, function(k) alt[[k]]$ngfb$summary$net_income, 0))) < 1e-3,
      "net-income attribution does not reproduce the scenario runs")

## what the intercept correction does: the baseline without it on the fee and
## cost ratios (credit and pricing keep theirs), and without it anywhere
p_noic_pnl <- project(lib$baseline, ic_families = setdiff(CFG$ic_families, "pnl"))
p_noic <- project_satellites(S1$sats, P$s1pan, lib$baseline, ic_window = 0L)
no_ic <- list(ngfb = run_one(lib$baseline, proj = p_noic_pnl)$summary,
              sector = run_one(lib$baseline, ebs = sec_ebs, share = NULL, hedge = NULL, proj = p_noic_pnl)$summary,
              sector_all = run_one(lib$baseline, ebs = sec_ebs, share = NULL, hedge = NULL, proj = p_noic)$summary,
              nir_assets = p_noic_pnl$nir_assets, nir_assets_ic = res$baseline$ngfb$proj$nir_assets,
              opex_assets = p_noic_pnl$opex_assets, opex_assets_ic = res$baseline$ngfb$proj$opex_assets)

saveRDS(list(base = base, library = lib, results = res, variants = variants, model_risk = model_risk,
             what_changed = what_changed,
             clr_convex = clr_convex[["clr"]], hedge0 = hedge0, sensitivities = sens,
             sensitivities_nohedge = sens_nohedge, attribution = scen_attr, shocks = shocks,
             sector_bs = sec_bal, eng_args = eng_args, no_ic = no_ic), out_path("scenarios.rds"))
tab <- do.call(rbind, lapply(names(res), function(k) cbind(scenario = k, res[[k]]$ngfb$summary)))
ngfb_log("scenarios: NGFB ROE baseline %.1f%%, upside %.1f%%, downside %.1f%% (net income R%.1f / %.1f / %.1f bn over three years)",
         100 * tab$roe[1], 100 * tab$roe[2], 100 * tab$roe[3],
         tab$net_income[1] / 1e6, tab$net_income[2] / 1e6, tab$net_income[3] / 1e6)
