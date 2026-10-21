# =============================================================================
#  analysis/14_strategy_lab.R -- positioning NGFB's balance sheet across the
#                                macro outcomes history has produced
#
#  Three scenarios show three outcomes. The lab replays every complete
#  12-quarter macro episode in the history (overlapping windows) from today's
#  starting point. Rates, the long bond, inflation and unemployment move from
#  their jump-off levels by the change they made over the episode (the policy
#  rate floored at 3.5%, its 2020-21 low, and inflation at zero). GDP, house
#  prices, the rand and equities are replayed as growth: the episode's
#  quarter-on-quarter growth of the underlying level, compounded from today's
#  level, so that an episode starting in a quarter distorted by base effects
#  (2020-21) does not shift its whole path. Every episode runs through the full
#  chain -- the sector models, NGFB's shares, the engine -- so a strategy is
#  judged on the distribution of ROE, NII and capital over some sixty macro
#  paths as well as on the scenarios.
#
#  Strategies, all against NGFB's plan (a 49.6% hedge of non-maturity
#  deposits reinvested at five years, Treasury bills for HQLA, a 50% payout):
#   A. Static positioning chosen today: hedge ratio x reinvestment tenor;
#      a share of HQLA in 10-year government bonds; a quarter of HQLA in
#      asset-swapped 5-year government bonds; a liquidity buffer cut to an
#      LCR of 170% (Treasury bills released to repay wholesale funding); the
#      payout ratio.
#   B. Rules keyed on the macro picture as it unfolds. Each quarter's
#      decision uses only what is known at the end of the previous quarter,
#      and every parameter is set in advance, as a round number, not fitted
#      to the episodes (fitting them on the same sixty paths would overstate
#      what they add):
#      R1 hedge ratio with the policy rate against its neutral level
#         (plan ratio + 10pp per point above 6.25%, within 20-80%): receive
#         more fixed when rates are high, less when they are low;
#      R2 reinvestment tenor with the policy rate: ten years when the rate is
#         at least half a point above neutral, three years when it is half a
#         point below, five otherwise;
#      R3 HQLA duration with the government curve's slope: a quarter of
#         earning HQLA in 10-year bonds at the median slope, an eighth of
#         HQLA more or less per point of slope, within 0-50%;
#      R4 payout with growth and capital: 70% while GDP grows and CET1 is
#         above the 12.5% target, 30% otherwise;
#      R5 unsecured lending (cards, personal loans) with the labour market:
#         growth 5pp a year faster while unemployment falls and GDP grows
#         above 1%, 5pp slower when unemployment has risen by more than a
#         point over the year or GDP contracts;
#      R6 asset-swapped bonds with the 5-year bond-swap spread: none below
#         0.5pp, a quarter of earning HQLA at 1pp, half at 1.5pp or more;
#      R7 the liquidity buffer with stress: an LCR of 170% in calm quarters,
#         185% after a quarter in which GDP contracted or the rand lost more
#         than 15% over the year;
#      and all seven together (the bond books and the release kept within 90%
#      of earning HQLA, the rest in bills).
#   C. The oracle: the best static hedge ratio for each path chosen with
#      hindsight -- an upper bound on what timing the hedge could add.
#  Robustness: the scenarios' swap curve is the floating benchmark plus
#  constant term spreads, so it never prices the coming policy path, and a
#  rule that receives fixed ahead of cuts is flattered by that. The hedge
#  strategies are re-run with swap rates that price half of the coming path
#  (the average benchmark over each swap's life, realised along the path).
#  All macro paths are known (perfect foresight); nothing is forecast.
# =============================================================================
source("analysis/00_config.R")
P  <- readRDS(out_path("panels.rds"))
S1 <- readRDS(out_path("stage1.rds"))
S2 <- readRDS(out_path("stage2.rds"))
N  <- readRDS(out_path("ngfb.rds"))
SC <- readRDS(out_path("scenarios.rds"))
A  <- readRDS(out_path("audit.rds"))
hist <- P$macro[P$macro$date <= CFG$date0, ]
base <- SC$base
ea <- SC$eng_args
h0 <- SC$hedge0
nmd0 <- N$nmd0
n <- CFG$horizon
core <- scenario_core_vars()
qlab <- function(d) paste0(format(d, "%Y"), "Q", (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L)
t_start <- Sys.time()

## ---- 1. macro episodes ------------------------------------------------------------
## jump-off levels: the last published value, or the scenario's own junction
## value where the jump-off quarter is not yet published (perfect foresight)
hj <- pf_history(hist, attr(base, "junction"))
x0 <-vapply(core, function(v) hj[[v]][hj$date == CFG$date0], 0)
guard(all(is.finite(x0)), "a core macro variable has no jump-off level")
mh <- hist[, c("date", core)]
full <- stats::complete.cases(mh[, core])
st <- which(vapply(seq_len(nrow(mh)), function(i) i + n <= nrow(mh) && all(full[i:(i + n)]), TRUE))
floors <- c(repo = 3.5, cpi_yy = 0)
## growth variables and the level each is the annual growth of (log growth,
## as in the panel)
hist$zar_level <- ifelse(is.finite(hist$usdzar), hist$usdzar, hist$fred_usdzar)   # rand per dollar, as in the panel
lev <- c(gdp_yy = "gdp_real", hpi_yy = "fred_house_px_nominal", zar_yy = "zar_level", alsi_yy = "fred_share_px")
guard(all(lev %in% names(hist)), "a level series behind a growth variable is missing")
for (v in names(lev)) {
  chk <- growth_yy(hist[[lev[[v]]]]) - hist[[v]]
  guard(max(abs(chk[hist$date >= CFG$sample_from]), na.rm = TRUE) < 1e-8, "%s is not the annual growth of %s", v, lev[[v]])
}
## the last four quarters' levels at the jump-off, unpublished ones filled
## from the scenario's junction growth rates (perfect foresight)
q4 <- utils::tail(hj$date[hj$date <= CFG$date0], 8L)
lev0 <- lapply(stats::setNames(names(lev), names(lev)), function(v) {
  L <- hist[[lev[[v]]]][match(q4, hist$date)]
  for (j in 5:8) if (!is.finite(L[j])) L[j] <- L[j - 4L] * exp(hj[[v]][match(q4[j], hj$date)] / 100)
  utils::tail(L, 4L)
})
guard(all(vapply(lev0, function(x) all(is.finite(x)), TRUE)), "a growth variable has no jump-off level")
full <- full & Reduce(`&`, lapply(lev, function(l) is.finite(hist[[l]][match(mh$date, hist$date)])))
st <- which(vapply(seq_len(nrow(mh)), function(i) i + n <= nrow(mh) && all(full[i:(i + n)]), TRUE))
episodes <- lapply(st, function(i) {
  e <- data.frame(date = base$date)
  for (v in setdiff(core, names(lev))) e[[v]] <- x0[[v]] + mh[[v]][i + seq_len(n)] - mh[[v]][i]
  for (v in names(lev)) {
    L <- hist[[lev[[v]]]][match(mh$date[i + 0:n], hist$date)]
    idx <- lev0[[v]][4] * exp(cumsum(diff(log(L))))          # the episode's growth from today's level
    e[[v]] <- utils::tail(growth_yy(c(lev0[[v]], idx)), n)
  }
  for (v in names(floors)) e[[v]] <- pmax(e[[v]], floors[[v]])
  e
})
ep_lab <- vapply(st, function(i) paste0(qlab(mh$date[i + 1L]), "-", qlab(mh$date[i + n])), "")
names(episodes) <- ep_lab
guard(length(episodes) >= 50, "too few complete macro episodes (%d)", length(episodes))
ep_info <- do.call(rbind, lapply(seq_along(st), function(j) {
  i <- st[j]; e <- episodes[[j]]; ch <- function(v) mh[[v]][i + n] - mh[[v]][i]
  data.frame(episode = ep_lab[j], start = mh$date[i + 1L], d_repo = ch("repo"), d_gov10y = ch("gov10y"),
             d_unemp = ch("unemp"), gdp_mean = mean(e$gdp_yy), gdp_min = min(e$gdp_yy), gdp_max = max(e$gdp_yy),
             hpi_mean = mean(e$hpi_yy), zar_mean = mean(e$zar_yy), alsi_mean = mean(e$alsi_yy),
             cpi_mean = mean(e$cpi_yy), repo_mean = mean(e$repo), repo_min = min(e$repo), repo_max = max(e$repo),
             repo_floored = any(x0[["repo"]] + mh$repo[i + seq_len(n)] - mh$repo[i] < floors[["repo"]]),
             cpi_floored = any(x0[["cpi_yy"]] + mh$cpi_yy[i + seq_len(n)] - mh$cpi_yy[i] < floors[["cpi_yy"]]),
             stringsAsFactors = FALSE)
}))

## ---- 2. each episode through the sector models, NGFB's shares and the engine ---------
project <- function(scen) project_satellites(S1$sats, P$s1pan, scen, ic_window = CFG$ic_window,
                                             ic_families = CFG$ic_families, ic_mode = CFG$ic_mode)
inputs_of <- function(scen) engine_inputs(project(scen), scen, scenario_regimes(scen, hist),
                                          share_params = N$share_params, mean_g = S2$mean_g, drift = FALSE,
                                          pricing = N$pricing)
to_scen <- function(e, name)
  scenario_apply(base, lapply(stats::setNames(core, core), function(v) e[[v]] - base[[v]]), hist, name = name)
scen_ep <- lapply(seq_along(episodes), function(j) to_scen(episodes[[j]], ep_lab[j]))
names(scen_ep) <- ep_lab
inp_ep <- lapply(scen_ep, inputs_of)
named <- c("baseline", "upside", "downside")
scen_all <- c(lapply(SC$results[named], `[[`, "scenario"), list(downside_cuts = SC$variants$downside_cuts$scenario), scen_ep)
inp_all <- c(lapply(SC$results[named], function(r) r$ngfb$inputs), list(downside_cuts = SC$variants$downside_cuts$ngfb$inputs),
             inp_ep)
kind <- c(rep("scenario", 4L), rep("episode", length(inp_ep)))
t_proj <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

## ---- 3. the engine under each strategy ----------------------------------------------
## the strategies, the rules' signals and the metrics, shared with the
## risk-budgeted optimiser (15_risk_budget.R)
source("analysis/lab_tools.R", local = TRUE)
## the lab reproduces the plan in every named scenario
for (k in names(inp_all)[kind == "scenario"]) {
  ref <- if (k == "downside_cuts") SC$variants$downside_cuts$ngfb$run else SC$results[[k]]$ngfb$run
  guard(max(abs(run(inp_all[[k]])$quarterly$roe - ref$quarterly$roe)) < 1e-12, "the lab does not reproduce the plan in %s", k)
}
## the episodes are built the way the scenarios are: replaying the baseline's
## own path reproduces the baseline
e_b <- data.frame(date = base$date); for (v in core) e_b[[v]] <- base[[v]]
guard(max(abs(run(inputs_of(to_scen(e_b, "check")))$quarterly$roe - SC$results$baseline$ngfb$run$quarterly$roe)) < 1e-12,
      "an episode equal to the baseline does not reproduce the baseline")

lab_path <- function(k) {
  inp <- inp_all[[k]]; sc <- scen_all[[k]]; rp <- rule_paths(sc, inp)
  r0 <- run(inp)
  rows <- list()
  put <- function(family, strategy, setting, r, phi = 0)
    rows[[length(rows) + 1L]] <<- data.frame(path = k, kind = kind[match(k, names(inp_all))], family = family,
                                             strategy = strategy, setting = setting, phi = phi, met(r),
                                             stringsAsFactors = FALSE)
  put("Plan", "Plan", "49.6% hedge, 5-year reinvestment, bills, 50% payout", r0)
  ## A. static positioning
  for (rf in names(tenors)) for (ra in ratios)
    put("Static hedge", sprintf("%.0f%% at %d years", 100 * ra, tenors[[rf]] / 4L), rf, run(inp, hedge = hedge_static(ra, rf)))
  for (th in c(0.25, 0.50))
    put("Static HQLA", sprintf("%.0f%% of HQLA in 10-year bonds", 100 * th), as.character(th),
        run(inp, books = list(dur_book(rep(th, n), inp))))
  put("Static HQLA", "25% of HQLA in asset-swapped 5-year bonds", "0.25", run(inp, books = list(as_book(rep(0.25, n), inp))))
  put("Static buffer", "Buffer at an LCR of 170%", "1.70", run(inp, release = rep(rel_170, n)))
  for (po in c(0.3, 0.7)) put("Static payout", sprintf("%.0f%% payout", 100 * po), as.character(po), run(inp, payout = po))
  ## B. rules on the macro picture as it unfolds
  r1 <- run(inp, hedge = hedge_rule(rp)); put("Rule", "R1 hedge ratio with the policy rate", "", r1)
  put("Rule", "R2 reinvestment tenor with the policy rate", "", run(inp, hedge = hedge_rule(rp, FALSE, TRUE)))
  put("Rule", "R3 HQLA duration with the curve's slope", "", run(inp, books = list(dur_book(rp$theta, inp))))
  put("Rule", "R4 payout with growth and capital", "", run(inp, payout = payout_rule(rp$good)))
  put("Rule", "R5 unsecured growth with the labour market", "", run(inp, growth = tilt_growth(inp, rp$tilt)))
  put("Rule", "R6 asset swap with the bond-swap spread", "", run(inp, books = list(as_book(rp$alpha, inp))))
  put("Rule", "R7 liquidity buffer with stress", "", run(inp, release = rp$release))
  put("Rule", "All seven rules", "", run(inp, hedge = hedge_rule(rp, TRUE, TRUE), payout = payout_rule(rp$good),
                                         books = list(dur_book(rp$theta_all, inp), as_book(rp$alpha_all, inp)),
                                         growth = tilt_growth(inp, rp$tilt), release = rp$release))
  ## swap rates that price half of the coming path: the hedge strategies again
  ia <- inp; ia$rates <- anticipate(inp$rates, 0.5)
  put("Plan", "Plan", "", run(ia), phi = 0.5)
  for (ra in ratios) put("Static hedge", sprintf("%.0f%% at %d years", 100 * ra, 5L), "swap5y",
                         run(ia, hedge = hedge_static(ra, "swap5y")), phi = 0.5)
  put("Rule", "R1 hedge ratio with the policy rate", "", run(ia, hedge = hedge_rule(rp)), phi = 0.5)
  put("Rule", "R2 reinvestment tenor with the policy rate", "", run(ia, hedge = hedge_rule(rp, FALSE, TRUE)), phi = 0.5)
  out <- do.call(rbind, rows)
  attr(out, "signals") <- rp
  attr(out, "plan_roe_q") <- r0$quarterly$roe
  out
}
lab_list <- lapply(names(inp_all), lab_path)
names(lab_list) <- names(inp_all)
lab <- do.call(rbind, lab_list)
rownames(lab) <- NULL
t_lab <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))

## ---- 4. distributions over the episodes -----------------------------------------------
## the oracle: the best static hedge ratio (five-year reinvestment) for each
## path, chosen with hindsight
for (ph in c(0, 0.5)) {
  g <- lab[lab$family == "Static hedge" & lab$setting == "swap5y" & lab$phi == ph, ]
  orc <- do.call(rbind, lapply(split(g, g$path), function(d) {
    o <- d[which.max(d$roe), ]; o$family <- "Oracle"; o$strategy <- "Best static hedge ratio with hindsight"
    o$setting <- sub(" at.*", "", d$strategy[which.max(d$roe)]); o }))
  lab <- rbind(lab, orc)
}
plan <- lab[lab$strategy == "Plan", c("path", "phi", "roe", "nii", "ni", "cet1_min", "pv01_end", "eve200")]
names(plan)[-(1:2)] <- paste0(names(plan)[-(1:2)], "_plan")
lab <- merge(lab, plan, by = c("path", "phi"), all.x = TRUE, sort = FALSE)
lab$d_roe <- lab$roe - lab$roe_plan
lab$d_nii <- lab$nii - lab$nii_plan
lab$d_ni <- lab$ni - lab$ni_plan
lab$d_pv01 <- lab$pv01_end - lab$pv01_end_plan
lab$d_eve200 <- lab$eve200 - lab$eve200_plan
## the direction of the episode's policy rate: its average over the horizon
## against the jump-off
dr <- c(stats::setNames(ep_info$repo_mean - repo_jo, ep_info$episode),
        vapply(scen_all[kind == "scenario"], function(s) mean(s$repo) - repo_jo, 0))
lab$d_repo_mean <- unname(dr[lab$path])
q5 <- function(x) unname(stats::quantile(x, 0.05, type = 7))
dist <- do.call(rbind, lapply(split(lab[lab$kind == "episode", ], lab$strategy[lab$kind == "episode"]), function(d)
  do.call(rbind, lapply(split(d, d$phi), function(e) data.frame(
    family = e$family[1], strategy = e$strategy[1], phi = e$phi[1], n = nrow(e),
    roe_mean = mean(e$roe), roe_sd = stats::sd(e$roe), roe_p5 = q5(e$roe), roe_min = min(e$roe),
    nii_mean = mean(e$nii), ni_mean = mean(e$ni),
    d_roe_mean = mean(e$d_roe), d_roe_p5 = q5(e$d_roe), d_roe_min = min(e$d_roe), win = mean(e$d_roe > 0),
    d_nii_mean = mean(e$d_nii), d_ni_mean = mean(e$d_ni),
    d_roe_up = mean(e$d_roe[e$d_repo_mean > 0.25]), d_roe_down = mean(e$d_roe[e$d_repo_mean < -0.25]),
    d_roe_flat = mean(e$d_roe[abs(e$d_repo_mean) <= 0.25]),
    d_pv01 = mean(e$d_pv01), d_eve200 = mean(e$d_eve200),
    cet1_min = min(e$cet1_min), cet1_min_fv = min(e$cet1_min_fv), cet1_p5 = q5(e$cet1_min_fv),
    breach = mean(e$cet1_min_fv < CFG$cet1_min), coe_hit = mean(e$roe >= CFG$coe),
    pv01_end = mean(e$pv01_end), hedge_mean = mean(e$hedge_mean), stringsAsFactors = FALSE)))))
rownames(dist) <- NULL
dist <- dist[order(dist$phi, dist$family, -dist$roe_mean), ]
guard(all(abs(dist$d_roe_mean[dist$strategy == "Plan"]) < 1e-12), "the plan should not differ from itself")
for (ph in c(0, 0.5))
  guard(dist$roe_mean[dist$family == "Oracle" & dist$phi == ph] >=
          max(dist$roe_mean[dist$family == "Static hedge" & grepl("at 5 years", dist$strategy) & dist$phi == ph]) - 1e-12,
        "the oracle should beat every static five-year hedge ratio on average")

## the static hedge frontier: mean ROE against its spread across the episodes
frontier <- dist[dist$family == "Static hedge" & dist$phi == 0, ]
frontier$ratio <- as.numeric(sub("%.*", "", frontier$strategy)) / 100
frontier$tenor <- as.numeric(sub(".* at (\\d+) years", "\\1", frontier$strategy))
## efficient: no other static choice has a higher mean and a lower spread
frontier$efficient <- vapply(seq_len(nrow(frontier)), function(i) !any(
  frontier$roe_mean > frontier$roe_mean[i] + 1e-12 & frontier$roe_sd < frontier$roe_sd[i] - 1e-12), TRUE)

## what drives ROE across the episodes: the plan's ROE on the episode's mean
## policy-rate change, mean growth and change in unemployment (HAC-free OLS on
## overlapping windows: descriptive, the t-ratios overstate precision)
pe <- merge(lab[lab$strategy == "Plan" & lab$kind == "episode" & lab$phi == 0, c("path", "roe", "nii")],
            ep_info, by.x = "path", by.y = "episode")
pe$d_repo_mean <- pe$repo_mean - repo_jo
drivers <- stats::lm(roe ~ d_repo_mean + gdp_mean + d_unemp, pe)
drv <- data.frame(term = names(coef(drivers)), coef = unname(coef(drivers)), stringsAsFactors = FALSE)
drv_r2 <- summary(drivers)$r.squared

## the transition: the spread at risk on floating assets (audit), across the episodes
fl_a <- A$transition$lines_assets
cas_ep <- do.call(rbind, lapply(names(inp_ep), function(k) {
  inp <- inp_ep[[k]]; lr <- inp$line_rates; pst <- lr$date > jibar_transition()$last_fixing
  for (ln in fl_a) lr[[ln]][pst] <- lr[[ln]][pst] - jibar_transition()$cas
  data.frame(path = k, d_roe = mean(run(inp, line_rates = lr)$quarterly$roe) - mean(run(inp)$quarterly$roe))
}))

saveRDS(list(episodes = episodes, info = ep_info, x0 = x0, floors = floors, lab = lab, dist = dist,
             frontier = frontier, drivers = drv, drivers_r2 = drv_r2, plan_by_path = pe,
             rule_paths = lapply(lab_list, function(d) c(attr(d, "signals"), list(plan_roe_q = attr(d, "plan_roe_q")))),
             inputs = inp_all, kind = kind, jo_mk = jo_mk, rel_170 = rel_170, rel_185 = rel_185, sc_hq = sc_hq,
             scen_paths = lapply(scen_all, function(s) s[, c("date", "repo", "float3m", "gov10y", "gdp_yy", "unemp", "cpi_yy",
                                                             "d4_unemp", "zar_yy")]),
             params = list(ratios = ratios, plan_ratio = plan_ratio, r_star = r_star, slope_med = slope_med,
                           slope_jo = slope_jo, repo_jo = repo_jo, jo_swap = jo_swap, H0 = H0, unsecured = unsecured,
                           lcr_jo = lcr_jo, hq_cap = hq_cap, zar_jo = zar_jo, gdp_jo = gdp_jo),
             cas_episodes = cas_ep, minutes = c(projection = t_proj, total = t_lab)),
        out_path("lab.rds"))
pd <- dist[dist$phi == 0, ]
best <- pd[pd$family %in% c("Static hedge", "Static HQLA", "Static buffer", "Static payout", "Rule"), ]
best <- best[which.max(best$d_roe_mean), ]
ngfb_log("strategy lab: %d episodes x %d strategy runs in %.1f min; plan ROE %.1f%% on average (5th percentile %.1f%%); best: %s %+.0f bps",
         length(inp_ep), nrow(lab) / length(inp_all), t_lab, 100 * pd$roe_mean[pd$strategy == "Plan"],
         100 * pd$roe_p5[pd$strategy == "Plan"], best$strategy, 1e4 * best$d_roe_mean)
