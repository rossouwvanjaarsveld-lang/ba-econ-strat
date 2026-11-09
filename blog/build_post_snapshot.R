# =============================================================================
#  blog/build_post_snapshot.R -- the data behind the blog post
#
#  Writes blog/ngfb-macro-strategy/post_artefacts.rds: the numbers, tables and
#  paths the post quotes, taken from output/*.rds after analysis/run_all.R.
#  Run from the package root.
#
#  What goes in is deliberately narrow. The post is published with its page
#  bundle, and Hugo serves every file in the bundle, so the snapshot carries
#  results (scenario outcomes, levers, the lab, the optimiser) and the public
#  market data behind the figures, and nothing from the industry models
#  themselves: no coefficients, lags, weights, multipliers or skill scores.
# =============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a
source("analysis/00_config.R")
o <- function(f) readRDS(out_path(f))
P  <- o("panels.rds");  N <- o("ngfb.rds");   SC <- o("scenarios.rds")
O  <- o("optimise.rds"); ST <- o("strategy.rds"); PF <- o("performance.rds")
S2 <- o("stage2.rds");   A <- o("audit.rds");  LV <- o("levers.rds")
LB <- o("lab.rds");      RB <- o("risk_budget.rds"); MM <- o("monthly.rds")
date0 <- CFG$date0
res <- SC$results; allr <- c(res, SC$variants)
sc4 <- c("baseline", "upside", "downside", "downside_cuts")

## ---- NGFB ------------------------------------------------------------------------------
bs <- N$engine_bs; ia <- bs$side == "asset"
ta <- sum(bs$balance[ia]); E0 <- sum(bs$balance[bs$side == "equity"])
rwa0 <- sum(bs$balance[ia] * bs$rw[ia]) + SC$eng_args$rwa_other * ta
sec_ta <- P$gp$total_assets[P$gp$entity == "Sector" & P$gp$date == date0]
q0 <- res$baseline$ngfb$run$quarterly
sh <- N$shares[N$shares$date == date0 & N$shares$entity == "NGFB", c("product", "share")]
ngfb <- list(
  ta = ta, sector_ta = sec_ta, equity = E0, rwa = rwa0, cet1_share = SC$eng_args$cet1_share,
  cet1_0 = SC$eng_args$cet1_share * E0 / rwa0, tax = SC$eng_args$tax,
  lcr_1 = q0$lcr[1], nsfr_1 = q0$nsfr[1], lev_0 = SC$eng_args$cet1_share * E0 / ta,
  nmd0 = N$nmd0, hedge_notional = SC$hedge0$notional, hedge_ratio = SC$hedge0$notional / N$nmd0,
  hedge_fixed0 = SC$hedge0$fixed0, hedge_tenor_q = SC$hedge0$tenor_q,
  mix = data.frame(line = names(N$asset_draw$line_draw), ngfb = unname(N$asset_draw$line_draw),
                   centre = unname(N$asset_draw$line_mean[names(N$asset_draw$line_draw)]), stringsAsFactors = FALSE),
  distance = N$asset_draw$distance, radius = N$asset_draw$radius,
  shares = sh, balance = N$balance[, c("line", "side", "balance")],
  liq_raw = N$calibration$liq$raw, lcr_sector = CFG$lcr_sector, nsfr_sector = CFG$nsfr_sector,
  wholesale_spread = N$calibration$wholesale_spread, hqla_scale = N$calibration$hqla_scale)

## ---- Stage 2: the share model (peer groups only, k >= 4) ------------------------------------
cg <- S2$coef_groups
rg_path <- merge(S2$regimes, P$macro[, c("date", "repo", "gdp_yy")], by = "date")
rg_path <- rg_path[rg_path$date >= as.Date("2008-01-01") & rg_path$date <= date0, ]
stage2 <- list(coef = cg[cg$entity %in% c("Big Four", "Challengers", "Specialists"), c("entity", "product", "param", "est", "t")],
               regimes = table(S2$regimes$regime[S2$regimes$date >= as.Date("2008-01-01") & S2$regimes$date <= date0]),
               regime_path = rg_path)

## ---- scenarios -----------------------------------------------------------------------------
vars <- c("date", "repo", "gdp_yy", "cpi_yy", "gov10y", "unemp", "hpi_yy", "zar_yy")
paths <- do.call(rbind, lapply(sc4, function(k) cbind(scenario = k, allr[[k]]$scenario[, vars])))
hist <- P$macro[P$macro$date >= as.Date("2015-01-01") & P$macro$date <= date0, vars]
sumtab <- do.call(rbind, lapply(sc4, function(k) data.frame(
  scenario = k, ngfb = allr[[k]]$ngfb$summary$roe, ngfb_nohedge = allr[[k]]$ngfb_unhedged$summary$roe,
  sector = allr[[k]]$sector$summary$roe, ni = allr[[k]]$ngfb$summary$net_income, nim = allr[[k]]$ngfb$summary$nim,
  clr = allr[[k]]$ngfb$summary$clr, sector_clr = allr[[k]]$sector$summary$clr,
  cet1_min = allr[[k]]$ngfb$summary$cet1_min, lcr_min = allr[[k]]$ngfb$summary$lcr_min, stringsAsFactors = FALSE)))
att <- SC$attribution[SC$attribution$metric == "roe", c("scenario", "entity", "volume", "pricing", "credit", "fees_costs", "interaction", "total")]
scen <- list(paths = paths, hist = hist, sumtab = sumtab, attribution = att,
             sens = SC$sensitivities[, c("shock", "total", "volume", "pricing", "credit", "fees_costs", "interaction", "net_income")],
             sens_nohedge = SC$sensitivities_nohedge[, c("shock", "total")], weights = CFG$scenario_weights,
             hedge_carry = vapply(split(res$baseline$ngfb$run$quarterly$hedge_pl, rep(1:3, each = 4)), sum, 0),
             float_mean = mean(res$baseline$scenario$float3m))

## ---- positioning ahead of a scenario ---------------------------------------------------------
## the optimiser without implied marginal costs: what maximising the engine's ROE directly would claim
raw_opt <- solve_positioning(O$gradients, O$moves, O$weights, lambda = 4, cet1_min = CFG$cet1_min,
                             lcr_min = CFG$lcr_min, reference = NULL)
raw_gain <- sum(O$weights * (raw_opt$roe_pred - O$gradients$R0[names(O$weights)])) - raw_opt$exec_cost
raw_up <- sum(abs(raw_opt$x[1:6] - O$moves$upper[1:6]) < 1e-6)
pos <- list(moves = O$moves[, c("move", "label", "lower", "upper", "cost")], positions = O$regret$positions,
            gain = O$regret$gain, gain_ni = O$regret$gain_ni, cuts = O$cuts$x, robust_hedge = O$robust$x[["hedge"]],
            size = O$aggressiveness$downside$size[, c("alpha", "realised", "gain", "gain_ni", "exp_gain")],
            capacity = O$aggressiveness$downside$capacity[, c("cost_scale", "bound_scale", "realised", "gain", "gain_ni", "hedge")],
            naive_gain = raw_gain, naive_up = raw_up, robust_x = O$robust$x, gradients = O$gradients, implied = O$implied_cost, lambda = O$lambda,
            m_ni = O$m_ni)

## ---- where to grow; macro-adjusted scorecards -------------------------------------------------
mg <- ST$margins[ST$margins$scenario == "baseline", ]
margins <- stats::aggregate(cbind(rate, ftp, liquidity, expected_loss, capital_charge, headroom) ~ product + funding, mg, mean)
share <- list(playbook = ST$positioning[, c("product", "regime", "market_growth", "market_dev", "ngfb_effect", "par", "frontier",
                                            "percentile", "action", "quadrant")],
              margins = margins, scorecards = PF$scorecards[, c("scenario", "product", "market_growth", "ngfb_growth",
                                                                "share_effect", "par", "frontier", "percentile", "verdict")],
              margins_all = stats::aggregate(cbind(rate, ftp, liquidity, expected_loss, capital_charge, headroom) ~
                                               scenario + product + funding, ST$margins, mean),
              gainers = ST$gainers)

## ---- are the numbers right? -----------------------------------------------------------------------
pm <- MM$panel_m[order(MM$panel_m$date), c("date", "alsi", "share_px")]
pm$d_sarb <- c(NA, diff(log(pm$alsi))); pm$d_oecd <- c(NA, diff(log(pm$share_px)))
brk <- pm[which(abs(pm$d_sarb - pm$d_oecd) > 0.3), c("date", "d_sarb", "d_oecd")]
qm <- P$macro[order(P$macro$date), ]
qm$eq_sarb <- growth_yy(qm$alsi); qm$eq_oecd <- growth_yy(qm$fred_share_px)
qm <- qm[qm$date >= CFG$sample_from & qm$date <= date0 & is.finite(qm$eq_sarb) & is.finite(qm$eq_oecd), c("date", "eq_sarb", "eq_oecd")]
audit <- list(proxy = A$proxy_vs_actual, proxy_err = A$proxy_err, fixed0 = A$fixed0, correction = A$correction[, c("scenario", "d_roe", "ni_v4", "ni_proxy")],
              nii_sens = A$nii_sens, disclosure = A$disclosure, neutral_ratio = A$neutral_ratio,
              spreads = A$spreads[, c("col", "tenor", "spread")], transition = A$transition[c("spreads", "switch_level", "n_post", "switch_effect", "exposure", "cas_risk")],
              fund_overlay = A$fund_overlay,
              equity_breaks = brk, equity = qm, jz = A$monthly[, intersect(c("date", "jibar3m_rb", "zaronia"), names(A$monthly))])

## ---- more levers ---------------------------------------------------------------------------------
bsm <- A$bs_model
levers <- list(summary = LV$summary, levers = LV$levers[, c("lever", "setting", "scenario", "d_roe", "cet1_min")],
               ratio_curve = LV$ratio_curve, pv01 = LV$pv01, hqla = LV$hqla, asset_swap = LV$asset_swap, as_jo = LV$as_jo,
               as_spv01 = LV$as_spv01, as_shock100 = LV$as_shock100, buffer = LV$buffer, lcr0 = LV$lcr0,
               release_share = LV$release_share, plan_liq = LV$plan_liq, buf_spread = LV$buf_spread, H0 = LV$H0,
               theta = LV$theta, swap5_jo = LV$swap5_jo, gov10_jo = LV$gov10_jo, cet1_0 = LV$cet1_0, excess = LV$excess,
               bs_model = bsm[c("beta", "beta_t", "kappa", "kappa_t", "kappa_r2", "kappa_d1", "n_kappa", "bs_mean", "bs_min", "bs_max", "from", "to", "fit")],
               wholesale_line = N$engine_bs[N$engine_bs$line == "wholesale_other", c("balance", "runoff", "asf")],
               combo = LV$combo[, c("scenario", "d_roe", "cet1_min", "cet1_min_fvoci")])

## ---- the strategy lab ------------------------------------------------------------------------------
ep <- names(LB$inputs)[LB$kind == "episode"]
ep_paths <- do.call(rbind, lapply(names(LB$scen_paths), function(k) cbind(path = k, kind = LB$kind[match(k, names(LB$inputs))],
  LB$scen_paths[[k]][, c("date", "repo", "gdp_yy", "gov10y", "unemp")])))
lab <- list(info = LB$info, paths = ep_paths, dist = LB$dist, frontier = LB$frontier,
            plan = LB$lab[LB$lab$strategy == "Plan" & LB$lab$phi == 0, c("path", "kind", "roe", "nii", "cet1_min_fv")],
            plan_by_path = LB$plan_by_path, drivers = LB$drivers, drivers_r2 = LB$drivers_r2, params = LB$params,
            rel_170 = LB$rel_170, rel_185 = LB$rel_185, sc_hq = LB$sc_hq,
            gains_r3 = LB$lab[LB$lab$phi == 0 & LB$lab$kind == "episode" & grepl("^R3", LB$lab$strategy), c("path", "gains", "d_roe")],
            theta0 = LB$rule_paths$baseline$theta[1], stress_share = mean(vapply(LB$rule_paths[LB$kind == "episode"], function(z) mean(z$stress), 0)))

## ---- the risk-budgeted optimiser, and the carry book ---------------------------------------------------
rb <- RB[c("levers", "plan_stats", "n_episodes", "n_paths", "n_combos", "n_admissible", "n_feasible", "n_runs_per_path",
           "binds", "frontier", "verify", "budgets", "b_central", "central", "regime", "scenarios", "oos", "limits_used",
           "variants", "limits", "carry")]
rb$hedge_set <- RB$hedge_set
rbc <- RB$candidates
rb$plan_row <- rbc[rbc$hedge == RB$plan_opt[["hedge"]] & rbc$dur == 1 & rbc$as == 1 & rbc$buf == 1 & rbc$pay == 2,
                   intersect(c("roe", "roe_p5", "nii", "cet1_min", "lcr_min", "nsfr_min", "eve_max", "use_max"), names(rbc))]
## the carry book against the buffer's bills, and the spread widenings history holds against the downside's
rr1 <- LB$inputs$baseline$rates[1, ]
cy_w <- vapply(LB$inputs[LB$kind == "episode"], function(i)
  max(asset_swap_spread(i$rates, LB$jo_mk, bsm$beta, bsm$kappa)) - LB$jo_mk$bs, 0)
rb$carry_extra <- list(gap = rr1$repo + RB$carry$repo_spread - rr1$tbill, widen_hist = max(cy_w),
                       widen_hist_path = names(cy_w)[which.max(cy_w)],
                       widen_down = max(asset_swap_spread(LB$inputs$downside$rates, LB$jo_mk, bsm$beta, bsm$kappa)) - LB$jo_mk$bs)

S <- list(meta = list(date0 = date0, horizon = CFG$horizon, version = as.character(utils::packageVersion("baeconstrat")),
                      built = format(Sys.time(), "%Y-%m-%d"), coe = CFG$coe, cet1_min = CFG$cet1_min, cet1_target = CFG$cet1_target,
                      lcr_min = CFG$lcr_min, sample_from = CFG$sample_from, n_sector_models = nrow(o("stage1.rds")$summary)),
          ngfb = ngfb, stage2 = stage2, scen = scen, pos = pos, share = share, audit = audit, levers = levers, lab = lab, rb = rb)
## nothing from the industry models' internals
bad <- grep("coef|lag|bma|weight_|multiplier|skill|spec", names(unlist(lapply(S[c("scen", "levers", "lab", "rb")], names))), value = TRUE)
stopifnot(!length(bad))
saveRDS(S, "blog/ngfb-macro-strategy/post_artefacts.rds", compress = "xz")
cat(sprintf("post snapshot: %.0f KB\n", file.size("blog/ngfb-macro-strategy/post_artefacts.rds") / 1024))
