# =============================================================================
#  analysis/06_optimise.R -- pre-positioning NGFB ahead of each scenario
#
#  Moves are made at the jump-off and then ride each line's projected growth.
#  Each asset move transfers 1% of total assets out of HQLA (the liquidity
#  numeraire) into a lending line; each funding move transfers 1% of total
#  assets out of wholesale funding into a deposit category; the hedge move
#  changes the receive-fixed structural hedge by 1% of the NMD base.
#
#  The status quo is taken to be optimal under the baseline (R/optimise.R):
#  each move's baseline ROE gradient is its implied marginal cost, and the
#  positioning for scenario X trades on how X changes that gradient. Every
#  result is reported in ROE and in net income, net of the implied marginal
#  cost and the execution cost.
# =============================================================================
source("analysis/00_config.R")
SC <- readRDS(out_path("scenarios.rds"))
N  <- readRDS(out_path("ngfb.rds"))
bs <- N$engine_bs
ta <- sum(bs$balance[bs$side == "asset"])
sh <- function(l) 100 * bs$balance[bs$line == l] / ta

## move table. Bounds: what a bank can credibly do inside a year. `cost`: the
## ROE (decimal) given up by a one-point move made inside the year -- pricing
## concessions and acquisition cost to grow a book quickly (dearer for
## unsecured retail and for transactional deposits, which are slow to build),
## attrition to shrink it; swaps are cheap, so the hedge is bounded by policy
## (a hedge ratio between roughly 25% and 75% of NMD) rather than by cost.
moves <- data.frame(
  move  = c("into_mortgages", "into_vaf", "into_cards", "into_personal", "into_corporate",
            "into_cre", "fund_term", "fund_savings", "fund_operational", "hedge"),
  label = c("Residential mortgages", "Vehicle and asset finance", "Credit cards",
            "Personal loans", "Corporate loans", "Commercial property", "Term deposits",
            "Savings deposits", "Transactional deposits", "Structural hedge"),
  from  = c(rep("hqla", 6), rep("wholesale_other", 3), NA),
  to    = c("mortgages_res", "vaf", "cards", "personal_other", "corporate_loans", "mortgages_com",
            "beh_term", "beh_savings", "beh_operational", NA),
  unit  = c(rep("assets", 9), "hedge"),
  lower = c(-min(2, sh("mortgages_res")), -min(2, sh("vaf")), -min(1.5, sh("cards")),
            -min(1.5, sh("personal_other")), -min(2, sh("corporate_loans")),
            -min(1.5, sh("mortgages_com")), -2, -1, -1, -25),
  upper = c(2, 1.5, 1, 1, 2, 1.5, 2, 1, 1, 25),
  cost  = c(0.0008, 0.0008, 0.0015, 0.0015, 0.0008, 0.0008, 0.0004, 0.0008, 0.0012, 0.00002),
  stringsAsFactors = FALSE)
## the most a lending book can shrink is the book itself (used when the
## aggressiveness analysis widens the bounds)
attr(moves, "book_share") <- c(sh("mortgages_res"), sh("vaf"), sh("cards"), sh("personal_other"),
                               sh("corporate_loans"), sh("mortgages_com"), rep(Inf, 4))

eng_args <- SC$eng_args
all_runs <- c(SC$results, SC$variants)
ctx <- list(bs = bs, moves = moves, hedge = SC$hedge0,
            run = function(b, h, s) {
              inp <- all_runs[[s]]$ngfb$inputs
              do.call(run_engine, c(list(bs = b, growth = inp$growth, rates = inp$rates,
                                         credit = inp$credit, ratios = inp$ratios, hedge = h,
                                         line_rates = inp$line_rates), eng_args))
            })
scen <- names(SC$results)
grad <- positioning_gradients(ctx, c(scen, names(SC$variants)), step = 0.5)
guard(all(abs(grad$R0[scen] - vapply(scen, function(s) SC$results[[s]]$ngfb$summary$roe, 0)) < 1e-12),
      "optimiser context does not reproduce the scenario runs")
guard(all(abs(grad$NI0[scen] - vapply(scen, function(s) SC$results[[s]]$ngfb$summary$net_income, 0)) < 1e-3),
      "optimiser context does not reproduce the scenarios' net income")
c0 <- grad$G["baseline", ]
c0_ni <- grad$Jni["baseline", ]             # implied marginal cost in net income (R'000 per unit)
fl <- list(cet1_min = CFG$cet1_min, lcr_min = CFG$lcr_min, reference = "baseline")
m_ni <- grad$NI0 / grad$R0                  # net income (R'000, 3 years) per unit of ROE

## positioned for each scenario in turn, and the regret of being wrong
rg <- do.call(regret_matrix, c(list(ctx, grad, moves, scen), fl))
guard(all(rg$positions["baseline", ] == 0), "status quo is not optimal under the baseline")

## robust positioning: probability-weighted, penalising cross-scenario variance
w <- CFG$scenario_weights[scen]
lam <- c(0, 4, 20)
robust_all <- lapply(lam, function(l) do.call(solve_positioning, c(list(grad, moves, w, lambda = l), fl)))
names(robust_all) <- paste0("lambda_", lam)
robust <- robust_all[["lambda_4"]]
robust_val <- t(vapply(scen, function(s) positioning_value(robust$x, ctx, s, c0, moves, m_ni[[s]], c0_ni),
                      numeric(9)))
status_quo <- grad$R0[scen]

## linear-prediction check: the QP's predicted ROE vs the full engine (both
## net of implied cost, before execution cost)
own <- vapply(scen, function(s) {
  x <- rg$positions[s, ]
  eval_positioning(x, ctx, s)[["roe"]] - sum(c0 * x)
}, 0)
pred <- vapply(scen, function(s) do.call(solve_positioning,
                                          c(list(grad, moves, stats::setNames(1, s)), fl))$roe_pred[[s]], 0)
guard(max(abs(own - pred)) < 0.0005, "QP linearisation error above 5 bps of ROE (%.5f)",
      max(abs(own - pred)))

## ---- step by step, per scenario ----------------------------------------------
## the optimisation unpacked: engine gradient G_s, implied cost c (= G under
## the baseline), net gradient G_s - c, execution cost, the unconstrained
## optimum (G_s - c) / (2 cost), the bounds, the QP solution, and each move's
## linear and full-engine contribution
walk <- lapply(scen, function(s) {
  x <- rg$positions[s, ]
  net <- grad$G[s, ] - c0
  x_unc <- net / (2 * moves$cost)
  pb <- positioning_bridge(x, ctx, s, grad, c0, moves)
  data.frame(move = moves$move, label = moves$label, unit = moves$unit, g = grad$G[s, ], c = c0, net = net,
             g_ni = grad$Jni[s, ], c_ni = c0_ni,
             cost = moves$cost, x_unc = x_unc, lower = moves$lower, upper = moves$upper, x = x,
             at_bound = abs(x - moves$lower) < 1e-9 | abs(x - moves$upper) < 1e-9,
             lin = net * x - moves$cost * x^2,
             engine = pb$roe_net[match(moves$move, pb$move)], engine_ni = pb$ni_net[match(moves$move, pb$move)],
             d_cet1 = grad$Jc[s, ] * x, d_lcr = grad$Jl[s, ] * x, stringsAsFactors = FALSE)
})
names(walk) <- scen
for (s in scen) walk[[s]]$engine[!is.finite(walk[[s]]$engine)] <- 0
for (s in scen) walk[[s]]$engine_ni[!is.finite(walk[[s]]$engine_ni)] <- 0
guard(all(vapply(setdiff(scen, "baseline"), function(s)
  abs(sum(walk[[s]]$engine) - rg$gain[s, s]) < 1e-10, TRUE)), "walk-through does not add up to the gain")
guard(all(vapply(setdiff(scen, "baseline"), function(s)
  abs(sum(walk[[s]]$engine_ni) - rg$gain_ni[s, s]) < 1e-3, TRUE)), "walk-through does not add up in net income")

## ALCO bridges: baseline plan -> scenario arrives -> positioned for it.
## Scenario bars by channel (05), positioning bars net of implied and
## execution cost; ROE, net income and NIM-equivalent (one factor m = ROE /
## NIM-equivalent of the baseline run converts ROE to NIM-equivalent bps).
m_conv <- grad$R0[["baseline"]] / grad$N0[["baseline"]]
bridge_for <- function(X) {
  att <- SC$attribution[SC$attribution$scenario == X & SC$attribution$entity == "NGFB", ]
  a_r <- att[att$metric == "roe", ]; a_n <- att[att$metric == "net_income", ]
  pb <- positioning_bridge(rg$positions[X, ], ctx, X, grad, c0, moves)
  ch <- c("volume", "pricing", "credit", "fees_costs", "interaction")
  br <- rbind(
    data.frame(step = "Baseline plan", kind = "level", roe = grad$R0[["baseline"]], ni = grad$NI0[["baseline"]]),
    data.frame(step = c("Volume", "Pricing and hedge", "Credit", "Fees and costs", "Interaction"),
               kind = "scenario", roe = unlist(a_r[ch]), ni = unlist(a_n[ch])),
    data.frame(step = moves$label[match(pb$move, moves$move)], kind = rep("positioning", nrow(pb)),
               roe = pb$roe_net, ni = pb$ni_net),
    data.frame(step = paste("Positioned for the", X), kind = "level", roe = rg$roe[X, X], ni = rg$ni[X, X]))
  rownames(br) <- NULL
  br$nim_eq_bps <- 1e4 * br$roe / m_conv
  br$roe_bps <- 1e4 * br$roe
  guard(abs(sum(br$roe[br$kind != "level"]) + br$roe[1] - utils::tail(br$roe, 1)) < 1e-10,
        "ALCO bridge (%s) does not add up in ROE", X)
  guard(abs(sum(br$ni[br$kind != "level"]) + br$ni[1] - utils::tail(br$ni, 1)) < 1e-3,
        "ALCO bridge (%s) does not add up in net income", X)
  list(bridge = br, moves = pb)
}
bridges <- lapply(c(upside = "upside", downside = "downside"), bridge_for)

## conviction ladders: probability of X rising from its prior weight to
## certainty (the rest of the prior scaled down pro rata)
pgrid <- seq(0, 1, by = 0.1)
ladder_for <- function(X) do.call(rbind, lapply(pgrid, function(p) {
  wp <- (1 - p) * w; wp[X] <- wp[X] + p
  z <- do.call(solve_positioning, c(list(grad, moves, wp, lambda = 4), fl))
  g <- z$roe_pred - grad$R0[names(wp)] - z$exec_cost
  data.frame(p = p, t(z$x), exp_gain = sum(wp * g), gain_if_x = g[[X]], gain_if_base = g[["baseline"]],
             gain_if_other = g[[setdiff(scen, c(X, "baseline"))]],
             exp_gain_ni = sum(wp * g * m_ni[names(wp)]), gain_if_x_ni = g[[X]] * m_ni[[X]])
}))
ladders <- lapply(c(upside = "upside", downside = "downside"), ladder_for)

## ---- how aggressive? -------------------------------------------------------------
aggr <- lapply(c(upside = "upside", downside = "downside"), function(X)
  do.call(positioning_aggressiveness, c(list(ctx, grad, moves, X, scen, w,
                                             alpha = c(0.5, 1, 1.5, 2, 3),
                                             cost_scale = c(2, 1, 0.5, 0.25, 0.1), bound_scale = c(1, 2)), fl)))
for (X in names(aggr)) {
  a <- aggr[[X]]$size
  guard(abs(a$gain[a$alpha == 1 & a$realised == X] - rg$gain[X, X]) < 1e-10,
        "aggressiveness (%s): alpha = 1 does not reproduce the optimum", X)
}

## ---- the downside's key judgement: what if the SARB cuts instead? ---------------
xc <- do.call(solve_positioning, c(list(grad, moves, c(downside_cuts = 1)), fl))
cuts_vs <- do.call(rbind, lapply(c("downside_cuts", scen), function(Y) {
  pv <- function(x) positioning_value(x, ctx, Y, c0, moves, m_ni[[Y]], c0_ni)
  data.frame(realised = Y, sq = grad$R0[[Y]], sq_ni = grad$NI0[[Y]],
             pos_cuts = pv(xc$x)[["roe_net"]] - grad$R0[[Y]], pos_cuts_ni = pv(xc$x)[["ni_net"]] - grad$NI0[[Y]],
             pos_down = pv(rg$positions["downside", ])[["roe_net"]] - grad$R0[[Y]],
             pos_down_ni = pv(rg$positions["downside", ])[["ni_net"]] - grad$NI0[[Y]])
}))

saveRDS(list(moves = moves, gradients = grad, implied_cost = c0, regret = rg, robust = robust,
             robust_all = robust_all, robust_value = robust_val, status_quo = status_quo,
             own = own, pred = pred, weights = w, lambda = lam, m_conv = m_conv, m_ni = m_ni, implied_ni = c0_ni,
             walk = walk, bridges = bridges, ladders = ladders, aggressiveness = aggr,
             cuts = list(x = xc$x, binding = xc$binding, value = cuts_vs)),
        out_path("optimise.rds"))
ngfb_log("optimiser: positioning adds %.0f bps of ROE if the downside arrives (R%.0f m over three years), %.0f bps if the upside arrives; max regret %.0f bps",
         1e4 * rg$gain["downside", "downside"], rg$gain_ni["downside", "downside"] / 1e3,
         1e4 * rg$gain["upside", "upside"], 1e4 * max(rg$regret))
