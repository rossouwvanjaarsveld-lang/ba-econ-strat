# =============================================================================
#  analysis/10_share_strategy.R -- market share x market growth: where NGFB
#                                  under-participates, and what fixing it is
#                                  worth
#
#  1. Market growth by macro regime (sector history) and NGFB's share effect
#     in each regime against the population of large banks (par, spread,
#     NGFB's percentile, the top-quintile frontier).
#  2. A strategy per product and regime (push / defend / guard / let go /
#     ride) from economic rules, with the peer groups that gain the share
#     NGFB loses.
#  3. What closing each gap is worth: the scenario re-run with NGFB's share
#     effect in that regime moved to par (and to the frontier), through the
#     full engine -- net income, ROE, risk-weighted assets, and economic
#     profit after a charge for the capital the extra book consumes. For
#     unsecured share GAINS in a weak market, the same run asks the reverse:
#     at what marginal loss rate does the extra share stop paying?
# =============================================================================
source("analysis/00_config.R")
S2 <- readRDS(out_path("stage2.rds"))
N  <- readRDS(out_path("ngfb.rds"))
SC <- readRDS(out_path("scenarios.rds"))
prods <- model_products()

## ---- 1-2. positioning and strategies --------------------------------------------
mkt <- regime_market_growth(S2$sector_growth, S2$regimes, prods, from = CFG$sample_from)
pos <- strategy_rules(regime_positioning(N$share_params, S2$population_hyper, mkt, S2$mean_g,
                                         frontier_q = CFG$frontier_q))
grp <- group_regime_effects(S2$coef_groups, mkt, S2$mean_g)
## who gains the share NGFB loses (proportional convention of share_flow_matrix)
gainers <- do.call(rbind, lapply(seq_len(nrow(pos)), function(i) {
  f <- N$flows[[paste(pos$product[i], pos$regime[i])]]
  if (is.null(f) || !"NGFB" %in% colnames(f) || sum(f[, "NGFB"]) <= 0) return(NULL)
  v <- f[, "NGFB"]; v <- v[v > 0]
  data.frame(product = pos$product[i], regime = pos$regime[i],
             to = names(v), share_of_loss = as.numeric(v / sum(v)), stringsAsFactors = FALSE)
}))
guard(all(abs(stats::aggregate(share_of_loss ~ product + regime, gainers, sum)$share_of_loss - 1) < 1e-9),
      "share-loss attribution does not add up")

## ---- 3. value of closing gaps: marginal product economics ---------------------------
hp <- S2$population_hyper; hp$sd <- sqrt(hp$tau^2 + hp$se_mu^2)
hv <- function(p, k, v) hp[hp$product == p & hp$param == k, v][1]
bs <- N$engine_bs
b0 <- vapply(prods, function(p) sum(bs$balance[bs$line %in% names(line_driver_map())[line_driver_map() == p]]), 0)
margins <- list()
for (s in names(SC$results)) {
  r <- SC$results[[s]]$ngfb
  for (fnd in c("wholesale", "blended")) {
    m <- product_margins(r$inputs$line_rates, r$inputs$credit$clr, bs,
                         tax = SC$eng_args$tax, cet1 = CFG$cet1_target, coe = CFG$coe, funding = fnd,
                         cost_of_funds = 100 * r$run$quarterly$cost_of_funds)
    m$scenario <- s; m$funding <- fnd
    margins[[paste(s, fnd)]] <- m
  }
}
margins <- do.call(rbind, margins)
guard(all(is.finite(margins$contribution)), "a product margin is not finite")

cells <- pos[pos$action %in% c("Push", "Defend", "Guard") & pos$regime != "neutral", ]
value <- list()
for (i in seq_len(nrow(cells))) {
  p <- cells$product[i]; r <- cells$regime[i]
  par_d <- hv(p, r, "mu"); fr_d <- par_d + stats::qnorm(CFG$frontier_q) * hv(p, r, "sd")
  for (s in names(SC$results)) {
    rg <- SC$results[[s]]$ngfb$regimes$regime
    if (!any(rg == r)) next
    g <- SC$results[[s]]$ngfb$proj[[paste0("g_", p)]][seq_along(rg)]
    eq <- mean(SC$results[[s]]$ngfb$run$quarterly$equity)
    base_b <- share_balance_path(b0[[p]], g, N$share_params[p, ], rg, S2$mean_g[[p]])
    for (to in c("par", "frontier")) {
      if (cells$action[i] == "Guard" && to == "frontier") next
      pr <- N$share_params[p, ]; pr[[r]] <- if (to == "par") par_d else fr_d
      new_b <- share_balance_path(b0[[p]], g, pr, rg, S2$mean_g[[p]])
      dB <- new_b - base_b
      dB_avg <- (c(0, utils::head(dB, -1)) + dB) / 2           # average balance in each quarter
      for (fnd in c("wholesale", "blended")) {
        mg <- margins[margins$scenario == s & margins$funding == fnd & margins$product == p, ]
        if (fnd == "blended" && p %in% c("nmd", "beh_term")) next
        value[[length(value) + 1L]] <- data.frame(
          product = p, regime = r, action = cells$action[i], scenario = s, to = to, funding = fnd,
          quarters = sum(rg == r), d_balance_end = utils::tail(dB, 1), d_balance_avg = mean(dB_avg),
          contribution = sum(dB_avg * mg$contribution / 4),
          d_roe = sum(dB_avg * (mg$contribution + mg$capital_charge / 100) / 4) / 3 / eq,
          margin = mean(mg$contribution), headroom = mean(mg$headroom),
          stringsAsFactors = FALSE)
      }
    }
  }
}
value <- do.call(rbind, value)
## the whole playbook (every push and defend cell to par), by scenario and funding
playbook <- stats::aggregate(cbind(contribution, d_roe) ~ scenario + funding,
                             value[value$to == "par" & value$action %in% c("Push", "Defend"), ], sum)

saveRDS(list(market = mkt, positioning = pos, groups = grp, gainers = gainers, value = value,
             margins = margins, playbook = playbook, cells = cells, b0 = b0,
             assumptions = CFG[c("coe", "cet1_target", "frontier_q")]),
        out_path("strategy.rds"))
ngfb_log("strategy: %d product-regime cells, %d with an action", nrow(pos), sum(pos$action != "Hold"))
