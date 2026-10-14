# =============================================================================
#  strategy.R -- market share, market growth and where NGFB should lean in
#
#  Stage 2 gives every bank (and NGFB) a share response to each macro regime:
#
#    400 dlog s_t = a + delta_r + kappa (g_t - mean g) + e_t
#
#  so NGFB's growth in product p is the market's growth plus its share
#  effect. Two numbers per product and regime therefore position NGFB:
#
#    * how fast the MARKET grows in that regime (Stage 1, from history), and
#    * how NGFB's SHARE moves relative to the market, compared with the
#      population of large banks it was drawn from (Stage 2 hyperparameters:
#      a typical large bank's effect is N(mu, tau^2 + se_mu^2)).
#
#  The structural drift a is left out throughout (NGFB is projected without
#  one), so every comparison is about the cycle, not about trend share gains.
#  Only population hyperparameters are used for the comparison, never an
#  individual bank's estimate (k-anonymity >= 4).
# =============================================================================

#' Average sector growth by macro regime
#' @param sector_growth Long data frame `date, product, growth` (annualised \%).
#' @param regimes Data frame `date, regime`.
#' @param products Products to keep.
#' @param from First quarter used.
#' @return Data frame `product, regime, growth, n, mean_all, dev` (growth in
#'   the regime, quarters, the product's average growth, and the difference).
#' @export
regime_market_growth <- function(sector_growth, regimes, products, from = NULL) {
  z <- merge(sector_growth[sector_growth$product %in% products, c("date", "product", "growth")],
             regimes, by = "date")
  if (!is.null(from)) z <- z[z$date >= as.Date(from), , drop = FALSE]
  z <- z[is.finite(z$growth), , drop = FALSE]
  a <- stats::aggregate(list(growth = z$growth), list(product = z$product, regime = z$regime), mean)
  n <- stats::aggregate(list(n = z$growth), list(product = z$product, regime = z$regime), length)
  m <- stats::aggregate(list(mean_all = z$growth), list(product = z$product), mean)
  a <- merge(merge(a, n, by = c("product", "regime")), m, by = "product")
  a$dev <- a$growth - a$mean_all
  a[order(a$product, a$regime), ]
}

#' Share effect of one parameter vector in a regime (drift excluded)
#' @param par Named parameter vector (`contraction`, `hiking`, `easing`,
#'   `kappa`).
#' @param regime Regime label.
#' @param g Sector growth in the regime (annualised \%).
#' @param mean_g Centring growth of the share model.
#' @return Expected `400 dlog s` (percentage points a year).
#' @export
regime_share_effect <- function(par, regime, g, mean_g) {
  d <- if (regime == "neutral") 0 else par[[regime]]
  d + par[["kappa"]] * (g - mean_g)
}

#' Where NGFB sits in each product and regime
#'
#' For every product and regime: the market's growth, NGFB's share effect,
#' the typical large bank's (population mean) effect, the spread of the
#' population's posterior predictive, NGFB's percentile in it, and the 80th
#' percentile ("frontier": what a top-quintile large bank achieves).
#' @param ngfb_par NGFB's share parameters (product x param).
#' @param hyper Population hyperparameters (`product, param, mu, tau, se_mu`).
#' @param mkt Output of [regime_market_growth()].
#' @param mean_g Named centring growth per product.
#' @param frontier_q Quantile defining the frontier.
#' @return Data frame by product and regime.
#' @export
regime_positioning <- function(ngfb_par, hyper, mkt, mean_g, frontier_q = 0.80) {
  hyper$sd <- sqrt(hyper$tau^2 + hyper$se_mu^2)
  hp <- function(p, k, v) { x <- hyper[hyper$product == p & hyper$param == k, v]; if (length(x)) x[1] else 0 }
  out <- list()
  for (i in seq_len(nrow(mkt))) {
    p <- mkt$product[i]; r <- mkt$regime[i]
    if (!p %in% rownames(ngfb_par)) next
    dg <- mkt$growth[i] - mean_g[[p]]
    e_n <- regime_share_effect(ngfb_par[p, ], r, mkt$growth[i], mean_g[[p]])
    mu <- (if (r == "neutral") 0 else hp(p, r, "mu")) + hp(p, "kappa", "mu") * dg
    sd <- sqrt((if (r == "neutral") 0 else hp(p, r, "sd"))^2 + (dg * hp(p, "kappa", "sd"))^2)
    pct <- if (sd > 0) stats::pnorm((e_n - mu) / sd) else NA_real_
    out[[length(out) + 1L]] <- data.frame(product = p, regime = r, market_growth = mkt$growth[i],
                                          market_dev = mkt$dev[i], quarters = mkt$n[i],
                                          ngfb_effect = e_n, ngfb_growth = mkt$growth[i] + e_n,
                                          par = mu, sd = sd, frontier = mu + stats::qnorm(frontier_q) * sd,
                                          percentile = pct, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

#' Peer groups' share effects by regime (drift excluded)
#' @param coefs Group share-model coefficients ([fit_share_models()]).
#' @param mkt Output of [regime_market_growth()].
#' @param mean_g Named centring growth per product.
#' @return Data frame `entity, product, regime, effect, drift`.
#' @export
group_regime_effects <- function(coefs, mkt, mean_g) {
  out <- list()
  for (e in unique(coefs$entity)) for (i in seq_len(nrow(mkt))) {
    p <- mkt$product[i]; r <- mkt$regime[i]
    cf <- coefs[coefs$entity == e & coefs$product == p, ]
    if (!nrow(cf)) next
    par <- stats::setNames(cf$est, cf$param)
    out[[length(out) + 1L]] <- data.frame(entity = e, product = p, regime = r,
                                          effect = regime_share_effect(par, r, mkt$growth[i], mean_g[[p]]),
                                          drift = par[["(Intercept)"]], stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

#' Classify each product-regime cell and attach a strategy
#'
#' Rules (economics first): a share LOSS in a regime where the market grows
#' at or above its average is under-participation -- push, if the product
#' earns its cost of capital; a share GAIN in unsecured lending while the
#' market shrinks or stalls is a warning of adverse selection -- guard; a
#' share loss in a weak market for a secured or funding product is
#' competitors taking good business -- defend the core; a share loss in
#' unsecured lending in a contraction is the risk appetite working -- let it
#' go; gains in a growing market -- ride, with capacity and pricing discipline.
#' @param pos Output of [regime_positioning()].
#' @param unsecured Products treated as unsecured (high-loss) lending.
#' @param funding Funding products.
#' @param move Threshold (pp a year) for a material share move.
#' @return `pos` with `quadrant`, `action`, `rationale`.
#' @export
strategy_rules <- function(pos, unsecured = c("cards", "personal_other"),
                           funding = c("nmd", "beh_term"), move = 0.5) {
  pos$quadrant <- with(pos, ifelse(market_dev >= 0,
                                   ifelse(ngfb_effect >= 0, "Growing market, gaining share",
                                          "Growing market, losing share"),
                                   ifelse(ngfb_effect >= 0, "Weak market, gaining share",
                                          "Weak market, losing share")))
  act <- rep("Hold", nrow(pos)); why <- rep("NGFB moves with the market and with its peers.", nrow(pos))
  below <- is.finite(pos$percentile) & pos$percentile < 0.35
  above <- is.finite(pos$percentile) & pos$percentile > 0.65
  lose <- pos$ngfb_effect < -move; gain <- pos$ngfb_effect > move
  uns <- pos$product %in% unsecured; fund <- pos$product %in% funding
  grow <- pos$market_dev >= 0
  i <- lose & grow & !fund
  act[i] <- "Push"; why[i] <- "Losing share when the market grows fastest: the cheapest growth NGFB forgoes."
  i <- lose & grow & fund
  act[i] <- "Defend"; why[i] <- "Losing deposit share in a growing deposit market: funding NGFB will later buy wholesale."
  i <- gain & !grow & uns
  act[i] <- "Guard"; why[i] <- "Gaining unsecured share while the market weakens: check it is not the business others decline."
  i <- lose & !grow & uns & pos$regime == "contraction"
  act[i] <- "Let go"; why[i] <- "Ceding unsecured share in a contraction is risk appetite working, not a weakness."
  i <- lose & !grow & !uns & act == "Hold"
  act[i] <- "Defend"; why[i] <- "Losing share in a weak market: competitors are taking good-quality business."
  i <- gain & grow & act == "Hold"
  act[i] <- "Ride"; why[i] <- "Gaining share in a growing market: keep capacity and pricing discipline."
  pos$action <- act; pos$rationale <- why
  pos$below_par <- below; pos$above_par <- above
  pos
}

#' Marginal economics of each modelled product, per rand of balance a year
#'
#' What one more rand of a product contributes once it is funded, buffered,
#' lost and capitalised -- the yardstick for a share strategy, which adds
#' balances at the margin. (The scenario engine instead grows NGFB's treasury
#' and other assets with its loan book, the right convention for a whole
#' scenario but not for a single-product decision.) Per quarter:
#'
#'   loans:    (yield - FTP - liquidity buffer - expected loss) after tax,
#'             less a capital charge: CET1 target x risk weight x (cost of
#'             equity - after-tax FTP), since the capital also funds;
#'   deposits: (FTP - deposit cost - liquidity buffer) after tax: a deposit
#'             displaces wholesale funding.
#'
#' The result is a CONTRIBUTION before operating costs, because the returns
#' carry no costs by product: it is the most the extra business could cost
#' to originate and service before it destroys value (`headroom`, pre-tax).
#' FTP is the marginal wholesale rate (`funding = "wholesale"`) or NGFB's
#' average cost of funds (`"blended"`: a push that brings its own deposits).
#' The liquidity buffer is the HQLA that a rand of wholesale funding (or of
#' the deposit) needs for its 30-day runoff, held at the HQLA yield.
#' @param line_rates Engine line rates (`date` + percent per line).
#' @param clr Credit-loss ratio path (percent).
#' @param bs Engine balance sheet (line characteristics and balances).
#' @param products Product -> lines map ([line_driver_map()]).
#' @param tax Tax rate.
#' @param cet1 CET1 target (capital per unit of RWA).
#' @param coe Cost of equity.
#' @param funding `"wholesale"` or `"blended"`.
#' @param cost_of_funds Average cost of funds path (percent; for `"blended"`).
#' @param wholesale_runoff Runoff factor of marginal wholesale funding.
#' @return Long data frame `date, product`, the components (percent a year)
#'   and `contribution` (fraction a year, after tax and the capital charge,
#'   before operating costs) and `headroom` (pre-tax, percent a year).
#' @export
product_margins <- function(line_rates, clr, bs, products = line_driver_map(), tax, cet1, coe,
                            funding = c("wholesale", "blended"), cost_of_funds = NULL,
                            wholesale_runoff = 0.25) {
  funding <- match.arg(funding)
  out <- list()
  for (q in seq_len(nrow(line_rates))) {
    lr <- line_rates[q, ]
    ftp <- if (funding == "wholesale") lr[["wholesale_other"]] else cost_of_funds[q]
    hq <- lr[["hqla"]]
    for (p in unique(products)) {
      ln <- names(products)[products == p]
      b <- bs[match(ln, bs$line), ]
      w <- b$balance / sum(b$balance)
      rate <- sum(w * vapply(ln, function(l) lr[[l]], 0))
      if (all(b$side == "deposit")) {
        liq <- sum(w * b$runoff) * (ftp - hq); el <- 0; cap <- 0
        pre <- ftp - rate - liq
      } else {
        liq <- wholesale_runoff * (ftp - hq)
        el <- clr[q] * sum(w * b$clr_mult)
        pre <- rate - ftp - liq - el
        cap <- cet1 * sum(w * b$rw) * (100 * coe - ftp * (1 - tax))
      }
      contrib <- pre * (1 - tax) - cap
      out[[length(out) + 1L]] <- data.frame(date = lr$date, product = p, rate = rate, ftp = ftp,
                                            liquidity = liq, expected_loss = el, capital_charge = cap,
                                            contribution = contrib / 100, headroom = contrib / (1 - tax),
                                            stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}

#' NGFB's balance path in one product under given share parameters
#' @param b0 Jump-off balance.
#' @param g Sector growth path (annualised \%).
#' @param par Share parameters for the product.
#' @param regimes Regime path.
#' @param mean_g Centring growth.
#' @return Balances at the end of each quarter.
#' @export
share_balance_path <- function(b0, g, par, regimes, mean_g) {
  ds <- share_growth(par, regimes, g, mean_g, drift = FALSE)
  b0 * exp(cumsum((g + ds) / 400))
}
