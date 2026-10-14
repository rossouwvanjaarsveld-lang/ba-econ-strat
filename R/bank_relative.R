# =============================================================================
#  bank_relative.R -- Stage 2: individual banks relative to the sector
#
#  Stage 1 explains the SECTOR with macro variables. Stage 2 explains each
#  bank's (or peer group's) deviation from the sector, so a bank's macro
#  exposure is always routed through the sector: the macro elasticities are
#  estimated once, on the long, well-identified aggregate, and the bank
#  layer only has to explain market-share dynamics. For product p:
#
#    100 * 4 * dlog s_{g,p,t} = a + sum_r delta_r 1[regime_t = r]
#                               + kappa * (g_{sector,p,t} - mean g) + e_t
#
#  where s is the market share, g the sector's annualised growth in p, and the
#  regimes are contraction, hiking and easing (neutral is the base). a is the
#  structural share drift, delta_r the extra share drift in regime r, and
#  kappa = beta - 1 the share's sensitivity to the sector cycle (beta > 1:
#  the bank amplifies the cycle). This answers "who takes share from whom,
#  and when" directly, and gives the projection rule for a bank's book.
# =============================================================================

#' Classify each quarter into a macro regime
#'
#' Priority: contraction (real activity y/y < 0) > hiking (policy rate up at
#' least 25 bps over two quarters) > easing (down at least 25 bps) > neutral.
#' @param m Data frame with `repo` and an activity measure.
#' @param activity Activity column (default `gdp_yy`).
#' @param window Quarters over which the policy-rate change is measured.
#' @return Character vector of regimes.
#' @export
classify_regimes <- function(m, activity = "gdp_yy", window = 2L) {
  # real GDP only: the SARB coincident indicator ran negative through 2025-26
  # while GDP grew, so it is not used as a fallback. Quarters without GDP data
  # (the most recent ones) are classified on the policy rate alone.
  a <- m[[activity]]
  dr <- m$repo - lag_vec(m$repo, window)
  out <- rep("neutral", nrow(m))
  out[is.finite(dr) & dr <= -0.25] <- "easing"
  out[is.finite(dr) & dr >= 0.25] <- "hiking"
  out[is.finite(a) & a < 0] <- "contraction"
  out
}

#' Default peer-group assignment of BA900 institution keys
#'
#' Groups have at least four members in every period (k-anonymity >= 4).
#' Acquired banks are assigned to their acquirer's group (pro-forma), so
#' mergers (Mercantile into Capitec, Imperial into Nedbank, Ubank into
#' African Bank) do not masquerade as share shifts between groups.
#' @return Named character vector key -> group.
#' @export
peer_groups <- function() {
  c(ABSA = "Big Four", FIRSTRAND = "Big Four", NEDBANK = "Big Four", STANDARD = "Big Four",
    IMPERIAL = "Big Four",
    CAPITEC = "Challengers", MERCANTILE = "Challengers", AFRICAN = "Challengers",
    UBANK = "Challengers", TYME = "Challengers", DISCOVERY = "Challengers",
    BANKZERO = "Challengers", OLDMUTUAL = "Challengers", POSTBANK = "Challengers",
    INVESTEC = "Specialists", SASFIN = "Specialists", GRINDROD = "Specialists",
    BIDVEST = "Specialists", ALBARAKA = "Specialists", HBZ = "Specialists",
    HABIB = "Specialists", ACCESS = "Specialists")
}

#' Who the peer-group members are
#'
#' One-line descriptions of the named banks in [peer_groups()] (corporate
#' history as publicly reported; checked October 2026). Membership only: the
#' package never reports results for an individual bank.
#' @return Data frame `key, bank, description`.
#' @export
peer_group_profiles <- function() {
  d <- function(key, bank, description) data.frame(key = key, bank = bank, description = description,
                                                   stringsAsFactors = FALSE)
  rbind(
    d("ABSA", "Absa Bank", paste("Universal bank; part of Barclays Africa Group until Barclays cut its stake to",
      "14.9% in 2017, after which the group took the Absa name (2018).")),
    d("FIRSTRAND", "FirstRand Bank", paste("Universal bank operating as FNB (retail and commercial), RMB",
      "(corporate and investment banking) and WesBank (vehicle and asset finance).")),
    d("NEDBANK", "Nedbank", paste("Universal bank with a large corporate and commercial-property book;",
      "Old Mutual distributed most of its majority stake to shareholders in its 2018 managed separation.")),
    d("STANDARD", "The Standard Bank of South Africa", paste("The largest bank by assets; China's ICBC has",
      "held 20% of Standard Bank Group since March 2008.")),
    d("IMPERIAL", "Imperial Bank", paste("Vehicle and asset finance bank founded by Imperial Holdings in 1996;",
      "Nedbank held 50.1% from 2001 and bought the rest in 2009. Its return ends in 2010Q3 and the book",
      "continues inside Nedbank, so it is counted with the Big Four (Nedbank pro forma).")),
    d("CAPITEC", "Capitec Bank", paste("Retail bank founded in 2001 on low-cost transactional banking and",
      "unsecured lending; now the largest retail bank by number of clients. Grouped with the challengers",
      "for its growth path and business model, not its size.")),
    d("MERCANTILE", "Mercantile Bank", paste("Business bank bought by Capitec from Caixa Geral de Dep\u00f3sitos",
      "in November 2019 (R3.56 bn); its return ends in 2022Q1 as the business moved into Capitec.")),
    d("AFRICAN", "African Bank", paste("Unsecured lender placed under curatorship in August 2014 and relaunched",
      "in April 2016; has since bought Grindrod Bank (agreed 2022), most of Ubank (2022) and Sasfin's",
      "equipment- and property-finance books (agreed 2023) to build a business bank.")),
    d("UBANK", "Ubank", paste("Formerly Teba Bank, serving mining communities; placed under curatorship in",
      "May 2022 and most of its book taken over by African Bank from November 2022; return ends 2024Q2.")),
    d("TYME", "TymeBank", paste("Digital bank licensed in 2017 as TymeDigital under Commonwealth Bank of",
      "Australia, sold to African Rainbow Capital in 2018; rebranded GoTyme Bank in January 2026.")),
    d("DISCOVERY", "Discovery Bank", paste("Banking arm of the insurer Discovery; launched in late 2018 and",
      "opened to clients in 2019 (its return starts in 2016Q4, before launch).")),
    d("BANKZERO", "Bank Zero", paste("Digital mutual bank registered in 2018 and launched in 2021; Lesaka",
      "agreed to buy it in 2025, subject to approval.")),
    d("OLDMUTUAL", "OM Bank", paste("Old Mutual's new retail bank (licensed March 2024, national rollout from",
      "late 2025). It reports under a name the pull did not match, so in this data it sits in Other.")),
    d("POSTBANK", "Postbank", "In the name patterns, but it does not appear in the BA900 listings used here."),
    d("INVESTEC", "Investec Bank", paste("Specialist bank and wealth manager for private clients and",
      "corporates; part of the Investec group, listed in Johannesburg and London.")),
    d("SASFIN", "Sasfin Bank", paste("Business bank (equipment and trade finance). It sold its equipment- and",
      "property-finance books to African Bank, closed its business bank by the end of 2025 and is",
      "deregistering its licence; its return ends in 2026Q1.")),
    d("GRINDROD", "Grindrod Bank", paste("Business bank owned by the Grindrod logistics group until African",
      "Bank agreed to buy it in May 2022 (R1.5 bn); its return ends in 2024Q2.")),
    d("BIDVEST", "Bidvest Bank", paste("Part of the Bidvest group: foreign exchange, fleet and business",
      "banking. A sale to Access Bank agreed in December 2024 lapsed in January 2026.")),
    d("ALBARAKA", "Albaraka Bank", "Islamic bank, part of the Bahrain-based Al Baraka banking group."),
    d("HBZ", "HBZ Bank", "Subsidiary of Habib Bank AG Zurich (Switzerland)."),
    d("HABIB", "Habib Overseas Bank", paste("Small bank licensed in 1990, unrelated to HBZ; placed under",
      "curatorship in March 2023.")),
    d("ACCESS", "Access Bank South Africa", paste("The former Bank of Athens (South Africa), bought by Grobank",
      "in 2018 and renamed Grobank in 2019, then controlled by Nigeria's Access Bank from 2021. Its",
      "pre-2018 returns (The S A Bank of Athens) were not pulled and sit in Other."))
  )
}

#' The six large banks that form NGFB's reference population
#' @return Character vector of institution keys.
#' @export
ngfb_population_keys <- function() c("ABSA", "FIRSTRAND", "NEDBANK", "STANDARD", "CAPITEC", "INVESTEC")

#' Fit the share-dynamics model for one bank/group and product
#'
#' @param share Market-share series (levels, 0-1), quarterly.
#' @param g_sector Sector growth in the product (annualised \%, quarterly).
#' @param regime Regime labels (see [classify_regimes()]).
#' @param min_obs Minimum usable observations.
#' @return List with `coef`, `se`, `t`, `n`, `resid_sd`, `mean_g`, or `NULL`.
#' @export
fit_share_model <- function(share, g_sector, regime, min_obs = 24L) {
  ds <- 400 * c(NA, diff(log(share)))
  mg <- mean(g_sector, na.rm = TRUE)
  X <- cbind(`(Intercept)` = 1,
             contraction = as.numeric(regime == "contraction"),
             hiking = as.numeric(regime == "hiking"),
             easing = as.numeric(regime == "easing"),
             kappa = g_sector - mg)
  ok <- stats::complete.cases(ds, X) & is.finite(ds)
  if (sum(ok) < min_obs) return(NULL)
  keep <- colSums(abs(X[ok, , drop = FALSE])) > 0
  f <- ols_hac(ds[ok], X[ok, keep, drop = FALSE])
  full <- function(v) { o <- stats::setNames(rep(0, ncol(X)), colnames(X)); o[names(v)] <- v; o }
  sef <- function(v) { o <- stats::setNames(rep(NA_real_, ncol(X)), colnames(X)); o[names(v)] <- v; o }
  list(coef = full(f$coef), se = sef(f$se), t = sef(f$t), n = f$n,
       resid_sd = f$sigma, mean_g = mg, r2 = f$r2)
}

#' Share-dynamics models for every entity x product
#'
#' @param shares Long data frame `date, entity, product, share`.
#' @param sector Long data frame `date, product, growth` (annualised \%).
#' @param regimes Data frame `date, regime`.
#' @return Long data frame of coefficients with standard errors.
#' @export
fit_share_models <- function(shares, sector, regimes) {
  out <- list()
  for (p in unique(shares$product)) for (e in unique(shares$entity)) {
    s <- shares[shares$product == p & shares$entity == e, c("date", "share")]
    if (nrow(s) < 24L || any(s$share <= 0, na.rm = TRUE)) next
    z <- merge(merge(s, sector[sector$product == p, c("date", "growth")], by = "date"),
               regimes, by = "date")
    z <- z[order(z$date), ]
    f <- fit_share_model(z$share, z$growth, z$regime)
    if (is.null(f)) next
    out[[length(out) + 1L]] <- data.frame(entity = e, product = p, param = names(f$coef),
                                          est = unname(f$coef), se = unname(f$se),
                                          t = unname(f$t), n = f$n, resid_sd = f$resid_sd,
                                          mean_g = f$mean_g, stringsAsFactors = FALSE)
  }
  rbind_fill(out)
}

#' Radius that keeps the same central probability mass as Post 1's draw
#'
#' Post 1 truncated a 3-dimensional draw at Mahalanobis radius 1, i.e. the
#' central `pchisq(1, 3)` (19.9\%) of the posterior predictive. Holding that
#' probability fixed gives the equivalent radius in `d` dimensions.
#' @param d Dimension.
#' @param ref_d,ref_radius Reference dimension and radius (Post 1: 3 and 1).
#' @return Radius.
#' @export
truncation_radius <- function(d, ref_d = 3, ref_radius = 1) {
  sqrt(stats::qchisq(stats::pchisq(ref_radius^2, ref_d), df = d))
}

#' Draw NGFB's share-dynamics parameters from the population posterior predictive
#'
#' Per product, each parameter is pooled across the reference banks with
#' [eb_pool()] and NGFB's vector is drawn from `N(mu, tau^2 + se_mu^2)`,
#' truncated to the same central probability mass as Post 1's composition draw.
#' @param coefs Output of [fit_share_models()].
#' @param population Entities forming the population.
#' @param seed RNG seed (canonical 20260622).
#' @return List: `params` (product x param), `pools` (hyperparameters).
#' @export
draw_ngfb_share_params <- function(coefs, population, seed = 20260622L) {
  set.seed(seed)
  cf <- coefs[coefs$entity %in% population, ]
  prods <- unique(cf$product)
  pars <- c("(Intercept)", "contraction", "hiking", "easing", "kappa")
  P <- matrix(NA_real_, length(prods), length(pars), dimnames = list(prods, pars))
  hyper <- list()
  for (p in prods) {
    pools <- list()
    for (k in pars) {
      x <- cf[cf$product == p & cf$param == k, ]
      pools[[k]] <- eb_pool(stats::setNames(x$est, x$entity), x$se)
    }
    usable <- vapply(pools, function(z) z$k >= 4L, TRUE)
    if (!any(usable)) next
    r <- truncation_radius(sum(usable))
    d <- draw_posterior_predictive(pools[usable], seed = NULL, radius = r)
    P[p, names(d)] <- d
    P[p, !usable] <- 0
    hyper[[p]] <- data.frame(product = p, param = names(pools),
                             mu = vapply(pools, function(z) z$mu, 0),
                             tau = sqrt(vapply(pools, function(z) z$tau2, 0)),
                             se_mu = vapply(pools, function(z) z$se_mu, 0),
                             k = vapply(pools, function(z) z$k, 0L),
                             ngfb = P[p, names(pools)], radius = r,
                             distance = attr(d, "distance"), stringsAsFactors = FALSE)
  }
  list(params = P, pools = do.call(rbind, hyper))
}

#' Predicted share growth (annualised \%) for given parameters and conditions
#' @param par Named parameter vector for one product.
#' @param regime Regime labels.
#' @param g_sector Sector growth (annualised \%).
#' @param mean_g Mean sector growth used when the model was centred.
#' @param drift Include the structural drift `a`.
#' @return Numeric vector of `400 * dlog s`.
#' @export
share_growth <- function(par, regime, g_sector, mean_g, drift = TRUE) {
  v <- (if (drift) par[["(Intercept)"]] else 0) +
    par[["contraction"]] * (regime == "contraction") +
    par[["hiking"]] * (regime == "hiking") +
    par[["easing"]] * (regime == "easing") +
    par[["kappa"]] * (g_sector - mean_g)
  as.numeric(v)
}

#' Attribute net share changes to pairwise flows (proportional convention)
#'
#' Net share changes do not identify who took share from whom: a gain of 2pp
#' could come from one loser or from all of them. The convention here gives
#' each gainer's gain to the losers in proportion to their losses. It is a
#' presentation device and is labelled as such in the report.
#' @param ds Named numeric vector of share changes (summing to ~0).
#' @return Matrix: rows = gainers ("to"), columns = losers ("from").
#' @export
share_flow_matrix <- function(ds) {
  ds <- ds - mean(ds) * 0                 # keep as given; tiny residual is reported
  gain <- pmax(ds, 0); loss <- pmax(-ds, 0)
  M <- outer(gain, if (sum(loss) > 0) loss / sum(loss) else loss)
  dimnames(M) <- list(to = names(ds), from = names(ds))
  M
}
