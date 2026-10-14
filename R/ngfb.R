# =============================================================================
#  ngfb.R -- No Gut Feelings Bank: canonical constants, full-balance-sheet
#            extension, and embedding in the sector
#
#  Canonical artefacts inherited from the NGFB deposit series (Posts 1-4):
#    * seed 20260622
#    * deposit composition (deposit-beta-1-4/data/ngfb_composition_canonical.csv,
#      copied to inst/artefacts/): operational 41.7%, notice 10.8%,
#      savings 7.5%, term 39.9% -- as published in Post 1 and used in Post 4.
#      (franchise-duration-2-4/post1_betas.rds carries an older draw,
#      23.9/8.9/23.4/43.7, which Post 2 loaded; it is NOT canonical.)
#    * deposit pricing d = alpha + beta * repo, betas 0.463 / 0.625 /
#      0.463 (savings priced on operational) / 0.245
#    * BA100 bridge constants (post3_bridge_constants.rds, 2022-07..2025-06):
#        IEA / ordinary equity 12.3605, core deposits / IEA 0.6835,
#        all deposits / IEA 0.8929
#    * structural hedge: NMD beta 0.489, optimal hedge ratio ~ 49.6%
#
#  The extension follows the same rule as the deposit block: the asset
#  composition is a Mahalanobis-truncated logistic-normal draw from the
#  six-bank population (same seed, same central-probability truncation),
#  so NGFB is a typical large South African bank, not any one of them.
#
#  Embedding (the Post 1 convention): NGFB -- a bank the size of the average
#  top-six bank -- is ADDED to the sector. Reported banks' books are untouched;
#  market shares are measured against the augmented industry (reported sector
#  plus NGFB). NGFB's share dynamics are estimated relative to the reported
#  sector, which is also what the Stage 1 macro models describe.
# =============================================================================

#' Canonical NGFB constants from the deposit series
#' @return Named list.
#' @export
ngfb_canonical <- function() {
  list(
    seed = 20260622L,
    k_anon = 4L,
    deposit_mix = c(beh_operational = 0.41735290261287783, beh_notice = 0.10792005827991087,
                    beh_savings = 0.07536103240143384, beh_term = 0.3993660067057775),
    deposit_beta = c(beh_operational = 0.4631352099, beh_notice = 0.6251713161,
                     beh_savings = 0.4631352099, beh_term = 0.2448658583),
    deposit_alpha = c(beh_operational = 0.02578837334, beh_notice = 0.02930093616,
                      beh_savings = 0.02578837334, beh_term = 0.06887155457),
    nmd_beta = 0.4887138287,
    hedge_ratio = 0.4955343027,
    leverage_iea_equity = 12.36052867,
    core_dep_iea = 0.6834692425,
    all_dep_iea = 0.8929332954,
    provenance = c(
      deposit = "deposit-beta-1-4/data/ngfb_composition_canonical.csv (Post 1 canonical draw, seed 20260622)",
      betas = "franchise-duration-2-4/post1_betas.rds (Post 1 betas; its weights are a stale draw)",
      bridge  = "caterpillar-3-4/artefacts/post3_bridge_constants.rds (BA100 total banks, 2022-07..2025-06)",
      hedge   = "caterpillar-3-4/artefacts/post3_qp.rds (optimal notional at phi = 0.794)"))
}

#' Asset lines of NGFB's balance sheet
#' @return Character vector.
#' @export
ngfb_asset_lines <- function() unlist(ngfb_business_lines(), use.names = FALSE)

#' Business lines used for NGFB's composition draw
#'
#' Six banks cannot identify an 11 x 11 covariance of product shares, so the
#' draw is made over business lines (5 dimensions in additive log-ratio
#' space) and products are split within each line at the reference banks'
#' average within-line mix.
#' @return Named list: business line -> asset lines.
#' @export
ngfb_business_lines <- function() {
  list(retail_secured = c("mortgages_res", "vaf"),
       retail_unsecured = c("cards", "personal_other"),
       corporate = c("corporate_loans", "overdrafts", "mortgages_com"),
       treasury = c("interbank", "reverse_repo", "other_securities"),
       hqla = "hqla",
       other = "other_assets")
}

#' Draw NGFB's asset composition from the six-bank population
#'
#' @param bp Bank panel ([bank_panel_quarterly()]).
#' @param population Keys of the reference banks.
#' @param date0 Jump-off quarter; compositions are averaged over the
#'   `window` quarters ending there (smooths quarter-end noise).
#' @param window Averaging window in quarters.
#' @param seed RNG seed.
#' @param lines Business-line grouping ([ngfb_business_lines()]).
#' @return List: `draw` (product shares), `line_draw` (business-line shares),
#'   `line_mean`, `within` (within-line splits), `distance`, `radius`,
#'   `bank_shares` (internal; never published).
#' @export
ngfb_draw_assets <- function(bp, population = ngfb_population_keys(), date0,
                             window = 8L, seed = ngfb_canonical()$seed,
                             lines = ngfb_business_lines()) {
  al <- unlist(lines, use.names = FALSE)
  dts <- sort(unique(bp$date[bp$date <= as.Date(date0)]))
  dts <- utils::tail(dts, window)
  x <- bp[bp$key %in% population & bp$date %in% dts, c("key", al)]
  x[al] <- lapply(x[al], function(v) pmax(v, 0))
  m <- stats::aggregate(x[al], list(key = x$key), mean)
  S <- as.matrix(m[al]); rownames(S) <- m$key
  S <- S / rowSums(S)
  C <- sapply(lines, function(g) rowSums(S[, g, drop = FALSE]))
  rad <- truncation_radius(ncol(C) - 1L)
  d <- draw_logistic_normal(C, seed = seed, radius = rad)
  within <- lapply(lines, function(g) {
    w <- S[, g, drop = FALSE] / rowSums(S[, g, drop = FALSE])
    w[!is.finite(w)] <- 0
    colMeans(w)
  })
  draw <- unlist(lapply(names(lines), function(k) d$draw[[k]] * within[[k]]))
  names(draw) <- unlist(lapply(within, names))
  list(draw = draw[al], line_draw = d$draw, line_mean = d$mean_share, within = within,
       distance = d$distance, radius = rad, bank_shares = S)
}

#' Build NGFB's jump-off balance sheet
#'
#' @param bp Bank panel.
#' @param date0 Jump-off quarter.
#' @param asset_mix Named asset composition (from [ngfb_draw_assets()]).
#' @param population Reference-bank keys (size = their mean total assets).
#' @param canon Canonical constants ([ngfb_canonical()]).
#' @return Data frame `line, side, balance` in R'000 (assets, deposits by
#'   behavioural category, other funding, equity), balanced.
#' @export
ngfb_balance_sheet <- function(bp, date0, asset_mix, population = ngfb_population_keys(),
                               canon = ngfb_canonical()) {
  x <- bp[bp$key %in% population & bp$date == as.Date(date0), ]
  size <- mean(x$total_assets)
  al <- names(asset_mix)
  a <- asset_mix * size
  iea <- sum(a[setdiff(al, "other_assets")])
  equity <- iea / canon$leverage_iea_equity
  deposits <- canon$all_dep_iea * iea
  dep <- canon$deposit_mix * deposits
  # remaining funding split like the population's non-deposit funding
  rest <- size - equity - deposits
  fl <- c("repo_funding", "wholesale_other", "other_liabilities")
  f <- colSums(x[, fl, drop = FALSE]); f <- pmax(f, 0); f <- f / sum(f)
  fund <- rest * f
  out <- rbind(
    data.frame(line = al, side = "asset", balance = as.numeric(a)),
    data.frame(line = names(dep), side = "deposit", balance = as.numeric(dep)),
    data.frame(line = fl, side = "funding", balance = as.numeric(fund)),
    data.frame(line = "equity", side = "equity", balance = equity))
  attr(out, "size") <- size
  attr(out, "date0") <- as.Date(date0)
  guard(abs(sum(out$balance[out$side == "asset"]) -
              sum(out$balance[out$side != "asset"])) < 1e-6 * size,
        "NGFB balance sheet does not balance")
  guard(all(fund >= 0), "NGFB non-deposit funding is negative: deposits + equity exceed assets")
  out
}

#' Embed NGFB in the sector: back-cast its book and add it to the industry
#'
#' NGFB's share of the reported sector in each product is anchored at the
#' jump-off and back-cast through history with its drawn share-dynamics
#' parameters and the realised regimes and sector growth (no noise term), so
#' NGFB "lives through" 2008-2026 the way a bank with its parameters would.
#' Its book is then ADDED to the industry (Post 1 convention).
#' @param gp Group panel ([group_panel()]) -- must contain a "Sector" entity.
#' @param ngfb_bal Data frame `line, balance` of NGFB's jump-off book in the
#'   products to embed, with attribute `date0`.
#' @param params NGFB share parameters (product x param matrix).
#' @param mean_g Named vector of sector mean growth per product (centring).
#' @param regimes Data frame `date, regime`.
#' @param products Products to embed (must be columns of `gp`).
#' @param drift Include the structural share drift.
#' @return Group panel with an "NGFB" entity; "Sector" becomes the augmented
#'   industry (reported + NGFB) and "Sector (reported)" keeps the original.
#' @export
ngfb_embed <- function(gp, ngfb_bal, params, mean_g, regimes, products, drift = FALSE) {
  date0 <- attr(ngfb_bal, "date0")
  sec <- gp[gp$entity == "Sector", ]; sec <- sec[order(sec$date), ]
  dates <- sec$date
  reg <- regimes$regime[match(dates, regimes$date)]
  ng <- data.frame(date = dates)
  zero <- stats::setNames(rep(0, 5), c("(Intercept)", "contraction", "hiking", "easing", "kappa"))
  for (p in products) {
    b0 <- ngfb_bal$balance[ngfb_bal$line == p]
    if (!length(b0) || !is.finite(b0)) next
    s0 <- b0 / sec[[p]][dates == date0]
    g <- growth_qq(sec[[p]])
    par <- if (p %in% rownames(params)) params[p, ] else zero
    ds <- share_growth(par, reg, g, mean_g[[p]] %||% mean(g, na.rm = TRUE), drift = drift) / 400
    ds[!is.finite(ds)] <- 0
    ls <- numeric(length(dates)); i0 <- which(dates == date0)
    ls[i0] <- log(s0)
    if (i0 > 1L) for (t in (i0 - 1L):1L) ls[t] <- ls[t + 1L] - ds[t + 1L]
    if (i0 < length(dates)) for (t in (i0 + 1L):length(dates)) ls[t] <- ls[t - 1L] + ds[t]
    ng[[p]] <- exp(ls) * sec[[p]]
  }
  vals <- setdiff(names(gp), c("date", "entity"))
  add <- sec; add$entity <- "NGFB"
  for (v in vals) add[[v]] <- if (v %in% names(ng)) ng[[v]][match(add$date, ng$date)] else NA_real_
  aug <- sec; aug$entity <- "Sector"
  for (v in vals) aug[[v]] <- sec[[v]] + ifelse(is.na(add[[v]]), 0, add[[v]])
  rep <- sec; rep$entity <- "Sector (reported)"
  out <- rbind(gp[gp$entity != "Sector", ], add, aug, rep)
  out[order(out$entity, out$date), ]
}
