# =============================================================================
#  treasury.R -- the liquidity book as a source of return
#
#  Three ways to make the HQLA buffer earn more, each priced as a position
#  against the Treasury bills it replaces:
#   * duration: 10-year government bonds bought for HQLA, unhedged. Earns the
#     curve's slope over bills; the price moves with the 10-year yield.
#   * asset swap: 5-year government bonds bought for HQLA with a pay-fixed
#     5-year swap against them. Earns the bond-swap spread (the fiscal premium
#     South African bonds pay over swaps) plus the bills' discount to the
#     floating benchmark; the interest-rate risk is swapped away and what is
#     left is the spread: the price moves when the bond-swap spread does.
#   * size: Treasury bills released from the buffer to repay the dearest
#     wholesale funding. Saves the wholesale spread over bills; costs
#     liquidity coverage (handled by the engine's `buffer` argument).
#  Bonds bought for HQLA count as Level 1 assets like bills, so buying them
#  does not change the LCR; but Level 1 assets count at market value, so a
#  sell-off lowers the HQLA stock by the books' mark-to-market (for the asset
#  swap only its spread part: the swap's gain on a rate rise comes back as
#  cash variation margin). The books are held at fair value through OCI:
#  the unrealised mark-to-market sits in the OCI reserve and hence CET1, and
#  a sale recycles it to profit or loss (IFRS 9), so a rule that trades the
#  book books its realised gains and losses in earnings.
# =============================================================================

#' Modified duration of a par bond with semi-annual coupons
#' @param m Years to maturity.
#' @param y Yield (and coupon), percent.
#' @return Modified duration in years (0 at or after maturity).
#' @export
bond_md <- function(m, y) {
  if (m <= 0) return(0)
  cf <- c(rep(y / 2, max(round(2 * m), 1) - 1), 100 + y / 2)
  t <- seq_along(cf) / 2; d <- 1 / (1 + y / 200)^(2 * t)
  sum(t * cf * d) / sum(cf * d) / (1 + y / 200)
}

#' The 5-year bond-swap spread along a scenario
#'
#' A scenario carries the 10-year government yield and the floating
#' benchmark, but no 5-year bond and no swap curve that prices the policy
#' path (its swap rates are the benchmark plus constant term spreads,
#' swaps.R). The spread is therefore modelled directly. Both curves price
#' the expected policy path, so a policy-driven move should leave their
#' difference alone; what the spread takes is a share of the rest of the
#' long bond's move (the fiscal premium, global yields):
#'   bs_t = bs_0 + kappa * [(gov10y_t - gov10y_0) - beta * (float_t - float_0)],
#' with `beta` the 10-year yield's response to the benchmark and `kappa` the
#' share of the 10-year's remaining move that the 5-year bond-swap spread
#' takes rather than the swap curve (both estimated in
#' `analysis/12_audit.R`). `beta = kappa = 1` is the spread implied by the
#' scenarios' own swap curve, which loads every change in the government
#' curve's slope onto the spread -- a policy cut of five points would widen
#' it by up to five.
#' @param rates Engine rates (`gov10y` and the floating benchmark).
#' @param jo Jump-off values: `bs` (5-year bond less 5-year swap, pp),
#'   `gov10y`, `float3m`.
#' @param beta,kappa See above.
#' @return Numeric vector (pp), one value per quarter.
#' @export
asset_swap_spread <- function(rates, jo, beta = 1, kappa = 1) {
  jo$bs + kappa * ((rates$gov10y - jo$gov10y) - beta * (float_rate(rates) - jo$float3m))
}

#' A book of government bonds, inside the HQLA buffer or funded in repo
#'
#' The book is sized as a share of the earning HQLA at the jump-off (`H0`)
#' after each quarter's trade: bought at the jump-off's prices in quarter 1
#' and at the quarter's prices after (the convention the engine uses for swap
#' tranches); cut pro rata across vintages, realising their mark-to-market.
#' By default the bonds replace Treasury bills in the liquidity buffer. With
#' `funding` and `in_hqla = FALSE` they are held outside the buffer instead,
#' encumbered in repo: their carry is over the repo funding rate, their
#' mark-to-market does not move the HQLA stock, and they add to the leverage
#' exposure and to required stable funding (the engine's `overlay`).
#'
#' @param share Share of `H0` in the book after each quarter's trade (one
#'   value per quarter).
#' @param H0 Earning HQLA at the jump-off (rand).
#' @param rates Engine rates for the path (`tbill`, `gov10y`, and the
#'   floating benchmark).
#' @param jo Jump-off market values: `gov10y` and `float3m`, and for an asset
#'   swap `bs` (5-year bond less 5-year swap, pp) and `gov5y`.
#' @param type "duration" (10-year bonds, unhedged) or "asset_swap" (5-year
#'   bonds swapped to floating).
#' @param tax Tax rate applied to the mark-to-market.
#' @param spread For "asset_swap", the bond-swap spread path (pp, one value
#'   per quarter; default [asset_swap_spread()] with `beta = kappa = 1`).
#' @param funding Optional rate path (percent, one value per quarter) the
#'   book is funded at, in place of the Treasury bills' yield.
#' @param in_hqla Whether the bonds sit in the liquidity buffer (`TRUE`) or
#'   outside it, encumbered in repo (`FALSE`).
#' @return List of quarterly vectors: `carry` (rand of NII over the bills the
#'   book replaces: the engine's `overlay` `nii`), `gains` (realised gains,
#'   negative for losses, on the bonds sold in the quarter, pre-tax: under
#'   IFRS 9 a sale of debt held at fair value through OCI recycles its
#'   accumulated OCI to profit or loss, so these go to the engine's
#'   `overlay` `gains`), `oci_unreal` (unrealised mark-to-market on the bonds
#'   still held, after tax: the OCI reserve, which counts in CET1), `oci`
#'   (cumulative after-tax mark-to-market, realised and unrealised), `mtm`
#'   (the same before tax: what the buffer's market value differs from its
#'   book value by, the engine's `overlay` `hqla_mtm`), `held` (rand),
#'   `pv01` (rand per basis point of the bond yield for "duration", of the
#'   swap rate -- zero, hedged -- for "asset_swap"), `spv01` (rand per basis
#'   point of the bond-swap spread), `spread` (the path used), `in_hqla`.
#' @export
bond_book <- function(share, H0, rates, jo, type = c("duration", "asset_swap"), tax = 0.27, spread = NULL,
                      funding = NULL, in_hqla = TRUE) {
  type <- match.arg(type)
  n <- nrow(rates); share <- rep_len(share, n)
  tb <- if (is.null(funding)) rates$tbill else rep_len(funding, n)    # what the book is carried against
  fl <- float_rate(rates)
  ## the price that moves the book: the 10-year yield, or the 5-year spread
  px <- if (type == "duration") rates$gov10y else spread %||% asset_swap_spread(rates, jo)
  if (length(px) != n) stop("bond_book: the spread path needs one value per quarter")
  px0 <- if (type == "duration") jo$gov10y else jo$bs
  ylev <- if (type == "duration") rates$gov10y else jo$gov5y + (rates$gov10y - jo$gov10y)   # yield for duration
  m0 <- if (type == "duration") 10 else 5
  B <- numeric(0); lock <- numeric(0); yv <- numeric(0); mat <- numeric(0); realised <- 0
  carry <- gains <- oci <- oci_unreal <- mtm <- held <- pv01 <- spv01 <- numeric(n)
  mdv <- function(i, rem) vapply(seq_along(B), function(k) bond_md(rem[k], yv[k]), 0)
  for (t in seq_len(n)) {
    mat <- mat - 0.25
    p_tr <- if (t == 1L) px0 else px[t]
    y_tr <- if (t == 1L) (if (type == "duration") jo$gov10y else jo$gov5y) else ylev[t]
    target <- share[t] * H0; cur <- sum(B)
    if (target > cur + 1) {
      B <- c(B, target - cur); lock <- c(lock, p_tr); yv <- c(yv, y_tr); mat <- c(mat, m0)
    } else if (target < cur - 1) {
      f <- target / cur
      gains[t] <- sum(-mdv(t, mat) * (p_tr - lock) / 100 * B * (1 - f))
      realised <- realised + gains[t]
      B <- B * f
    }
    ## carry over bills (or the repo funding): the bond's yield (duration), or
    ## the locked spread plus the floating leg (asset swap), less the bills'
    ## yield (or the funding rate)
    carry[t] <- if (type == "duration") sum(B * (lock - tb[t])) / 400 else sum(B * (lock + fl[t] - tb[t])) / 400
    md_end <- mdv(t, mat - 0.25)
    unreal <- sum(-md_end * (px[t] - lock) / 100 * B)
    oci_unreal[t] <- unreal * (1 - tax)
    mtm[t] <- unreal + realised
    oci[t] <- mtm[t] * (1 - tax)
    held[t] <- sum(B)
    spv01[t] <- 1e-4 * sum(md_end * B)
    pv01[t] <- if (type == "duration") spv01[t] else 0
  }
  list(carry = carry, gains = gains, oci = oci, oci_unreal = oci_unreal, mtm = mtm, held = held,
       pv01 = pv01, spv01 = spv01, spread = px, in_hqla = in_hqla)
}

#' The engine overlay for a set of bond books
#'
#' Sums the books' carry (NII), realised gains (profit or loss) and
#' mark-to-market quarter by quarter, in the form [run_engine()]'s `overlay`
#' takes. Only books inside the buffer move the HQLA stock at market value;
#' books held outside it, encumbered in repo, add their holding to the
#' leverage exposure and the Basel factor for encumbered Level 1 assets to
#' required stable funding (encumbered for less than six months: the same as
#' unencumbered, 5%).
#' @param dates Quarter dates of the run.
#' @param books List of [bond_book()] results (may be empty).
#' @param rsf_encumbered Required-stable-funding factor of a book held
#'   outside the buffer.
#' @return Data frame `date`, `nii`, `gains`, `hqla_mtm`, `exposure`, `rsf`.
#' @export
books_overlay <- function(dates, books, rsf_encumbered = 0.05) {
  inb <- vapply(books, function(b) !isFALSE(b$in_hqla), TRUE)
  tot <- function(v, keep = rep(TRUE, length(books)))
    Reduce(`+`, lapply(books[keep], `[[`, v), rep(0, length(dates)))
  data.frame(date = dates, nii = tot("carry"), gains = tot("gains"), hqla_mtm = tot("mtm", inb),
             exposure = tot("held", !inb), rsf = rsf_encumbered * tot("held", !inb))
}

#' Share of the HQLA line to release for a target LCR
#'
#' Releases Treasury bills from the buffer and repays the funding plug with
#' them, on the jump-off balance sheet, until the LCR proxy reaches the target.
#' @param bs Engine balance sheet ([run_engine()] format, with liquidity
#'   factors).
#' @param target Target LCR (decimal, e.g. 1.5).
#' @param line,plug HQLA line and funding plug.
#' @param scale The HQLA line's earning share (the release cannot exceed it).
#' @return Share of the line's balance to release (0 if the LCR is already at
#'   or below the target); attribute `lcr` with the LCR reached.
#' @export
buffer_release_for_lcr <- function(bs, target, line = "hqla", plug = "wholesale_other", scale = 1) {
  h <- match(line, bs$line); p <- match(plug, bs$line)
  lcr_at <- function(r) { b <- bs$balance; rel <- r * b[h]; b[h] <- b[h] - rel; b[p] <- b[p] - rel
    liquidity_ratios(b, bs)[["lcr"]] }
  r_max <- min(0.98 * scale, 0.95 * bs$balance[p] / bs$balance[h])
  if (lcr_at(0) <= target) { out <- 0; attr(out, "lcr") <- lcr_at(0); return(out) }
  if (lcr_at(r_max) > target) { out <- r_max; attr(out, "lcr") <- lcr_at(r_max); return(out) }
  r <- stats::uniroot(function(r) lcr_at(r) - target, c(0, r_max), tol = 1e-10)$root
  attr(r, "lcr") <- lcr_at(r)
  r
}
