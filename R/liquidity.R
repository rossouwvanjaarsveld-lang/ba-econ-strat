# =============================================================================
#  liquidity.R -- LCR and NSFR proxies built from BA900's deposit matrix
#
#  BA900 Tables 1-2 cross the depositor (rows) with the contractual bucket
#  (columns: cheque, savings, <= 1 day, 1 day - 1 month, 1 - 6 months,
#  > 6 months). Basel III run-off rates (LCR) and available-stable-funding
#  factors (NSFR) depend on BOTH, so each behavioural deposit line carries the
#  balance-weighted factor of the depositors that hold it: a bank funded by
#  fund managers' call money gets a very different LCR from one funded by
#  household savings, even with the same behavioural mix.
#
#  The proxies cannot see everything the regulatory ratios do (the 30-day
#  slice of term deposits, operational-deposit status, committed facilities,
#  derivatives, haircuts). One scalar per ratio is therefore solved so that
#  the SECTOR proxy equals the ratio the Prudential Authority publishes for the
#  calibration month, and the same scalars are applied to NGFB. NGFB's
#  liquidity is then "the sector's ratio, adjusted for how NGFB's balance
#  sheet differs from the sector's" -- which is the question the optimiser
#  asks when a move uses up HQLA or swaps wholesale funding for deposits.
# =============================================================================

#' BA900 depositor classes (leaf rows of the rand deposit table)
#'
#' `retail` = households, unincorporated businesses and NPISH (Basel retail
#' and small-business treatment); `corporate_public` = non-financial
#' corporates and the government sector; `financial` = banks, other monetary
#' institutions and financial corporates (insurers, pension funds, the PIC,
#' unit trusts, fund managers).
#' @return Data frame `item, class, desc` (item numbers as integer keys).
#' @export
ba900_depositor_classes <- function() {
  data.frame(
    item = c("4", "5", "8", "9", "10", "11", "12", "14", "15", "16", "17", "18", "19",
             "21", "22", "23", "24", "25", "26", "27", "28"),
    class = c("financial", "financial", "corporate_public", "corporate_public",
              "corporate_public", "corporate_public", "financial", "corporate_public",
              "financial", "financial", "corporate_public", "financial", "financial",
              "financial", "financial", "financial", "financial", "corporate_public",
              "retail", "retail", "retail"),
    desc = c("ncd", "other deposits", "tax and loan", "other", "provincial", "social security",
             "other monetary", "local government", "public financial", "public investment",
             "public non.?financial", "insurers", "pension", "money.?market", "other unit trusts",
             "fund managers", "other", "private non.?financial", "unincorporated",
             "^households", "non.?profit"),
    stringsAsFactors = FALSE)
}

#' Depositor-by-bucket deposit matrix from BA900 long data
#'
#' @param long Combined BA900 long frame.
#' @param keys Institution keys to pool (see [institution_keys()]); "TOTAL"
#'   is the sector.
#' @param patterns Institution patterns.
#' @return Long data frame `date, class, col, value` (R'000, quarter ends),
#'   summed over `keys`.
#' @export
ba900_deposit_matrix <- function(long, keys = "TOTAL", patterns = default_ba900_institutions()) {
  cl <- ba900_depositor_classes()
  d <- long[.colkey(as.character(long$table_no)) %in% c("1", "2"), ]
  d$item <- .colkey(as.character(d$item_no)); d$col <- .colkey(as.character(d$col_no))
  d <- d[d$item %in% cl$item & d$col %in% as.character(1:6), ]
  d$month <- month_start(d$period)
  d <- d[is_quarter_end_month(d$month), ]
  d$date <- quarter_end(d$month)
  d$inst_id <- as.character(d$inst_id); d$inst_name <- as.character(d$inst_name)
  k <- institution_keys(d, patterns)
  d <- merge(d, k[k$key %in% keys, ], by = "inst_id")
  d$class <- cl$class[match(d$item, cl$item)]
  d$value[is.na(d$value)] <- 0
  out <- stats::aggregate(list(value = d$value), list(date = d$date, class = d$class, col = d$col), sum)
  out[order(out$date, out$class, as.integer(out$col)), ]
}

#' Basel III liquidity factors by depositor class and BA900 bucket
#'
#' Outflow (LCR run-off) and ASF (NSFR) factors from the Basel III standards
#' (BCBS 238, 2013; BCBS 295, 2014) as adopted in the South African
#' regulations, simplified to what BA900 identifies:
#' retail deposits are treated as less stable (10\% run-off, 90\% ASF);
#' non-retail cheque accounts as operational (25\% / 50\%); other non-retail
#' demand and short-dated deposits as non-operational (corporate and public
#' 40\% / 50\%, financial 100\% / 0\%). Term buckets run off only on the share
#' assumed to mature inside 30 days (`m30`). Long-dated non-retail funding
#' (> 6 months) is assumed to be half beyond one year (ASF 75\%).
#' @param m30 Share of the 1-6 month and > 6 month buckets maturing inside
#'   30 days.
#' @return Data frame `class, col, outflow, asf`.
#' @export
basel_liquidity_factors <- function(m30 = c(`5` = 0.30, `6` = 0.05)) {
  f <- expand.grid(col = as.character(1:6), class = c("retail", "corporate_public", "financial"),
                   stringsAsFactors = FALSE)
  base_out <- c(retail = 0.10, corporate_public = 0.40, financial = 1.00)
  f$outflow <- base_out[f$class]
  f$outflow[f$col == "1" & f$class != "retail"] <- 0.25            # operational
  mat <- c(`1` = 1, `2` = 1, `3` = 1, `4` = 1, m30)
  f$outflow <- f$outflow * mat[f$col]
  f$asf <- c(retail = 0.90, corporate_public = 0.50, financial = 0.00)[f$class]
  f$asf[f$col == "1" & f$class != "retail"] <- 0.50                # operational
  f$asf[f$col == "6"] <- c(retail = 0.95, corporate_public = 0.75, financial = 0.75)[f$class[f$col == "6"]]
  f[, c("class", "col", "outflow", "asf")]
}

#' Behavioural deposit lines' liquidity factors implied by a deposit matrix
#'
#' Each behavioural line (Post 1 map: operational = cheque + <= 1 day,
#' savings, notice = 1 day - 1 month, term = 1 - 6 months + > 6 months)
#' takes the balance-weighted factor of the depositors and buckets in it.
#' @param dm Deposit matrix ([ba900_deposit_matrix()]) for ONE entity; if it
#'   carries several dates, `date` selects one.
#' @param date Date to use (default: the last in `dm`).
#' @param factors Basel factors ([basel_liquidity_factors()]).
#' @return Data frame `line, runoff, asf, balance, retail_share,
#'   financial_share`.
#' @export
deposit_liquidity_factors <- function(dm, date = NULL, factors = basel_liquidity_factors()) {
  if (is.null(date)) date <- max(dm$date)
  x <- dm[dm$date == as.Date(date), ]
  guard(nrow(x) > 0, "no deposit matrix rows at %s", format(as.Date(date)))
  x <- merge(x, factors, by = c("class", "col"))
  cat_map <- ba900_behavioural_map()$col_category
  names(cat_map) <- .colkey(names(cat_map))
  x$line <- paste0("beh_", cat_map[x$col])
  do.call(rbind, lapply(split(x, x$line), function(z) {
    v <- sum(z$value)
    data.frame(line = z$line[1], runoff = sum(z$value * z$outflow) / v,
               asf = sum(z$value * z$asf) / v, balance = v,
               retail_share = sum(z$value[z$class == "retail"]) / v,
               financial_share = sum(z$value[z$class == "financial"]) / v,
               stringsAsFactors = FALSE)
  }))
}

#' LCR and NSFR proxies for a balance sheet
#'
#' LCR = HQLA / (outflows - min(inflows, 75\% of outflows)) with HQLA the sum
#' of `hqla`-weighted balances (a negative weight on a secured-funding line
#' removes the collateral it encumbers); outflows and inflows are `runoff`-
#' weighted liability and asset balances. NSFR = sum(ASF) / sum(RSF).
#' @param balance Numeric balances, aligned with `bs`.
#' @param bs Balance-sheet definition (see [bs_columns()]).
#' @return Named numeric: `lcr`, `nsfr`, `hqla`, `outflows`, `inflows`, `asf`,
#'   `rsf`.
#' @export
liquidity_ratios <- function(balance, bs) {
  is_a <- bs$side == "asset"; is_l <- bs$side %in% c("deposit", "funding", "liability")
  is_e <- bs$side == "equity"
  hq <- max(sum(balance * bs$hqla), 0)
  outf <- sum(balance[is_l] * bs$runoff[is_l])
  inf <- min(sum(balance[is_a] * bs$runoff[is_a]), 0.75 * outf)
  asf <- sum(balance[is_l | is_e] * bs$asf[is_l | is_e]); rsf <- sum(balance[is_a] * bs$rsf[is_a])
  c(lcr = hq / max(outf - inf, 1e-9), nsfr = asf / max(rsf, 1e-9), hqla = hq,
    outflows = outf, inflows = inf, asf = asf, rsf = rsf)
}

#' Entity-specific line characteristics: deposit factors and liquidity scalars
#'
#' @param chars Line characteristics (after credit calibration).
#' @param lf Deposit factors ([deposit_liquidity_factors()]).
#' @param k_lcr Scalar on liability run-off rates (from [calibrate_liquidity()]).
#' @param k_nsfr Scalar on required stable funding.
#' @return `chars` with deposit `runoff`/`asf` replaced and the scalars applied.
#' @export
liquidity_chars <- function(chars, lf, k_lcr = 1, k_nsfr = 1) {
  i <- match(lf$line, chars$line)
  chars$runoff[i] <- lf$runoff; chars$asf[i] <- lf$asf
  liab <- chars$line %in% c(lf$line, "repo_funding", "wholesale_other", "other_liabilities")
  chars$runoff[liab] <- chars$runoff[liab] * k_lcr
  chars$rsf <- chars$rsf * k_nsfr
  attr(chars, "liquidity") <- list(k_lcr = k_lcr, k_nsfr = k_nsfr, deposit_factors = lf)
  chars
}

#' Solve the LCR and NSFR scalars on the sector
#'
#' @param bal Sector balance sheet (`line, side, balance`) at the calibration
#'   date.
#' @param chars Line characteristics with the sector's deposit factors
#'   (scalars not yet applied).
#' @param lcr_target,nsfr_target Published sector ratios (decimals).
#' @return List `k_lcr`, `k_nsfr`, `raw` (unscaled proxies), `check`
#'   (proxies after scaling).
#' @export
calibrate_liquidity <- function(bal, chars, lcr_target, nsfr_target) {
  b <- merge(bal, chars, by = "line", all.x = TRUE, sort = FALSE)
  for (v in c("runoff", "hqla", "asf", "rsf")) b[[v]][is.na(b[[v]])] <- 0
  raw <- liquidity_ratios(b$balance, b)
  is_a <- b$side == "asset"
  infl <- sum(b$balance[is_a] * b$runoff[is_a])
  k_lcr <- (raw[["hqla"]] / lcr_target + infl) / raw[["outflows"]]
  if (infl > 0.75 * k_lcr * raw[["outflows"]])                       # inflow cap binds
    k_lcr <- raw[["hqla"]] / (0.25 * lcr_target * raw[["outflows"]])
  k_nsfr <- raw[["asf"]] / (nsfr_target * raw[["rsf"]])
  b2 <- b
  liab <- b2$side %in% c("deposit", "funding", "liability")
  b2$runoff[liab] <- b2$runoff[liab] * k_lcr; b2$rsf <- b2$rsf * k_nsfr
  list(k_lcr = k_lcr, k_nsfr = k_nsfr, raw = raw, check = liquidity_ratios(b2$balance, b2))
}
