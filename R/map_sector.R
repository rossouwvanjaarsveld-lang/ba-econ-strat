# =============================================================================
#  map_sector.R -- sector ("Total Banks") returns: BA100 balance sheet,
#                  BA120 income statement, BA200 credit risk, BA700 capital
#
#  The PA returns are published on the IFData service for the aggregate only
#  (GetInstitutions lists a single institution, "Total Banks"); they are the
#  sector P&L that BA900 lacks. BA100 was revised in 2012 and 2013 (105 -> 122
#  -> 132 rows), so concepts are located by DESCRIPTION within the right
#  table, never by item number or row position. BA120 columns: 1-3 are the
#  month (banking, trading, total), 4-6 year-to-date. Year-to-date mixes banks
#  with different financial year-ends, so quarterly flows are built from the
#  monthly total column.
# =============================================================================

#' Concept dictionary for BA100 (sector balance sheet)
#' @return Data frame `concept, table_rx, desc_rx, pick`.
#' @export
ba100_concepts <- function() {
  A <- "assets"; L <- "liabilit"; E <- "equity"
  r <- function(concept, table_rx, desc_rx, pick = "first", item = NA_character_)
    data.frame(concept = concept, table_rx = table_rx, desc_rx = desc_rx, pick = pick, item = item,
               stringsAsFactors = FALSE)
  rbind(
    r("cash_cb", A, "^cash and balances with (the )?central bank"),
    r("st_securities", A, "^short.term negotiable securities"),
    r("homeloans", A, "^home ?loans"),
    r("commercial_mortgages", A, "^commercial mortgages"),
    r("credit_cards", A, "^credit cards"),
    r("lease_instalment", A, "^lease and instalment"),
    r("overdrafts", A, "^overdrafts"),
    r("pref_shares", A, "^redeemable preference"),
    r("trade_bills", A, "^trade, other bills"),
    r("term_loans", A, "^term loans"),
    r("factoring", A, "^factoring"),
    r("resale", A, "resale agreements"),
    r("other_loans", A, "^other loans to customers"),
    r("loan_impairments", A, "^less: credit impairments", pick = "max"),
    r("investment_securities", A, "^investment and trading securities"),
    r("gov_securities", A, "^government and government.guaranteed securities", pick = "max"),
    r("other_dated_securities", A, "^other dated securities", pick = "max"),
    r("derivative_assets", A, "^derivative financial instruments"),
    r("pledged_assets", A, "^pledged assets"),
    r("total_assets", A, "^total assets"),
    r("dep_current", L, "^current accounts"),
    r("dep_savings", L, "^savings deposits"),
    r("dep_call", L, "^call deposits"),
    r("dep_fixed_notice", L, "^fixed and notice deposits"),
    r("dep_ncd", L, "^negotiable certificates of deposit"),
    r("dep_other", L, "^other deposits and loan accounts"),
    r("dep_repo", L, "^deposits received under repurchase"),
    r("term_debt", L, "^term debt instruments"),
    r("total_liabilities", L, "^total liabilities"),
    r("equity_ordinary", E, "^total equity attributable"),
    r("equity_total", E, "^total equity( \\(|$)"),
    r("avg_ib_loans", "", "daily average balance .*loans and advances"),
    r("avg_ib_deposits", "", "interest bearing deposits: daily average")
  )
}

#' Concept dictionary for BA120 (sector income statement)
#' @return Data frame `concept, table_rx, desc_rx, pick`.
#' @export
ba120_concepts <- function() {
  r <- function(concept, desc_rx, pick = "first", item = NA_character_)
    data.frame(concept = concept, table_rx = "", desc_rx = desc_rx, pick = pick, item = item,
               stringsAsFactors = FALSE)
  rbind(
    r("ii_total", "^interest and similar income"),
    r("ii_st_securities", "^short.term negotiable securities"),
    r("ii_loans", "^loans and advances to customers"),
    r("ii_homeloans", "^home ?loans"),
    r("ii_commercial_mortgages", "^commercial mortgages"),
    r("ii_credit_cards", "^credit cards"),
    r("ii_lease_instalment", "^lease and instalment"),
    r("ii_overdrafts", "^overdrafts"),
    r("ii_pref_shares", "^redeemable preference"),
    r("ii_trade_bills", "^trade, other bills"),
    r("ii_term_loans", "^term loans"),
    r("ii_factoring", "^factoring"),
    r("ii_gov_securities", "^government and other dated securities"),
    r("ii_trading_alloc", "^less: interest income on trading"),
    r("ie_total", "^interest expense and similar charges"),
    r("ie_deposits", "^deposits, current accounts and other"),
    r("ie_current", "^current accounts"),
    r("ie_savings", "^savings deposits"),
    r("ie_fixed_notice", "^fixed and notice deposits"),
    r("ie_term_other", "^other$", item = "22"),          # term and other deposits: other (call)
    r("ie_ncd", "^negotiable certificates of deposit"),
    r("ie_other_deposits", "^other deposits and loans"),
    r("ie_other_liabilities", "^other liabilities"),
    r("ie_term_debt", "^term debt instruments"),
    r("ie_trading_alloc", "^less: interest expense on trading"),
    r("nii", "^net interest income"),
    r("fees_net", "^net fee and commission income"),
    r("trading", "^net trading income"),
    r("nir", "^non interest revenue"),
    r("gross_income", "^gross operating income"),
    r("credit_losses", "^credit losses"),
    r("opex", "^operating expenses"),
    r("staff", "^staff"),
    r("indirect_tax", "^indirect taxation"),
    r("operating_profit", "^operating profit"),
    r("non_trading", "^non.trading and capital items"),
    r("associates", "^share of profit"),
    r("pbt", "^profit / \\(loss\\) before income tax|^profit before"),
    r("tax", "^direct taxation"),
    r("profit", "^profit / \\(loss\\) for the year|^profit for the")
  )
}

#' Extract concepts from a sector form, one value per period
#'
#' @param long Combined long frame of one form ([combine_form_cells()]).
#' @param concepts Concept dictionary.
#' @param col Column to read (e.g. `"3"`): the first matching column number
#'   after stripping leading zeros.
#' @return Data frame `period, concept, value, item_no, item_desc`.
#' @export
extract_concepts <- function(long, concepts, col = "3") {
  d <- long
  d$col_key <- .colkey(as.character(d$col_no))
  d <- d[d$col_key == col, ]
  d$item_desc <- as.character(d$item_desc); d$table_desc <- tolower(as.character(d$table_desc))
  d$desc_l <- tolower(gsub("\\s+", " ", trimws(d$item_desc)))
  d$item_int <- suppressWarnings(as.integer(as.character(d$item_no)))
  out <- list()
  ups <- sort(unique(d$period))
  for (k in seq_along(ups)) {
    p <- ups[k]                                  # keeps the Date class
    x <- d[d$period == p, ]
    x <- x[order(x$item_int), ]
    last_item <- -Inf
    for (i in seq_len(nrow(concepts))) {
      cc <- concepts[i, ]
      cand <- x[grepl(cc$desc_rx, x$desc_l) &
                  (cc$table_rx == "" | grepl(cc$table_rx, x$table_desc)), ]
      if (!is.null(cc$item) && !is.na(cc$item) && nzchar(cc$item))
        cand <- cand[cand$item_int == as.integer(cc$item), ]
      if (!nrow(cand)) next
      pick <- if (cc$pick == "max") cand[which.max(abs(cand$value)), ] else cand[1, ]
      out[[length(out) + 1L]] <- data.frame(period = p, concept = cc$concept,
                                            value = pick$value, item_no = as.character(pick$item_no),
                                            item_desc = pick$item_desc, stringsAsFactors = FALSE)
    }
  }
  rbind_fill(out)
}

#' Quarterly sector P&L and balance-sheet panel from BA100 and BA120
#'
#' Flows (BA120) are summed over the three months of each quarter (only
#' complete quarters are kept); stocks (BA100) are quarter-end values and
#' quarterly averages use the mean of the opening and closing stock.
#' Rates and ratios are annualised percentages.
#' @param ba100 Long BA100 frame.
#' @param ba120 Long BA120 frame.
#' @return Data frame, one row per quarter (quarter-end `date`).
#' @export
sector_pnl_panel <- function(ba100, ba120) {
  s <- extract_concepts(ba100, ba100_concepts(), col = "3")
  f <- extract_concepts(ba120, ba120_concepts(), col = "3")
  s$date <- quarter_end(month_start(s$period))
  s <- s[is_quarter_end_month(month_start(s$period)), ]
  sw <- stats::reshape(s[, c("date", "concept", "value")], idvar = "date",
                       timevar = "concept", direction = "wide")
  names(sw) <- sub("^value\\.", "", names(sw))
  f$q <- quarter_end(month_start(f$period))
  cnt <- stats::aggregate(list(n = f$period), list(q = f$q, concept = f$concept),
                          function(z) length(unique(z)))
  fq <- stats::aggregate(list(value = f$value), list(q = f$q, concept = f$concept), sum, na.rm = TRUE)
  fq <- merge(fq, cnt, by = c("q", "concept"))
  fq <- fq[fq$n == 3L, ]
  fw <- stats::reshape(fq[, c("q", "concept", "value")], idvar = "q", timevar = "concept",
                       direction = "wide")
  names(fw) <- sub("^value\\.", "", names(fw)); names(fw)[1] <- "date"
  m <- merge(sw, fw, by = "date", all = TRUE)
  m <- m[order(m$date), ]
  avg <- function(v) if (v %in% names(m)) (m[[v]] + lag_vec(m[[v]], 1L)) / 2 else NA_real_
  rate <- function(flow, stock) if (all(c(flow) %in% names(m))) 400 * m[[flow]] / stock else NA_real_
  gl <- c("homeloans", "commercial_mortgages", "credit_cards", "lease_instalment", "overdrafts",
          "pref_shares", "trade_bills", "term_loans", "factoring", "resale", "other_loans")
  m$gross_loans <- rowSums(m[, intersect(gl, names(m)), drop = FALSE], na.rm = TRUE)
  m$deposits <- rowSums(m[, intersect(c("dep_current", "dep_savings", "dep_call", "dep_fixed_notice",
                                        "dep_ncd", "dep_other", "dep_repo"), names(m)), drop = FALSE],
                        na.rm = TRUE)
  # interest-earning assets as in the Post 3 bridge constants: cash and
  # central-bank balances, short-term securities, net loans, dated securities
  # and pledged assets
  m$iea <- rowSums(m[, intersect(c("cash_cb", "st_securities", "gov_securities",
                                   "other_dated_securities", "pledged_assets"), names(m)),
                     drop = FALSE], na.rm = TRUE) +
    m$gross_loans - m$loan_impairments
  m$avg_loans <- (m$gross_loans + lag_vec(m$gross_loans, 1L)) / 2
  m$avg_iea <- (m$iea + lag_vec(m$iea, 1L)) / 2
  m$avg_assets <- avg("total_assets")
  m$avg_equity <- avg("equity_ordinary")
  m$avg_deposits <- (m$deposits + lag_vec(m$deposits, 1L)) / 2
  ## implied annualised yields and costs (percent)
  m$y_homeloans  <- rate("ii_homeloans", avg("homeloans"))
  m$y_commercial <- rate("ii_commercial_mortgages", avg("commercial_mortgages"))
  m$y_cards      <- rate("ii_credit_cards", avg("credit_cards"))
  m$y_vaf        <- rate("ii_lease_instalment", avg("lease_instalment"))
  m$y_overdrafts <- rate("ii_overdrafts", avg("overdrafts"))
  m$y_term_loans <- rate("ii_term_loans", avg("term_loans"))
  m$y_loans      <- rate("ii_loans", m$avg_loans)
  m$y_st_sec     <- rate("ii_st_securities", avg("st_securities"))
  m$y_gov_sec    <- if ("other_dated_securities" %in% names(m))
    400 * m$ii_gov_securities / (avg("gov_securities") + avg("other_dated_securities")) else NA_real_
  m$c_current    <- rate("ie_current", avg("dep_current"))
  m$c_savings    <- rate("ie_savings", avg("dep_savings"))
  m$c_fixed_notice <- rate("ie_fixed_notice", avg("dep_fixed_notice"))
  m$c_call       <- rate("ie_term_other", avg("dep_call"))
  m$c_ncd        <- rate("ie_ncd", avg("dep_ncd"))
  m$c_deposits   <- rate("ie_deposits", m$avg_deposits)
  m$c_term_debt  <- rate("ie_term_debt", avg("term_debt"))
  m$nim          <- 400 * m$nii / m$avg_iea
  m$clr          <- 400 * m$credit_losses / m$avg_loans
  m$nir_assets   <- 400 * m$nir / m$avg_assets
  m$opex_assets  <- 400 * (m$opex + m$indirect_tax) / m$avg_assets
  m$cost_to_income <- 100 * m$opex / (m$nii + m$nir)
  m$roe          <- 400 * m$profit / m$avg_equity
  m$roa          <- 400 * m$profit / m$avg_assets
  m$tax_rate     <- 100 * m$tax / m$pbt
  m$leverage_iea <- m$iea / m$equity_ordinary
  m
}
