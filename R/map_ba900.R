# =============================================================================
#  map_ba900.R -- BA900 line items -> the package's product taxonomy
#
#  BA900 ("economic return") is the only SARB return published per bank. Its
#  structure (identical item numbering 2008-2013, verified; later periods are
#  guarded by description checks):
#
#    Tables 1-2  deposits: rows = counterparty, columns 0001-0006 = cheque,
#                savings, <=1 day, 1 day-1 month, 1-6 months, >6 months;
#                0007 = total; 0008 = NCDs/PNs (memo, included in 0007)
#    Tables 3-4  other borrowed funds / other liabilities; 0004 = total
#    Table  5    equity; 0001 = total
#    Tables 6-13 assets; 0005 = total (0001 domestic, 0003 foreign, ...)
#
#  Every line is keyed on (item, column) and guarded by a regular expression on
#  the item description, so a form revision that renumbers items fails loudly
#  instead of silently shifting balances between products.
# =============================================================================

#' BA900 line-item map
#'
#' One row per (line, item) contribution. `sign` is +1 or -1.
#' @return Data frame `line, side, item, col, sign, desc`.
#' @export
ba900_item_map <- function() {
  A <- "0005"; T7 <- "0007"; T4 <- "0004"
  r <- function(line, side, item, col, desc, sign = 1)
    data.frame(line = line, side = side, item = as.character(item), col = col,
               sign = sign, desc = desc, stringsAsFactors = FALSE)
  rbind(
    ## ---- assets ---------------------------------------------------------------
    r("mortgages_res",  "asset", 155, A, "residential mortgages"),
    r("mortgages_com",  "asset", 159, A, "commercial.*mortgage"),
    r("mortgages_com",  "asset", 151, A, "farm mortgages"),
    r("vaf",            "asset", 139, A, "instalment debtors.*leases"),
    r("cards",          "asset", 166, A, "credit.?card"),
    r("overdrafts",     "asset", 181, A, "^overdrafts"),
    r("personal_other", "asset", 192, A, "^households"),
    r("personal_other", "asset", 191, A, "unincorporated"),
    r("personal_other", "asset", 193, A, "non.?profit"),
    r("corporate_loans","asset", 189, A, "financial corporate"),
    r("corporate_loans","asset", 190, A, "non.?financial corporate"),
    r("corporate_loans","asset", 187, A, "factoring"),
    r("corporate_loans","asset", 171, A, "public sector"),
    r("corporate_loans","asset", 135, A, "redeemable preference"),
    r("corporate_loans","asset", 130, A, "other advances"),
    r("corporate_loans","asset", 134, A, "other advances to foreign"),
    r("interbank",      "asset", 111, A, "sa banks"),
    r("interbank",      "asset", 117, A, "foreign banks"),
    r("interbank",      "asset", 128, A, "reserve bank"),
    r("interbank",      "asset", 129, A, "sa banks"),
    r("interbank",      "asset", 133, A, "foreign banks"),
    r("reverse_repo",   "asset", 118, A, "resale agreements"),
    r("hqla",           "asset", 103, A, "central bank money"),
    r("hqla",           "asset", 127, A, "notes and coin"),
    r("hqla",           "asset", 196, A, "government securities"),
    r("hqla",           "asset", 208, A, "reserve bank debentures"),
    r("hqla",           "asset", 250, A, "treasury bills"),
    r("hqla",           "asset", 251, A, "reserve bank bills"),
    r("other_securities","asset", 207, A, "public.?sector interest.?bearing"),
    r("other_securities","asset", 208, A, "reserve bank debentures", sign = -1),
    r("other_securities","asset", 213, A, "debentures"),
    r("other_securities","asset", 233, A, "securitisation"),
    r("other_securities","asset", 246, A, "acceptances"),
    r("other_securities","asset", 250, A, "treasury bills", sign = -1),
    r("other_securities","asset", 251, A, "reserve bank bills", sign = -1),
    r("other_assets",   "asset", 217, A, "subsidiaries"),
    r("other_assets",   "asset", 221, A, "associates"),
    r("other_assets",   "asset", 225, A, "listed equities"),
    r("other_assets",   "asset", 229, A, "unlisted equities"),
    r("other_assets",   "asset", 241, A, "other investments"),
    r("other_assets",   "asset", 245, A, "allowances for impairment", sign = -1),
    r("other_assets",   "asset", 237, A, "derivative"),
    r("other_assets",   "asset", 258, A, "non.?financial assets"),
    r("other_assets",   "asset", 267, A, "other assets"),
    r("impairments",    "asset", 194, A, "credit impairments", sign = -1),
    r("total_assets",   "check", 277, A, "total assets"),
    ## ---- deposits by counterparty (column total) ------------------------------
    r("dep_retail",     "liability", 27, T7, "^households"),
    r("dep_retail",     "liability", 26, T7, "unincorporated"),
    r("dep_retail",     "liability", 28, T7, "non.?profit"),
    r("dep_retail",     "liability", 35, T7, "household"),
    r("dep_corporate",  "liability", 25, T7, "private non.?financial"),
    r("dep_corporate",  "liability", 17, T7, "public non.?financial"),
    r("dep_corporate",  "liability", 37, T7, "non.?financial corporate"),
    r("dep_financial",  "liability", 12, T7, "other monetary"),
    r("dep_financial",  "liability", 15, T7, "public financial"),
    r("dep_financial",  "liability", 16, T7, "public investment"),
    r("dep_financial",  "liability", 18, T7, "insurers"),
    r("dep_financial",  "liability", 19, T7, "pension"),
    r("dep_financial",  "liability", 20, T7, "private financial"),
    r("dep_financial",  "liability", 36, T7, "financial corporate"),
    r("dep_government", "liability",  6, T7, "government"),
    r("dep_government", "liability", 14, T7, "local government"),
    r("dep_government", "liability", 34, T7, "government"),
    r("dep_interbank",  "liability",  3, T7, "sa bank"),
    r("dep_interbank",  "liability", 33, T7, "sa banks"),
    r("dep_foreign",    "liability", 29, T7, "foreign sector"),
    r("dep_foreign",    "liability", 38, T7, "foreign sector"),
    r("deposits_total", "check",      1, T7, "^deposits"),
    r("ncd_memo",       "memo",       1, "0008", "^deposits"),
    ## ---- other funding --------------------------------------------------------
    r("repo_funding",   "liability", 42, T4, "repurchase"),
    r("wholesale_other","liability", 51, T4, "collateralised borrowing"),
    r("wholesale_other","liability", 57, T4, "^other"),
    r("wholesale_other","liability", 58, T4, "foreign currency funding"),
    r("wholesale_other","liability", 68, T4, "debt securities"),
    r("wholesale_other","liability", 73, T4, "credits in transit"),
    r("wholesale_other","liability", 74, T4, "^other"),
    r("wholesale_other","liability", 77, T4, "foreign sector"),
    r("other_liabilities","liability", 79, T4, "per contra"),
    r("other_liabilities","liability", 80, T4, "other liabilities"),
    r("equity",         "equity",    96, "0001", "total equity"),
    r("total_le",       "check",    102, "0001", "total equity and liabilities")
  )
}

#' Leaf deposit rows and the behavioural column map (Post 1 convention)
#'
#' Operational = cheque + <=1 day; savings = savings; notice = 1 day-1 month;
#' term = 1-6 months + >6 months. Leaf counterparty rows only (no subtotals),
#' rand-denominated domestic deposits, exactly as in Post 1, so that the
#' canonical NGFB composition and its betas apply unchanged.
#' @return List with `leaf_items` and `col_category`.
#' @export
ba900_behavioural_map <- function() {
  list(leaf_items = c("4", "5", "8", "9", "10", "11", "12", "14", "15", "16", "17",
                      "18", "19", "21", "22", "23", "24", "25", "26", "27", "28"),
       col_category = c("0001" = "operational", "0002" = "savings",
                        "0003" = "operational", "0004" = "notice",
                        "0005" = "term", "0006" = "term"))
}

#' Aggregate BA900 long data into product lines per bank and period
#'
#' @param long Combined BA900 long frame (see [combine_form_cells()]).
#' @param map Item map (default [ba900_item_map()]).
#' @param strict If `TRUE`, stop when an item description fails its guard.
#' @return List: `lines` (long: `period, inst_id, inst_name, line, value` in
#'   R'000), `guard` (failed description checks).
#' @export
ba900_lines <- function(long, map = ba900_item_map(), strict = FALSE) {
  d <- long
  # item and column numbers are zero-padded up to 2018 ("0005") and plain
  # from 2019 ("5"): key everything on the integer value
  d$item_no <- .colkey(as.character(d$item_no)); d$col_no <- .colkey(as.character(d$col_no))
  d$item_desc <- as.character(d$item_desc)
  map$item <- .colkey(map$item); map$col <- .colkey(map$col)
  key <- paste(d$item_no, d$col_no)
  # description guard, per period (descriptions are the same for all banks)
  dd <- unique(d[, c("period", "item_no", "item_desc")])
  um <- unique(map[, c("item", "desc")])
  g <- merge(dd, um, by.x = "item_no", by.y = "item")
  g$ok <- mapply(function(de, rx) grepl(rx, tolower(gsub("\\s+", " ", de))), g$item_desc, g$desc)
  bad <- g[!g$ok, c("period", "item_no", "item_desc", "desc")]
  if (nrow(bad) && strict) stop("BA900 description guard failed for ", nrow(bad), " item-periods")
  badkey <- paste(bad$period, bad$item_no)
  ## contributions
  mk <- paste(map$item, map$col)
  sel <- d[key %in% mk & !(paste(d$period, d$item_no) %in% badkey), ]
  sel$mk <- paste(sel$item_no, sel$col_no)
  sel <- merge(sel, map[, c("line", "item", "col", "sign")], by.x = c("item_no", "col_no"),
               by.y = c("item", "col"))
  sel$v <- sel$value * sel$sign
  sel$v[is.na(sel$v)] <- 0
  agg <- stats::aggregate(list(value = sel$v),
                          list(period = sel$period, inst_id = as.character(sel$inst_id),
                               inst_name = as.character(sel$inst_name), line = sel$line), sum)
  ## behavioural deposit categories
  bm <- ba900_behavioural_map()
  names(bm$col_category) <- .colkey(names(bm$col_category))
  b <- d[.colkey(as.character(d$table_no)) %in% c("1", "2") & d$item_no %in% .colkey(bm$leaf_items) &
           d$col_no %in% names(bm$col_category), ]
  b$line <- paste0("beh_", bm$col_category[b$col_no])
  b$value[is.na(b$value)] <- 0
  bagg <- stats::aggregate(list(value = b$value),
                           list(period = b$period, inst_id = as.character(b$inst_id),
                                inst_name = as.character(b$inst_name), line = b$line), sum)
  out <- rbind(agg, bagg)
  out <- out[order(out$inst_id, out$period, out$line), ]
  list(lines = out, guard = bad)
}

#' Reconciliation checks on BA900 product lines
#'
#' Assets: products minus impairments must equal reported total assets.
#' Funding: deposits + other funding + equity must equal total equity and
#' liabilities. Returns the relative gaps so callers can `guard()` on them.
#' @param lines The `lines` element of [ba900_lines()].
#' @return Data frame per (period, inst_id) with `asset_gap`, `le_gap`.
#' @export
ba900_reconcile <- function(lines) {
  w <- stats::reshape(lines[, c("period", "inst_id", "line", "value")],
                      idvar = c("period", "inst_id"), timevar = "line", direction = "wide")
  names(w) <- sub("^value\\.", "", names(w))
  z <- function(v) if (v %in% names(w)) { x <- w[[v]]; x[is.na(x)] <- 0; x } else 0
  a_lines <- c("mortgages_res", "mortgages_com", "vaf", "cards", "overdrafts", "personal_other",
               "corporate_loans", "interbank", "reverse_repo", "hqla", "other_securities",
               "other_assets", "impairments")
  assets <- Reduce(`+`, lapply(a_lines, z))
  deps <- z("dep_retail") + z("dep_corporate") + z("dep_financial") + z("dep_government") +
    z("dep_interbank") + z("dep_foreign")
  le <- deps + z("repo_funding") + z("wholesale_other") + z("other_liabilities") + z("equity")
  data.frame(period = w$period, inst_id = w$inst_id,
             asset_gap = safe_div(assets - z("total_assets"), z("total_assets")),
             deposit_gap = safe_div(deps - z("deposits_total"), z("deposits_total")),
             le_gap = safe_div(le - z("total_le"), z("total_le")))
}
