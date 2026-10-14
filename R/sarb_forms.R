# =============================================================================
#  sarb_forms.R -- parse and cache SARB BA-form returns (BA900, BA100, ...)
#
#  The parser is the one proven in caterpillar-3-4/sarb_ba_forms.R. Values live
#  in XML ATTRIBUTES, not element text:
#
#    <Table TableNumber="1" TableDescription="ASSETS">
#      <ColumnHeader ColumnNumber="3" ColumnDescription="Total1"/>
#      <Row RowNumber="001" ItemNumber="001" ItemDescription="Cash and ...">
#        <Column ColumnNumber="3" Value="366857821" Format="#,###"/>
#      </Row>
#
#  Item numbers restart in every table, so (table, item, column) is the key.
#  Nothing is keyed on row position: BA100's row count stepped 105 -> 122 -> 132
#  across the 2012/2013 revisions. Rows are iterated one by one rather than via
#  xml_parent() on a node set, because xml2 de-duplicates and re-orders parents,
#  which silently misaligned assets under liabilities in an earlier version.
# =============================================================================

#' Parse SARB numerals with comma-decimal awareness
#'
#' SARB rate payloads use a decimal comma ("6,75") while balance-sheet amounts
#' may use a grouping comma ("1,234,567"). Stripping commas blindly turns 6,75
#' into 675 -- a silent 100x error that passes every statistical test. Rules,
#' in order: both "." and "," present -> comma is grouping; a pure
#' `d{1,3}(,d{3})+` pattern -> grouping; otherwise a lone comma is a decimal.
#' @param x Character (or numeric) vector.
#' @return Numeric vector.
#' @export
sarb_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  s <- trimws(as.character(x))
  s[!nzchar(s) | s %in% c("-", "NA", "n/a", "N/A", ".")] <- NA_character_
  s <- gsub(" ", "", s, fixed = TRUE)
  # non-breaking spaces as UTF-8 (c2 a0) or latin-1 (a0); byte-wise so the
  # result does not depend on the session locale
  s <- gsub("\xc2\xa0", "", s, fixed = TRUE, useBytes = TRUE)
  s <- gsub("\xa0", "", s, fixed = TRUE, useBytes = TRUE)
  has_dot <- !is.na(s) & grepl(".", s, fixed = TRUE)
  has_com <- !is.na(s) & grepl(",", s, fixed = TRUE)
  both <- which(has_dot & has_com)
  s[both] <- gsub(",", "", s[both], fixed = TRUE)
  jj <- which(has_com & !has_dot)
  if (length(jj)) {
    grouped <- grepl("^-?[0-9]{1,3}(,[0-9]{3})+$", s[jj])
    s[jj[grouped]]  <- gsub(",", "", s[jj[grouped]], fixed = TRUE)
    s[jj[!grouped]] <- sub(",", ".", s[jj[!grouped]], fixed = TRUE)
  }
  suppressWarnings(as.numeric(s))
}

.colkey <- function(x) {
  k <- suppressWarnings(as.integer(x))
  ifelse(is.na(k), as.character(x), as.character(k))
}

#' Parse one BA-form XML payload into a long data frame
#'
#' @param xs XML string (the `XMLData` field of a GetInstitutionData response).
#' @return Data frame with columns `table_no`, `table_desc`, `item_no`,
#'   `item_desc`, `col_no`, `col_desc`, `value`; `NULL` if nothing parses.
#'   Section-heading rows (no `ItemNumber`) are dropped.
#' @export
parse_sarb_form <- function(xs) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("package 'xml2' is required")
  if (is.null(xs) || !nzchar(xs)) return(NULL)
  doc <- tryCatch(xml2::read_xml(as.character(xs)), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  xml2::xml_ns_strip(doc)
  tables <- xml2::xml_find_all(doc, ".//Table")
  if (!length(tables)) return(NULL)
  out <- list()
  for (tb in tables) {
    hdr  <- xml2::xml_find_all(tb, "./ColumnHeader")
    # headers say ColumnNumber="1" while cells say ColumnNumber="0001":
    # key both on the integer value
    hmap <- if (length(hdr))
      stats::setNames(trimws(xml2::xml_attr(hdr, "ColumnDescription")),
                      .colkey(xml2::xml_attr(hdr, "ColumnNumber"))) else character(0)
    t_no <- xml2::xml_attr(tb, "TableNumber")
    t_de <- trimws(xml2::xml_attr(tb, "TableDescription"))
    rows <- xml2::xml_find_all(tb, ".//Row")
    for (rw in rows) {
      cs <- xml2::xml_find_all(rw, "./Column")
      if (!length(cs)) next
      cn <- xml2::xml_attr(cs, "ColumnNumber")
      out[[length(out) + 1L]] <- data.frame(
        table_no   = t_no,
        table_desc = t_de,
        item_no    = xml2::xml_attr(rw, "ItemNumber"),
        item_desc  = trimws(xml2::xml_attr(rw, "ItemDescription")),
        col_no     = cn,
        col_desc   = unname(hmap[.colkey(cn)]),
        value      = sarb_num(xml2::xml_attr(cs, "Value")),
        stringsAsFactors = FALSE)
    }
  }
  d <- do.call(rbind, out)
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- d[!is.na(d$item_no) & nzchar(d$item_no), , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' Path of the per-cell cache file for (form, period, institution)
#' @param root Cache root directory.
#' @param form,period,iid Identifiers.
#' @return File path.
#' @export
form_cell_path <- function(root, form, period, iid) {
  file.path(root, "cells", form, sprintf("%s_%s.rds", period, gsub("[^A-Za-z0-9]", "", iid)))
}

#' Fetch, parse and cache one return (form x period x institution)
#'
#' Cached cells are never re-fetched unless `refresh = TRUE`, so an interrupted
#' pull resumes where it stopped. An empty response is cached as a zero-row
#' marker so that dead cells are not re-tried on every run.
#' @param form,period,iid,name Identifiers (name is stored alongside).
#' @param root Cache root.
#' @param refresh Force a re-download.
#' @return Data frame of the parsed cell (possibly zero rows), or `NULL` on a
#'   transport failure (not cached, so it will be retried).
#' @export
fetch_form_cell <- function(form, period, iid, name = NA_character_,
                            root = "data-raw/sarb", refresh = FALSE) {
  cf <- form_cell_path(root, form, period, iid)
  if (file.exists(cf) && !refresh) return(readRDS(cf))
  xs <- sarb_if_institution_xml(form, period, iid)
  if (is.null(xs)) return(NULL)
  d <- parse_sarb_form(xs)
  if (is.null(d)) d <- data.frame(table_no = character(0), table_desc = character(0),
                                  item_no = character(0), item_desc = character(0),
                                  col_no = character(0), col_desc = character(0),
                                  value = numeric(0), stringsAsFactors = FALSE)
  if (nrow(d)) {
    d$form <- form; d$period <- period; d$inst_id <- iid; d$inst_name <- name
  }
  ensure_dir(dirname(cf))
  saveRDS(d, cf)
  d
}

#' Combine all cached cells of a form into one long data frame
#'
#' Strings are stored as factors to keep the combined file small.
#' @param form Form code.
#' @param root Cache root.
#' @param months `"all"` (every cached month) or `"quarter_end"` (quarter-end
#'   months plus the latest cached month, the layout of the quarterly pull).
#'   The monthly BA900 cells share the cache, so the quarterly file asks for
#'   `"quarter_end"` to keep the size and shape it always had.
#' @param items Optional item numbers to keep (compared on the integer value).
#' @return Data frame (columns of [parse_sarb_form()] plus `form`, `period`,
#'   `inst_id`, `inst_name`).
#' @export
combine_form_cells <- function(form, root = "data-raw/sarb", months = c("all", "quarter_end"),
                               items = NULL) {
  months <- match.arg(months)
  fs <- list.files(file.path(root, "cells", form), pattern = "\\.rds$", full.names = TRUE)
  if (!length(fs)) return(NULL)
  if (months == "quarter_end") {
    per <- substr(basename(fs), 1, 10)
    fs <- fs[is_quarter_end_month(per) | month_start(per) == max(month_start(per))]
  }
  keep <- if (is.null(items)) NULL else .colkey(items)
  lst <- lapply(fs, function(f) {
    d <- readRDS(f)
    if (!is.null(keep) && nrow(d)) d <- d[.colkey(as.character(d$item_no)) %in% keep, , drop = FALSE]
    if (nrow(d)) d else NULL
  })
  d <- rbind_fill(lst)
  if (is.null(d)) return(NULL)
  for (v in c("table_no", "table_desc", "item_no", "item_desc", "col_no",
              "col_desc", "form", "inst_id", "inst_name"))
    d[[v]] <- factor(d[[v]])
  d$period <- as.Date(substr(as.character(d$period), 1, 10))
  d
}
