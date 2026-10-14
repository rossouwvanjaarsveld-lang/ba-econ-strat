# =============================================================================
#  panel_bank.R -- quarterly bank and sector panels from BA900 product lines
# =============================================================================

#' Products modelled at sector and bank level
#' @return Named list: `loans`, `deposits_beh` (behavioural categories),
#'   `deposits_cpty` (counterparty), `treasury`, `other`.
#' @export
product_sets <- function() {
  list(loans = c("mortgages_res", "mortgages_com", "vaf", "cards", "overdrafts",
                 "personal_other", "corporate_loans"),
       deposits_beh = c("beh_operational", "beh_savings", "beh_notice", "beh_term"),
       deposits_cpty = c("dep_retail", "dep_corporate", "dep_financial", "dep_government"),
       treasury = c("interbank", "reverse_repo", "hqla", "other_securities"),
       other = c("other_assets", "impairments", "repo_funding", "wholesale_other",
                 "other_liabilities", "equity", "dep_interbank", "dep_foreign"))
}

#' Map BA900 institution names to stable keys
#' @param ids_names Data frame with `inst_id`, `inst_name`.
#' @param patterns Patterns as in [default_ba900_institutions()].
#' @return Data frame `inst_id, key` (unmatched institutions dropped).
#' @export
institution_keys <- function(ids_names, patterns = default_ba900_institutions()) {
  u <- unique(ids_names[, c("inst_id", "inst_name")])
  s <- select_institutions(data.frame(Id = u$inst_id, Name = u$inst_name), patterns)
  if (is.null(s)) return(data.frame(inst_id = character(0), key = character(0)))
  unique(data.frame(inst_id = s$Id, key = s$key, stringsAsFactors = FALSE))
}

#' Quarterly wide panel of product lines by entity
#'
#' Keeps quarter-end months only (BA900 periods are month-end dates up to
#' 2025 and month-start dates from 2026; both are normalised).
#' @param lines Output `lines` of [ba900_lines()].
#' @param patterns Institution patterns.
#' @return Data frame `date, key, <lines...>` in R'000.
#' @export
bank_panel_quarterly <- function(lines, patterns = default_ba900_institutions()) {
  lines$month <- month_start(lines$period)
  q <- lines[is_quarter_end_month(lines$month), ]
  q$date <- quarter_end(q$month)
  keys <- institution_keys(q, patterns)
  q <- merge(q, keys, by = "inst_id")
  agg <- stats::aggregate(list(value = q$value), list(date = q$date, key = q$key, line = q$line), sum)
  w <- stats::reshape(agg, idvar = c("date", "key"), timevar = "line", direction = "wide")
  names(w) <- sub("^value\\.", "", names(w))
  for (v in setdiff(names(w), c("date", "key"))) w[[v]][is.na(w[[v]])] <- 0
  w[order(w$key, w$date), ]
}

#' Aggregate banks into peer groups (plus the residual "Other")
#'
#' @param bp Output of [bank_panel_quarterly()].
#' @param groups Key -> group map (default [peer_groups()]).
#' @return Data frame `date, entity, <lines...>` with entities = groups,
#'   "Other" (sector total less all named banks) and "Sector".
#' @export
group_panel <- function(bp, groups = peer_groups()) {
  vals <- setdiff(names(bp), c("date", "key"))
  tot <- bp[bp$key == "TOTAL", c("date", vals)]
  named <- bp[bp$key != "TOTAL", ]
  named$entity <- groups[named$key]
  named$entity[is.na(named$entity)] <- "Specialists"
  g <- stats::aggregate(named[, vals], list(date = named$date, entity = named$entity), sum)
  sumnamed <- stats::aggregate(named[, vals], list(date = named$date), sum)
  oth <- merge(tot, sumnamed, by = "date", suffixes = c("", ".n"))
  for (v in vals) oth[[v]] <- oth[[v]] - oth[[paste0(v, ".n")]]
  oth <- oth[, c("date", vals)]; oth$entity <- "Other"
  tot$entity <- "Sector"
  out <- rbind(g[, c("date", "entity", vals)], oth[, c("date", "entity", vals)],
               tot[, c("date", "entity", vals)])
  out[order(out$entity, out$date), ]
}

#' Membership count per group and quarter (k-anonymity check)
#' @param bp Output of [bank_panel_quarterly()].
#' @param groups Key -> group map.
#' @param line Line used to decide whether a bank is active.
#' @return Data frame `date, entity, members`.
#' @export
group_membership <- function(bp, groups = peer_groups(), line = "total_assets") {
  b <- bp[bp$key != "TOTAL" & bp[[line]] > 0, ]
  b$entity <- groups[b$key]; b$entity[is.na(b$entity)] <- "Specialists"
  stats::aggregate(list(members = b$key), list(date = b$date, entity = b$entity), length)
}

#' Long market-share frame from a group (or bank) panel
#' @param gp Panel with `date`, `entity` and product columns, including a
#'   "Sector" entity.
#' @param products Product columns.
#' @return Long data frame `date, entity, product, value, sector, share`.
#' @export
market_shares <- function(gp, products) {
  sec <- gp[gp$entity == "Sector", ]
  out <- list()
  for (p in products) {
    x <- gp[!grepl("^Sector", gp$entity), c("date", "entity", p)]
    names(x)[3] <- "value"
    s <- sec[, c("date", p)]; names(s)[2] <- "sector"
    x <- merge(x, s, by = "date")
    x$product <- p
    x$share <- safe_div(x$value, x$sector)
    out[[p]] <- x
  }
  d <- do.call(rbind, out)
  d[order(d$product, d$entity, d$date), c("date", "entity", "product", "value", "sector", "share")]
}

#' Sector growth by product (annualised q/q log growth, percent)
#' @param gp Group panel (uses the "Sector" rows).
#' @param products Product columns.
#' @return Long data frame `date, product, level, growth`.
#' @export
sector_growth <- function(gp, products) {
  sec <- gp[gp$entity == "Sector", ]
  sec <- sec[order(sec$date), ]
  do.call(rbind, lapply(products, function(p)
    data.frame(date = sec$date, product = p, level = sec[[p]], growth = growth_qq(sec[[p]]))))
}

#' Peer-group roster: which institutions sit in which group, and when
#'
#' Banks whose BA900 returns are in the pulled data are assigned to their
#' peer group ([peer_groups()]; unmapped named banks are Specialists, as in
#' [group_panel()]). Every other institution in the SARB's BA900 institution
#' listings is part of the residual "Other" (sector total less the named
#' banks), even where a name pattern would now match it: its returns were
#' not pulled, so its balance sheet is in the residual.
#' @param b9 Long BA900 data (as pulled).
#' @param listing_dir Folder of cached institution listings
#'   (`BA900_<period>.rds`), or `NULL`.
#' @param groups Key -> group map.
#' @param patterns Institution patterns.
#' @return Data frame `group, key, name, first, last, quarters, source`.
#' @export
peer_group_roster <- function(b9, listing_dir = NULL, groups = peer_groups(),
                              patterns = default_ba900_institutions()) {
  norm <- function(x) gsub("\\s+", " ", trimws(toupper(as.character(x))))
  u <- unique(data.frame(inst_id = as.character(b9$inst_id), inst_name = as.character(b9$inst_name),
                         month = month_start(b9$period), stringsAsFactors = FALSE))
  u <- u[is_quarter_end_month(u$month), ]
  keys <- institution_keys(unique(u[, c("inst_id", "inst_name")]), patterns)
  u$key <- keys$key[match(u$inst_id, keys$inst_id)]
  u <- u[!is.na(u$key) & u$key != "TOTAL", ]
  u$date <- quarter_end(u$month)
  named <- do.call(rbind, lapply(split(u, list(u$key, u$inst_name), drop = TRUE), function(z)
    data.frame(key = z$key[1], name = norm(z$inst_name[1]), first = min(z$date), last = max(z$date),
               quarters = length(unique(z$date)), stringsAsFactors = FALSE)))
  named$group <- groups[named$key]; named$group[is.na(named$group)] <- "Specialists"
  named$source <- "BA900 return in the data"
  out <- named
  if (!is.null(listing_dir) && dir.exists(listing_dir)) {
    fs <- list.files(listing_dir, pattern = "^BA900_.*\\.rds$", full.names = TRUE)
    L <- do.call(rbind, lapply(fs, function(f) {
      d <- readRDS(f)
      m <- month_start(as.Date(sub("^BA900_(.*)\\.rds$", "\\1", basename(f))))
      data.frame(name = norm(d$Name), month = m, stringsAsFactors = FALSE)
    }))
    L <- L[is_quarter_end_month(L$month), ]
    L$date <- quarter_end(L$month)
    L <- L[!grepl("TOTAL", L$name) & !(L$name %in% named$name), ]
    if (nrow(L)) {
      oth <- do.call(rbind, lapply(split(L, L$name), function(z)
        data.frame(key = NA_character_, name = z$name[1], first = min(z$date), last = max(z$date),
                   quarters = length(unique(z$date)), group = "Other",
                   source = "in the SARB listing; balance sheet in the residual", stringsAsFactors = FALSE)))
      out <- rbind(out, oth[, names(out)])
    }
  }
  ord <- order(match(out$group, c("Big Four", "Challengers", "Specialists", "Other")), out$key, out$first)
  out <- out[ord, c("group", "key", "name", "first", "last", "quarters", "source")]
  rownames(out) <- NULL
  out
}
