# =============================================================================
#  pull.R -- orchestrates every download the package needs
#
#  Order matters: the fast macro sources come first so that a slow or
#  interrupted BA-form pull still leaves a usable macro panel; BA900 (the
#  bank-level panel) comes before the sector P&L forms; the optional bank-level
#  BA1xx probe runs last. Every BA cell is cached on disk, so re-running the
#  pull resumes rather than restarts.
# =============================================================================

#' Default BA900 institutions to download (regex on the institution name)
#'
#' Everything not matched is still in the sector total and enters the analysis
#' as the residual "other" group. Patterns are case-insensitive.
#' @return Named character vector of regular expressions.
#' @export
default_ba900_institutions <- function() {
  c(TOTAL      = "^\\*?\\s*TOTAL",
    ABSA       = "ABSA",
    FIRSTRAND  = "FIRSTRAND|FIRST RAND",
    NEDBANK    = "NEDBANK",
    STANDARD   = "STANDARD BANK",
    INVESTEC   = "INVESTEC",
    CAPITEC    = "CAPITEC",
    AFRICAN    = "AFRICAN BANK",
    DISCOVERY  = "DISCOVERY",
    TYME       = "TYME",
    BIDVEST    = "BIDVEST",
    GRINDROD   = "GRINDROD",
    SASFIN     = "SASFIN",
    MERCANTILE = "MERCANTILE",
    UBANK      = "UBANK|TEBA",
    BANKZERO   = "BANK ZERO|BANKZERO",
    OLDMUTUAL  = "OLD MUTUAL|^OM BANK",
    ACCESS     = "ACCESS BANK|GROBANK|GRO BANK|BANK OF ATHENS",
    ALBARAKA   = "ALBARAKA",
    HBZ        = "HBZ",
    HABIB      = "HABIB",
    POSTBANK   = "POSTBANK|POST BANK",
    IMPERIAL   = "IMPERIAL BANK")
}

#' Select institutions from a GetInstitutions listing by name pattern
#' @param ins Data frame with `Id`, `Name`.
#' @param patterns Named regex vector (see [default_ba900_institutions()]).
#' @return `ins` restricted to matches, with a `key` column (pattern name).
#' @export
select_institutions <- function(ins, patterns = default_ba900_institutions()) {
  if (is.null(ins) || !nrow(ins)) return(NULL)
  key <- rep(NA_character_, nrow(ins))
  for (k in names(patterns)) {
    hit <- is.na(key) & grepl(patterns[[k]], ins$Name, ignore.case = TRUE)
    if (k == "TOTAL") hit <- hit | (is.na(key) & toupper(ins$Id) == "TOTAL")
    key[hit] <- k
  }
  out <- ins[!is.na(key), , drop = FALSE]
  out$key <- key[!is.na(key)]
  out
}

.institutions_cached <- function(form, period, root) {
  cf <- file.path(root, "institutions", sprintf("%s_%s.rds", form, period))
  if (file.exists(cf)) return(readRDS(cf))
  ins <- sarb_if_institutions(form, period)
  if (!is.null(ins)) { ensure_dir(dirname(cf)); saveRDS(ins, cf) }
  ins
}

.eta <- function(t0, done, total) {
  el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  if (done == 0) return("?")
  sprintf("%.1f min elapsed, ~%.1f min left", el, el / done * (total - done))
}

#' Pull one BA form for a set of periods and institutions
#'
#' @param form Form code.
#' @param periods Character vector of periods.
#' @param patterns Institution name patterns; `NULL` means the sector total only.
#' @param root Cache root for SARB data.
#' @return Invisibly, a data frame log of attempted cells.
#' @export
pull_form <- function(form, periods, patterns = NULL, root = "data-raw/sarb") {
  ensure_dir(root)
  tot_pat <- c(TOTAL = "TOTAL")
  t0 <- Sys.time(); log <- list(); n_ok <- 0L; n_fail <- 0L
  for (i in seq_along(periods)) {
    p <- periods[i]
    ins <- .institutions_cached(form, p, root)
    if (is.null(ins)) { ngfb_log("%s %s: no institution list", form, p); next }
    sel <- if (is.null(patterns)) {
      k <- which(toupper(ins$Id) == "TOTAL" | grepl("total", ins$Name, ignore.case = TRUE))
      if (length(k)) cbind(ins[k[1], c("Id", "Name")], key = "TOTAL") else NULL
    } else select_institutions(ins, patterns)
    if (is.null(sel) || !nrow(sel)) { ngfb_log("%s %s: nothing selected", form, p); next }
    for (j in seq_len(nrow(sel))) {
      d <- fetch_form_cell(form, p, sel$Id[j], sel$Name[j], root = root)
      ok <- !is.null(d)
      if (ok) n_ok <- n_ok + 1L else n_fail <- n_fail + 1L
      log[[length(log) + 1L]] <- data.frame(form = form, period = p, id = sel$Id[j],
                                            name = sel$Name[j], key = sel$key[j],
                                            ok = ok, rows = if (ok) nrow(d) else NA_integer_,
                                            stringsAsFactors = FALSE)
    }
    if (i %% 5L == 0L || i == length(periods))
      ngfb_log("%s: %d/%d periods (%s) | cells ok %d, failed %d | %s",
               form, i, length(periods), p, n_ok, n_fail, .eta(t0, i, length(periods)))
  }
  lg <- rbind_fill(log)
  if (!is.null(lg)) {
    f <- file.path(root, sprintf("pull_log_%s.csv", form))
    utils::write.csv(lg, f, row.names = FALSE)
  }
  invisible(lg)
}

#' Run the full data pull (SARB macro, FRED, BA900, BA1xx)
#'
#' Designed for the analyst's own machine (the SARB API is not reachable from
#' sandboxed environments). Safe to interrupt and re-run.
#' @param root Root directory for raw data (default `"data-raw"`).
#' @param start First period to pull (`"YYYY-MM-DD"`).
#' @param ba900_quarterly Pull BA900 at quarter-end months only (plus the
#'   latest month). Monthly multiplies the call count by three.
#' @param ba900_patterns Institution patterns for BA900.
#' @param forms_stock Sector forms holding stocks (pulled at quarter ends).
#' @param forms_flow Sector forms holding flows (pulled every month).
#' @param probe_bank_level If `TRUE`, check whether BA1xx forms are published
#'   per bank and, if so, pull the six largest banks at quarter ends.
#' @return Invisibly, the manifest data frame.
#' @export
pull_all <- function(root = "data-raw", start = "2008-01-01",
                     ba900_quarterly = TRUE,
                     ba900_patterns = default_ba900_institutions(),
                     forms_stock = c("BA100", "BA200", "BA700"),
                     forms_flow = c("BA120"),
                     probe_bank_level = TRUE) {
  sroot <- ensure_dir(file.path(root, "sarb"))
  mroot <- ensure_dir(file.path(root, "macro"))
  froot <- ensure_dir(file.path(root, "fred"))
  t_all <- Sys.time()
  ngfb_log("== baeconstrat data pull, start %s ==", start)

  ## 1. SARB monthly indicators (KBP) ------------------------------------------
  ngfb_log("[1/6] SARB monthly indicators")
  dts <- sarb_release_datatypes()
  if (!length(dts)) dts <- c("CDACA", "CDACM3", "CDACSM", "CDACSQ", "CDADS", "CDASA",
                             "MRDBM", "MRDCM", "MRDEI", "MRDFG", "MRDIE", "MRDMA",
                             "MRGCM", "MRGEI", "MRGFG", "MRGIE", "MRGMA")
  for (dt in dts) {
    d <- sarb_monthly_indicators(dt)
    if (!is.null(d)) {
      saveRDS(d, file.path(mroot, sprintf("kbp_%s.rds", gsub("[^A-Za-z0-9]", "", dt))))
      ngfb_log("  %-8s %6d records", dt, nrow(d))
    } else ngfb_log("  %-8s no data", dt)
  }
  help <- http_get(paste0(sarb_base_url(), "/Help"), accept = "text/html", tries = 1L)
  if (identical(help$status, 200L)) writeLines(help$body, file.path(sroot, "api_help.html"))

  ## 2. FRED --------------------------------------------------------------------
  ngfb_log("[2/6] FRED series")
  for (nm in names(fred_default_series())) {
    code <- fred_default_series()[[nm]]
    url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", code)
    r <- http_get(url, accept = "text/csv", tries = 2L, timeout = 60, pause = 0.2)
    ok <- identical(r$status, 200L) && !is.null(parse_fred_csv(r$body %||% ""))
    if (ok) writeLines(r$body, file.path(froot, paste0(code, ".csv")))
    ngfb_log("  %-18s %-18s %s", nm, code, if (ok) "ok" else paste("failed", r$status %||% ""))
  }

  ## 3. IFData catalogue --------------------------------------------------------
  ngfb_log("[3/6] IFData catalogue")
  ty <- sarb_if_types()
  if (!is.null(ty)) {
    saveRDS(ty, file.path(sroot, "if_types.rds"))
    utils::write.csv(ty[, intersect(c("Id", "Name", "Description", "Frequency", "LastUpdate"),
                                    names(ty)), drop = FALSE],
                     file.path(sroot, "if_types.csv"), row.names = FALSE)
    ngfb_log("  %d return types: %s", nrow(ty), paste(ty$Id, collapse = ", "))
  }

  ## 4. BA900 bank panel ----------------------------------------------------------
  ngfb_log("[4/6] BA900 (bank-level balance sheets)")
  pv <- sarb_if_periods("BA900")
  pv <- pv[pv >= start]
  if (length(pv)) {
    if (ba900_quarterly) pv <- unique(c(pv[is_quarter_end_month(pv)], max(pv)))
    ngfb_log("  %d periods %s .. %s", length(pv), min(pv), max(pv))
    pull_form("BA900", pv, patterns = ba900_patterns, root = sroot)
  } else ngfb_log("  ! no BA900 periods returned")

  ## 5. Sector forms (TOTAL BANKS) -----------------------------------------------
  ngfb_log("[5/6] sector forms (total banks)")
  for (f in c(forms_flow, forms_stock)) {
    pv <- sarb_if_periods(f)
    pv <- pv[pv >= start]
    if (!length(pv)) { ngfb_log("  ! %s: no periods", f); next }
    if (f %in% forms_stock) pv <- unique(c(pv[is_quarter_end_month(pv)], max(pv)))
    ngfb_log("  %s: %d periods %s .. %s", f, length(pv), min(pv), max(pv))
    pull_form(f, pv, patterns = NULL, root = sroot)
  }

  ## 6. Optional: are BA1xx returns published per bank? ---------------------------
  if (isTRUE(probe_bank_level)) {
    ngfb_log("[6/6] probing bank-level BA1xx availability")
    big6 <- ba900_patterns[c("ABSA", "FIRSTRAND", "NEDBANK", "STANDARD", "CAPITEC", "INVESTEC")]
    for (f in c("BA120", "BA100", "BA700")) {
      pv <- sarb_if_periods(f); pv <- pv[pv >= start]
      if (!length(pv)) next
      ins <- .institutions_cached(f, max(pv), sroot)
      n <- if (is.null(ins)) 0L else nrow(ins)
      sel <- select_institutions(ins, big6)
      ngfb_log("  %s %s: %d institutions listed, %d of the six largest", f, max(pv), n,
               if (is.null(sel)) 0L else nrow(sel))
      if (!is.null(sel) && nrow(sel) >= 4L) {
        qv <- unique(c(pv[is_quarter_end_month(pv)], max(pv)))
        pull_form(f, qv, patterns = big6, root = sroot)
      }
    }
  }

  ## combine + manifest --------------------------------------------------------
  ngfb_log("combining cached cells")
  man <- list()
  for (f in unique(c("BA900", forms_flow, forms_stock))) {
    d <- combine_form_cells(f, root = sroot,
                            months = if (f == "BA900" && ba900_quarterly) "quarter_end" else "all")
    if (is.null(d)) next
    saveRDS(d, file.path(sroot, sprintf("%s_long.rds", f)))
    man[[f]] <- data.frame(form = f, rows = nrow(d), periods = length(unique(d$period)),
                           institutions = length(unique(d$inst_id)),
                           first = as.character(min(d$period)), last = as.character(max(d$period)))
    ngfb_log("  %s: %d rows, %d periods, %d institutions", f, nrow(d),
             length(unique(d$period)), length(unique(d$inst_id)))
  }
  man <- rbind_fill(man)
  if (!is.null(man)) utils::write.csv(man, file.path(root, "manifest.csv"), row.names = FALSE)
  ngfb_log("== done in %.1f min ==", as.numeric(difftime(Sys.time(), t_all, units = "mins")))
  invisible(man)
}

#' BA900 item numbers the package maps (product lines and behavioural deposits)
#'
#' Used to keep the monthly BA900 file compact: every item that
#' [ba900_lines()] and [ba900_deposit_matrix()] read, and nothing else.
#' @return Character vector of item numbers (integer form).
#' @export
ba900_monthly_items <- function() {
  sort(unique(as.integer(c(ba900_item_map()$item, ba900_behavioural_map()$leaf_items))))
}

#' Pull BA900 for every month and write a compact monthly file
#'
#' The quarterly pull fetches quarter-end months only. This fetches the other
#' months for the same institutions (cells already cached are skipped, so it
#' resumes after an interruption) and writes `sarb/BA900m_long.rds`, holding
#' only the items in [ba900_monthly_items()]. `sarb/BA900_long.rds`, the
#' quarterly file, is left as it is. The sector balance sheet (BA100, one
#' institution) is also pulled for every month, so monthly yields and deposit
#' costs can be formed against monthly average balances; it is written to
#' `sarb/BA100m_long.rds`.
#' @param root Root directory for raw data.
#' @param start First period.
#' @param patterns Institution patterns (default: those of the quarterly pull).
#' @param sector_forms Sector-total forms to pull for every month.
#' @return Invisibly, the compact monthly BA900 long data frame.
#' @export
pull_ba900_monthly <- function(root = "data-raw", start = "2008-01-01",
                               patterns = default_ba900_institutions(),
                               sector_forms = "BA100") {
  sroot <- ensure_dir(file.path(root, "sarb"))
  t0 <- Sys.time()
  ngfb_log("== BA900 monthly pull, start %s ==", start)
  ## the sector balance sheet first: one institution, a few minutes
  for (f in sector_forms) {
    pf <- sarb_if_periods(f); pf <- pf[pf >= start]
    if (!length(pf)) { ngfb_log("  ! %s: no periods", f); next }
    ngfb_log("  %s: %d periods %s .. %s", f, length(pf), min(pf), max(pf))
    pull_form(f, pf, patterns = NULL, root = sroot)
    s <- combine_form_cells(f, root = sroot, months = "all")
    if (!is.null(s)) {
      saveRDS(s, file.path(sroot, sprintf("%sm_long.rds", f)))
      ngfb_log("  %sm_long.rds: %d rows, %d periods", f, nrow(s), length(unique(s$period)))
    }
  }
  pv <- sarb_if_periods("BA900")
  pv <- pv[pv >= start]
  if (!length(pv)) stop("no BA900 periods returned by the SARB API")
  have <- unique(substr(list.files(file.path(sroot, "cells", "BA900"), pattern = "\\.rds$"), 1, 10))
  ngfb_log("  BA900: %d periods %s .. %s; %d already have cached cells", length(pv), min(pv),
           max(pv), sum(pv %in% have))
  pull_form("BA900", pv, patterns = patterns, root = sroot)
  ngfb_log("compacting monthly cells")
  d <- combine_form_cells("BA900", root = sroot, months = "all", items = ba900_monthly_items())
  if (is.null(d)) stop("no BA900 cells to combine")
  saveRDS(d, file.path(sroot, "BA900m_long.rds"))
  ngfb_log("  BA900m_long.rds: %d rows, %d periods, %d institutions", nrow(d),
           length(unique(d$period)), length(unique(d$inst_id)))
  ngfb_log("== monthly pull done in %.1f min ==", as.numeric(difftime(Sys.time(), t0, units = "mins")))
  invisible(d)
}
