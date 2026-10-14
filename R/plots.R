# =============================================================================
#  plots.R -- one visual system for every chart in the report
#
#  Categorical hues are assigned in a FIXED order to entities and scenarios
#  (colour follows the entity, never its rank); "Other" folds to grey; the
#  diverging scale is blue <-> red through a neutral grey. Thin marks,
#  hairline grid, one y-axis per panel. ggplot2 is a Suggests dependency.
# =============================================================================

#' Report palette (validated reference instance, light mode)
#' @return List of named colour vectors.
#' @export
viz_palette <- function() {
  list(
    categorical = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
                    "#6250d6", "#e34948"),
    entity = c(NGFB = "#2a78d6", `Big Four` = "#eb6834", Challengers = "#1baf7a",
               Specialists = "#eda100", Other = "#898781"),
    scenario = c(baseline = "#2a78d6", upside = "#1baf7a", downside = "#eb6834",
                 downside_cuts = "#eda100"),
    diverging = c(low = "#2a78d6", mid = "#f0efec", high = "#e34948"),
    ink = c(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb"))
}

#' Scenario display labels
#' @return Named character vector.
#' @export
scenario_labels <- function() {
  c(baseline = "Baseline", upside = "Upside: faster disinflation and a growth dividend",
    downside = "Downside: stagflationary shock",
    downside_cuts = "Downside with cuts: demand-led recession")
}

#' ggplot2 theme for the report
#' @param base_size Base font size.
#' @return A ggplot2 theme.
#' @export
theme_ngfb <- function(base_size = 11) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required for plots")
  ink <- viz_palette()$ink
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = ink[["surface"]], colour = NA),
      panel.grid.major = ggplot2::element_line(colour = ink[["grid"]], linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = ink[["axis"]], linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = ink[["muted"]]),
      axis.title = ggplot2::element_text(colour = ink[["secondary"]]),
      plot.title = ggplot2::element_text(colour = ink[["primary"]], face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = ink[["secondary"]]),
      plot.caption = ggplot2::element_text(colour = ink[["muted"]], hjust = 0),
      strip.text = ggplot2::element_text(colour = ink[["secondary"]], face = "bold", hjust = 0),
      legend.position = "top", legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = ink[["secondary"]]))
}
