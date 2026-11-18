#' Extract cost and health totals from an outcome summary
#'
#' @param x An outcome summary from summariseOutcomes() or summariseSimulation().
#' @param discounted Use discounted outcomes. Defaults to TRUE.
#' @param perPatient Use outcomes per initial patient instead of cohort totals.
#'   Defaults to FALSE.
#' @details This function selects stored totals without changing their accounting
#'   convention, horizon, discount rates or precision. summariseOutcomes() counts
#'   completed intervals; summariseSimulation() counts retained full-cycle labels,
#'   including the initial label. Uncomputed outcomes remain NA. The short output
#'   names do not indicate discounting or scale; these are selected by the arguments.
#' @return A named numeric vector with cost, life_years and qalys, in that order.
#' @seealso [summariseOutcomes()], [summariseSimulation()], [compareScenarios()]
#' @export
outcomeTotals <- function(x, discounted = TRUE, perPatient = FALSE) {
  if (!inherits(x, c("tma_outcomes", "tma_simulation_summary"))) {
    stop("x must be a summary from summariseOutcomes() or summariseSimulation().", call. = FALSE)
  }
  for (name in c("discounted", "perPatient")) {
    value <- get(name)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(name, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  metrics <- c("cost", "life_years", "qalys")
  selected <- if (discounted) paste0("discounted_", metrics) else metrics
  column <- if (perPatient) "per_patient" else "total"
  totals <- x$totals
  if (!is.data.frame(totals) || !all(c("metric", column) %in% names(totals)) ||
      anyDuplicated(totals$metric) || !all(selected %in% totals$metric) ||
      !is.numeric(totals[[column]])) {
    stop("x must retain its outcome totals table.", call. = FALSE)
  }
  stats::setNames(totals[[column]][match(selected, totals$metric)], metrics)
}
