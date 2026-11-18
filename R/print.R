#' Inspect models and results without printing their internal settings
#'
#' Printing a TMA object shows its main dimensions and a short result preview.
#' All stored tables, settings and diagnostics remain available on the object.
#' No observations are removed, no probabilities are changed, and no disclosure
#' control is applied. Inspect patient-level tables only in an appropriate
#' analysis environment.
#'
#' @param x A TMA model or result.
#' @param ... Reserved for additional print arguments.
#' @return The input object, invisibly and unchanged.
#' @name print-tma
NULL

.tmaPrintTable <- function(data, columns = names(data), limit = 6L) {
  if (is.null(data) || !nrow(data)) return(invisible(NULL))
  columns <- intersect(columns, names(data))
  print(as.data.frame(utils::head(data[columns], limit)), row.names = FALSE)
  if (nrow(data) > limit) cat("...", nrow(data) - limit, "more rows\n")
  invisible(NULL)
}

.tmaPrintModel <- function(x) {
  meta <- x$metadata
  method <- switch(if (is.null(meta$estimator)) "unknown" else meta$estimator,
    adjacent_cycles = "Observed adjacent-cycle transitions",
    observed_stages = "Observed ordered-stage transitions",
    specified_stages = "Specified ordered-stage transitions",
    specified_mortality = "Specified mortality transitions",
    "Transition model")
  cat(method, "\n", sep = "")
  cat(length(meta$states), "states |", meta$cycleLength, "days per cycle |",
      nrow(x$periods), "period(s)\n")
  if (length(meta$stage_order)) cat("Stage order:", paste(meta$stage_order, collapse = " -> "), "\n")
  if (!is.null(x$strata)) cat(nrow(x$strata), "stratum/strata\n")
  if (!is.null(x$estimates)) {
    unsupported <- x$estimates[is.na(x$estimates$probability), , drop = FALSE]
    keys <- intersect(c("stratum_id", "stage", "period", "from_state"), names(unsupported))
    if (nrow(unsupported)) cat(nrow(unique(unsupported[keys])), "unsupported source row(s); probabilities remain NA.\n")
    cat("Estimates: $estimates | Supporting information: $diagnostics\n")
  }
  cat("Inspect probabilities with transitionMatrix().\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_transitions <- function(x, ...) .tmaPrintModel(x)

#' @rdname print-tma
#' @export
print.tma_stage_transitions <- function(x, ...) .tmaPrintModel(x)

#' @rdname print-tma
#' @export
print.tma_stratified_transitions <- function(x, ...) .tmaPrintModel(x)

#' @rdname print-tma
#' @export
print.tma_trajectories <- function(x, ...) {
  cat("Prepared patient histories\n")
  cat(nrow(x$patients), "patients |", nrow(x$data), "intervals |", length(x$states), "clinical states\n")
  if (!is.null(x$cycleLength)) cat("Cycle length:", x$cycleLength, "days\n")
  d <- x$diagnostics
  cat("Gaps:", d$gap_count, "| Unresolved intervals:", d$unresolved_intervals,
      "| Partial cycles:", d$partial_cycles, "\n")
  cat("Histories: $data | Observation bounds: $patients | Checks: $diagnostics\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_projection <- function(x, ...) {
  cat("Expected cohort projection\n")
  cat(x$metadata$cohortSize, "patients |", x$metadata$nCycles,
      "transition steps |", x$metadata$cycleLength, "days per step\n")
  cat("Final expected state counts:\n")
  final <- x$occupancy[x$occupancy$cycle == x$metadata$nCycles, , drop = FALSE]
  .tmaPrintTable(final, c("state", "probability", "expected_count"))
  cat("All timepoints: $occupancy | Movements: $flows")
  if (!is.null(x$stageFlows)) cat(" | Within-cycle movements: $stageFlows")
  cat("\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_simulation <- function(x, ...) {
  cat("Simulated patient histories\n")
  cat(nrow(x$population), "patients |", x$metadata$steps,
      "completed simulation step(s) |", x$metadata$cycleLength, "days per step\n")
  cat("Stopped:", x$metadata$stop_reason, "\n")
  counts <- as.data.frame(table(factor(x$population$state_label,
    levels = x$.config$bundle$metadata$states)), stringsAsFactors = FALSE)
  names(counts) <- c("state", "patients")
  .tmaPrintTable(counts)
  if (!is.null(x$totals) && nrow(x$totals)) {
    cat("Recorded cost totals:", x$metadata$currency, "\n")
    .tmaPrintTable(x$totals)
  }
  cat("Histories: $trajectories | Final patients: $population | Movements: $transitions\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_state_costs <- function(x, ...) {
  cat("Estimated recorded costs\n")
  cat(nrow(x$estimates), "estimate rows |", nrow(x$periods), "period(s)\n")
  .tmaPrintTable(x$estimates, c(x$metadata$strata, "period", "cost_state", "currency_concept_id",
    "observed_days", "total_cost", "mean_cost_per_day"))
  cat("Unallocated records:", x$diagnostics$unallocated_cost_records,
      "| Unknown-currency records:", x$diagnostics$unknown_currency_records, "\n")
  cat("All estimates: $estimates | Allocation: $intervalCosts | Checks: $diagnostics\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_cost_model <- function(x, ...) {
  cat("Simulation healthcare cost model\n")
  cat("Unit:", x$metadata$unit, "| Currency:", x$metadata$currency,
      "| Price year:", x$metadata$priceYear, "| Terminal policy:", x$metadata$terminal, "\n")
  cat("Accounting: full labelled intervals, including the initial living label.\n")
  .tmaPrintTable(x$rates, c(x$metadata$strata, "period", "cost_state", "unit_cost"))
  cat("Rates: $rates | Definitions: $metadata\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_model_bundle <- function(x, ...) {
  cat("Simulation model bundle\n")
  cat(length(x$metadata$states), "states |", x$metadata$cycleLength,
      "days per cycle |", nrow(x$strata), "stratum/strata\n")
  if (!is.null(x$costModel)) cat("Healthcare costs: configured\n") else if (!is.null(x$costEstimates)) {
    cat("Healthcare costs: descriptive estimates only; not charged\n")
  } else cat("Healthcare costs: not configured\n")
  if (!is.null(x$extrapolation)) cat("A later model is configured.\n")
  if (inherits(x, "tma_mortality_bundle")) {
    cat("Future mortality: current-profile background plus specified excess mortality.\n")
    cat("Reference: $reference | Calibration: $calibration | Definitions: $metadata\n")
  } else cat("Models: $models | Groups: $strata | Definitions: $metadata\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_rewards <- function(x, ...) {
  cat("Completed-interval costs and health outcomes\n")
  cat("Cost timing:", x$costTiming, "| Health timing:", x$healthTiming, "\n")
  cat("Annual discount rates: costs", x$costDiscountRate,
      "| health", x$healthDiscountRate, "\n")
  if (x$costs_configured) cat("Currency:", x$currency, "| Price year:", x$priceYear, "\n")
  cat("State costs: $stateCosts | Upfront costs: $upfrontCosts | Utilities: $utilities\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_outcomes <- function(x, ...) {
  cat("Completed-interval outcomes\n")
  cat(x$metadata$n, "patients |", x$metadata$nCycles, "intervals |",
      x$metadata$accounted_days, "days per initial patient\n")
  if (!is.null(x$metadata$currency)) cat("Currency:", x$metadata$currency, "| Price year:", x$metadata$priceYear, "\n")
  .tmaPrintTable(x$totals)
  cat("Totals: $totals | By interval: $byCycle | Cost breakdown: $costs\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_simulation_summary <- function(x, ...) {
  cat("Labelled-cycle simulation outcomes\n")
  cat(x$metadata$n, "patients |", x$metadata$steps, "simulation steps\n")
  cat("Accounting includes initial and retained destination labels.\n")
  .tmaPrintTable(x$totals)
  cat("Totals: $totals | Per patient: $byPatient | Cost breakdown: $costs\n")
  invisible(x)
}

#' @rdname print-tma
#' @export
print.tma_scenario_comparison <- function(x, ...) {
  cat("Scenario comparison: intervention minus reference\n")
  if (!is.null(x$metadata$currency)) cat("Currency:", x$metadata$currency, "| Price year:", x$metadata$priceYear, "\n")
  cat("Discounted cohort totals:\n")
  metrics <- paste0("discounted_", c("cost", "life_years", "qalys"))
  .tmaPrintTable(x$outcomes[match(metrics, x$outcomes$metric), , drop = FALSE],
    c("metric", "reference_total", "intervention_total", "incremental_total"))
  cat("Discounted incremental costs and effects per patient:\n")
  .tmaPrintTable(x$costEffectiveness)
  cat("Cohort and per-patient outcomes: $outcomes | Cost breakdown: $costs\n")
  invisible(x)
}
