#' Compare compatible summaries of simulated scenarios
#'
#' @param reference A summary from summariseSimulation() or summariseOutcomes().
#' @param intervention A second tma_simulation_summary with compatible outcome
#'   definitions and the same recorded initial joint population distribution.
#' @param scenarioCovariates Explicit initial covariate columns that may differ
#'   between scenarios, for example "treatment_active". Each must exist in at
#'   least one initial population. Source patient identifiers, initial state,
#'   elapsed_day, index_date and birth_date cannot be excluded. Generated
#'   simulation_id, stratum_id, model_phase and cycle columns are ignored automatically.
#' @details Initial populations must have the same rows and multiplicities on
#'   the checked columns, regardless of row order. Matching separate covariate
#'   distributions is insufficient when their joint distributions differ.
#'   Column classes must match, except for plain integer and double vectors.
#'   Simulation identifiers do not establish paired patient random streams.
#'   With an explicit entry model, comparisons use pre-entry profiles rather
#'   than sampled first-cycle outcomes. Both summaries must use the same
#'   initial_population_basis. Entry and already-labelled starts cannot be mixed.
#'
#'   Comparisons use the larger recorded step count. A shorter run is accepted
#'   only when it stopped because every patient was terminal, with verified
#'   terminal final states and zero terminal health contributions. Its existing
#'   cost and health totals are retained without adding future contributions.
#'   This extension is recorded in metadata. Custom stop_when runs are rejected
#'   even when step counts match. No planned simulation horizon is inferred.
#'
#'   State dictionaries, terminal and living-state definitions, utilities,
#'   cycle length, discount assumptions and full-label timing conventions must agree. Both
#'   configured cost scenarios must use the same currency and price year.
#'   Uncomputed outcomes remain NA. An absent cost component is zero only when
#'   that scenario has configured costs.
#'
#'   Incremental results are intervention minus reference. Cost-effectiveness
#'   uses discounted per-patient costs and discounted per-patient life-years or
#'   QALYs. An ICER is reported only for a positive incremental effect and a
#'   nonnegative incremental cost. Other quadrants receive an explicit status
#'   and an NA ICER. Zero differences are evaluated exactly. No willingness-to-pay
#'   threshold, uncertainty estimate or causal interpretation is supplied.
#'   Scenarios may use different extrapolation assumptions. Each supplied
#'   extrapolation definition is retained separately in the comparison metadata.
#'   Model-generated group columns are not compared when their recorded raw
#'   inputs are present in both initial populations. Those raw inputs remain
#'   subject to the population check. Directly supplied groups are not inferred
#'   to be generated, and missing raw inputs do not justify skipping a column.
#' @return A tma_scenario_comparison with outcomes, costs, costEffectiveness and
#'   metadata. outcomes contains six outcome metrics with scenario totals,
#'   per-patient values and differences. costs uses the same columns for each
#'   component and the cost and discounted_cost metrics. costEffectiveness has
#'   two rows with outcome, basis, incremental_cost, incremental_effect, icer and
#'   status. Metadata records checked population columns and terminal extension.
#' @export
compareScenarios <- function(reference, intervention, scenarioCovariates = character()) {
  if (inherits(reference, "tma_outcomes") || inherits(intervention, "tma_outcomes"))
    return(.tmaCompareIntervalOutcomes(reference, intervention, scenarioCovariates))
  reference <- .tmaScenarioSummary(reference, "reference")
  intervention <- .tmaScenarioSummary(intervention, "intervention")
  r <- reference$metadata
  i <- intervention$metadata
  if (r$n != i$n) stop("Scenarios must have the same original simulated population size.", call. = FALSE)
  if (r$initial_population_basis != i$initial_population_basis) {
    stop("Scenario initial_population_basis must match: pre_entry and first_label starts cannot be mixed.", call. = FALSE)
  }
  if (!setequal(r$states, i$states)) stop("Scenario model states must match.", call. = FALSE)
  if (!setequal(r$terminalStates, i$terminalStates)) {
    stop("Scenario terminalStates definitions must match.", call. = FALSE)
  }
  if (is.null(r$aliveStates) != is.null(i$aliveStates) || !setequal(r$aliveStates, i$aliveStates)) {
    stop("Scenario aliveStates definitions must match, including whether they are uncomputed.", call. = FALSE)
  }
  if (is.null(r$utilities) != is.null(i$utilities) ||
      (!is.null(r$utilities) && any(r$utilities[r$states] != i$utilities[r$states]))) {
    stop("Scenario utilities must match by state, including whether they are uncomputed.", call. = FALSE)
  }
  numericDefinitions <- c("cycleLength", "daysPerYear", "discountOriginDay", "costDiscountRate", "healthDiscountRate")
  for (field in numericDefinitions) if (r[[field]] != i[[field]]) {
    stop("Scenario ", field, " definitions must match.", call. = FALSE)
  }
  if (r$costs_configured && i$costs_configured &&
      (r$currency != i$currency || r$priceYear != i$priceYear)) {
    stop("Configured scenario costs must use the same currency and priceYear.", call. = FALSE)
  }
  if (any(c(r$stop_reason, i$stop_reason) == "stop_when")) {
    stop("Custom stop_when scenarios cannot establish a comparable completed horizon.", call. = FALSE)
  }
  commonSteps <- max(r$steps, i$steps)
  extension <- c(reference = r$steps < commonSteps, intervention = i$steps < commonSteps)
  if ((extension[["reference"]] && !(r$stop_reason == "all_terminal" && r$all_terminal)) ||
      (extension[["intervention"]] && !(i$stop_reason == "all_terminal" && i$all_terminal))) {
    stop("Different recorded horizons require the shorter scenario to have stopped all_terminal.", call. = FALSE)
  }
  derivedColumns <- .tmaScenarioDerivedColumns(reference, intervention)
  columns <- .tmaScenarioPopulation(reference$initialPopulation, intervention$initialPopulation,
    scenarioCovariates, derivedColumns)
  metrics <- c("cost", "discounted_cost", "life_years", "discounted_life_years", "qalys", "discounted_qalys")
  rt <- reference$totals[match(metrics, reference$totals$metric), ]
  it <- intervention$totals[match(metrics, intervention$totals$metric), ]
  outcomes <- .tmaScenarioDifference(metrics, rt$total, it$total, rt$per_patient, it$per_patient)
  components <- union(reference$costs$component, intervention$costs$component)
  costs <- data.frame(component = rep(components, each = 2), stringsAsFactors = FALSE)
  costMetrics <- rep(c("cost", "discounted_cost"), times = length(components))
  scenarioCosts <- function(summary) {
    totals <- means <- rep(NA_real_, nrow(costs))
    if (summary$metadata$costs_configured) {
      totals[] <- means[] <- 0
      index <- match(costs$component, summary$costs$component)
      for (metric in c("cost", "discounted_cost")) {
        rows <- which(costMetrics == metric & !is.na(index))
        totals[rows] <- summary$costs[[metric]][index[rows]]
        means[rows] <- summary$costs[[paste0(metric, "_per_patient")]][index[rows]]
      }
    }
    list(total = totals, mean = means)
  }
  rc <- scenarioCosts(reference)
  ic <- scenarioCosts(intervention)
  costs <- cbind(costs, .tmaScenarioDifference(costMetrics, rc$total, ic$total, rc$mean, ic$mean))
  incrementalCost <- outcomes$incremental_per_patient[match("discounted_cost", outcomes$metric)]
  effects <- c("life_years", "qalys")
  incrementalEffect <- outcomes$incremental_per_patient[match(paste0("discounted_", effects), outcomes$metric)]
  costEffectiveness <- data.frame(outcome = effects, basis = "discounted",
    incremental_cost = rep(incrementalCost, 2), incremental_effect = incrementalEffect,
    icer = rep(NA_real_, 2), status = rep("unavailable", 2), stringsAsFactors = FALSE)
  for (row in seq_len(nrow(costEffectiveness))) {
    cost <- costEffectiveness$incremental_cost[row]
    effect <- costEffectiveness$incremental_effect[row]
    if (is.na(cost) || is.na(effect)) next
    if (effect > 0) {
      status <- if (cost > 0) "higher_cost_more_effective" else if (cost < 0) "dominant" else "equal_cost_more_effective"
      if (cost >= 0) costEffectiveness$icer[row] <- cost / effect
    } else if (effect < 0) {
      status <- if (cost > 0) "dominated" else if (cost < 0) "lower_cost_less_effective" else "equal_cost_less_effective"
    } else {
      status <- if (cost > 0) "equal_effect_higher_cost" else if (cost < 0) "equal_effect_lower_cost" else "no_difference"
    }
    costEffectiveness$status[row] <- status
  }
  if (any(!is.finite(costEffectiveness$icer[!is.na(costEffectiveness$icer)]))) {
    stop("Scenario ICER exceeds finite numeric limits.", call. = FALSE)
  }
  units <- if (r$costs_configured) r else if (i$costs_configured) i else NULL
  result <- structure(list(outcomes = outcomes, costs = costs, costEffectiveness = costEffectiveness,
    metadata = list(n = r$n, states = r$states, common_steps = commonSteps,
      cycleLength = r$cycleLength, compared_labelled_days = (commonSteps + 1) * r$cycleLength,
      reference_steps = r$steps, intervention_steps = i$steps, terminal_extension = extension,
      scenarioCovariates = scenarioCovariates, checked_population_columns = columns,
      derived_group_columns_ignored = derivedColumns,
      initial_population_basis = r$initial_population_basis,
      terminalStates = r$terminalStates, aliveStates = r$aliveStates, utilities = r$utilities,
      currency = units$currency, priceYear = units$priceYear,
      costDiscountRate = r$costDiscountRate, healthDiscountRate = r$healthDiscountRate,
      daysPerYear = r$daysPerYear, discountOriginDay = r$discountOriginDay,
      costs_configured = c(reference = r$costs_configured, intervention = i$costs_configured),
      timing = r$timing, discount_timing = r$discount_timing, health_time = r$health_time,
      half_cycle_correction = r$half_cycle_correction)),
    class = "tma_scenario_comparison")
  if (!is.null(r$extrapolation) || !is.null(i$extrapolation)) {
    result$metadata$extrapolation <- list(reference = r$extrapolation,
                                         intervention = i$extrapolation)
  }
  result
}

.tmaScenarioSummary <- function(summary, label) {
  if (!inherits(summary, "tma_simulation_summary") || !is.list(summary$metadata)) {
    stop(label, " must be a summary from summariseSimulation().", call. = FALSE)
  }
  meta <- summary$metadata
  if (!is.character(meta$initial_population_basis) || length(meta$initial_population_basis) != 1L ||
      is.na(meta$initial_population_basis) ||
      !meta$initial_population_basis %in% c("first_label", "pre_entry")) {
    stop(label, " must retain a supported initial_population_basis. Rebuild it with summariseSimulation().", call. = FALSE)
  }
  meta$n <- .tmaProjectionInteger(meta$n, paste0(label, " metadata$n"), 1)
  meta$steps <- .tmaProjectionInteger(meta$steps, paste0(label, " metadata$steps"), 0)
  meta$cycleLength <- .tmaProjectionInteger(meta$cycleLength, paste0(label, " cycleLength"), 1)
  for (field in c("daysPerYear", "discountOriginDay", "costDiscountRate", "healthDiscountRate")) {
    .tmaScenarioNumbers(meta[[field]], paste0(label, " ", field))
    if (length(meta[[field]]) != 1L || meta[[field]] < 0 || (field == "daysPerYear" && meta[[field]] == 0)) {
      stop(label, " ", field, " must be one valid positive or nonnegative value.", call. = FALSE)
    }
  }
  for (field in c("states", "terminalStates", "aliveStates")) {
    value <- meta[[field]]
    if (field == "aliveStates" && is.null(value)) next
    if (!is.character(value) || !is.null(dim(value)) || anyNA(value) || anyDuplicated(value) ||
        any(!nzchar(trimws(value))) || (field == "states" && !length(value)) ||
        (field != "states" && any(!value %in% meta$states))) {
      stop(label, " ", field, " must contain unique known state labels.", call. = FALSE)
    }
  }
  if (any(meta$aliveStates %in% meta$terminalStates)) stop(label, " aliveStates include terminal states.", call. = FALSE)
  if (!is.null(meta$utilities)) {
    .tmaScenarioNumbers(meta$utilities, paste0(label, " utilities"))
    if (is.null(meta$aliveStates) || is.null(names(meta$utilities)) || anyNA(names(meta$utilities)) ||
        anyDuplicated(names(meta$utilities)) || !setequal(names(meta$utilities), meta$states) ||
        any(meta$utilities > 1) || any(meta$utilities[!names(meta$utilities) %in% meta$aliveStates] != 0)) {
      stop(label, " utilities must cover every state and be zero outside explicit aliveStates.", call. = FALSE)
    }
  }
  conventions <- list(
    timing = "each retained label represents one complete cycle, including the initial label",
    discount_timing = "recorded elapsed_day of each label or charge, from one shared origin",
    health_time = "full labelled cycles in explicitly supplied aliveStates",
    half_cycle_correction = "none")
  for (field in names(conventions)) if (!identical(meta[[field]], conventions[[field]])) {
    stop(label, " uses an unsupported ", field, " convention.", call. = FALSE)
  }
  if (!is.character(meta$stop_reason) || length(meta$stop_reason) != 1L || is.na(meta$stop_reason) ||
      !meta$stop_reason %in% c("max_steps", "all_terminal", "stop_when")) {
    stop(label, " must retain a supported stop_reason.", call. = FALSE)
  }
  for (field in c("all_terminal", "costs_configured")) if (!is.logical(meta[[field]]) ||
      length(meta[[field]]) != 1L || is.na(meta[[field]])) {
    stop(label, " must retain a logical ", field, " flag.", call. = FALSE)
  }
  metrics <- c("cost", "discounted_cost", "life_years", "discounted_life_years", "qalys", "discounted_qalys")
  patient <- summary$byPatient
  initial <- summary$initialPopulation
  needed <- c("simulation_id", "first_day", "last_label_day", "labelled_cycles", "labelled_days", "final_state", metrics)
  if (!is.data.frame(patient) || anyDuplicated(names(patient)) || !all(needed %in% names(patient)) ||
      nrow(patient) != meta$n || !is.character(patient$simulation_id) || anyNA(patient$simulation_id) ||
      any(!nzchar(trimws(patient$simulation_id))) || anyDuplicated(patient$simulation_id) ||
      !is.character(patient$final_state) || anyNA(patient$final_state) || any(!patient$final_state %in% meta$states)) {
    stop(label, " byPatient must retain unique simulated IDs, known final states and all outcome fields.", call. = FALSE)
  }
  actualTerminal <- all(patient$final_state %in% meta$terminalStates)
  if (meta$all_terminal != actualTerminal || (meta$stop_reason == "all_terminal" && !actualTerminal)) {
    stop(label, " all_terminal flag and stop reason must agree with declared terminal final states.", call. = FALSE)
  }
  for (field in c("first_day", "last_label_day", "labelled_cycles", "labelled_days")) {
    .tmaScenarioNumbers(patient[[field]], paste0(label, " ", field))
  }
  if (any(patient$first_day < 0 | patient$first_day >= 2^53 | patient$first_day != trunc(patient$first_day)) ||
      any(patient$labelled_cycles != meta$steps + 1) ||
      any(patient$labelled_days != patient$labelled_cycles * meta$cycleLength) ||
      any(patient$last_label_day != patient$first_day + meta$steps * meta$cycleLength) ||
      meta$discountOriginDay > min(patient$first_day)) {
    stop(label, " retained horizon, labelled duration and discount origin must agree.", call. = FALSE)
  }
  if (!is.data.frame(initial) || anyDuplicated(names(initial)) ||
      !all(c("simulation_id", "state_label", "elapsed_day") %in% names(initial)) ||
      nrow(initial) != meta$n || !is.character(initial$simulation_id) || anyNA(initial$simulation_id) ||
      anyDuplicated(initial$simulation_id) || !setequal(initial$simulation_id, patient$simulation_id) ||
      !is.character(initial$state_label) || anyNA(initial$state_label) || any(!initial$state_label %in% meta$states)) {
    stop(label, " must retain its complete initialPopulation with matching simulated IDs and known states.", call. = FALSE)
  }
  .tmaScenarioNumbers(initial$elapsed_day, paste0(label, " initial elapsed_day"))
  matched <- match(patient$simulation_id, initial$simulation_id)
  if (any(initial$elapsed_day[matched] != patient$first_day)) {
    stop(label, " initial simulation IDs and elapsed_day must align with byPatient first_day.", call. = FALSE)
  }
  if ("source_subject_id" %in% union(names(initial), names(patient)) &&
      (!all(c("source_subject_id") %in% names(initial)) || !"source_subject_id" %in% names(patient) ||
        !identical(initial$source_subject_id[matched], patient$source_subject_id))) {
    stop(label, " source_subject_id must align between initialPopulation and byPatient.", call. = FALSE)
  }
  totals <- summary$totals
  if (!is.data.frame(totals) || anyDuplicated(names(totals)) ||
      !all(c("metric", "total", "per_patient") %in% names(totals)) ||
      !is.character(totals$metric) || anyNA(totals$metric) || anyDuplicated(totals$metric) ||
      !setequal(totals$metric, metrics)) stop(label, " totals must contain the six outcome metrics.", call. = FALSE)
  unavailable <- c(rep(!meta$costs_configured, 2), rep(is.null(meta$aliveStates), 2), rep(is.null(meta$utilities), 2))
  for (j in seq_along(metrics)) {
    metric <- metrics[j]
    values <- patient[[metric]]
    row <- match(metric, totals$metric)
    .tmaScenarioNumbers(values, paste0(label, " ", metric), allowNA = unavailable[j])
    .tmaScenarioNumbers(c(totals$total[row], totals$per_patient[row]), paste0(label, " totals ", metric), allowNA = unavailable[j])
    if (unavailable[j]) {
      if (!all(is.na(c(values, totals$total[row], totals$per_patient[row])))) {
        stop(label, " uncomputed ", metric, " must be NA for all patients and totals.", call. = FALSE)
      }
    } else if (!.tmaScenarioClose(sum(values), totals$total[row]) ||
               !.tmaScenarioClose(totals$total[row] / meta$n, totals$per_patient[row])) {
      stop(label, " ", metric, " totals and per-patient values must reconcile.", call. = FALSE)
    }
  }
  costs <- summary$costs
  costFields <- c("cost", "discounted_cost", "cost_per_patient", "discounted_cost_per_patient")
  if (!is.data.frame(costs) || anyDuplicated(names(costs)) ||
      !all(c("component", costFields) %in% names(costs)) || !is.character(costs$component) ||
      anyNA(costs$component) || any(!nzchar(trimws(costs$component))) || anyDuplicated(costs$component) ||
      (meta$costs_configured && !nrow(costs)) || (!meta$costs_configured && nrow(costs))) {
    stop(label, " cost components must agree with its costs_configured flag.", call. = FALSE)
  }
  for (field in costFields) .tmaScenarioNumbers(costs[[field]], paste0(label, " costs ", field))
  if (meta$costs_configured) {
    if (!is.character(meta$currency) || length(meta$currency) != 1L || is.na(meta$currency) || !nzchar(trimws(meta$currency))) {
      stop(label, " configured costs need one currency label.", call. = FALSE)
    }
    .tmaProjectionInteger(meta$priceYear, paste0(label, " priceYear"), 1)
    for (metric in c("cost", "discounted_cost")) {
      if (!.tmaScenarioClose(sum(costs[[metric]]), totals$total[match(metric, totals$metric)]) ||
          !.tmaScenarioClose(costs[[metric]] / meta$n, costs[[paste0(metric, "_per_patient")]])) {
        stop(label, " ", metric, " component totals and per-patient values must reconcile.", call. = FALSE)
      }
    }
  }
  summary$metadata <- meta
  summary
}

.tmaScenarioDerivedColumns <- function(reference, intervention) {
  maps <- list(reference$metadata$derived_group_inputs,
               intervention$metadata$derived_group_inputs)
  for (map in maps) if (length(map)) {
    if (!is.list(map) || is.data.frame(map) || is.null(names(map)) ||
        anyNA(names(map)) || anyDuplicated(names(map)) || any(!nzchar(names(map))) ||
        !all(vapply(map, function(x) is.character(x) && length(x) > 0 &&
          !anyNA(x) && !anyDuplicated(x) && all(nzchar(x)), logical(1)))) {
      stop("Derived group metadata must identify each output and its raw input columns.", call. = FALSE)
    }
  }
  rules <- c(maps[[1L]], maps[[2L]])
  if (!length(rules)) return(character())
  common <- intersect(names(reference$initialPopulation), names(intervention$initialPopulation))
  # Never drop a column used as another rule's raw input, or rely on ignored
  # simulation bookkeeping as evidence that patient characteristics agree.
  raw <- unique(unlist(rules, use.names = FALSE))
  outputs <- setdiff(unique(names(rules)), raw)
  bookkeeping <- c("simulation_id", "stratum_id", "model_phase", "cycle")
  outputs[vapply(outputs, function(output) {
    inputs <- unique(unlist(rules[names(rules) == output], use.names = FALSE))
    generatedReference <- !output %in% names(reference$initialPopulation) ||
      output %in% names(maps[[1L]])
    generatedIntervention <- !output %in% names(intervention$initialPopulation) ||
      output %in% names(maps[[2L]])
    generatedReference && generatedIntervention && all(inputs %in% setdiff(common, bookkeeping))
  }, logical(1))]
}

.tmaScenarioPopulation <- function(reference, intervention, exclusions,
                                  derivedColumns = character()) {
  protected <- c("source_subject_id", "state_label", "elapsed_day", "index_date", "birth_date")
  available <- union(names(reference), names(intervention))
  if (!is.character(exclusions) || !is.null(dim(exclusions)) || anyNA(exclusions) ||
      anyDuplicated(exclusions) || any(!nzchar(trimws(exclusions))) || any(!exclusions %in% available)) {
    stop("scenarioCovariates must name unique columns present in at least one initial population.", call. = FALSE)
  }
  if (any(exclusions %in% protected)) {
    stop("scenarioCovariates cannot exclude protected initial columns: ",
      paste(intersect(exclusions, protected), collapse = ", "), ".", call. = FALSE)
  }
  ignored <- c("simulation_id", "stratum_id", "model_phase", "cycle", exclusions, derivedColumns)
  columns <- setdiff(names(reference), ignored)
  other <- setdiff(names(intervention), ignored)
  if (!setequal(columns, other)) {
    stop("Scenario initial population column sets differ after exclusions: ",
      paste(union(setdiff(columns, other), setdiff(other, columns)), collapse = ", "), ".", call. = FALSE)
  }
  for (column in columns) {
    left <- reference[[column]]
    right <- intervention[[column]]
    plainNumeric <- typeof(left) %in% c("integer", "double") && !is.object(left) &&
      typeof(right) %in% c("integer", "double") && !is.object(right)
    if (!plainNumeric && !identical(class(left), class(right))) {
      stop("Scenario initial population column classes differ for ", column,
        ". Use the same encoding or declare an intended scenarioCovariate.", call. = FALSE)
    }
  }
  marker <- ".tma_scenario_difference"
  while (marker %in% available) marker <- paste0(marker, "_")
  left <- reference[columns]
  right <- intervention[columns]
  left[[marker]] <- rep(1L, nrow(left))
  right[[marker]] <- rep(-1L, nrow(right))
  counts <- tryCatch({
    combined <- dplyr::bind_rows(left, right)
    grouped <- dplyr::group_by(combined, dplyr::across(dplyr::all_of(columns)))
    dplyr::summarise(grouped, !!marker := sum(.data[[marker]]), .groups = "drop")
  }, error = function(error) stop("Scenario initial population columns cannot be compared jointly (",
    paste(columns, collapse = ", "), "): ", conditionMessage(error), call. = FALSE))
  if (any(counts[[marker]] != 0)) {
    stop("Scenario initial joint population rows or multiplicities differ in checked columns: ",
      paste(columns, collapse = ", "), ". Use scenarioCovariates only for intended scenario assignments.", call. = FALSE)
  }
  columns
}

.tmaScenarioNumbers <- function(value, label, allowNA = FALSE) {
  if (!is.numeric(value) || is.complex(value) || inherits(value, "integer64") ||
      !is.null(dim(value)) || any(is.nan(value)) ||
      any(!is.finite(value) & !(allowNA & is.na(value)))) {
    stop(label, " must contain finite numeric values", if (allowNA) " or wholly uncomputed NA values" else "", ".", call. = FALSE)
  }
  invisible(value)
}

.tmaScenarioClose <- function(left, right) {
  length(left) == length(right) && all(is.finite(c(left, right))) &&
    all(abs(left - right) <= 1e-10 * pmax(1, abs(left), abs(right)))
}

.tmaScenarioDifference <- function(metric, referenceTotal, interventionTotal, referenceMean, interventionMean) {
  result <- data.frame(metric = metric, reference_total = referenceTotal,
    intervention_total = interventionTotal, incremental_total = interventionTotal - referenceTotal,
    reference_per_patient = referenceMean, intervention_per_patient = interventionMean,
    incremental_per_patient = interventionMean - referenceMean, stringsAsFactors = FALSE)
  for (field in names(result)[-1L]) .tmaScenarioNumbers(result[[field]], paste0("Scenario ", field), allowNA = TRUE)
  result
}
