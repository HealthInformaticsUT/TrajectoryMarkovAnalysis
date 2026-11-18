#' Summarise recorded simulation costs and explicitly defined health outcomes
#'
#' @param simulation A result from simulateTrajectories() or advanceSimulation().
#' @param aliveStates Optional unique model state labels counted as living.
#'   Supply these explicitly to calculate life-years. They cannot include the
#'   simulation's terminalStates. NULL leaves health outcomes unknown. An empty
#'   character vector explicitly assigns zero living time to every state.
#' @param utilities Optional named numeric vector with exactly one finite value
#'   per model state. Values cannot exceed one and may be negative. States
#'   outside aliveStates must have utility zero. Requires explicit aliveStates.
#'   NULL leaves QALYs unknown even when life-years are calculated.
#' @param costDiscountRate Nonnegative finite annual cost discount rate.
#' @param healthDiscountRate Nonnegative finite annual health discount rate.
#' @param daysPerYear Positive finite number of days used to convert labelled
#'   model time into years. Defaults to 365.25.
#' @param discountOriginDay Nonnegative finite origin on the recorded elapsed_day
#'   scale. It must be no later than the earliest retained history or charge day.
#'   The origin is shared across patients and is not reset at each first label.
#' @details Every retained label represents one complete cycle of modelled
#'   occupancy. With k completed steps there are k + 1 full labels. For example,
#'   52 weekly steps give 53 labels and 371 labelled days for a continuously
#'   living patient, although the last label starts on day 364. There is no
#'   half-cycle correction, partial-cycle adjustment or inferred event timing.
#'
#'   Each label in aliveStates contributes cycleLength / daysPerYear life-years.
#'   Its QALYs multiply that duration by the supplied state utility. Terminal
#'   labels contribute no living time. stateTime includes terminal rows as
#'   represented snapshot occupancy. Its person_days are full-label model
#'   occupancy, not observed clinical person-time or evidence of life after a
#'   terminal event. Nothing is extrapolated beyond the retained labels.
#'
#'   Costs are sums of saved charges, including negative credits. They are not
#'   recalculated from states or rules. No configured cost rules or cost model
#'   means costs are unknown, reported as NA. Configured components with no
#'   eligible charges contribute zero. Averages divide by the original number
#'   of simulated patients, including terminal and zero-cost patients. Repeated
#'   source_subject_id values identify separate simulated draws.
#'
#'   Discounting multiplies each charge or health contribution by
#'   exp(-log1p(rate) * (elapsed_day - discountOriginDay) / daysPerYear).
#'   All contributions use their recorded label start day. This also applies to
#'   daily cost-period fragments that share one saved label start day. Calendar
#'   dates and cycle counters do not replace elapsed_day for discounting.
#'   Currency labels and price year are retained without conversion or inflation
#'   adjustment. The function does not mutate the simulation, draw random
#'   numbers, invoke callbacks or open connections.
#' @return A tma_simulation_summary containing byPatient, totals, costs,
#'   stateTime, initialPopulation and metadata. initialPopulation retains the
#'   first snapshot of every simulated patient, including identifiers and
#'   covariates, so compareScenarios() can check the starting populations.
#'   When an entry model was used, it instead retains the pre-entry profiles.
#'   These profiles add no health time. Explicit entry fees are included once
#'   using their baseline ledger rows, even for terminal entry outcomes.
#'   Metadata records this choice
#'   as initial_population_basis, either first_label or pre_entry.
#'   byPatient includes the final state. totals reports total and per_patient values for
#'   cost, discounted_cost, life_years, discounted_life_years, qalys and
#'   discounted_qalys. costs retains every configured component, including
#'   zeros. stateTime retains every model state. Metadata records the full-label
#'   convention, discount assumptions and the simulation's stop reason.
#'   When the bundle has an extrapolation model, metadata also retains its
#'   cutoff, source label, grouping rules and period definitions. Summarising
#'   the run does not add further extrapolated outcomes.
#'   derived_group_inputs records the raw inputs of generated grouping columns
#'   so scenario comparisons can check the patients without comparing different
#'   model-specific categorisations of the same inputs.
#' @seealso compareScenarios() for differences between compatible summaries.
#' @export
summariseSimulation <- function(simulation, aliveStates = NULL, utilities = NULL,
                                costDiscountRate = 0, healthDiscountRate = 0,
                                daysPerYear = 365.25, discountOriginDay = 0) {
  checked <- .tmaSimulationSummaryInput(simulation)
  population <- checked$population
  history <- checked$history
  ledger <- checked$ledger
  states <- checked$states
  n <- nrow(population)
  cycleLength <- checked$cycleLength
  for (name in c("costDiscountRate", "healthDiscountRate", "daysPerYear", "discountOriginDay")) {
    value <- get(name)
    if (!is.numeric(value) || is.complex(value) || inherits(value, "integer64") ||
        !is.null(dim(value)) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || (name == "daysPerYear" && value == 0)) {
      stop(name, " must be one finite ", if (name == "daysPerYear") "positive" else "nonnegative",
        " numeric value.", call. = FALSE)
    }
  }
  if (discountOriginDay > min(c(history$elapsed_day, ledger$elapsed_day))) {
    stop("discountOriginDay cannot be later than the earliest retained history or charge day.", call. = FALSE)
  }
  if (!is.null(aliveStates) && (!is.character(aliveStates) || !is.null(dim(aliveStates)) ||
      anyNA(aliveStates) || anyDuplicated(aliveStates) || any(!aliveStates %in% states))) {
    stop("aliveStates must explicitly contain unique known model state labels.", call. = FALSE)
  }
  if (!is.null(aliveStates) && any(aliveStates %in% checked$terminalStates)) {
    stop("aliveStates cannot include the simulation's terminalStates.", call. = FALSE)
  }
  if (!is.null(utilities)) {
    if (is.null(aliveStates)) stop("utilities require explicit aliveStates.", call. = FALSE)
    if (!is.numeric(utilities) || is.complex(utilities) || inherits(utilities, "integer64") ||
        !is.null(dim(utilities)) || is.null(names(utilities)) || anyNA(names(utilities)) ||
        anyDuplicated(names(utilities)) || !setequal(names(utilities), states) ||
        anyNA(utilities) || any(!is.finite(utilities) | utilities > 1)) {
      stop("utilities must supply one finite named value at most 1 for every model state.", call. = FALSE)
    }
    utilities <- utilities[states]
    if (any(utilities[!states %in% aliveStates] != 0)) {
      stop("utilities must be zero for every state outside aliveStates.", call. = FALSE)
    }
  }
  byPatient <- data.frame(simulation_id = population$simulation_id, stringsAsFactors = FALSE)
  if ("source_subject_id" %in% names(population)) {
    byPatient$source_subject_id <- population$source_subject_id
  }
  byPatient$first_day <- history$elapsed_day[checked$first]
  byPatient$last_label_day <- history$elapsed_day[checked$last]
  byPatient$labelled_cycles <- checked$counts
  byPatient$labelled_days <- checked$counts * cycleLength
  byPatient$final_state <- population$state_label
  byPatient$cost <- byPatient$discounted_cost <- rep(NA_real_, n)
  byPatient$life_years <- byPatient$discounted_life_years <- rep(NA_real_, n)
  byPatient$qalys <- byPatient$discounted_qalys <- rep(NA_real_, n)

  components <- checked$components
  costs <- data.frame(component = components, cost = numeric(length(components)),
    discounted_cost = numeric(length(components)), cost_per_patient = numeric(length(components)),
    discounted_cost_per_patient = numeric(length(components)), stringsAsFactors = FALSE)
  if (checked$costsConfigured) {
    costWeight <- exp(-log1p(costDiscountRate) * (ledger$elapsed_day - discountOriginDay) / daysPerYear)
    discounted <- ledger$amount * costWeight
    patient <- match(ledger$simulation_id, population$simulation_id)
    component <- match(ledger$component, components)
    byPatient$cost <- .tmaSimulationSummarySum(ledger$amount, patient, n)
    byPatient$discounted_cost <- .tmaSimulationSummarySum(discounted, patient, n)
    costs$cost <- .tmaSimulationSummarySum(ledger$amount, component, length(components))
    costs$discounted_cost <- .tmaSimulationSummarySum(discounted, component, length(components))
    costs$cost_per_patient <- costs$cost / n
    costs$discounted_cost_per_patient <- costs$discounted_cost / n
  }
  if (!is.null(aliveStates)) {
    alive <- history$state_label %in% aliveStates
    lifeYears <- numeric(nrow(history))
    lifeYears[alive] <- cycleLength / daysPerYear
    healthWeight <- exp(-log1p(healthDiscountRate) *
      (history$elapsed_day - discountOriginDay) / daysPerYear)
    byPatient$life_years <- .tmaSimulationSummarySum(lifeYears, checked$patient, n)
    byPatient$discounted_life_years <- .tmaSimulationSummarySum(
      lifeYears * healthWeight, checked$patient, n)
    if (!is.null(utilities)) {
      qalys <- lifeYears * unname(utilities[history$state_label])
      byPatient$qalys <- .tmaSimulationSummarySum(qalys, checked$patient, n)
      byPatient$discounted_qalys <- .tmaSimulationSummarySum(qalys * healthWeight, checked$patient, n)
    }
  }
  outcomes <- c("cost", "discounted_cost", "life_years", "discounted_life_years", "qalys", "discounted_qalys")
  totals <- data.frame(metric = outcomes, total = vapply(byPatient[outcomes], function(value) {
    if (all(is.na(value))) NA_real_ else sum(value)
  }, numeric(1)), row.names = NULL, stringsAsFactors = FALSE)
  totals$per_patient <- totals$total / n
  stateTime <- data.frame(state_label = states,
    labelled_cycles = tabulate(match(history$state_label, states), nbins = length(states)),
    stringsAsFactors = FALSE)
  stateTime$person_days <- stateTime$labelled_cycles * cycleLength
  if (any(!is.finite(byPatient$labelled_days)) || any(!is.finite(stateTime$person_days)) ||
      any(!is.finite(totals$total[!is.na(totals$total)])) || any(is.nan(totals$total))) {
    stop("Simulation summary exceeds finite numeric limits.", call. = FALSE)
  }
  initialPopulation <- checked$initialPopulation
  rownames(initialPopulation) <- NULL
  result <- structure(list(byPatient = byPatient, totals = totals, costs = costs, stateTime = stateTime,
    initialPopulation = initialPopulation,
    metadata = list(n = n, states = states, cycleLength = cycleLength,
      steps = simulation$metadata$steps, stop_reason = simulation$metadata$stop_reason,
      initial_population_basis = if (is.null(simulation$entry)) "first_label" else "pre_entry",
      terminalStates = checked$terminalStates,
      all_terminal = all(population$state_label %in% checked$terminalStates),
      aliveStates = aliveStates, utilities = utilities,
      costDiscountRate = costDiscountRate, healthDiscountRate = healthDiscountRate,
      daysPerYear = daysPerYear, discountOriginDay = discountOriginDay,
      currency = simulation$metadata$currency, priceYear = simulation$metadata$priceYear,
      costs_configured = checked$costsConfigured,
      timing = "each retained label represents one complete cycle, including the initial label",
      discount_timing = "recorded elapsed_day of each label or charge, from one shared origin",
      health_time = "full labelled cycles in explicitly supplied aliveStates",
      half_cycle_correction = "none")),
    class = "tma_simulation_summary")
  if (!is.null(simulation$metadata$extrapolation)) {
    result$metadata$extrapolation <- simulation$metadata$extrapolation
  }
  bundle <- simulation$.config$bundle
  groupRules <- c(bundle$groups, bundle$extrapolation$bundle$groups)
  if (length(groupRules)) {
    outputs <- unique(names(groupRules))
    phase <- .tmaBundlePhase(bundle, initialPopulation)
    costGroups <- intersect(names(bundle$groups), bundle$costModel$metadata$strata)
    outputs <- outputs[vapply(outputs, function(output) {
      if (!output %in% names(initialPopulation)) return(TRUE)
      generated <- rep(FALSE, nrow(initialPopulation))
      for (p in unique(phase)) {
        phaseGroups <- if (p == "base") names(bundle$groups) else
          names(bundle$extrapolation$bundle$groups)
        if (output %in% union(phaseGroups, costGroups)) generated[phase == p] <- TRUE
      }
      generated[initialPopulation$state_label %in% checked$terminalStates] <- FALSE
      !any(!is.na(initialPopulation[[output]]) & !generated)
    }, logical(1))]
    result$metadata$derived_group_inputs <- stats::setNames(lapply(outputs, function(output) {
      unique(vapply(groupRules[names(groupRules) == output], `[[`, character(1), "column"))
    }), outputs)
  }
  result
}

.tmaSummaryInitialPopulation <- function(simulation, first, states, terminalStates) {
  entry <- simulation$entry
  usedEntry <- identical(simulation$metadata$initialisation, "entry_model")
  if (usedEntry != !is.null(entry)) {
    stop("Simulation entry details must agree with its entry_model initialization.", call. = FALSE)
  }
  if (!usedEntry) return(first)
  if (!is.list(entry)) stop("Simulation entry details must be retained as a list.", call. = FALSE)
  baseline <- entry$population
  required <- c("simulation_id", "state_label", "cycle", "elapsed_day")
  if (!is.data.frame(baseline) || anyDuplicated(names(baseline)) ||
      !all(required %in% names(baseline)) || nrow(baseline) != nrow(first) ||
      !is.character(baseline$simulation_id) || anyNA(baseline$simulation_id) ||
      anyDuplicated(baseline$simulation_id) ||
      !setequal(baseline$simulation_id, first$simulation_id) ||
      !is.character(baseline$state_label) || anyNA(baseline$state_label) ||
      any(!baseline$state_label %in% states)) {
    stop("Entry population must retain one known pre-entry state per simulated patient.", call. = FALSE)
  }
  baseline <- baseline[match(first$simulation_id, baseline$simulation_id), , drop = FALSE]
  for (column in c("cycle", "elapsed_day")) {
    value <- baseline[[column]]
    if (!is.numeric(value) || is.complex(value) || anyNA(value) ||
        any(!is.finite(value) | value != 0) || any(first[[column]] != 0)) {
      stop("Entry population and first outcome labels must start at cycle and day zero.", call. = FALSE)
    }
  }
  unchanged <- setdiff(names(baseline), c("state_label", "stratum_id"))
  if (!all(unchanged %in% names(first)) ||
      ("source_subject_id" %in% names(first) && !"source_subject_id" %in% names(baseline)) ||
      !all(vapply(unchanged, function(column) isTRUE(all.equal(
        baseline[[column]], first[[column]], tolerance = 0, check.attributes = FALSE)), logical(1)))) {
    stop("Pre-entry identities, covariates and clocks must agree with the first outcome snapshots.", call. = FALSE)
  }
  draws <- entry$transitions
  active <- !baseline$state_label %in% terminalStates
  clocks <- c("source_cycle", "destination_cycle", "source_day", "destination_day")
  fields <- c("simulation_id", "source_state", "destination_state", "transition_probability", clocks)
  if (!is.data.frame(draws) || anyDuplicated(names(draws)) || !all(fields %in% names(draws)) ||
      !is.character(draws$simulation_id) || anyNA(draws$simulation_id) ||
      anyDuplicated(draws$simulation_id) ||
      !setequal(draws$simulation_id, baseline$simulation_id[active])) {
    stop("Entry draws must identify each nonterminal pre-entry patient exactly once.", call. = FALSE)
  }
  draws <- draws[match(baseline$simulation_id[active], draws$simulation_id), , drop = FALSE]
  if (!all(vapply(draws[clocks], function(value) is.numeric(value) &&
      !is.complex(value) && !anyNA(value) && all(is.finite(value) & value == 0), logical(1)))) {
    stop("Entry transition source and destination clocks must remain at cycle and day zero.", call. = FALSE)
  }
  entryIds <- .tmaSummaryEntryModelIds(entry, baseline[active, , drop = FALSE], draws, states)
  for (column in c("source_state", "destination_state")) {
    if (!is.character(draws[[column]]) || anyNA(draws[[column]]) ||
        any(!draws[[column]] %in% states)) stop("Entry draws contain unknown state labels.", call. = FALSE)
  }
  expected <- numeric(nrow(draws))
  for (id in unique(entryIds)) {
    selected <- which(entryIds == id)
    matrix <- .tmaEntryMatrix(entry, id)
    expected[selected] <- matrix[cbind(match(draws$source_state[selected], rownames(matrix)),
      match(draws$destination_state[selected], colnames(matrix)))]
  }
  expected <- .tmaSummaryEntryEffects(simulation, draws,
    baseline[active, , drop = FALSE], expected, entryIds)
  probability <- draws$transition_probability
  if (!is.numeric(probability) || is.complex(probability) || anyNA(probability) ||
      any(!is.finite(probability) | probability <= 0 | probability > 1) ||
      anyNA(expected) || any(!is.finite(expected)) || any(probability != expected) ||
      any(draws$source_state != baseline$state_label[active]) ||
      any(draws$destination_state != first$state_label[active]) ||
      any(baseline$state_label[!active] != first$state_label[!active])) {
    stop("Entry draws must reconcile with pre-entry states, first outcomes and stored probabilities.", call. = FALSE)
  }
  baseline
}

.tmaSimulationSummaryInput <- function(simulation) {
  if (!inherits(simulation, "tma_simulation") || !is.list(simulation$metadata) ||
      !is.list(simulation$.config) || !is.list(simulation$.config$bundle) ||
      !is.list(simulation$.config$bundle$metadata) ||
      !"costRules" %in% names(simulation$.config)) {
    stop("simulation must retain the data and configuration from simulateTrajectories().", call. = FALSE)
  }
  model <- simulation$.config$bundle$metadata
  if (!identical(simulation$metadata$extrapolation,
                 .tmaBundleExtrapolationInfo(simulation$.config$bundle))) {
    stop("Simulation extrapolation metadata must agree with the saved bundle.", call. = FALSE)
  }
  states <- model$states
  if (!is.character(states) || !is.null(dim(states)) || !length(states) ||
      anyNA(states) || anyDuplicated(states) ||
      any(!nzchar(trimws(states)))) {
    stop("Simulation model states must be unique nonempty labels.", call. = FALSE)
  }
  n <- .tmaProjectionInteger(simulation$metadata$n, "simulation metadata$n", 1)
  steps <- .tmaProjectionInteger(simulation$metadata$steps, "simulation metadata$steps", 0)
  cycleLength <- .tmaProjectionInteger(simulation$metadata$cycleLength, "simulation cycleLength", 1)
  if (length(model$cycleLength) != 1L || !is.numeric(model$cycleLength) ||
      is.na(model$cycleLength) || model$cycleLength != cycleLength) {
    stop("Simulation and bundle cycleLength must agree.", call. = FALSE)
  }
  terminalStates <- simulation$metadata$terminalStates
  if (!is.character(terminalStates) || !is.null(dim(terminalStates)) ||
      anyNA(terminalStates) || anyDuplicated(terminalStates) ||
      any(!terminalStates %in% states)) {
    stop("Simulation terminalStates must contain unique known model states.", call. = FALSE)
  }
  if (!is.character(simulation$metadata$stop_reason) || length(simulation$metadata$stop_reason) != 1L ||
      is.na(simulation$metadata$stop_reason) || !nzchar(simulation$metadata$stop_reason)) {
    stop("Simulation metadata must retain a nonempty stop_reason.", call. = FALSE)
  }
  population <- simulation$population
  history <- simulation$trajectories
  ledger <- simulation$costs
  for (name in c("population", "history", "ledger")) {
    value <- get(name)
    required <- c("simulation_id", "cycle", "elapsed_day", "state_label")
    if (name == "ledger") required <- c(required, "component", "type", "amount")
    if (!is.data.frame(value) || anyDuplicated(names(value)) ||
        !all(required %in% names(value)) || (name != "ledger" && !nrow(value))) {
      stop("Simulation ", name, " must retain its required snapshot or charge columns.", call. = FALSE)
    }
    if (!is.character(value$simulation_id) || anyNA(value$simulation_id) ||
        any(!nzchar(trimws(value$simulation_id))) || !is.character(value$state_label) ||
        anyNA(value$state_label) || any(!value$state_label %in% states)) {
      stop("Simulation ", name, " contains invalid identifiers or unknown state labels.", call. = FALSE)
    }
    for (field in c("cycle", "elapsed_day")) {
      x <- value[[field]]
      if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") || !is.null(dim(x)) ||
          anyNA(x) || any(!is.finite(x) | x < 0 | x != trunc(x) | x >= 2^53)) {
        stop("Simulation ", name, "$", field, " must contain exact nonnegative integers.", call. = FALSE)
      }
    }
  }
  ids <- population$simulation_id
  if (nrow(population) != n || anyDuplicated(ids) ||
      !setequal(history$simulation_id, ids) || any(!ledger$simulation_id %in% ids)) {
    stop("Simulation histories and charges must use the original unique simulation_id population.", call. = FALSE)
  }
  history <- history[order(match(history$simulation_id, ids), history$cycle), , drop = FALSE]
  patient <- match(history$simulation_id, ids)
  samePatient <- diff(patient) == 0
  cycleIncrement <- diff(history$cycle)[samePatient]
  if (any(cycleIncrement == 0)) {
    stop("Simulation trajectories must have a unique simulation_id/cycle row.", call. = FALSE)
  }
  counts <- tabulate(patient, nbins = n)
  if (any(counts != steps + 1)) {
    stop("Each simulation_id must retain steps + 1 labelled snapshots.", call. = FALSE)
  }
  if (any(cycleIncrement != 1) ||
      any(diff(history$elapsed_day)[samePatient] != cycleLength) ||
      any(history$elapsed_day %% cycleLength != 0)) {
    stop("Simulation trajectories must have continuous cycle and elapsed_day increments matching cycleLength.", call. = FALSE)
  }
  first <- cumsum(c(1, utils::head(counts, -1L)))
  last <- cumsum(counts)
  if (!setequal(names(history), names(population)) ||
      !all(vapply(names(population), function(column) isTRUE(all.equal(
        history[[column]][last], population[[column]], tolerance = 0,
        check.attributes = FALSE)), logical(1)))) {
    stop("Final trajectory snapshots must match population for every simulation_id.", call. = FALSE)
  }
  if ("source_subject_id" %in% names(population)) {
    source <- population$source_subject_id
    if (!is.character(source) || anyNA(source) || any(!nzchar(trimws(source))) ||
        !is.character(history$source_subject_id) || anyNA(history$source_subject_id) ||
        any(history$source_subject_id != source[patient])) {
      stop("source_subject_id must remain consistent within each simulation_id.", call. = FALSE)
    }
  }
  .tmaCostAmounts(ledger$amount, "Simulation charge amounts")
  for (field in c("component", "type")) {
    if (!is.character(ledger[[field]]) || anyNA(ledger[[field]]) ||
        any(!nzchar(trimws(ledger[[field]])))) {
      stop("Simulation charge ", field, " must contain nonempty labels.", call. = FALSE)
    }
  }
  initialPopulation <- .tmaSummaryInitialPopulation(simulation,
    history[first, , drop = FALSE], states, terminalStates)
  rules <- .tmaSimulationCostRules(simulation$.config$costRules, states)
  entryComponents <- names(rules)[vapply(rules, function(rule) rule$type == "entry", logical(1))]
  entryCharge <- ledger$type == "entry"
  if (any(entryCharge != (ledger$component %in% entryComponents)) ||
      (length(entryComponents) && is.null(simulation$entry))) {
    stop("Entry charges must use configured entry rules and a retained pre-entry population.", call. = FALSE)
  }
  if (any(entryCharge)) {
    fee <- ledger[entryCharge, , drop = FALSE]
    baseline <- initialPopulation[match(fee$simulation_id, initialPopulation$simulation_id), , drop = FALSE]
    if (anyDuplicated(fee[c("simulation_id", "component")]) ||
        any(fee$cycle != 0 | fee$elapsed_day != 0) ||
        any(fee$state_label != baseline$state_label) ||
        any(baseline$state_label %in% terminalStates)) {
      stop("Entry fees must match nonterminal baseline states once per component and patient at day zero.", call. = FALSE)
    }
    for (component in unique(fee$component)) {
      rule <- rules[[component]]
      selected <- fee$component == component
      if (any(fee$amount[selected] != rule$amount) || rule$startDay > 0 || rule$endDay <= 0 ||
          (!is.null(rule$states) && any(!fee$state_label[selected] %in% rule$states))) {
        stop("Entry fee amounts, states and days must agree with their configured rules.", call. = FALSE)
      }
    }
  }
  if (nrow(ledger)) {
    chargedPatient <- match(ledger$simulation_id, ids)
    offset <- ledger$cycle - history$cycle[first[chargedPatient]]
    if (any(offset < 0 | offset >= counts[chargedPatient])) {
      stop("Every charge must align with a retained snapshot's cycle, day and state.", call. = FALSE)
    }
    matched <- first[chargedPatient] + offset
    if (any(ledger$elapsed_day[!entryCharge] != history$elapsed_day[matched[!entryCharge]]) ||
        any(ledger$state_label[!entryCharge] != history$state_label[matched[!entryCharge]])) {
      stop("Every charge must align with a retained snapshot's cycle, day and state.", call. = FALSE)
    }
  }
  hasModel <- !is.null(simulation$.config$bundle$costModel)
  expectedComponents <- c(if (hasModel) "healthcare", names(rules))
  storedTotals <- simulation$totals
  if (!is.data.frame(storedTotals) || anyDuplicated(names(storedTotals)) ||
      !all(c("component", "amount") %in% names(storedTotals)) ||
      !is.character(storedTotals$component) || anyNA(storedTotals$component) ||
      anyDuplicated(storedTotals$component) || anyDuplicated(expectedComponents) ||
      !setequal(storedTotals$component, expectedComponents) ||
      any(!ledger$component %in% storedTotals$component)) {
    stop("Simulation totals and charge components must match the configured cost rules and model.", call. = FALSE)
  }
  .tmaCostAmounts(storedTotals$amount, "Simulation stored cost totals")
  components <- storedTotals$component
  recomputed <- .tmaSimulationSummarySum(ledger$amount, match(ledger$component, components), length(components))
  if (any(abs(recomputed - storedTotals$amount) >
      1e-10 * pmax(1, abs(recomputed), abs(storedTotals$amount)))) {
    stop("Simulation stored totals must agree with the retained charge ledger.", call. = FALSE)
  }
  costsConfigured <- length(expectedComponents) > 0
  if (costsConfigured || !is.null(simulation$metadata$currency)) {
    currency <- simulation$metadata$currency
    if (!is.character(currency) || length(currency) != 1L || is.na(currency) || !nzchar(trimws(currency))) {
      stop("Configured costs need a nonempty simulation currency label.", call. = FALSE)
    }
  }
  if (costsConfigured || !is.null(simulation$metadata$priceYear)) {
    .tmaProjectionInteger(simulation$metadata$priceYear, "simulation priceYear", 1)
  }
  list(population = population, history = history, ledger = ledger, states = states,
    terminalStates = terminalStates, cycleLength = cycleLength, patient = patient,
    counts = counts, first = first, last = last, components = components,
    costsConfigured = costsConfigured, initialPopulation = initialPopulation)
}

.tmaSimulationSummarySum <- function(values, group, size) {
  if (any(!is.finite(values))) stop("Simulation summary contributions must be finite.", call. = FALSE)
  result <- numeric(size)
  if (length(values)) {
    sums <- rowsum(values, group, reorder = FALSE)
    result[as.integer(rownames(sums))] <- sums[, 1L]
  }
  if (any(!is.finite(result))) stop("Simulation summary exceeds finite numeric limits.", call. = FALSE)
  result
}
