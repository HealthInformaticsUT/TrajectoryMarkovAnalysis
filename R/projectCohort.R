#' Project a cohort through period-specific transition matrices
#'
#' @param transitions A result from estimateTransitions(),
#'   estimateStageTransitions() or composeTransitionStages(), or an unstratified
#'   createMortalityTransitions()
#'   model. Supplied models need an explicit initial distribution or a separate
#'   entry model. Select one stratum's model explicitly to project that group.
#' @param nCycles Number of transition steps, a finite nonnegative integer.
#' @param initial Optional named probability vector. Omitted known states receive
#'   zero probability. NULL uses entryCounts probabilities for known first-cycle
#'   labels, including first-cycle terminal labels, not a pre-event baseline.
#' @param cohortSize Positive exact integer cohort size. Defaults to 1.
#' @param transitionEffects Optional rules described in transition-effects.
#'   An optional stage targets a composed stage. Eligibility callbacks require
#'   patient simulation and are not accepted here. Effects leave the input
#'   model unchanged.
#' @param entry Optional result from estimateEntryTransitions(). Its eligible
#'   baseline distribution passes through its entry matrix once to initialise
#'   cycle zero. Do not supply a non-NULL initial together with entry.
#'   For stratified entry, select one model from entry$models, or use
#'   simulateTrajectories() with explicit baseline patient profiles. This
#'   function does not pool entry groups.
#' @details Each step uses the matrix for the source cycle's start day. An
#'   unestimated row is allowed only while its state has zero projected mass.
#'   The last period's matrix applies to every requested step within that period;
#'   an infinite final period does not establish empirical lifetime validity.
#'   Probabilities are never normalised. Reward and cost accounting are separate;
#'   see summariseOutcomes() and projectCosts().
#'   Mortality extrapolations from extrapolateMortality() require current
#'   patient profiles and must use simulateTrajectories(). This function does
#'   not substitute an average background mortality profile.
#'
#'   With entry supplied, cycle zero still means the first recorded cycle label.
#'   Entry adds no timepoint and nCycles still counts subsequent adjacent-cycle
#'   steps. Thus nCycles = 0 applies entry only. Entry outcome probabilities are
#'   conditional on an evaluable first cycle, without censoring adjustment or
#'   reconstruction of within-cycle event timing. Initialising from the eligible
#'   subset does not refit the adjacent-cycle matrices. Matching source counts
#'   cannot establish that the two models used identical patients.
#'
#'   Flows report every state pair at each step. transition_probability is
#'   conditional on the source state; probability_mass is the share of the
#'   whole projected cohort taking that step, and expected_count multiplies
#'   that share by cohortSize. Unoccupied unestimated rows retain NA conditional
#'   probabilities but have zero joint mass. Absorbing self-transitions retain
#'   existing occupancy; they are not new deaths or other terminal events.
#'   Entry flows occur once before cycle zero, with missing source cycle, source
#'   day and period. With no entry and nCycles = 0, flows is an empty table.
#'
#'   Composed models also return stageFlows. These are movements within each
#'   adjacent step, not additional cycles or person-time. The existing flows
#'   table still describes net cycle outcomes. Intermediate changes can occur
#'   even when the net outcome is a self-transition. Stage matrices are applied
#'   in their declared order using the source cycle's period. Entry remains a
#'   separate operation and is not decomposed into stages. Existing costing and
#'   occupancy comparisons use cycle outcomes, not stageFlows.
#' @return A list containing occupancy, steps, flows, metadata and diagnostics.
#'   For composed models, stageMatrices and stageFlows retain the stage models
#'   and their expected movements. Both are NULL for ordinary models. With
#'   stage effects, stageCycleMatrices retains the matrices actually used at
#'   each source cycle and cycleMatrices retains their products. stageMatrices
#'   retains the original stage definitions. Net-cycle effects leave stageFlows
#'   unavailable because intermediate paths are no longer defined. stageFlows
#'   is an empty table when a composed model takes no adjacent steps.
#'   Occupancy includes cycle zero and nCycles subsequent distributions.
#'   Metadata preserves the source subject count when available and the entry
#'   denominator when default initialisation uses entryCounts.
#'   When used, entry retains its baseline distribution, matrix, eligibility
#'   description, coverage and eligible subject identifiers. These identifiers
#'   must remain within the permitted patient-level analysis environment.
#' @export
projectCohort <- function(transitions, nCycles, initial = NULL, cohortSize = 1,
                           entry = NULL, transitionEffects = NULL) {
  if (inherits(transitions, "tma_mortality_bundle") ||
      (inherits(transitions, "tma_model_bundle") &&
       .tmaBundleUsesFutureMortality(transitions))) {
    stop("Mortality extrapolation requires current patient profiles. Use simulateTrajectories() with the complete bundle from extrapolateMortality().", call. = FALSE)
  }
  tolerance <- 1e-10
  if (inherits(entry, "tma_stratified_entry")) {
    stop("Stratified entry requires explicit patient profiles in simulateTrajectories(), or select one group from entry$models for projectCohort(). Entry groups are not pooled.", call. = FALSE)
  }
  if (!is.null(entry) && !is.null(initial)) {
    stop("Supply either entry or initial, not both.", call. = FALSE)
  }
  nCycles <- .tmaProjectionInteger(nCycles, "nCycles", minimum = 0)
  cohortSize <- .tmaProjectionInteger(cohortSize, "cohortSize", minimum = 1)
  if (!is.list(transitions) || !is.list(transitions$metadata) ||
      !all(c("states", "absorbingStates", "cycleLength") %in% names(transitions$metadata))) {
    stop("transitions must include matrices, periods and metadata from estimateTransitions().", call. = FALSE)
  }
  states <- transitions$metadata$states
  absorbingStates <- transitions$metadata$absorbingStates
  dictionary <- .tmaStateDictionary(character(), states, absorbingStates)
  if (is.null(states) || !length(states)) {
    stop("transitions must declare at least one clinical state.", call. = FALSE)
  }
  states <- dictionary$states
  cycleLength <- .tmaProjectionInteger(transitions$metadata$cycleLength,
                                        "metadata$cycleLength", minimum = 1)
  if ((nCycles + 1) * length(states) > .Machine$integer.max ||
      (nCycles + as.integer(!is.null(entry))) * length(states)^2 > .Machine$integer.max ||
      nCycles * cycleLength >= 2^53) {
    stop("The requested horizon exceeds supported output indexing or exact elapsed days.", call. = FALSE)
  }
  suppliedPeriods <- transitions$periods
  if (!is.data.frame(suppliedPeriods) || !nrow(suppliedPeriods) ||
      !all(c("period", "start_day", "end_day") %in% names(suppliedPeriods)) ||
      !is.numeric(suppliedPeriods$end_day) || is.complex(suppliedPeriods$end_day) ||
      inherits(suppliedPeriods$end_day, "integer64") || anyNA(suppliedPeriods$end_day)) {
    stop("transitions$periods must describe contiguous periods from day 0 to Inf.", call. = FALSE)
  }
  periods <- .tmaPeriods(c(suppliedPeriods$start_day, tail(suppliedPeriods$end_day, 1L)),
                         suppliedPeriods$period, cycleLength)
  if (any(suppliedPeriods$end_day != periods$end_day)) {
    stop("transitions$periods must be contiguous, without gaps or overlaps.", call. = FALSE)
  }
  matrices <- transitions$matrices
  if (!is.list(matrices) || length(matrices) != nrow(periods) ||
      is.null(names(matrices)) || anyNA(names(matrices)) || anyDuplicated(names(matrices)) ||
      !setequal(names(matrices), periods$period)) {
    stop("transitions$matrices must contain one named matrix for every period.", call. = FALSE)
  }
  matrices <- matrices[periods$period]
  missingRows <- vector("list", length(matrices))
  for (i in seq_along(matrices)) {
    matrix <- matrices[[i]]
    if (!is.matrix(matrix) || !is.numeric(matrix) || is.complex(matrix) ||
        inherits(matrix, "integer64") ||
        !identical(dim(matrix), rep(length(states), 2L)) ||
        is.null(rownames(matrix)) || is.null(colnames(matrix)) ||
        anyDuplicated(rownames(matrix)) || anyDuplicated(colnames(matrix)) ||
        !setequal(rownames(matrix), states) || !setequal(colnames(matrix), states)) {
      stop("Matrix for period '", periods$period[i], "' must have the declared state names on both axes.", call. = FALSE)
    }
    matrix <- matrix[states, states, drop = FALSE]
    missingCount <- rowSums(is.na(matrix))
    if (any(missingCount > 0L & missingCount < length(states))) {
      stop("Matrix for period '", periods$period[i], "' contains a partially missing probability row.", call. = FALSE)
    }
    values <- matrix[!is.na(matrix)]
    if (any(is.nan(matrix)) || any(!is.finite(values) | values < 0 | values > 1)) {
      stop("Matrix probabilities must be finite values in [0, 1], or entire NA rows.", call. = FALSE)
    }
    missingRows[[i]] <- missingCount == length(states)
    complete <- matrix[!missingRows[[i]], , drop = FALSE]
    if (any(abs(rowSums(complete) - 1) > tolerance)) {
      stop("Complete probability rows must sum to 1 within 1e-10; period '",
           periods$period[i], "'.", call. = FALSE)
    }
    for (state in absorbingStates) {
      if (anyNA(matrix[state, ]) || any(matrix[state, ] != as.numeric(states == state))) {
        stop("Absorbing state '", state, "' must have an identity row in period '", periods$period[i], "'.", call. = FALSE)
      }
    }
    matrices[[i]] <- matrix
  }

  stageMatrices <- NULL
  if (identical(transitions$metadata$estimator, "specified_stages") &&
      is.null(transitions$stageMatrices)) {
    stop("A composed model must retain its stageMatrices. Rebuild it with composeTransitionStages().", call. = FALSE)
  }
  if (!is.null(transitions$stageMatrices)) {
    stageDefinition <- .tmaStageDefinition(transitions$stageMatrices, periods, states, absorbingStates)
    stageMatrices <- stageDefinition$stageMatrices
    if (!identical(transitions$metadata$stage_order, names(stageMatrices))) {
      stop("The declared stage order must match stageMatrices. Rebuild the composed model after changing its stages.", call. = FALSE)
    }
    for (i in seq_along(matrices)) {
      expected <- stageDefinition$matrices[[i]]
      actual <- matrices[[i]]
      if (any(is.na(actual) != is.na(expected)) ||
          any(abs(actual[!is.na(actual)] - expected[!is.na(expected)]) > tolerance)) {
        stop("The combined matrix disagrees with its stages in period '", periods$period[i],
             "'. Rebuild it with composeTransitionStages() after changing probabilities.", call. = FALSE)
      }
    }
    if (nCycles * length(stageMatrices) * length(states)^2 > .Machine$integer.max) {
      stop("The requested horizon exceeds supported stage-flow indexing.", call. = FALSE)
    }
    if (is.null(initial) && is.null(entry)) {
      stop("A composed stage model needs an explicit initial distribution or a separate entry model.", call. = FALSE)
    }
  }

  effects <- .tmaTransitionEffects(transitionEffects, states, absorbingStates, cycleLength)
  .tmaCheckEffectStages(effects, names(stageMatrices))
  if (any(vapply(effects, function(rule) !is.null(rule$eligible), logical(1)))) {
    stop("Eligibility callbacks require patient profiles in simulateTrajectories(). Select a homogeneous projection population explicitly.", call. = FALSE)
  }
  if (is.null(entry) && any(vapply(effects, function(rule)
      rule$applyAt %in% c("entry", "both"), logical(1)))) {
    stop("Entry effects require an explicit entry model.", call. = FALSE)
  }
  netEffects <- effects[is.na(.tmaEffectStages(effects)) &
    vapply(effects, function(rule) rule$applyAt %in% c("transitions", "both"), logical(1))]
  retainStages <- !is.null(stageMatrices) && !length(netEffects)
  entryDetails <- NULL
  initialSource <- if (!is.null(entry)) "entry_model" else if (is.null(initial)) "entryCounts" else "supplied"
  if (initialSource == "entry_model") {
    entryResult <- .tmaProjectionEntry(entry, states, absorbingStates, cycleLength, tolerance)
    initial <- entryResult$distribution
    entryDetails <- entryResult$entry
    if (length(effects)) {
      changed <- .tmaEffectMatrix(entryDetails$matrix, effects, 0, states,
        source = entryDetails$baseline_distribution, applyAt = "entry")
      entryDetails$matrix <- changed$matrix
      occupied <- which(entryDetails$baseline_distribution > 0)
      initial <- as.numeric(entryDetails$baseline_distribution[occupied] %*%
        changed$matrix[occupied, , drop = FALSE])
      names(initial) <- states
    }
  } else if (is.null(initial)) {
    entries <- transitions$entryCounts
    if (!is.data.frame(entries) || !all(c("state", "probability") %in% names(entries)) ||
        !is.character(entries$state) || anyNA(entries$state) || anyDuplicated(entries$state) ||
        !setequal(entries$state, states)) {
      stop("Default initialization requires entryCounts with one probability per declared state.", call. = FALSE)
    }
    initial <- stats::setNames(entries$probability, entries$state)
  }
  if (!is.numeric(initial) || is.complex(initial) || inherits(initial, "integer64") ||
      !is.null(dim(initial)) || !length(initial) ||
      is.null(names(initial)) || anyNA(names(initial)) || anyDuplicated(names(initial)) ||
      any(!nzchar(trimws(names(initial)))) || any(!names(initial) %in% states) ||
      anyNA(initial) || any(!is.finite(initial) | initial < 0 | initial > 1) ||
      abs(sum(initial) - 1) > tolerance) {
    stop("initial must be a uniquely named numeric probability vector over known states, summing to 1 within 1e-10.", call. = FALSE)
  }
  initialDistribution <- stats::setNames(rep(0, length(states)), states)
  initialDistribution[names(initial)] <- initial
  denominator <- NULL
  if (!is.null(transitions$diagnostics) && !is.list(transitions$diagnostics)) {
    stop("transitions$diagnostics must be a list when provided.", call. = FALSE)
  }
  preparation <- transitions$diagnostics$preparation
  if (!is.null(preparation) && !is.list(preparation)) {
    stop("diagnostics$preparation must be a list when provided.", call. = FALSE)
  }
  totalSubjects <- preparation$n_subjects
  if (!is.null(totalSubjects)) {
    totalSubjects <- .tmaProjectionInteger(totalSubjects, "preparation$n_subjects", minimum = 1)
  }
  entrySourceMatches <- NA
  if (!is.null(entryDetails) && !is.null(totalSubjects)) {
    entrySourceMatches <- entryDetails$source_subjects == totalSubjects
    if (!entrySourceMatches) {
      warning("Entry and adjacent-cycle models have different source subject counts. ",
              "These counts do not establish patient-level identity.", call. = FALSE)
    }
  }
  if (initialSource == "entryCounts") {
    if ("denominator" %in% names(entries)) {
      if (!is.numeric(entries$denominator) || is.complex(entries$denominator) ||
          inherits(entries$denominator, "integer64") || anyNA(entries$denominator) ||
          length(unique(entries$denominator)) != 1L) {
        stop("entryCounts$denominator must contain one common positive integer denominator.", call. = FALSE)
      }
      denominator <- .tmaProjectionInteger(entries$denominator[1L],
                                            "entryCounts$denominator", minimum = 1)
    }
    if (!is.null(denominator) && !is.null(totalSubjects)) {
      if (denominator > totalSubjects) {
        stop("entryCounts denominator cannot exceed preparation$n_subjects.", call. = FALSE)
      }
      if (denominator < totalSubjects) {
        warning("Default initialization uses known first-cycle labels only: ", denominator,
                " of ", totalSubjects, " subjects.", call. = FALSE)
      }
    }
  }

  sourceCycles <- seq_len(nCycles) - 1L
  sourceDays <- sourceCycles * cycleLength
  periodIndex <- .tmaPeriodIndex(sourceDays, periods)
  steps <- data.frame(source_cycle = sourceCycles, source_day = sourceDays,
                       destination_cycle = sourceCycles + 1L,
                       destination_day = (sourceCycles + 1) * cycleLength,
                       period = periods$period[periodIndex], stringsAsFactors = FALSE)
  probability <- matrix(NA_real_, nrow = nCycles + 1, ncol = length(states))
  probability[1L, ] <- initialDistribution
  maxError <- abs(sum(initialDistribution) - 1)
  cycleMatrices <- vector("list", nCycles)
  stageCycleMatrices <- if (retainStages) vector("list", nCycles) else NULL
  for (i in seq_len(nCycles)) {
    occupied <- which(probability[i, ] > 0)
    modelIndex <- periodIndex[i]
    selected <- matrices[[modelIndex]]
    if (retainStages) {
      stages <- lapply(stageMatrices, function(x) x[[modelIndex]])
      altered <- .tmaStageCycle(stages, probability[i, ], effects, sourceDays[i], states)
      stageCycleMatrices[[i]] <- altered$stages
      selected <- .tmaStageProduct(altered$stages, states)
    } else if (length(effects)) {
      selected <- .tmaEffectMatrix(selected, effects, sourceDays[i], states,
        source = probability[i, ])$matrix
    }
    cycleMatrices[[i]] <- selected
    unavailable <- occupied[rowSums(is.na(selected[occupied, , drop = FALSE])) > 0]
    if (length(unavailable)) {
      stop("Unestimated transition row for state(s) ", paste(states[unavailable], collapse = ", "),
           " in period '", periods$period[modelIndex], "' at source cycle ", sourceCycles[i], ".", call. = FALSE)
    }
    nextProbability <- as.numeric(probability[i, occupied] %*%
                                    selected[occupied, , drop = FALSE])
    error <- abs(sum(nextProbability) - 1)
    if (any(!is.finite(nextProbability)) || !is.finite(error) || error > 1e-8) {
      stop("Projected probabilities are nonfinite or fail to sum to 1 within 1e-8 at destination cycle ",
           sourceCycles[i] + 1, ".", call. = FALSE)
    }
    probability[i + 1L, ] <- nextProbability
    maxError <- max(maxError, error)
  }
  cycles <- seq.int(0, nCycles)
  occupancy <- data.frame(cycle = rep(cycles, each = length(states)),
                           elapsed_day = rep(cycles * cycleLength, each = length(states)),
                           state = rep(states, times = nCycles + 1),
                           probability = as.vector(t(probability)), stringsAsFactors = FALSE)
  occupancy$expected_count <- occupancy$probability * cohortSize
  flowSteps <- data.frame(stage = rep("transition", nCycles), steps,
                          stringsAsFactors = FALSE)
  if (!is.null(entryDetails)) {
    flowSteps <- rbind(data.frame(stage = "entry", source_cycle = NA_integer_,
                                  source_day = NA_real_, destination_cycle = 0L,
                                  destination_day = 0, period = NA_character_,
                                  stringsAsFactors = FALSE), flowSteps)
  }
  nStates <- length(states)
  nPairs <- nStates^2
  flows <- flowSteps[rep(seq_len(nrow(flowSteps)), each = nPairs), , drop = FALSE]
  flows$from_state <- rep(states, times = nStates * nrow(flowSteps))
  flows$to_state <- rep(rep(states, each = nStates), times = nrow(flowSteps))
  flows$transition_probability <- rep(NA_real_, nrow(flows))
  flows$probability_mass <- numeric(nrow(flows))
  for (i in seq_len(nrow(flowSteps))) {
    if (flowSteps$stage[i] == "entry") {
      sourceProbability <- entryDetails$baseline_distribution
      conditional <- entryDetails$matrix
    } else {
      sourceRow <- flowSteps$source_cycle[i] + 1L
      sourceProbability <- probability[sourceRow, ]
      conditional <- cycleMatrices[[sourceRow]]
    }
    joint <- matrix(0, nStates, nStates)
    occupied <- which(sourceProbability > 0)
    joint[occupied, ] <- conditional[occupied, , drop = FALSE] * sourceProbability[occupied]
    rows <- (i - 1) * nPairs + seq_len(nPairs)
    flows$transition_probability[rows] <- as.vector(conditional)
    flows$probability_mass[rows] <- as.vector(joint)
  }
  flows$expected_count <- flows$probability_mass * cohortSize
  rownames(flows) <- NULL
  stageResult <- if (retainStages) {
    .tmaProjectStageFlows(stageMatrices, steps, probability, states, cohortSize,
      stageCycleMatrices = stageCycleMatrices)
  } else NULL
  result <- list(occupancy = occupancy, steps = steps, flows = flows, entry = entryDetails,
       stageMatrices = stageMatrices, stageFlows = stageResult$flows,
       cycleMatrices = cycleMatrices, stageCycleMatrices = stageCycleMatrices,
       metadata = list(initial_distribution = initialDistribution, initial_source = initialSource,
                       initial_denominator = denominator, source_n_subjects = totalSubjects,
                       nCycles = nCycles, cohortSize = cohortSize, cycleLength = cycleLength,
                       states = states, absorbingStates = absorbingStates, periods = periods,
                       stage_order = names(stageMatrices),
                       transition_effects = effects,
                       stage_paths = if (retainStages) "retained" else if (!is.null(stageMatrices))
                         "unavailable after net-cycle effects" else "not specified"),
       diagnostics = list(max_probability_sum_error = maxError,
                          max_stage_endpoint_error = stageResult$max_endpoint_error,
                          entry_source_subject_count_matches = entrySourceMatches))
  structure(result, class = "tma_projection")
}

.tmaProjectStageFlows <- function(stageMatrices, steps, probability, states, cohortSize,
                                   stageCycleMatrices = NULL) {
  nStages <- length(stageMatrices)
  nStates <- length(states)
  nPairs <- nStates^2
  flows <- steps[rep(seq_len(nrow(steps)), each = nStages * nPairs), , drop = FALSE]
  flows$stage_index <- rep(rep(seq_len(nStages), each = nPairs), times = nrow(steps))
  flows$stage <- names(stageMatrices)[flows$stage_index]
  flows$from_state <- rep(states, times = nStates * nStages * nrow(steps))
  flows$to_state <- rep(rep(states, each = nStates), times = nStages * nrow(steps))
  flows$transition_probability <- rep(NA_real_, nrow(flows))
  flows$probability_mass <- numeric(nrow(flows))
  maxError <- 0
  for (i in seq_len(nrow(steps))) {
    source <- probability[i, ]
    for (j in seq_len(nStages)) {
      conditional <- if (is.null(stageCycleMatrices)) stageMatrices[[j]][[steps$period[i]]] else
        stageCycleMatrices[[i]][[j]]
      occupied <- which(source > 0)
      if (anyNA(conditional[occupied, , drop = FALSE])) {
        stop("Missing probabilities in reachable stage '", names(stageMatrices)[j],
             "' at source cycle ", steps$source_cycle[i], ".", call. = FALSE)
      }
      joint <- matrix(0, nStates, nStates)
      joint[occupied, ] <- conditional[occupied, , drop = FALSE] * source[occupied]
      rows <- ((i - 1) * nStages + j - 1) * nPairs + seq_len(nPairs)
      flows$transition_probability[rows] <- as.vector(conditional)
      flows$probability_mass[rows] <- as.vector(joint)
      source <- colSums(joint)
    }
    error <- max(abs(source - probability[i + 1L, ]))
    if (!is.finite(error) || error > 1e-8) {
      stop("Stage movements disagree with the combined cycle projection at destination cycle ",
           steps$destination_cycle[i], ".", call. = FALSE)
    }
    maxError <- max(maxError, error)
  }
  flows$expected_count <- flows$probability_mass * cohortSize
  rownames(flows) <- NULL
  list(flows = flows, max_endpoint_error = maxError)
}

.tmaProjectionInteger <- function(x, name, minimum) {
  if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") ||
      length(x) != 1L || !is.null(dim(x)) || is.na(x) || !is.finite(x) ||
      x < minimum || x != trunc(x) || x >= 2^53) {
    stop(name, " must be an exact finite integer from ", minimum, " through 2^53 - 1.", call. = FALSE)
  }
  x
}

.tmaProjectionEntry <- function(entry, states, absorbingStates, cycleLength, tolerance,
                                allowEmpty = FALSE) {
  if (inherits(entry, "tma_stratified_entry")) {
    stop("Stratified entry requires explicit patient profiles in simulateTrajectories(), or select one group from entry$models for projectCohort(). Entry groups are not pooled.", call. = FALSE)
  }
  if (!is.list(entry) || !is.list(entry$metadata) || !is.list(entry$diagnostics) ||
      !all(c("states", "absorbingStates", "cycleLength", "estimator", "eligibility") %in%
           names(entry$metadata)) || !identical(entry$metadata$estimator, "entry_outcomes")) {
    stop("entry must be a result from estimateEntryTransitions().", call. = FALSE)
  }
  model <- entry$metadata
  .tmaStateDictionary(character(), model$states, model$absorbingStates)
  if (is.null(model$states) || !setequal(model$states, states) ||
      !setequal(model$absorbingStates, absorbingStates) ||
      .tmaProjectionInteger(model$cycleLength, "entry cycleLength", 1) != cycleLength) {
    stop("Entry states, absorbingStates and cycleLength must match the adjacent-cycle model.", call. = FALSE)
  }
  if (!is.character(model$eligibility) || length(model$eligibility) != 1L ||
      is.na(model$eligibility) || !nzchar(trimws(model$eligibility))) {
    stop("entry must provide a nonempty eligibility description.", call. = FALSE)
  }
  sourceSubjects <- .tmaProjectionInteger(entry$diagnostics$source_subjects,
                                           "entry source_subjects", 1)
  eligibleSubjects <- .tmaProjectionInteger(entry$diagnostics$eligible_subjects,
                                             "entry eligible_subjects", 0)
  excludedSubjects <- .tmaProjectionInteger(entry$diagnostics$excluded_subjects,
                                             "entry excluded_subjects", 0)
  if (eligibleSubjects + excludedSubjects != sourceSubjects) {
    stop("Entry eligible and excluded counts must sum to source_subjects.", call. = FALSE)
  }
  subjects <- entry$subjects
  required <- c("subject_id", "entry_state", "first_state", "first_cycle_days", "eligible", "status")
  if (!is.data.frame(subjects) || anyDuplicated(names(subjects)) ||
      !all(required %in% names(subjects)) || nrow(subjects) != sourceSubjects) {
    stop("entry$subjects must describe every source subject once, with eligibility information.", call. = FALSE)
  }
  ids <- .tmaSubjectIds(subjects$subject_id, "entry$subjects$subject_id")
  if (anyDuplicated(ids) || !is.logical(subjects$eligible) || !is.null(dim(subjects$eligible)) ||
      anyNA(subjects$eligible) || sum(subjects$eligible) != eligibleSubjects ||
      !is.character(subjects$entry_state) || anyNA(subjects$entry_state) ||
      any(!subjects$entry_state %in% states) || !is.character(subjects$first_state) ||
      any(!is.na(subjects$first_state) & !subjects$first_state %in% c(states, "OUT OF COHORT")) ||
      !is.character(subjects$status) || anyNA(subjects$status) || any(!nzchar(trimws(subjects$status)))) {
    stop("Entry subject identifiers, state labels or eligibility flags are invalid.", call. = FALSE)
  }
  days <- subjects$first_cycle_days
  if (!is.numeric(days) || is.complex(days) || inherits(days, "integer64") ||
      !is.null(dim(days)) || any(is.nan(days)) ||
      any(!is.na(days) & (!is.finite(days) | days <= 0 | days > cycleLength | days != trunc(days))) ||
      any(is.na(days) != is.na(subjects$first_state))) {
    stop("Entry first_cycle_days must describe valid observed days or a missing first cycle.", call. = FALSE)
  }
  known <- !is.na(subjects$first_state) & subjects$first_state != "OUT OF COHORT"
  expectedEligible <- known & !is.na(days) &
    (days == cycleLength | subjects$first_state %in% absorbingStates)
  if (any(subjects$eligible != expectedEligible) ||
      any(known & subjects$entry_state %in% absorbingStates &
            subjects$first_state != subjects$entry_state)) {
    stop("Entry eligibility or absorbing entry states contradict the reported first-cycle outcomes.", call. = FALSE)
  }
  coverage <- entry$coverage
  if (!is.data.frame(coverage) || anyDuplicated(names(coverage)) ||
      !all(c("status", "n") %in% names(coverage)) || !is.character(coverage$status) ||
      anyNA(coverage$status) || any(!nzchar(trimws(coverage$status))) ||
      anyDuplicated(coverage$status) || any(!subjects$status %in% coverage$status)) {
    stop("entry$coverage must contain unique statuses covering the subject records.", call. = FALSE)
  }
  coverageCount <- .tmaProjectionEntryCountsVector(coverage$n, nrow(coverage), "entry coverage counts")
  if (any(coverageCount != tabulate(match(subjects$status, coverage$status), nbins = nrow(coverage)))) {
    stop("Entry coverage counts must agree with the subject records.", call. = FALSE)
  }
  if (!eligibleSubjects && !allowEmpty) {
    stop("Entry has no eligible first-cycle outcomes and cannot initialise a projection.", call. = FALSE)
  }
  baseline <- .tmaProjectionEntryDistribution(entry$baselineCounts, states,
                                              subjects$entry_state[subjects$eligible],
                                              eligibleSubjects, "baselineCounts", tolerance)
  .tmaProjectionEntryDistribution(entry$outcomeCounts, states,
                                   subjects$first_state[subjects$eligible],
                                   eligibleSubjects, "outcomeCounts", tolerance)
  entryMatrix <- entry$matrix
  if (!is.matrix(entryMatrix) || !is.numeric(entryMatrix) || is.complex(entryMatrix) ||
      inherits(entryMatrix, "integer64") || !identical(dim(entryMatrix), rep(length(states), 2L)) ||
      is.null(rownames(entryMatrix)) || is.null(colnames(entryMatrix)) ||
      anyDuplicated(rownames(entryMatrix)) || anyDuplicated(colnames(entryMatrix)) ||
      !setequal(rownames(entryMatrix), states) || !setequal(colnames(entryMatrix), states)) {
    stop("entry$matrix must have the declared states on both axes.", call. = FALSE)
  }
  entryMatrix <- entryMatrix[states, states, drop = FALSE]
  missingCount <- rowSums(is.na(entryMatrix))
  missingRows <- missingCount == length(states)
  values <- entryMatrix[!is.na(entryMatrix)]
  if (any(missingCount > 0 & !missingRows) || any(is.nan(entryMatrix)) ||
      any(!is.finite(values) | values < 0 | values > 1) ||
      any(abs(rowSums(entryMatrix[!missingRows, , drop = FALSE]) - 1) > tolerance)) {
    stop("Entry probabilities must be complete stochastic rows or entirely NA rows.", call. = FALSE)
  }
  for (state in absorbingStates) {
    if (anyNA(entryMatrix[state, ]) || any(entryMatrix[state, ] != as.numeric(states == state))) {
      stop("Absorbing entry state '", state, "' must have an exact identity row.", call. = FALSE)
    }
  }
  if (!eligibleSubjects && any(!missingRows[!states %in% absorbingStates])) {
    stop("An entry group without eligible outcomes must have entirely NA nonabsorbing probability rows.", call. = FALSE)
  }
  occupied <- which(baseline > 0)
  if (any(missingRows[occupied])) {
    stop("Unestimated entry row for occupied baseline state(s): ",
         paste(states[occupied[missingRows[occupied]]], collapse = ", "), ".", call. = FALSE)
  }
  distribution <- if (eligibleSubjects) {
    stats::setNames(as.numeric(baseline[occupied] %*%
                                entryMatrix[occupied, , drop = FALSE]), states)
  } else stats::setNames(rep(NA_real_, length(states)), states)
  list(distribution = distribution,
       entry = list(baseline_distribution = baseline, matrix = entryMatrix,
                    eligible_subject_ids = ids[subjects$eligible],
                    source_subjects = sourceSubjects, eligible_subjects = eligibleSubjects,
                    excluded_subjects = excludedSubjects,
                    coverage = coverage, eligibility = model$eligibility))
}

.tmaProjectionEntryCountsVector <- function(x, expectedLength, name) {
  if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") ||
      !is.null(dim(x)) || length(x) != expectedLength || anyNA(x) ||
      any(!is.finite(x) | x < 0 | x != trunc(x) | x >= 2^53)) {
    stop(name, " must contain exact nonnegative integer counts.", call. = FALSE)
  }
  x
}

.tmaProjectionEntryDistribution <- function(counts, states, labels, denominator, name, tolerance) {
  if (!is.data.frame(counts) || anyDuplicated(names(counts)) ||
      !all(c("state", "count", "denominator", "probability") %in% names(counts)) ||
      !is.character(counts$state) || anyNA(counts$state) || anyDuplicated(counts$state) ||
      !setequal(counts$state, states)) {
    stop("entry$", name, " must contain each declared state exactly once.", call. = FALSE)
  }
  counts <- counts[match(states, counts$state), , drop = FALSE]
  observedCounts <- .tmaProjectionEntryCountsVector(counts$count, length(states), paste0("entry$", name, "$count"))
  denominators <- .tmaProjectionEntryCountsVector(counts$denominator, length(states),
                                                 paste0("entry$", name, "$denominator"))
  expected <- tabulate(match(labels, states), nbins = length(states))
  probability <- counts$probability
  if (!denominator) {
    if (any(observedCounts != expected) || any(denominators != 0) ||
        !is.numeric(probability) || is.complex(probability) || inherits(probability, "integer64") ||
        !is.null(dim(probability)) || !all(is.na(probability)) || any(is.nan(probability))) {
      stop("entry$", name, " must report zero counts and NA probabilities without eligible subjects.", call. = FALSE)
    }
    return(stats::setNames(probability, states))
  }
  if (any(observedCounts != expected) || any(denominators != denominator) ||
      !is.numeric(probability) || is.complex(probability) || inherits(probability, "integer64") ||
      !is.null(dim(probability)) || anyNA(probability) ||
      any(!is.finite(probability) | probability < 0 | probability > 1) ||
      any(abs(probability - observedCounts / denominator) > tolerance) ||
      abs(sum(probability) - 1) > tolerance) {
    stop("entry$", name, " must agree with its eligible subjects and probabilities.", call. = FALSE)
  }
  stats::setNames(probability, states)
}
