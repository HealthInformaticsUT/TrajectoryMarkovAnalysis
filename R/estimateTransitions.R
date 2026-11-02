#' Estimate adjacent-cycle transition probabilities by follow-up period
#'
#' @param trajectories Lowercase C2T trajectories or a tma_trajectories object.
#' @param cycleLength Positive integer cycle length in days. Required unless
#'   already recorded in prepared trajectories.
#' @param absorbingStates Clinical absorbing state labels.
#' @param states Optional ordered clinical state dictionary.
#' @param periodBreaks Period boundaries in days since each patient's index.
#'   Supply increasing whole days starting at 0 and ending at Inf, for example
#'   c(0, 7, 35, Inf). Finite boundaries must align with cycleLength. NULL pools
#'   all follow-up.
#' @param periodLabels Optional unique names for the periods between boundaries.
#' @param strata Optional character vector of categorical trajectory column names.
#'   Values may change between rows. Numeric values define exact-value groups;
#'   create age bands or other categories explicitly before estimation.
#' @details Only a full source cycle followed immediately by an observed known
#'   state contributes a transition. Gaps, OUT OF COHORT and administrative EXIT
#'   never create transitions. Entry counts describe known states at index_date;
#'   a first-cycle death does not create an unobserved preceding state.
#'
#'   Each transition belongs to the period containing the source cycle's start.
#'   Periods include their lower boundary and exclude their upper boundary.
#'   A transition into the next period is counted under its source period, not
#'   discarded. Denominators count observed outgoing pairs, not all patients or
#'   all source cycles. This does not correct for informative censoring or recover
#'   event dates within a cycle. Periods are not landmark eligibility rules.
#'
#'   With strata, adjacent pairs are constructed before grouping and assigned
#'   to the source row's covariate values, retaining transitions across groups.
#'   Follow-up periods and entry counts retain the original patient index.
#'   Registry IDs are generated after ordering the observed covariate tuples;
#'   their meaning is defined by the returned strata registry. Distinct patient
#'   counts describe support within each group and cannot be added across groups.
#' @return A list with estimates, matrices, periods, entryCounts, diagnostics and
#'   metadata. matrices contains one named matrix per period. matrix contains
#'   that matrix only when one period is requested, otherwise it is NULL.
#'   Unestimated nonabsorbing rows contain NA probabilities. Absorbing identity
#'   rows are structural, with zero observed counts and denominators.
#'   With strata, a tma_stratified_transitions object contains models named by
#'   stratum_id, a strata registry, combined estimates and entryCounts, and common
#'   periods, diagnostics and metadata. Each model has the ordinary result schema.
#' @export
estimateTransitions <- function(trajectories, cycleLength = NULL,
                                absorbingStates = character(), states = NULL,
                                periodBreaks = NULL, periodLabels = NULL,
                                strata = NULL) {
  args <- list(trajectories = trajectories)
  if (!missing(cycleLength)) args$cycleLength <- cycleLength
  if (!missing(absorbingStates)) args$absorbingStates <- absorbingStates
  if (!missing(states)) args$states <- states
  prepared <- do.call(prepareTrajectories, args)
  if (is.null(prepared$cycleLength)) {
    stop("cycleLength must be supplied explicitly for transition estimation or stored in prepared trajectories.", call. = FALSE)
  }
  data <- prepared$data
  states <- prepared$states
  absorbingStates <- prepared$absorbingStates
  periods <- .tmaPeriods(periodBreaks, periodLabels, prepared$cycleLength)
  grouping <- .tmaStrata(data, strata)
  source <- seq_len(max(0L, nrow(data) - 1L))
  target <- source + 1L
  sameSubject <- data$subject_id[source] == data$subject_id[target]
  contiguous <- data$state_start_date[target] == data$state_end_date[source] + 1
  known <- data$state_label[source] != "OUT OF COHORT" &
    data$state_label[target] != "OUT OF COHORT"
  fullSource <- data$observed_days[source] == prepared$cycleLength
  eligible <- sameSubject & contiguous & known & fullSource
  origins <- prepared$patients$index_date[match(data$subject_id,
                                               prepared$patients$subject_id)]
  periodIndex <- .tmaPeriodIndex(as.numeric(data$state_start_date - origins), periods)
  model <- .tmaTransitionModel(prepared, periods, source, target, sameSubject,
                               contiguous, known, eligible, origins, periodIndex)
  if (is.null(grouping)) return(model)
  registry <- grouping$registry
  models <- vector("list", nrow(registry))
  names(models) <- registry$stratum_id
  for (i in seq_len(nrow(registry))) {
    selected <- !is.na(grouping$row_ids) & grouping$row_ids == registry$stratum_id[i]
    models[[i]] <- .tmaTransitionModel(prepared, periods, source, target, sameSubject,
                                       contiguous, known, eligible, origins, periodIndex,
                                       selected)
    models[[i]]$metadata$strata <- grouping$columns
    models[[i]]$metadata$stratum_id <- registry$stratum_id[i]
  }
  model$metadata$strata <- grouping$columns
  model$metadata$stratum_assignment <- "source row values; pairs formed before grouping"
  estimates <- lapply(seq_along(models), function(i) {
    .tmaAddStratum(models[[i]]$estimates, registry[i, , drop = FALSE])
  })
  entries <- lapply(seq_along(models), function(i) {
    .tmaAddStratum(models[[i]]$entryCounts, registry[i, , drop = FALSE])
  })
  emptyEstimates <- cbind(registry[FALSE, , drop = FALSE], model$estimates[FALSE, , drop = FALSE])
  emptyEstimates$patients <- integer()
  emptyEstimates$source_patients <- integer()
  emptyEntries <- cbind(registry[FALSE, , drop = FALSE], model$entryCounts[FALSE, , drop = FALSE])
  model$diagnostics$by_stratum <- lapply(models, function(x) x$diagnostics)
  structure(list(models = models, strata = registry,
                 estimates = if (length(estimates)) do.call(rbind, estimates) else emptyEstimates,
                 entryCounts = if (length(entries)) do.call(rbind, entries) else emptyEntries,
                 periods = periods, diagnostics = model$diagnostics, metadata = model$metadata),
            class = "tma_stratified_transitions")
}

.tmaTransitionModel <- function(prepared, periods, source, target, sameSubject,
                                 contiguous, known, eligible, origins, periodIndex,
                                 selected = NULL) {
  data <- prepared$data
  states <- prepared$states
  absorbingStates <- prepared$absorbingStates
  nStates <- length(states)
  nPeriods <- nrow(periods)
  stratified <- !is.null(selected)
  if (!stratified) selected <- rep(TRUE, nrow(data))
  eligible <- eligible & selected[source]
  counts <- array(0, c(nStates, nStates, nPeriods),
                  dimnames = list(states, states, periods$period))
  if (any(eligible)) {
    from <- match(data$state_label[source[eligible]], states)
    to <- match(data$state_label[target[eligible]], states)
    period <- periodIndex[source[eligible]]
    counts[] <- tabulate((period - 1L) * nStates * nStates +
                          (to - 1L) * nStates + from,
                        nbins = nStates * nStates * nPeriods)
  }
  matrices <- vector("list", nPeriods)
  estimateList <- vector("list", nPeriods)
  periodCounts <- numeric(nPeriods)
  for (i in seq_len(nPeriods)) {
    periodCount <- matrix(counts[, , i], nStates, nStates,
                           dimnames = list(states, states))
    denominators <- rowSums(periodCount)
    probabilities <- periodCount
    if (nStates) {
      probabilities <- periodCount / denominators
      probabilities[denominators == 0, ] <- NA_real_
      for (state in absorbingStates) {
        probabilities[state, ] <- 0
        probabilities[state, state] <- 1
      }
    }
    estimates <- expand.grid(from_state = states, to_state = states,
                             stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    estimates$period <- rep(periods$period[i], nrow(estimates))
    estimates <- estimates[c("period", "from_state", "to_state")]
    estimates$count <- as.vector(periodCount)
    estimates$denominator <- unname(denominators[match(estimates$from_state, states)])
    estimates$probability <- as.vector(probabilities)
    estimates$structural <- estimates$from_state %in% absorbingStates
    if (stratified) {
      pairRows <- which(eligible & periodIndex[source] == i)
      fromLabels <- data$state_label[source[pairRows]]
      toLabels <- data$state_label[target[pairRows]]
      subjects <- data$subject_id[source[pairRows]]
      estimates$patients <- vapply(seq_len(nrow(estimates)), function(j) {
        length(unique(subjects[fromLabels == estimates$from_state[j] &
                                 toLabels == estimates$to_state[j]]))
      }, integer(1))
      estimates$source_patients <- vapply(estimates$from_state, function(state) {
        length(unique(subjects[fromLabels == state]))
      }, integer(1), USE.NAMES = FALSE)
    }
    matrices[[i]] <- probabilities
    estimateList[[i]] <- estimates
    periodCounts[i] <- sum(periodCount)
  }
  names(matrices) <- periods$period
  estimates <- do.call(rbind, estimateList)

  first <- which(!duplicated(data$subject_id) & selected)
  atIndex <- data$state_start_date[first] == origins[first]
  knownEntry <- atIndex & data$state_label[first] != "OUT OF COHORT"
  entryCount <- tabulate(match(data$state_label[first[knownEntry]], states), nbins = nStates)
  entryDenominator <- sum(entryCount)
  entryCounts <- data.frame(state = states, count = entryCount,
                            denominator = rep(entryDenominator, nStates),
                            probability = if (entryDenominator) entryCount / entryDenominator else rep(NA_real_, nStates),
                            stringsAsFactors = FALSE)
  last <- !duplicated(data$subject_id, fromLast = TRUE) & selected
  result <- list(
    estimates = estimates, matrices = matrices, periods = periods,
    matrix = if (nPeriods == 1L) matrices[[1L]] else NULL, entryCounts = entryCounts,
    diagnostics = list(
      preparation = prepared$diagnostics,
      observed_transition_pairs = sum(counts),
      pairs_by_period = data.frame(period = periods$period,
                                    observed_transition_pairs = periodCounts),
      gap_pairs_excluded = sum(sameSubject & !contiguous & selected[source]),
      unresolved_pairs_excluded = sum(sameSubject & contiguous & !known & selected[source]),
      incomplete_final_cycles = sum(last & data$observed_days < prepared$cycleLength),
      boundary_censored_subjects = sum(last & !data$state_label %in% absorbingStates),
      terminal_observations = sum(data$state_label %in% absorbingStates & selected),
      unresolved_entries = sum(atIndex & !knownEntry),
      missing_index_entries = sum(!atIndex)
    ),
    metadata = list(states = states, absorbingStates = absorbingStates,
                    cycleLength = prepared$cycleLength, time_unit = "days",
                    estimator = "adjacent_cycles", date_endpoints = "inclusive",
                    period_assignment = "source cycle start, days since index_date",
                    period_endpoints = "lower inclusive, upper exclusive",
                    denominator = "observed adjacent known pairs with a full source cycle",
                    entry_denominator = "subjects with a known state at index_date")
  )
  if (stratified) {
    result$diagnostics$patients <- length(unique(data$subject_id[selected]))
    result$diagnostics$supporting_patients <- length(unique(data$subject_id[source[eligible]]))
    result$diagnostics$pairs_by_period$patients <- vapply(seq_len(nPeriods), function(i) {
      length(unique(data$subject_id[source[eligible & periodIndex[source] == i]]))
    }, integer(1))
  }
  result
}
