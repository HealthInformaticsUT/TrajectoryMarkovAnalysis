#' Summarise available state labels and follow-up coverage by cycle
#'
#' @param trajectories Lowercase C2T trajectories or prepared trajectories.
#' @param nCycles Final zero-based cycle index to report. NULL reports through
#'   the latest supplied cycle label. The output includes cycles 0 through nCycles.
#' @param cycleLength Positive integer cycle length in days, required unless
#'   already recorded in prepared trajectories.
#' @param absorbingStates Explicit clinical labels whose occupancy continues
#'   after their final observed cycle.
#' @param states Optional ordered clinical state dictionary, including states
#'   that are not observed.
#' @details A supplied known label contributes to its current cycle, including
#'   a shortened final cycle. A declared absorbing label is carried from the
#'   following cycle onward and reported separately from observed labels.
#'   Nonabsorbing states are never carried beyond follow-up. OUT OF COHORT and missing cycles
#'   remain visible in the coverage table.
#'
#'   State proportions use current known labels plus carried absorbing labels
#'   as their denominator. This available-state denominator can change over time.
#'   The cohort_fraction instead uses the original number of included subjects.
#'   Neither quantity estimates the unknown states of censored subjects or
#'   corrects for informative censoring. These summaries are not survival or
#'   multistate probability estimators.
#' @return A list with occupancy, coverage and metadata. Occupancy separates
#'   observed and carried counts. Coverage partitions all included subjects into
#'   observed_known, unresolved, carried_absorbing, missing_within_observation
#'   and censored for each cycle. Proportions are NA when no state is available.
#' @export
summariseOccupancy <- function(trajectories, nCycles = NULL, cycleLength = NULL,
                               absorbingStates = character(), states = NULL) {
  args <- list(trajectories = trajectories)
  if (!missing(cycleLength)) args$cycleLength <- cycleLength
  if (!missing(absorbingStates)) args$absorbingStates <- absorbingStates
  if (!missing(states)) args$states <- states
  prepared <- do.call(prepareTrajectories, args)
  .tmaSummariseOccupancy(prepared, nCycles = nCycles)
}

.tmaSummariseOccupancy <- function(prepared, nCycles = NULL, subjectIds = NULL) {
  if (!inherits(prepared, "tma_trajectories")) {
    stop("prepared must be a tma_trajectories object.", call. = FALSE)
  }
  if (is.null(prepared$cycleLength)) {
    stop("cycleLength must be supplied explicitly for occupancy summaries or stored in prepared trajectories.", call. = FALSE)
  }
  cycleLength <- .tmaProjectionInteger(prepared$cycleLength, "cycleLength", minimum = 1)
  patients <- prepared$patients
  data <- prepared$data
  if (!is.null(subjectIds)) {
    subjectIds <- .tmaSubjectIds(subjectIds, "subjectIds")
    if (!length(subjectIds) || anyDuplicated(subjectIds) ||
        any(!subjectIds %in% patients$subject_id)) {
      stop("subjectIds must be a unique, nonempty subset of prepared subject identifiers.", call. = FALSE)
    }
    patients <- patients[patients$subject_id %in% subjectIds, , drop = FALSE]
    data <- data[data$subject_id %in% subjectIds, , drop = FALSE]
  }
  nSubjects <- nrow(patients)
  states <- prepared$states
  absorbingStates <- prepared$absorbingStates
  origins <- patients$index_date[match(data$subject_id, patients$subject_id)]
  rowCycles <- as.numeric(data$state_start_date - origins) / cycleLength
  if (is.null(nCycles)) nCycles <- max(rowCycles)
  nCycles <- .tmaProjectionInteger(nCycles, "nCycles", minimum = 0)
  nRows <- nCycles + 1
  nStates <- length(states)
  if (nRows * max(1L, nStates) > .Machine$integer.max ||
      nCycles * cycleLength >= 2^53) {
    stop("The requested horizon exceeds supported output indexing or exact elapsed days.", call. = FALSE)
  }
  withinHorizon <- rowCycles <= nCycles
  known <- data$state_label != "OUT OF COHORT"
  observed <- matrix(0, nrow = nRows, ncol = nStates)
  if (nStates && any(withinHorizon & known)) {
    selected <- which(withinHorizon & known)
    stateIndex <- match(data$state_label[selected], states)
    observed[] <- tabulate(rowCycles[selected] + 1 + (stateIndex - 1L) * nRows,
                           nbins = nRows * nStates)
  }
  unresolved <- tabulate(rowCycles[withinHorizon & !known] + 1, nbins = nRows)
  partial <- tabulate(rowCycles[withinHorizon & data$observed_days < cycleLength] + 1,
                      nbins = nRows)

  last <- !duplicated(data$subject_id, fromLast = TRUE)
  terminal <- last & data$state_label %in% absorbingStates
  carried <- matrix(0, nrow = nRows, ncol = nStates)
  carryStarts <- rowCycles[terminal] + 1
  if (nStates && any(carryStarts <= nCycles)) {
    included <- carryStarts <= nCycles
    terminalStates <- match(data$state_label[terminal][included], states)
    carried[] <- tabulate(carryStarts[included] + 1 + (terminalStates - 1L) * nRows,
                          nbins = nRows * nStates)
    for (state in seq_len(nStates)) carried[, state] <- cumsum(carried[, state])
  }
  censorStarts <- rowCycles[last & !terminal] + 1
  censored <- cumsum(tabulate(censorStarts[censorStarts <= nCycles] + 1, nbins = nRows))
  observedKnown <- rowSums(observed)
  carriedAbsorbing <- rowSums(carried)
  denominator <- observedKnown + carriedAbsorbing
  missingWithin <- nSubjects - observedKnown - unresolved - carriedAbsorbing - censored
  if (any(missingWithin < 0)) {
    stop("Prepared trajectories do not form a unique subject-by-cycle coverage partition.", call. = FALSE)
  }
  cycles <- seq.int(0, nCycles)
  coverage <- data.frame(
    cycle = cycles, elapsed_day = cycles * cycleLength, cohort_patients = nSubjects,
    observed_known = observedKnown, unresolved = unresolved,
    carried_absorbing = carriedAbsorbing, missing_within_observation = missingWithin,
    censored = censored, observed_patients = observedKnown + unresolved,
    denominator = denominator, available_fraction = denominator / nSubjects,
    partial_cycle_patients = partial
  )
  knownCount <- observed + carried
  proportion <- knownCount
  if (nStates) {
    proportion <- knownCount / denominator
    proportion[denominator == 0, ] <- NA_real_
  }
  occupancy <- data.frame(
    cycle = rep(cycles, each = nStates),
    elapsed_day = rep(cycles * cycleLength, each = nStates),
    state = rep(states, times = nRows),
    observed_count = as.vector(t(observed)),
    carried_absorbing_count = as.vector(t(carried)),
    known_count = as.vector(t(knownCount)),
    denominator = rep(denominator, each = nStates),
    proportion = as.vector(t(proportion)),
    cohort_fraction = as.vector(t(knownCount)) / nSubjects,
    stringsAsFactors = FALSE
  )
  list(
    occupancy = occupancy, coverage = coverage,
    metadata = list(
      cycleLength = cycleLength, states = states, absorbingStates = absorbingStates,
      nCycles = nCycles, n_subjects = nSubjects
    )
  )
}
