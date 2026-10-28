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
#' @return A list with estimates, matrices, periods, entryCounts, diagnostics and
#'   metadata. matrices contains one named matrix per period. matrix contains
#'   that matrix only when one period is requested, otherwise it is NULL.
#'   Unestimated nonabsorbing rows contain NA probabilities. Absorbing identity
#'   rows are structural, with zero observed counts and denominators.
#' @export
estimateTransitions <- function(trajectories, cycleLength = NULL,
                                absorbingStates = character(), states = NULL,
                                periodBreaks = NULL, periodLabels = NULL) {
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
  nStates <- length(states)
  nPeriods <- nrow(periods)
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
    matrices[[i]] <- probabilities
    estimateList[[i]] <- estimates
    periodCounts[i] <- sum(periodCount)
  }
  names(matrices) <- periods$period
  estimates <- do.call(rbind, estimateList)

  first <- which(!duplicated(data$subject_id))
  atIndex <- data$state_start_date[first] == origins[first]
  knownEntry <- atIndex & data$state_label[first] != "OUT OF COHORT"
  entryCount <- tabulate(match(data$state_label[first[knownEntry]], states), nbins = nStates)
  entryDenominator <- sum(entryCount)
  entryCounts <- data.frame(state = states, count = entryCount,
                            denominator = rep(entryDenominator, nStates),
                            probability = if (entryDenominator) entryCount / entryDenominator else rep(NA_real_, nStates),
                            stringsAsFactors = FALSE)
  last <- !duplicated(data$subject_id, fromLast = TRUE)
  list(
    estimates = estimates, matrices = matrices, periods = periods,
    matrix = if (nPeriods == 1L) matrices[[1L]] else NULL, entryCounts = entryCounts,
    diagnostics = list(
      preparation = prepared$diagnostics,
      observed_transition_pairs = sum(counts),
      pairs_by_period = data.frame(period = periods$period,
                                    observed_transition_pairs = periodCounts),
      gap_pairs_excluded = sum(sameSubject & !contiguous),
      unresolved_pairs_excluded = sum(sameSubject & contiguous & !known),
      incomplete_final_cycles = sum(last & data$observed_days < prepared$cycleLength),
      boundary_censored_subjects = sum(last & !data$state_label %in% absorbingStates),
      terminal_observations = sum(data$state_label %in% absorbingStates),
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
}
