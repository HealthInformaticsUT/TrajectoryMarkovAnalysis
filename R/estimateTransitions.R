#' Estimate pooled adjacent-cycle transition probabilities
#'
#' @param trajectories Lowercase C2T trajectories or a tma_trajectories object.
#' @param cycleLength Positive integer cycle length in days. Required unless
#'   already recorded in prepared trajectories.
#' @param absorbingStates Clinical absorbing state labels.
#' @param states Optional ordered clinical state dictionary.
#' @details Only a full source cycle followed immediately by an observed known
#'   state contributes a transition. Gaps, OUT OF COHORT and administrative EXIT
#'   never create transitions. Entry counts describe known states at index_date;
#'   a first-cycle death does not create an unobserved preceding state.
#' @return A list with estimates, matrix, entryCounts, diagnostics and metadata.
#'   Unestimated nonabsorbing rows contain NA probabilities. Absorbing identity
#'   rows are structural, with zero observed counts and denominators.
#' @export
estimateTransitions <- function(trajectories, cycleLength = NULL,
                                absorbingStates = character(), states = NULL) {
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
  nStates <- length(states)
  source <- seq_len(max(0L, nrow(data) - 1L))
  target <- source + 1L
  sameSubject <- data$subject_id[source] == data$subject_id[target]
  contiguous <- data$state_start_date[target] == data$state_end_date[source] + 1
  known <- data$state_label[source] != "OUT OF COHORT" &
    data$state_label[target] != "OUT OF COHORT"
  fullSource <- data$observed_days[source] == prepared$cycleLength
  eligible <- sameSubject & contiguous & known & fullSource
  counts <- matrix(0, nStates, nStates, dimnames = list(states, states))
  if (any(eligible)) {
    from <- match(data$state_label[source[eligible]], states)
    to <- match(data$state_label[target[eligible]], states)
    counts[] <- tabulate((to - 1L) * nStates + from, nbins = nStates * nStates)
  }
  denominators <- rowSums(counts)
  probabilities <- counts
  if (nStates) {
    probabilities <- counts / denominators
    probabilities[denominators == 0, ] <- NA_real_
    for (state in absorbingStates) {
      probabilities[state, ] <- 0
      probabilities[state, state] <- 1
    }
  }
  estimates <- expand.grid(from_state = states, to_state = states,
                           stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  estimates$count <- as.vector(counts)
  estimates$denominator <- unname(denominators[match(estimates$from_state, states)])
  estimates$probability <- as.vector(probabilities)
  estimates$structural <- estimates$from_state %in% absorbingStates

  first <- which(!duplicated(data$subject_id))
  origins <- prepared$patients$index_date[match(data$subject_id[first],
                                               prepared$patients$subject_id)]
  atIndex <- data$state_start_date[first] == origins
  knownEntry <- atIndex & data$state_label[first] != "OUT OF COHORT"
  entryCount <- tabulate(match(data$state_label[first[knownEntry]], states), nbins = nStates)
  entryDenominator <- sum(entryCount)
  entryCounts <- data.frame(state = states, count = entryCount,
                            denominator = rep(entryDenominator, nStates),
                            probability = if (entryDenominator) entryCount / entryDenominator else rep(NA_real_, nStates),
                            stringsAsFactors = FALSE)
  last <- !duplicated(data$subject_id, fromLast = TRUE)
  list(
    estimates = estimates, matrix = probabilities, entryCounts = entryCounts,
    diagnostics = list(
      preparation = prepared$diagnostics,
      observed_transition_pairs = sum(counts),
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
                    estimator = "pooled_adjacent_cycles", date_endpoints = "inclusive",
                    entry_denominator = "subjects with a known state at index_date")
  )
}
