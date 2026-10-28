#' Estimate first-cycle outcomes from explicit entry states
#'
#' @param trajectories Current Cohort2Trajectory output or prepared trajectories.
#' @param entryState A clinical state label shared by all patients before the
#'   first cycle, or a data frame with subject_id and entry_state columns. A table
#'   must cover every supplied patient exactly once. Entry states must belong to
#'   the declared state dictionary. No starting state is inferred from outcomes.
#' @param cycleLength Positive integer cycle length in days, required unless
#'   already stored in prepared trajectories.
#' @param absorbingStates Explicit clinical absorbing labels.
#' @param states Optional complete state dictionary, including entry states that
#'   do not appear in the recorded outcomes.
#' @details A known first-cycle outcome contributes when the cycle is complete
#'   or its label is an explicitly absorbing state. A shortened nonabsorbing
#'   cycle, unresolved label or missing first cycle does not supply a complete
#'   outcome. These exclusions are reported for every patient.
#'
#'   Probabilities are conditional on an evaluable first cycle within each entry
#'   state. They are not adjusted for censoring. The baseline distribution uses
#'   the same eligible patients, not all supplied patients. Exclusions can change
#'   that population and must be considered before applying the estimates more
#'   widely. These are first-cycle outcomes, not event times or a reconstruction
#'   of the order of events within that cycle.
#'
#'   The entry matrix is separate from adjacent-cycle matrices. Use it with
#'   projectCohort(entry = ...) to reach the first recorded cycle exactly once.
#'   Supplying a baseline state is an analyst definition, not proof that it could
#'   have been identified prospectively. Histories cannot leave an entry state
#'   that the analyst has declared absorbing.
#' @return A list containing estimates, matrix, baselineCounts, outcomeCounts,
#'   subjects, coverage, diagnostics and metadata. Counts and default baseline
#'   probabilities use eligible subjects only. Unestimated nonabsorbing rows
#'   remain NA. Absorbing rows are structural identities. With no eligible
#'   subjects, baseline and outcome probabilities are NA and projection is not
#'   possible. subjects contains patient identifiers and should remain within
#'   the permitted patient-level analysis environment.
#' @export
estimateEntryTransitions <- function(trajectories, entryState, cycleLength = NULL,
                                     absorbingStates = character(), states = NULL) {
  if (missing(entryState)) {
    stop("Supply entryState explicitly. The starting state is not inferred from first-cycle outcomes.", call. = FALSE)
  }
  args <- list(trajectories = trajectories)
  if (!missing(cycleLength)) args$cycleLength <- cycleLength
  if (!missing(absorbingStates)) args$absorbingStates <- absorbingStates
  if (!missing(states)) args$states <- states
  prepared <- do.call(prepareTrajectories, args)
  if (is.null(prepared$cycleLength)) {
    stop("cycleLength must be supplied or stored in prepared trajectories.", call. = FALSE)
  }
  patients <- prepared$patients
  data <- prepared$data
  states <- prepared$states
  absorbingStates <- prepared$absorbingStates
  nSubjects <- nrow(patients)
  if (is.character(entryState) && length(entryState) == 1L) {
    entryLabels <- rep(entryState, nSubjects)
  } else if (is.data.frame(entryState) && !anyDuplicated(names(entryState)) &&
             all(c("subject_id", "entry_state") %in% names(entryState))) {
    ids <- .tmaSubjectIds(entryState$subject_id, "entryState$subject_id")
    if (anyDuplicated(ids) || !setequal(ids, patients$subject_id)) {
      stop("entryState must contain every trajectory subject exactly once, with no extra subjects.", call. = FALSE)
    }
    if (!is.character(entryState$entry_state) && !is.factor(entryState$entry_state)) {
      stop("entryState$entry_state must contain clinical state labels.", call. = FALSE)
    }
    entryLabels <- as.character(entryState$entry_state[match(patients$subject_id, ids)])
  } else {
    stop("entryState must be one clinical state label or a subject_id/entry_state data frame.", call. = FALSE)
  }
  if (anyNA(entryLabels) || any(!nzchar(trimws(entryLabels))) ||
      any(!entryLabels %in% states)) {
    stop("Every entry state must be a nonmissing clinical label in states. Declare unobserved entry states with states = ... .", call. = FALSE)
  }
  entryForRows <- entryLabels[match(data$subject_id, patients$subject_id)]
  if (any(entryForRows %in% absorbingStates & data$state_label != "OUT OF COHORT" &
          data$state_label != entryForRows)) {
    stop("A recorded history cannot leave its declared absorbing entry state.", call. = FALSE)
  }
  first <- match(patients$subject_id, data$subject_id)
  atIndex <- data$state_start_date[first] == patients$index_date
  known <- data$state_label[first] != "OUT OF COHORT"
  terminal <- data$state_label[first] %in% absorbingStates
  full <- data$observed_days[first] == prepared$cycleLength
  status <- rep("missing_first_cycle", nSubjects)
  status[atIndex & !known] <- "unresolved_first_cycle"
  status[atIndex & known & !terminal & !full] <- "incomplete_first_cycle"
  status[atIndex & known & !terminal & full] <- "observed_complete"
  status[atIndex & known & terminal] <- "observed_terminal"
  eligible <- status %in% c("observed_complete", "observed_terminal")
  nEligible <- sum(eligible)
  firstState <- data$state_label[first]
  firstState[!atIndex] <- NA_character_
  firstDays <- data$observed_days[first]
  firstDays[!atIndex] <- NA_real_
  subjects <- data.frame(subject_id = patients$subject_id, entry_state = entryLabels,
                          first_state = firstState, first_cycle_days = firstDays,
                          eligible = eligible, status = status, stringsAsFactors = FALSE)
  statusLevels <- c("observed_complete", "observed_terminal", "incomplete_first_cycle",
                    "unresolved_first_cycle", "missing_first_cycle")
  coverage <- data.frame(status = statusLevels,
                         n = tabulate(match(status, statusLevels), nbins = length(statusLevels)))
  nStates <- length(states)
  counts <- matrix(0, nStates, nStates, dimnames = list(states, states))
  if (nEligible) {
    from <- match(entryLabels[eligible], states)
    to <- match(firstState[eligible], states)
    counts[] <- tabulate(from + (to - 1L) * nStates, nbins = nStates * nStates)
  }
  denominators <- rowSums(counts)
  probabilities <- counts / denominators
  probabilities[denominators == 0, ] <- NA_real_
  for (state in absorbingStates) {
    probabilities[state, ] <- 0
    probabilities[state, state] <- 1
  }
  estimates <- expand.grid(from_state = states, to_state = states,
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  estimates$count <- as.vector(counts)
  estimates$denominator <- unname(denominators[match(estimates$from_state, states)])
  estimates$probability <- as.vector(probabilities)
  estimates$structural <- estimates$from_state %in% absorbingStates
  makeCounts <- function(counts) {
    data.frame(state = states, count = unname(counts), denominator = nEligible,
               probability = if (nEligible) unname(counts) / nEligible else rep(NA_real_, nStates),
               stringsAsFactors = FALSE)
  }
  if (nEligible < nSubjects) {
    warning("Excluded ", nSubjects - nEligible, " of ", nSubjects,
            " patients from entry estimation because their first-cycle outcome was not evaluable. Inspect coverage and subjects. Estimates are conditional on evaluable outcomes, without censoring adjustment.",
            call. = FALSE)
  }
  list(estimates = estimates, matrix = probabilities,
       baselineCounts = makeCounts(rowSums(counts)), outcomeCounts = makeCounts(colSums(counts)),
       subjects = subjects, coverage = coverage,
       diagnostics = list(source_subjects = nSubjects, eligible_subjects = nEligible,
                          excluded_subjects = nSubjects - nEligible),
       metadata = list(states = states, absorbingStates = absorbingStates,
                       cycleLength = prepared$cycleLength, estimator = "entry_outcomes",
                       eligibility = "known first-cycle label with a complete cycle or an observed absorbing outcome",
                       denominator = "subjects with evaluable first-cycle outcomes in each supplied entry state",
                       baseline_population = "entry-eligible subjects only",
                       time_meaning = "supplied state before first cycle to its recorded outcome label"))
}
