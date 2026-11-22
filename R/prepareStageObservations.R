#' Prepare ordered stage observations from aligned trajectory views
#'
#' @param trajectories Named list of at least two discrete Cohort2Trajectory
#'   data frames or prepared trajectories, in within-cycle execution order.
#'   Each view supplies the state after its stage, with a cycle length supplied
#'   by its trajectory definition or by prepareTrajectories(). The final view
#'   supplies the complete state dictionary and absorbing states.
#' @param entryState Clinical state before the first cycle, supplied as one
#'   shared label or a data frame with subject_id and entry_state covering every
#'   patient exactly once. Entry states are never inferred from outcomes.
#' @param requireNextCycle One nonmissing logical value. FALSE evaluates a
#'   complete cycle or a final absorbing outcome. TRUE instead requires an
#'   adjacent next observed cycle or a final absorbing outcome, including when
#'   the last nonabsorbing cycle spans the full cycle length.
#' @details Views must have identical patients, index dates, observation ends
#'   and cycle intervals, without gaps or unresolved states. Row order may
#'   differ. The first stage starts from the preceding cycle's final outcome,
#'   or entryState for the first cycle. Later stages start from the current
#'   cycle's preceding view. All stages share outcome evaluability.
#'
#'   This pairs supplied state views; it does not infer event order or restore
#'   events hidden by state selection. The analyst must define each view so it
#'   represents the intended intermediate state, including in terminal cycles.
#'   Absorbing states cannot be left within or between cycles. Stage order adds
#'   no elapsed time. Probability estimation and any pooling assumptions remain
#'   the responsibility of estimateStageTransitions().
#'   Row indexing with observations[rows, , drop = FALSE] preserves the compact
#'   definition. If another operation drops it, pass definition and stageOrder
#'   explicitly to estimateStageTransitions().
#' @return A data frame with subject_id, source_day, stage, from_state, to_state,
#'   observed and index_date. Unevaluable destinations are NA while known source
#'   labels remain. Attributes trajectory_definition and stage_order carry only
#'   the compact model definition and execution order for stage estimation.
#' @export
prepareStageObservations <- function(trajectories, entryState, requireNextCycle = FALSE) {
  if (!is.list(trajectories) || is.data.frame(trajectories) ||
      !is.null(dim(trajectories)) || length(trajectories) < 2L ||
      is.null(names(trajectories)) || anyNA(names(trajectories)) ||
      anyDuplicated(names(trajectories)) || any(!nzchar(trimws(names(trajectories))))) {
    stop("trajectories must be a named list of at least two views with unique, nonempty stage names.",
         call. = FALSE)
  }
  stageOrder <- names(trajectories)
  if (any(stageOrder %in% c("start", "end", "transition"))) {
    stop("Stage names 'start', 'end' and 'transition' are reserved for interval timing and net transitions.",
         call. = FALSE)
  }
  if (!is.logical(requireNextCycle) || length(requireNextCycle) != 1L ||
      !is.null(dim(requireNextCycle)) || is.na(requireNextCycle)) {
    stop("requireNextCycle must be one nonmissing TRUE/FALSE value.", call. = FALSE)
  }
  if (missing(entryState)) {
    stop("Supply entryState explicitly. The starting state is not inferred from first-cycle outcomes.",
         call. = FALSE)
  }
  views <- lapply(trajectories, prepareTrajectories)
  final <- views[[length(views)]]
  definition <- .tmaTrajectoryDefinition(final, preparedFields = TRUE)
  cycleLength <- final$cycleLength
  if (is.null(cycleLength)) {
    stop("Every view must supply a discrete cycleLength through its definition or prepareTrajectories().",
         call. = FALSE)
  }
  states <- definition$states
  absorbing <- definition$absorbingStates
  patientFields <- c("subject_id", "index_date", "observation_end")
  cycleFields <- c("subject_id", "state_start_date", "state_end_date")
  for (i in seq_along(views)) {
    view <- views[[i]]
    if (is.null(view$cycleLength) || view$cycleLength != cycleLength) {
      stop("All views must have the same discrete cycleLength.", call. = FALSE)
    }
    if (length(setdiff(view$states, states)) ||
        length(setdiff(view$absorbingStates, absorbing))) {
      stop("The final view must declare every view's states and absorbing states.", call. = FALSE)
    }
    view$patients <- view$patients[order(view$patients$subject_id), , drop = FALSE]
    view$data <- view$data[order(view$data$subject_id, view$data$state_start_date), , drop = FALSE]
    rownames(view$patients) <- rownames(view$data) <- NULL
    .tmaValidateCycles(view$data, view$patients, cycleLength)
    .tmaValidateAbsorbing(view$data, absorbing)
    first <- !duplicated(view$data$subject_id)
    index <- view$patients$index_date[match(view$data$subject_id, view$patients$subject_id)]
    previousEnd <- c(as.Date(NA), head(view$data$state_end_date, -1L))
    if (any(view$data$state_label == "OUT OF COHORT") ||
        any(view$data$state_start_date[first] != index[first]) ||
        any(view$data$state_start_date[!first] != previousEnd[!first] + 1)) {
      stop("Every view must cover consecutive cycles from index_date without gaps or unresolved states.",
           call. = FALSE)
    }
    views[[i]] <- view
  }
  final <- views[[length(views)]]
  for (view in views) {
    if (!identical(view$patients[patientFields], final$patients[patientFields]) ||
        !identical(view$data[cycleFields], final$data[cycleFields])) {
      stop("Views must align on identical patients, index dates, observation ends and cycle intervals.",
           call. = FALSE)
    }
  }
  patients <- final$patients
  if (is.character(entryState) && length(entryState) == 1L && is.null(dim(entryState))) {
    entries <- rep(entryState, nrow(patients))
  } else if (is.data.frame(entryState) && !anyDuplicated(names(entryState)) &&
             all(c("subject_id", "entry_state") %in% names(entryState))) {
    ids <- .tmaSubjectIds(entryState$subject_id, "entryState$subject_id")
    if (anyDuplicated(ids) || !setequal(ids, patients$subject_id)) {
      stop("entryState must contain every trajectory subject exactly once, with no extra subjects.",
           call. = FALSE)
    }
    if (!is.character(entryState$entry_state) && !is.factor(entryState$entry_state)) {
      stop("entryState$entry_state must contain clinical state labels.", call. = FALSE)
    }
    entries <- as.character(entryState$entry_state[match(patients$subject_id, ids)])
  } else {
    stop("entryState must be one clinical state label or a subject_id/entry_state data frame.",
         call. = FALSE)
  }
  if (anyNA(entries) || any(!nzchar(trimws(entries))) || any(!entries %in% states)) {
    stop("Every entry state must be a nonmissing clinical label in the final view's states.",
         call. = FALSE)
  }
  data <- final$data
  first <- !duplicated(data$subject_id)
  last <- !duplicated(data$subject_id, fromLast = TRUE)
  patientRow <- match(data$subject_id, patients$subject_id)
  index <- patients$index_date[patientRow]
  observed <- if (requireNextCycle) !last else data$observed_days == cycleLength
  observed <- observed | data$state_label %in% absorbing
  source <- c(NA_character_, head(data$state_label, -1L))
  source[first] <- entries[patientRow[first]]
  rows <- vector("list", length(views))
  for (i in seq_along(views)) {
    destination <- views[[i]]$data$state_label
    if (any(source %in% absorbing & destination != source)) {
      stop("A stage cannot leave an absorbing state, including an absorbing entry state.", call. = FALSE)
    }
    outcome <- destination
    outcome[!observed] <- NA_character_
    rows[[i]] <- data.frame(subject_id = data$subject_id,
      source_day = as.numeric(data$state_start_date - index), stage = stageOrder[i],
      from_state = source, to_state = outcome, observed = observed, index_date = index,
      stringsAsFactors = FALSE)
    source <- destination
  }
  result <- do.call(rbind, rows)
  result <- result[order(result$subject_id, result$source_day,
                         match(result$stage, stageOrder)), , drop = FALSE]
  rownames(result) <- NULL
  attr(result, "trajectory_definition") <- definition
  attr(result, "stage_order") <- stageOrder
  result
}
