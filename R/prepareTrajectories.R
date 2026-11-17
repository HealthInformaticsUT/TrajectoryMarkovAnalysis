#' Validate and prepare Cohort2Trajectory intervals
#'
#' @param trajectories A data frame with lowercase subject_id, state_label,
#'   state_start_date and state_end_date columns, or a tma_trajectories object.
#'   Additional columns and unspecified prepared metadata are retained.
#' @param cycleLength Optional positive integer cycle length in days. Leave NULL
#'   for interval-based cost analyses.
#' @param absorbingStates Clinical state labels that terminate follow-up.
#' @param states Optional ordered clinical state dictionary, including unobserved
#'   states. START, EXIT and OUT OF COHORT are not clinical states.
#' @return A tma_trajectories list containing data, patients, states,
#'   absorbingStates, cycleLength and diagnostics. Dates are inclusive. Without
#'   START or EXIT, boundaries are inferred from observed intervals and cannot
#'   independently verify the original observation window. At least one observed
#'   interval is required per subject. Without cycleLength, EXIT may preserve an
#'   observation window that extends beyond the final retained interval.
#' @export
prepareTrajectories <- function(trajectories, cycleLength = NULL,
                                absorbingStates = character(), states = NULL) {
  if (inherits(trajectories, "tma_trajectories")) {
    if (missing(cycleLength) && missing(absorbingStates) && missing(states)) {
      return(trajectories)
    }
    prepared <- trajectories
    if (missing(cycleLength)) cycleLength <- prepared$cycleLength
    if (missing(absorbingStates)) absorbingStates <- prepared$absorbingStates
    if (missing(states)) states <- prepared$states
    cycleLength <- .tmaCycleLength(cycleLength)
    dictionary <- .tmaStateDictionary(prepared$data$state_label, states, absorbingStates)
    .tmaValidateAbsorbing(prepared$data, dictionary$absorbingStates)
    prepared$diagnostics$partial_cycles <- .tmaValidateCycles(
      prepared$data, prepared$patients, cycleLength)
    prepared$states <- dictionary$states
    prepared$absorbingStates <- dictionary$absorbingStates
    prepared$cycleLength <- cycleLength
    return(prepared)
  }
  required <- c("subject_id", "state_label", "state_start_date", "state_end_date")
  if (!is.data.frame(trajectories) || !all(required %in% names(trajectories))) {
    stop("trajectories must be a data frame with lowercase columns: ",
         paste(required, collapse = ", "), ".", call. = FALSE)
  }
  if (anyDuplicated(names(trajectories)) || nrow(trajectories) == 0L) {
    stop("trajectories must have unique column names and at least one row.", call. = FALSE)
  }
  cycleLength <- .tmaCycleLength(cycleLength)
  data <- as.data.frame(trajectories, stringsAsFactors = FALSE)
  data$subject_id <- .tmaSubjectIds(data$subject_id)
  data$state_label <- as.character(data$state_label)
  if (anyNA(data$state_label) || any(!nzchar(trimws(data$state_label)))) {
    stop("state_label must contain nonempty, nonmissing labels.", call. = FALSE)
  }
  data$state_start_date <- .tmaDates(data$state_start_date, "state_start_date")
  data$state_end_date <- .tmaDates(data$state_end_date, "state_end_date")
  if (any(data$state_end_date < data$state_start_date)) {
    stop("state_end_date cannot precede state_start_date.", call. = FALSE)
  }
  if (anyDuplicated(data[required])) {
    stop("Duplicate trajectory intervals are not allowed.", call. = FALSE)
  }
  data <- data[order(data$subject_id, data$state_start_date,
                     data$state_end_date, data$state_label), , drop = FALSE]
  boundary <- data$state_label %in% c("START", "EXIT")
  markers <- data[boundary, , drop = FALSE]
  data <- data[!boundary, , drop = FALSE]
  ids <- sort(unique(c(data$subject_id, markers$subject_id)))
  patients <- data.frame(subject_id = ids,
                         index_date = as.Date(rep(NA_character_, length(ids))),
                         observation_end = as.Date(rep(NA_character_, length(ids))),
                         stringsAsFactors = FALSE)
  hasStart <- hasExit <- rep(FALSE, length(ids))
  intervalGroups <- split(seq_len(nrow(data)), factor(data$subject_id, levels = ids))
  markerGroups <- split(seq_len(nrow(markers)), factor(markers$subject_id, levels = ids))
  gapCount <- 0L
  for (i in seq_along(ids)) {
    rows <- intervalGroups[[i]]
    markerRows <- markers[markerGroups[[i]], , drop = FALSE]
    starts <- markerRows[markerRows$state_label == "START", , drop = FALSE]
    exits <- markerRows[markerRows$state_label == "EXIT", , drop = FALSE]
    if (!length(rows)) {
      stop("Each subject must have at least one observed interval: ", ids[i], ".", call. = FALSE)
    }
    if (nrow(starts) > 1L || nrow(exits) > 1L) {
      stop("Only one target episode (one START and one EXIT) is supported per subject.", call. = FALSE)
    }
    if (any(markerRows$state_start_date != markerRows$state_end_date)) {
      stop("START and EXIT must be single-date boundary markers.", call. = FALSE)
    }
    firstDate <- data$state_start_date[rows[1L]]
    lastDate <- data$state_end_date[rows[length(rows)]]
    if (nrow(starts)) {
      if (starts$state_start_date > firstDate) {
        stop("START cannot follow the first observed interval.", call. = FALSE)
      }
      patients$index_date[i] <- starts$state_start_date
      hasStart[i] <- TRUE
    } else {
      patients$index_date[i] <- firstDate
    }
    if (nrow(exits)) {
      if (exits$state_start_date < lastDate + 1) {
        stop("EXIT must follow the final observed interval.", call. = FALSE)
      }
      if (!is.null(cycleLength) && exits$state_start_date != lastDate + 1) {
        stop("EXIT must be the day after the final observed interval.", call. = FALSE)
      }
      hasExit[i] <- TRUE
      patients$observation_end[i] <- exits$state_start_date - 1
    } else {
      patients$observation_end[i] <- lastDate
    }
    if (length(rows) > 1L) {
      previous <- rows[-length(rows)]
      following <- rows[-1L]
      if (any(data$state_start_date[following] <= data$state_end_date[previous])) {
        stop("Observed intervals must not overlap within a subject.", call. = FALSE)
      }
      gapCount <- gapCount + sum(data$state_start_date[following] >
                                 data$state_end_date[previous] + 1)
    }
  }
  rownames(data) <- NULL
  data$.row_id <- seq_len(nrow(data))
  data$observed_days <- as.numeric(data$state_end_date - data$state_start_date) + 1
  dictionary <- .tmaStateDictionary(data$state_label, states, absorbingStates)
  .tmaValidateAbsorbing(data, dictionary$absorbingStates)
  partial <- .tmaValidateCycles(data, patients, cycleLength)
  structure(list(
    data = data, patients = patients, states = dictionary$states,
    absorbingStates = dictionary$absorbingStates, cycleLength = cycleLength,
    diagnostics = list(n_subjects = nrow(patients), n_intervals = nrow(data),
                       boundary_markers_removed = nrow(markers),
                       subjects_without_start = ids[!hasStart],
                       subjects_without_exit = ids[!hasExit],
                       unresolved_intervals = sum(data$state_label == "OUT OF COHORT"),
                       gap_count = gapCount, partial_cycles = partial)
  ), class = "tma_trajectories")
}

.tmaSubjectIds <- function(x, name = "subject_id") {
  if (inherits(x, "integer64")) {
    ids <- as.character(x)
  } else if (is.character(x) || is.factor(x)) {
    ids <- as.character(x)
  } else if (is.numeric(x) && !is.complex(x)) {
    if (anyNA(x) || any(!is.finite(x) | x != trunc(x) | abs(x) >= 2^53)) {
      stop(name, " must contain exact IDs; supply large identifiers as character strings.", call. = FALSE)
    }
    ids <- sprintf("%.0f", x)
  } else {
    stop(name, " must contain character, integer64 or exact integer identifiers.", call. = FALSE)
  }
  if (anyNA(ids) || any(!nzchar(trimws(ids)))) {
    stop(name, " must not contain missing or blank identifiers.", call. = FALSE)
  }
  ids
}

.tmaDates <- function(x, name) {
  if (inherits(x, "Date")) {
    dates <- x
  } else if (is.character(x)) {
    if (anyNA(x) || any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))) {
      stop(name, " must contain Date values or strict ISO YYYY-MM-DD strings.", call. = FALSE)
    }
    dates <- as.Date(x, format = "%Y-%m-%d")
    if (anyNA(dates) || any(format(dates, "%Y-%m-%d") != x)) {
      stop(name, " contains invalid dates.", call. = FALSE)
    }
  } else {
    stop(name, " must contain Date values or strict ISO YYYY-MM-DD strings.", call. = FALSE)
  }
  if (anyNA(dates) || any(!is.finite(as.numeric(dates))) ||
      any(as.numeric(dates) != trunc(as.numeric(dates)))) {
    stop(name, " contains invalid dates.", call. = FALSE)
  }
  dates
}

.tmaCycleLength <- function(cycleLength) {
  if (!is.null(cycleLength) &&
      (!is.numeric(cycleLength) || is.complex(cycleLength) || length(cycleLength) != 1L ||
       is.na(cycleLength) || !is.finite(cycleLength) ||
       cycleLength <= 0 || cycleLength != trunc(cycleLength))) {
    stop("cycleLength must be a positive integer number of days or NULL.", call. = FALSE)
  }
  cycleLength
}

.tmaStateDictionary <- function(observed, states, absorbingStates) {
  validateLabels <- function(x, name) {
    if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) || anyDuplicated(x)) {
      stop(name, " must be a character vector of unique, nonempty state labels.", call. = FALSE)
    }
    if (any(x %in% c("START", "EXIT", "OUT OF COHORT"))) {
      stop(name, " must contain clinical states only, excluding START, EXIT and OUT OF COHORT.", call. = FALSE)
    }
  }
  validateLabels(absorbingStates, "absorbingStates")
  observed <- setdiff(unique(observed), "OUT OF COHORT")
  if (is.null(states)) {
    states <- sort(unique(c(observed, absorbingStates)))
  } else {
    validateLabels(states, "states")
    if (length(setdiff(c(observed, absorbingStates), states))) {
      stop("states must include every observed clinical state and declared absorbing state.", call. = FALSE)
    }
  }
  list(states = states, absorbingStates = absorbingStates)
}

.tmaValidateAbsorbing <- function(data, absorbingStates) {
  last <- !duplicated(data$subject_id, fromLast = TRUE)
  if (any(data$state_label %in% absorbingStates & !last)) {
    stop("An absorbing state must be the final observed interval for its subject.", call. = FALSE)
  }
  invisible(NULL)
}

.tmaValidateCycles <- function(data, patients, cycleLength) {
  if (is.null(cycleLength)) return(NA_integer_)
  last <- !duplicated(data$subject_id, fromLast = TRUE)
  observationEnds <- patients$observation_end[match(data$subject_id[last], patients$subject_id)]
  if (any(observationEnds != data$state_end_date[last])) {
    stop("For cycle data, observation_end must equal the final observed cycle end; trailing gaps are invalid.", call. = FALSE)
  }
  origins <- patients$index_date[match(data$subject_id, patients$subject_id)]
  elapsed <- as.numeric(data$state_start_date - origins)
  if (any(elapsed < 0 | elapsed %% cycleLength != 0)) {
    stop("Cycle starts must align with index_date and cycleLength.", call. = FALSE)
  }
  if (any(data$observed_days > cycleLength)) {
    stop("Observed intervals cannot exceed cycleLength; provide one row per cycle.", call. = FALSE)
  }
  partial <- data$observed_days < cycleLength
  if (any(partial & !last)) {
    stop("Only the final observed cycle may be incomplete; intermediate partial cycles are invalid.", call. = FALSE)
  }
  sum(partial)
}
