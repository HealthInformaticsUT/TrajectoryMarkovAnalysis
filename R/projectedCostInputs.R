# Validate the projection information used to attach costs to expected flows.
# This does not alter probabilities, reconstruct events or expose identifiers.
.tmaCostProjection <- function(projection) {
  projection <- .tmaComparisonProjection(projection)
  model <- projection$metadata
  states <- model$states
  nStates <- length(states)
  nCycles <- model$nCycles
  cycleLength <- model$cycleLength
  cohortSize <- model$cohortSize
  entryMode <- identical(model$initial_source, "entry_model")
  nBlocks <- nCycles + as.integer(entryMode)
  nPairs <- nStates^2
  if ((nCycles + 1) * nStates > .Machine$integer.max ||
      nBlocks * nPairs > .Machine$integer.max || nCycles * cycleLength >= 2^53) {
    stop("Projection horizon exceeds supported flow indexing or exact elapsed days.", call. = FALSE)
  }

  periods <- model$periods
  if (!is.data.frame(periods) || anyDuplicated(names(periods)) || !nrow(periods) ||
      !all(c("period", "start_day", "end_day") %in% names(periods))) {
    stop("Projection periods must describe contiguous periods from day 0 to Inf.", call. = FALSE)
  }
  for (field in c("start_day", "end_day")) {
    x <- periods[[field]]
    if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") ||
        !is.null(dim(x)) || anyNA(x)) {
      stop("Projection period boundaries must be numeric days.", call. = FALSE)
    }
  }
  checkedPeriods <- .tmaPeriods(c(periods$start_day, tail(periods$end_day, 1L)),
                                periods$period, cycleLength)
  if (any(periods$end_day != checkedPeriods$end_day)) {
    stop("Projection periods must be contiguous, without gaps or overlaps.", call. = FALSE)
  }

  steps <- projection$steps
  stepFields <- c("source_cycle", "source_day", "destination_cycle", "destination_day", "period")
  if (!is.data.frame(steps) || anyDuplicated(names(steps)) ||
      !all(stepFields %in% names(steps)) || nrow(steps) != nCycles ||
      !is.character(steps$period) || anyNA(steps$period)) {
    stop("Projection steps must describe every adjacent-cycle step exactly once.", call. = FALSE)
  }
  for (field in setdiff(stepFields, "period")) {
    .tmaCostFlowNumeric(steps[[field]], paste0("Projection steps$", field))
  }
  if (anyDuplicated(steps$source_cycle) ||
      !setequal(steps$source_cycle, seq_len(nCycles) - 1L) ||
      any(steps$destination_cycle != steps$source_cycle + 1 |
            steps$source_day != steps$source_cycle * cycleLength |
            steps$destination_day != steps$destination_cycle * cycleLength) ||
      any(steps$period != periods$period[.tmaPeriodIndex(steps$source_day, periods)])) {
    stop("Projection step timing or periods disagree with its metadata.", call. = FALSE)
  }
  steps <- steps[match(seq_len(nCycles), steps$destination_cycle), , drop = FALSE]

  baseline <- entryMatrix <- NULL
  if (entryMode) {
    baseline <- projection$entry$baseline_distribution
    if (!is.numeric(baseline) || is.complex(baseline) || inherits(baseline, "integer64") ||
        !is.null(dim(baseline)) || is.null(names(baseline)) ||
        anyNA(names(baseline)) || anyDuplicated(names(baseline)) ||
        !setequal(names(baseline), states) || anyNA(baseline) ||
        any(!is.finite(baseline) | baseline < 0 | baseline > 1) ||
        abs(sum(baseline) - 1) > 1e-10) {
      stop("Projection entry baseline_distribution must be a named probability vector over all states.", call. = FALSE)
    }
    baseline <- baseline[states]
    entryMatrix <- .tmaTransitionMatrix(projection$entry$matrix, states,
                                      model$absorbingStates, "Projection entry matrix")
  }

  flows <- projection$flows
  flowFields <- c("stage", stepFields, "from_state", "to_state",
                  "transition_probability", "probability_mass", "expected_count")
  if (!is.data.frame(flows) || anyDuplicated(names(flows)) ||
      !all(flowFields %in% names(flows)) || nrow(flows) != nBlocks * nPairs) {
    stop("Projection flows must include every state pair for each step and, when used, entry.", call. = FALSE)
  }
  for (field in c("stage", "period", "from_state", "to_state")) {
    if (!is.character(flows[[field]]) || !is.null(dim(flows[[field]]))) {
      stop("Projection flows$", field, " must contain character labels.", call. = FALSE)
    }
  }
  if (anyNA(flows$stage) || any(!flows$stage %in% c("entry", "transition")) ||
      anyNA(flows$from_state) || anyNA(flows$to_state) ||
      any(!flows$from_state %in% states | !flows$to_state %in% states)) {
    stop("Projection flow stages or state labels are invalid.", call. = FALSE)
  }
  for (field in c("source_cycle", "source_day", "destination_cycle", "destination_day",
                  "transition_probability", "probability_mass", "expected_count")) {
    .tmaCostFlowNumeric(flows[[field]], paste0("Projection flows$", field),
                        allowNA = field %in% c("source_cycle", "source_day", "transition_probability"))
  }
  isEntry <- flows$stage == "entry"
  adjacent <- !isEntry
  if ((!entryMode && any(isEntry)) ||
      any(!is.na(flows$source_cycle[isEntry]) | !is.na(flows$source_day[isEntry]) |
            !is.na(flows$period[isEntry]) | flows$destination_cycle[isEntry] != 0 |
            flows$destination_day[isEntry] != 0) ||
      anyNA(flows$source_cycle[adjacent]) || anyNA(flows$source_day[adjacent]) ||
      anyNA(flows$period[adjacent]) ||
      any(flows$destination_cycle[adjacent] < 1 | flows$destination_cycle[adjacent] > nCycles |
            flows$destination_cycle[adjacent] != trunc(flows$destination_cycle[adjacent]))) {
    stop("Projection entry and adjacent flow timing is invalid.", call. = FALSE)
  }
  for (field in stepFields) {
    if (any(flows[[field]][adjacent] != steps[[field]][flows$destination_cycle[adjacent]])) {
      stop("Projection flow timing and periods must match its steps.", call. = FALSE)
    }
  }
  if (any(flows$probability_mass < 0 | flows$probability_mass > 1 + 1e-8) ||
      any(flows$expected_count < 0) ||
      any(abs(flows$expected_count - flows$probability_mass * cohortSize) >
            1e-8 * pmax(1, abs(flows$probability_mass * cohortSize)))) {
    stop("Projection flow masses or expected counts disagree with its cohortSize.", call. = FALSE)
  }
  block <- flows$destination_cycle + as.integer(entryMode)
  block[isEntry] <- 1L
  pair <- match(flows$from_state, states) + (match(flows$to_state, states) - 1L) * nStates
  if (anyDuplicated((block - 1) * nPairs + pair) ||
      any(tabulate(block, nbins = nBlocks) != nPairs)) {
    stop("Projection flows must contain every state pair exactly once per step.", call. = FALSE)
  }
  if (!nBlocks) return(projection)

  occupancy <- matrix(0, nrow = nCycles + 1, ncol = nStates)
  occupancy[cbind(projection$occupancy$cycle + 1L,
                  match(projection$occupancy$state, states))] <- projection$occupancy$probability
  blockRows <- split(seq_len(nrow(flows)), block)
  periodMatrices <- vector("list", nrow(periods))
  for (i in seq_len(nBlocks)) {
    rows <- blockRows[[as.character(i)]]
    first <- rows[1L]
    destinationCycle <- flows$destination_cycle[first]
    source <- if (isEntry[first]) baseline else occupancy[flows$source_cycle[first] + 1L, ]
    conditional <- joint <- matrix(0, nStates, nStates, dimnames = list(states, states))
    conditional[pair[rows]] <- flows$transition_probability[rows]
    joint[pair[rows]] <- flows$probability_mass[rows]
    conditional <- .tmaTransitionMatrix(conditional, states, model$absorbingStates,
                                     paste0("Projection flow block ", i))
    reference <- if (isEntry[first]) entryMatrix else {
      periodIndex <- match(flows$period[first], periods$period)
      periodMatrices[[periodIndex]]
    }
    if (!is.null(reference) &&
        (any(is.na(conditional) != is.na(reference)) ||
         any(abs(conditional[!is.na(conditional)] - reference[!is.na(reference)]) > 1e-10))) {
      stop("Projection flow conditional probabilities disagree with the entry matrix or another step in the same period.", call. = FALSE)
    }
    if (!isEntry[first]) periodMatrices[[periodIndex]] <- conditional
    occupied <- which(source > 0)
    if (anyNA(conditional[occupied, , drop = FALSE])) {
      stop("Projection flow conditional probabilities are missing for an occupied source state.", call. = FALSE)
    }
    expected <- matrix(0, nStates, nStates)
    expected[occupied, ] <- conditional[occupied, , drop = FALSE] * source[occupied]
    if (any(joint[expected == 0] != 0) || any(abs(joint - expected) > 1e-10)) {
      stop("Projection joint flow masses must equal source probabilities times conditional probabilities.", call. = FALSE)
    }
    if (any(abs(rowSums(joint) - source) > 1e-8) ||
        any(abs(colSums(joint) - occupancy[destinationCycle + 1L, ]) > 1e-8)) {
      stop("Projection flow marginals disagree with source or destination occupancy.", call. = FALSE)
    }
  }
  projection
}

.tmaCostFlowNumeric <- function(x, name, allowNA = FALSE) {
  if (!is.numeric(x) || is.complex(x) || inherits(x, "integer64") ||
      !is.null(dim(x)) || any(is.nan(x)) || (!allowNA && anyNA(x)) ||
      any(!is.finite(x[!is.na(x)]))) {
    stop(name, " must contain finite numeric values", if (allowNA) " or permitted NA values" else "", ".", call. = FALSE)
  }
  invisible(NULL)
}
