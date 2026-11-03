#' Build cycle transition models from supplied death probabilities
#'
#' @param mortality A nonempty data frame containing death_probability and any
#'   columns named in strata. death_probability must be a probability between
#'   zero and one over probabilityDays, not a mortality rate, hazard or count.
#'   Supply one row per exact combination of strata, or exactly one row when
#'   strata is NULL. Other columns are ignored and are not retained.
#' @param states Ordered clinical state labels, including deathState and at
#'   least one living state. All states other than deathState are living states.
#' @param deathState One state label from states. This state is absorbing.
#' @param cycleLength Required positive integer cycle length in days.
#' @param probabilityDays Required positive finite duration in days covered by
#'   each supplied death probability. For annual probabilities, explicitly
#'   choose the number of days representing a year, such as 365 or 365.25.
#' @param strata Optional character vector naming categorical columns in
#'   mortality, such as sex and age_group. Numeric values form exact-value
#'   groups. death_probability cannot be a stratum.
#' @param source Required nonblank description of the supplied evidence or
#'   assumption. This is a provenance label, not a file path or download request.
#' @details The conversion assumes a constant death hazard within the supplied
#'   interval. For an interval death probability q, the cycle probability is
#'   1 - (1 - q)^(cycleLength / probabilityDays), calculated without subtracting
#'   nearly equal numbers. Zero remains zero. A supplied probability of one
#'   produces certain death in every positive-length cycle. This endpoint does
#'   not represent a finite constant hazard. An open-ended life-table group may
#'   report a probability of one because everyone eventually dies. That value
#'   is not an annual probability and must not be supplied as one.
#'
#'   Each living state either remains unchanged or enters deathState, using the
#'   same death probability within its stratum. No transitions between living
#'   states, excess mortality or disease effects are inferred. This is a
#'   mortality-only model. Adding it to a model that already includes all-cause
#'   mortality would count background deaths again. Combining background and
#'   disease mortality requires a separate, explicitly defined model.
#'
#'   Use the result with createModelBundle() or extendModelBundle(). Strata are
#'   matched to the patient's covariates at the source cycle. The existing
#'   simulator can update age and grouping rules between cycles. This function
#'   does not create a new ageing rule, split a cycle at a birthday, interpolate
#'   missing ages, extend an open-ended age group or forecast calendar trends.
#'   Every stratum reached by living simulated patients must be supplied.
#'
#'   The model has one follow-up period. Its probabilities remain available for
#'   every cycle in which that stratum is selected. The analyst is responsible
#'   for whether this reuse is appropriate, including when the cycle is longer
#'   than the supplied probability interval. No external data are downloaded.
#' @return An ordinary transition-model list from composeTransitionStages(), or
#'   a tma_stratified_transitions object with models, a strata registry, periods
#'   and metadata. Each model retains the source label, probabilityDays, original
#'   death_probability, converted cycle_probability and conversion assumption in
#'   metadata$mortality. No observed counts or starting population are inferred.
#' @export
createMortalityTransitions <- function(mortality, states, deathState,
                                       cycleLength, probabilityDays,
                                       strata = NULL, source) {
  if (missing(cycleLength) || missing(probabilityDays)) {
    stop("Supply cycleLength and probabilityDays explicitly, both in days.", call. = FALSE)
  }
  cycleLength <- .tmaProjectionInteger(cycleLength, "cycleLength", minimum = 1)
  if (!is.numeric(probabilityDays) || is.complex(probabilityDays) ||
      inherits(probabilityDays, "integer64") || length(probabilityDays) != 1L ||
      !is.null(dim(probabilityDays)) || is.na(probabilityDays) ||
      !is.finite(probabilityDays) || probabilityDays <= 0) {
    stop("probabilityDays must be one positive finite duration in days.", call. = FALSE)
  }
  if (missing(source) || !is.character(source) || length(source) != 1L ||
      !is.null(dim(source)) || is.na(source) || !nzchar(trimws(source))) {
    stop("source must be one nonblank analyst-supplied label.", call. = FALSE)
  }
  if (missing(states) || !is.character(states) || !is.null(dim(states)) ||
      length(states) < 2L) {
    stop("states must declare a death state and at least one living state.", call. = FALSE)
  }
  if (missing(deathState) || !is.character(deathState) || length(deathState) != 1L ||
      !is.null(dim(deathState)) || is.na(deathState) || !deathState %in% states) {
    stop("deathState must be one state label declared in states.", call. = FALSE)
  }
  dictionary <- .tmaStateDictionary(character(), states, deathState)
  states <- dictionary$states
  if (!is.data.frame(mortality) || !nrow(mortality) ||
      anyDuplicated(names(mortality)) || !"death_probability" %in% names(mortality)) {
    stop("mortality must be a nonempty data frame with unique column names and death_probability.",
         call. = FALSE)
  }
  probability <- mortality$death_probability
  if (!is.numeric(probability) || is.complex(probability) ||
      inherits(probability, "integer64") || !is.null(dim(probability)) ||
      anyNA(probability) || any(!is.finite(probability) | probability < 0 | probability > 1)) {
    stop("death_probability must contain finite numeric probabilities between zero and one.",
         call. = FALSE)
  }
  grouping <- NULL
  if (is.null(strata)) {
    if (nrow(mortality) != 1L) {
      stop("mortality must have exactly one row when strata is NULL.", call. = FALSE)
    }
  } else {
    if (!is.character(strata) || !is.null(dim(strata)) || !length(strata) ||
        anyNA(strata) || any(!nzchar(trimws(strata))) || anyDuplicated(strata)) {
      stop("strata must be NULL or a character vector of unique, nonempty column names.",
           call. = FALSE)
    }
    if ("death_probability" %in% strata) {
      stop("death_probability cannot be used as a stratum.", call. = FALSE)
    }
    # Ignore unrelated columns before registry construction. In particular, a
    # supplied state_label must not make a mortality row administrative.
    grouping <- .tmaStrata(mortality[, intersect(strata, names(mortality)), drop = FALSE], strata)
    if (anyDuplicated(grouping$row_ids)) {
      stop("mortality must have one row per exact combination of strata.", call. = FALSE)
    }
  }

  # Multiply first so an overflowing duration ratio does not turn tiny input
  # probabilities into certain death. The validated cycle length bounds this
  # multiplication for every representable probability below one.
  cycleProbability <- -expm1((log1p(-probability) * cycleLength) / probabilityDays)
  provenance <- list(source = source, probabilityDays = probabilityDays,
    conversion = "constant death hazard within probabilityDays; q = 1 means certain death")
  makeModel <- function(row) {
    matrix <- diag(length(states))
    dimnames(matrix) <- list(states, states)
    living <- which(states != deathState)
    matrix[cbind(living, living)] <- 1 - cycleProbability[row]
    matrix[living, deathState] <- cycleProbability[row]
    model <- composeTransitionStages(list(mortality = matrix),
      cycleLength = cycleLength, absorbingStates = deathState)
    model$metadata$estimator <- "specified_mortality"
    model$metadata$probability_source <- "analyst-supplied interval death probabilities"
    model$metadata$mortality <- c(provenance,
      list(death_probability = unname(probability[row]),
           cycle_probability = unname(cycleProbability[row])))
    model
  }
  if (is.null(grouping)) return(makeModel(1L))

  registry <- grouping$registry
  models <- lapply(seq_len(nrow(registry)), function(i) {
    model <- makeModel(match(registry$stratum_id[i], grouping$row_ids))
    model$metadata$strata <- grouping$columns
    model$metadata$stratum_id <- registry$stratum_id[i]
    model
  })
  names(models) <- registry$stratum_id
  metadata <- models[[1L]]$metadata
  metadata$stratum_id <- NULL
  metadata$mortality <- provenance
  metadata$stratum_assignment <- "source-cycle covariates matched to supplied mortality strata"
  structure(list(models = models, strata = registry, periods = models[[1L]]$periods,
                 metadata = metadata), class = "tma_stratified_transitions")
}
