#' Run independent trajectory analyses
#'
#' A convenience interface to the transition and cost estimators. Use either
#' estimator directly when only that result is needed. No plots or files are
#' created, and supplied database connections remain open.
#'
#' @param trajectories Current Cohort2Trajectory output or prepared trajectories.
#' @param cycleLength Days per cycle. Required for transition estimation unless
#'   already recorded by prepareTrajectories().
#' @param absorbingStates Explicit clinical terminal-state labels. START and
#'   EXIT are administrative markers, not clinical states.
#' @param states Optional complete state-label vector, including unobserved states.
#' @param analyses One or both of "transitions" and "costs". The default only
#'   estimates transitions and does not need a database.
#' @param cdm CDMConnector reference used only to extract costs. Not required
#'   when costData is supplied. The caller owns its connection.
#' @param costData Optional local cost records accepted by estimateStateCosts().
#'   Supply either cdm or costData for costing, not both.
#' @param costMeasure Cost column to extract from the CDM, such as "total_charge".
#' @param costDomains Optional cost domains to extract from the CDM.
#' @return A list containing transitions, costs and the prepared trajectories.
#'   An analysis not requested has a NULL result. No connection is stored.
#' @details This interface replaces the former database-driven workflow.
#'   It does not run the legacy GUI, continuous-time model, simulation or
#'   first-state cost reports. Transition estimates currently pool the observed
#'   cycles. Cost estimates use inclusive observed days. Period-specific and
#'   within-cycle models require separate, explicit rules.
#' @export
TrajectoryMarkovAnalysis <- function(trajectories,
                                     cycleLength = NULL,
                                     absorbingStates = character(),
                                     states = NULL,
                                     analyses = "transitions",
                                     cdm = NULL,
                                     costData = NULL,
                                     costMeasure = "total_charge",
                                     costDomains = NULL) {
  if (!is.character(analyses) || !length(analyses) || anyNA(analyses) ||
      anyDuplicated(analyses) || any(!analyses %in% c("transitions", "costs"))) {
    stop("analyses must contain 'transitions', 'costs', or both, without duplicates.", call. = FALSE)
  }
  costing <- "costs" %in% analyses
  if (!costing && (!is.null(cdm) || !is.null(costData))) {
    stop("Include 'costs' in analyses to use cdm or costData.", call. = FALSE)
  }
  if (costing && (is.null(cdm) == is.null(costData))) {
    stop("For costing, supply exactly one of cdm and costData.", call. = FALSE)
  }
  args <- list(trajectories = trajectories)
  if (!missing(cycleLength)) args$cycleLength <- cycleLength
  if (!missing(absorbingStates)) args$absorbingStates <- absorbingStates
  if (!missing(states)) args$states <- states
  prepared <- do.call(prepareTrajectories, args)
  transitions <- if ("transitions" %in% analyses) estimateTransitions(prepared) else NULL
  costs <- NULL
  if (costing) {
    if (!is.null(cdm)) {
      costData <- extractCosts(cdm, prepared, measure = costMeasure, domains = costDomains)
    }
    costs <- estimateStateCosts(prepared, costData)
  }
  list(transitions = transitions, costs = costs, trajectories = prepared)
}
