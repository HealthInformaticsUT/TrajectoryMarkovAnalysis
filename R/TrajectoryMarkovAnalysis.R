#' Run independent trajectory analyses
#'
#' A convenience interface to the transition and cost estimators. Use either
#' estimator directly when only that result is needed. No plots or files are
#' created, and supplied database connections remain open.
#'
#' @param trajectories Current Cohort2Trajectory output or prepared trajectories.
#' @param cycleLength Days per cycle. Required for transition estimation unless
#'   supplied by the trajectory definition or prepared trajectories.
#' @param absorbingStates Clinical absorbing state labels. START and
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
#' @param costVisitFilter Optional visit selection passed to extractCosts().
#'   Only valid when extracting costs through cdm. To use local costData, select
#'   its records before calling this function.
#' @param periodBreaks Follow-up boundaries in days since index, starting at 0
#'   and ending at Inf. NULL pools follow-up. Transition boundaries must align
#'   with cycleLength. Cost-only analyses may split intervals at any whole day.
#' @param periodLabels Optional unique names for the follow-up periods.
#' @param terminalCostMap Optional named mapping from declared absorbing states
#'   to defined nonabsorbing cost states, as in estimateStateCosts(). Only used
#'   when analyses includes "costs". Clinical trajectories remain unchanged.
#' @param strata Optional categorical trajectory column names passed to both
#'   estimators. Values may change by row; numeric values group by exact value.
#' @param costAllocation Cost allocation passed to extractCosts() and
#'   estimateStateCosts(). "start" assigns the amount to the event date.
#'   "duration" spreads it uniformly across the full inclusive event duration.
#'   NULL uses "start" for CDM extraction and otherwise preserves the allocation
#'   recorded by extractCosts(), or uses "start" for plain local costData.
#'   Only valid when analyses includes "costs".
#' @return A list containing transitions, costs and the prepared trajectories.
#'   An analysis not requested has a NULL result. No connection is stored.
#' @details Transitions use the source cycle's period.
#'   Costs use the selected allocation rule and split observed days at period
#'   boundaries. Duration allocation retains unobserved shares for review rather
#'   than redistributing them across observed days. Within-cycle models require
#'   separate, explicit rules.
#' @export
TrajectoryMarkovAnalysis <- function(trajectories,
                                     cycleLength = NULL,
                                     absorbingStates = character(),
                                     states = NULL,
                                     analyses = "transitions",
                                     cdm = NULL,
                                     costData = NULL,
                                     costMeasure = "total_charge",
                                     costDomains = NULL,
                                     periodBreaks = NULL,
                                     periodLabels = NULL,
                                     terminalCostMap = NULL,
                                     costVisitFilter = NULL,
                                     strata = NULL,
                                     costAllocation = NULL) {
  if (!is.character(analyses) || !length(analyses) || anyNA(analyses) ||
      anyDuplicated(analyses) || any(!analyses %in% c("transitions", "costs"))) {
    stop("analyses must contain 'transitions', 'costs', or both, without duplicates.", call. = FALSE)
  }
  costing <- "costs" %in% analyses
  if (!costing && (!is.null(cdm) || !is.null(costData))) {
    stop("Include 'costs' in analyses to use cdm or costData.", call. = FALSE)
  }
  if (!costing && !is.null(terminalCostMap)) {
    stop("Include 'costs' in analyses to use terminalCostMap.", call. = FALSE)
  }
  if (!costing && !is.null(costAllocation)) {
    stop("Include 'costs' in analyses to use costAllocation.", call. = FALSE)
  }
  if (costing && (is.null(cdm) == is.null(costData))) {
    stop("For costing, supply exactly one of cdm and costData.", call. = FALSE)
  }
  if (!is.null(costVisitFilter) && (!costing || is.null(cdm) || !is.null(costData))) {
    stop("costVisitFilter requires a costs analysis using cdm, not local costData.", call. = FALSE)
  }
  args <- list(trajectories = trajectories)
  if (!missing(cycleLength)) args$cycleLength <- cycleLength
  if (!missing(absorbingStates)) args$absorbingStates <- absorbingStates
  if (!missing(states)) args$states <- states
  prepared <- do.call(prepareTrajectories, args)
  .tmaPeriods(periodBreaks, periodLabels,
              if ("transitions" %in% analyses) prepared$cycleLength else NULL)
  if (costing) .tmaTerminalCostMap(terminalCostMap, prepared)
  .tmaStrata(prepared$data, strata)
  transitions <- if ("transitions" %in% analyses) {
    estimateTransitions(prepared, periodBreaks = periodBreaks, periodLabels = periodLabels,
                         strata = strata)
  } else NULL
  costs <- NULL
  if (costing) {
    if (!is.null(cdm)) {
      costData <- extractCosts(cdm, prepared, measure = costMeasure, domains = costDomains,
                                visitFilter = costVisitFilter,
                                allocation = if (is.null(costAllocation)) "start" else costAllocation)
    }
    costs <- estimateStateCosts(prepared, costData,
                                periodBreaks = periodBreaks, periodLabels = periodLabels,
                                terminalCostMap = terminalCostMap, strata = strata,
                                allocation = costAllocation)
  }
  list(transitions = transitions, costs = costs, trajectories = prepared)
}
