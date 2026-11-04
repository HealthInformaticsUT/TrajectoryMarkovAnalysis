#' Summarise recorded costs independently of transition estimation
#'
#' Allocate dated cost records to non-overlapping trajectory intervals. Dates
#' include both endpoints. The function does not query a database, fit a model,
#' change a connection or write files.
#'
#' @param trajectories Current Cohort2Trajectory output or the result of
#'   prepareTrajectories().
#' @param costData A data frame returned by extractCosts(), or a local data frame
#'   with subject_id, event_date and amount. Optional columns are cost_id,
#'   currency_concept_id and measure. Supply currency and measure information
#'   when known. Missing currency is reported as unknown, not converted.
#'   Duration allocation additionally requires event_end_date. Keep the full
#'   source dates and amount, including time outside the trajectories.
#' @param periodBreaks Follow-up day boundaries relative to each subject's index
#'   date, starting with 0 and ending with Inf. NULL pools all follow-up.
#' @param periodLabels Optional names for the periods between periodBreaks.
#' @param terminalCostMap Optional named character vector assigning terminal
#'   states to nonabsorbing cost states, for example c("Death" = "Alive").
#'   Names must be declared in prepared trajectories' absorbingStates, and
#'   destinations must be in their states dictionary. Unlisted states retain
#'   their own costs. NULL leaves every label unchanged.
#' @param strata Optional character vector of categorical trajectory column names.
#'   Values may change between rows. Numeric values define exact-value groups;
#'   create age bands or other categories explicitly before estimation.
#' @param allocation "start" assigns the full amount to event_date. "duration"
#'   spreads it uniformly over event_date through event_end_date, inclusive.
#'   NULL uses the allocation attribute from extractCosts(), or "start" for
#'   plain local records. To use duration allocation after start-date extraction,
#'   extract again with allocation = "duration" so overlapping events are not lost.
#' @return A list with state estimates, interval costs, unallocated cost records,
#'   periods, diagnostics and metadata. Each period and currency is summarised
#'   separately. Costs
#'   with missing amounts make the corresponding interval and state total
#'   unknown. known_recorded_cost reports only the observed monetary amounts.
#'   Duration results also include allocatedCosts. In both allocation ledgers,
#'   amount is the share, original_amount is the full source amount, .cost_row_id
#'   identifies its input row, and allocation_days and allocation_fraction
#'   describe the share. Original event dates are preserved. Allocated and
#'   unresolved fragments have allocation_start_date and allocation_end_date.
#'   An outside remainder can combine gaps and time outside observation, so its
#'   allocation dates and period are NA rather than implying a single interval.
#' @details An interval with no matching cost record has zero recorded cost.
#'   This is not evidence that all expenditure was captured. The mean cost per
#'   day divides the state total by observed days, including intervals without
#'   records. A missing amount produces an NA total and daily mean rather than
#'   being treated as zero. No confidence interval is calculated.
#'
#'   Periods include their starting day and exclude their ending day. Clinical
#'   intervals spanning a boundary are split: intervalCosts contains the clipped
#'   dates and observed days, retaining the original .row_id. Costs are assigned
#'   by their event date in start mode or by overlapping days in duration mode.
#'   The intervals and intervals_without_records estimates
#'   count fragments within each period; an original interval can contribute to
#'   multiple periods. Deduplicate .row_id to count original intervals across
#'   periods. Period-state combinations without observed days are absent, not
#'   estimated as zero. Period boundaries need not match a Markov cycle length.
#'
#'   START and EXIT are not clinical intervals. Costs in OUT OF COHORT rows,
#'   gaps or outside observation remain unallocated and are returned for review.
#'   For weekly input, the selected label describes the whole cycle. By default,
#'   costs in a death-labelled cycle are reported under that label. This function does
#'   not infer a previous living state or the order of events within a week.
#'
#'   terminalCostMap assigns both the recorded amount and observed days of each
#'   mapped interval to its destination cost state. It preserves state_label
#'   and adds cost_state in intervalCosts. Estimates are grouped by cost_state.
#'   The supplied trajectories and transition estimates are not changed.
#'   A first-cycle terminal interval can be mapped without a previous living
#'   interval. No days or event dates are added. The analyst must ensure that
#'   mapped intervals represent the care time to be attributed, not an extended
#'   period after death. Mapping cannot recover the actual event order.
#'
#'   The resulting daily mean uses cost-attributed days, which can differ from
#'   the occupancy of the unchanged clinical states. It is a descriptive cost
#'   estimate, not automatically a calibrated reward for a transition model.
#'   If extractCosts() restricted costs to selected visits, only the cost
#'   numerator changes. All observed state-days still contribute, including
#'   days without a qualifying visit. The result is not a mean per selected visit.
#'   Extraction scope and visit criteria are retained in metadata.
#'
#'   With strata, each interval's covariate values group its costs and observed
#'   days together. Period fragments retain the original interval's values and
#'   patient index. The result includes a strata registry; stratum_id and the
#'   requested covariates appear in intervalCosts and estimates. Patient counts
#'   are distinct within each group and may overlap between groups.
#'
#'   Duration allocation divides by the full original event duration. Only days
#'   overlapping resolved clinical intervals receive costs. Unobserved shares
#'   remain unallocated and are not redistributed over the remaining days.
#'   Missing or reversed end dates cause an error, not an imputed duration.
#'   Missing amounts affect only cells with an actual overlap. Uniform spreading
#'   is an accounting assumption, not a claim that care occurred evenly.
#'
#'   Cost-record diagnostics count unique input rows. A partly allocated record
#'   appears in both allocated_cost_records and unallocated_cost_records and is
#'   also counted in partially_allocated_cost_records. Fragment counts report
#'   ledger rows. The cost_records and missing_amounts in interval and state
#'   estimates count contributing record fragments, so they can exceed the
#'   number of source records when summed across periods or states.
#' @seealso createCostModel() for explicitly applying these daily means to
#'   labelled patient-simulation cycles.
#' @export
estimateStateCosts <- function(trajectories, costData,
                               periodBreaks = NULL, periodLabels = NULL,
                               terminalCostMap = NULL, strata = NULL,
                               allocation = NULL) {
  prepared <- prepareTrajectories(trajectories)
  periods <- .tmaPeriods(periodBreaks, periodLabels)
  terminalCostMap <- .tmaTerminalCostMap(terminalCostMap, prepared)
  grouping <- .tmaStrata(prepared$data, strata)
  if (!is.null(grouping)) prepared$data$stratum_id <- grouping$row_ids
  if (!is.data.frame(costData)) {
    stop("costData must be a data frame of dated cost records.", call. = FALSE)
  }
  required <- c("subject_id", "event_date", "amount")
  absent <- setdiff(required, names(costData))
  if (length(absent)) {
    stop("costData is missing: ", paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  sourceDiagnostics <- attr(costData, "diagnostics")
  sourceMeasure <- attr(costData, "measure")
  sourceCostScope <- attr(costData, "cost_scope")
  sourceVisitFilter <- attr(costData, "visit_filter")
  sourceAllocation <- attr(costData, "allocation")
  if (is.null(allocation)) {
    allocation <- if (is.null(sourceAllocation)) "start" else sourceAllocation
  }
  if (!is.character(allocation) || length(allocation) != 1L || is.na(allocation) ||
      !allocation %in% c("start", "duration")) {
    stop("allocation must be 'start', 'duration' or NULL to use the extraction choice.",
         call. = FALSE)
  }
  if (allocation == "duration" && identical(sourceAllocation, "start")) {
    stop("Extract costs again with allocation = 'duration'. Start-date extraction can omit overlapping events.",
         call. = FALSE)
  }
  costs <- as.data.frame(costData)
  costs$subject_id <- .tmaSubjectIds(costs$subject_id)
  costs$event_date <- .tmaDates(costs$event_date, "event_date")
  if (allocation == "duration") {
    if (!"event_end_date" %in% names(costs)) {
      stop("Duration allocation requires event_end_date for every cost record.", call. = FALSE)
    }
    costs$event_end_date <- .tmaDates(costs$event_end_date, "event_end_date")
    if (any(costs$event_end_date < costs$event_date)) {
      stop("event_end_date cannot precede event_date.", call. = FALSE)
    }
    reserved <- c(".cost_row_id", "original_amount", "allocation_days", "allocation_fraction",
                  "allocation_start_date", "allocation_end_date")
    if (any(reserved %in% names(costs))) {
      stop("costData contains reserved allocation output columns: ",
           paste(intersect(reserved, names(costs)), collapse = ", "), ".", call. = FALSE)
    }
  }
  if (!is.numeric(costs$amount) || is.complex(costs$amount) || inherits(costs$amount, "integer64") ||
      any(is.infinite(costs$amount))) {
    stop("amount must be numeric, with NA for missing amounts.", call. = FALSE)
  }
  if ("cost_id" %in% names(costs)) {
    costs$cost_id <- .tmaSubjectIds(costs$cost_id, "cost_id")
    if (anyDuplicated(costs$cost_id)) {
      stop("cost_id must be unique. Resolve duplicated cost records before allocation.", call. = FALSE)
    }
  }
  if (!"currency_concept_id" %in% names(costs)) costs$currency_concept_id <- rep(NA_character_, nrow(costs))
  costs$currency_concept_id <- as.character(costs$currency_concept_id)
  if (!"measure" %in% names(costs)) {
    measure <- if (length(sourceMeasure) == 1L) sourceMeasure else "amount"
    costs$measure <- rep(measure, nrow(costs))
  }
  measures <- unique(as.character(costs$measure))
  if (anyNA(measures) || any(!nzchar(trimws(measures))) || length(measures) > 1L) {
    stop("Supply one non-missing cost measure per analysis. Do not mix charges, costs and payments.", call. = FALSE)
  }
  measure <- if (length(measures)) measures else if (length(sourceMeasure) == 1L) sourceMeasure else "amount"

  intervals <- prepared$data
  origins <- prepared$patients$index_date[match(costs$subject_id, prepared$patients$subject_id)]
  eventPeriods <- .tmaPeriodIndex(as.numeric(costs$event_date - origins), periods)
  costs$period <- periods$period[eventPeriods]
  if (allocation == "duration") {
    shares <- .tmaDurationCostAllocation(costs, prepared, periods)
    allocated <- shares$allocated
    unallocated <- shares$unallocated
    allocatedRecords <- unique(allocated$.cost_row_id)
    unallocatedRecords <- unique(unallocated$.cost_row_id)
    allocatedCount <- length(allocatedRecords)
    unallocatedCount <- length(unallocatedRecords)
    partialCount <- length(intersect(allocatedRecords, unallocatedRecords))
  } else {
    rowsByPatient <- split(seq_len(nrow(intervals)), intervals$subject_id)
    costRowsByPatient <- split(seq_len(nrow(costs)), costs$subject_id)
    rowAllocation <- rep(NA_integer_, nrow(costs))
    for (id in intersect(names(rowsByPatient), names(costRowsByPatient))) {
      rows <- rowsByPatient[[id]]
      eventRows <- costRowsByPatient[[id]]
      positions <- findInterval(as.numeric(costs$event_date[eventRows]),
                                as.numeric(intervals$state_start_date[rows]))
      candidates <- which(positions > 0L)
      if (!length(candidates)) next
      selected <- rows[positions[candidates]]
      inside <- costs$event_date[eventRows[candidates]] <= intervals$state_end_date[selected]
      rowAllocation[eventRows[candidates[inside]]] <- selected[inside]
    }
    unresolved <- !is.na(rowAllocation) & intervals$state_label[rowAllocation] == "OUT OF COHORT"
    unresolved[is.na(unresolved)] <- FALSE
    reason <- rep(NA_character_, nrow(costs))
    reason[is.na(rowAllocation)] <- "Outside a supplied interval"
    reason[!costs$subject_id %in% intervals$subject_id] <- "Patient not in trajectories"
    reason[unresolved] <- "Unresolved trajectory interval"
    rowAllocation[unresolved] <- NA_integer_
    unallocated <- costs[is.na(rowAllocation), , drop = FALSE]
    unallocated$reason <- reason[is.na(rowAllocation)]

    allocated <- costs[!is.na(rowAllocation), , drop = FALSE]
    allocated$.row_id <- intervals$.row_id[rowAllocation[!is.na(rowAllocation)]]
    allocatedCount <- nrow(allocated)
    unallocatedCount <- nrow(unallocated)
    partialCount <- 0L
  }
  amounts <- dplyr::summarise(
    dplyr::group_by(allocated, .data$.row_id, .data$period, .data$currency_concept_id),
    cost_records = dplyr::n(),
    missing_amounts = sum(is.na(.data$amount)),
    known_recorded_cost = sum(.data$amount, na.rm = TRUE),
    .groups = "drop"
  )
  clinical <- .tmaCostSegments(prepared, periods)
  clinical$cost_state <- clinical$state_label
  mapped <- clinical$state_label %in% names(terminalCostMap)
  clinical$cost_state[mapped] <- unname(terminalCostMap[clinical$state_label[mapped]])
  currencies <- unique(costs$currency_concept_id)
  if (!length(currencies)) currencies <- NA_character_
  intervalCosts <- clinical[rep(seq_len(nrow(clinical)), times = length(currencies)), , drop = FALSE]
  intervalCosts$currency_concept_id <- rep(currencies, each = nrow(clinical))
  intervalCosts <- dplyr::left_join(intervalCosts, amounts,
                                  by = c(".row_id", "period", "currency_concept_id"))
  for (column in c("cost_records", "missing_amounts", "known_recorded_cost")) {
    intervalCosts[[column]][is.na(intervalCosts[[column]])] <- 0
  }
  intervalCosts$total_cost <- ifelse(intervalCosts$missing_amounts > 0L,
                                    NA_real_, intervalCosts$known_recorded_cost)
  intervalCosts$measure <- rep(measure, nrow(intervalCosts))
  grouped <- if (is.null(grouping)) {
    dplyr::group_by(intervalCosts, .data$period, .data$cost_state, .data$currency_concept_id)
  } else {
    columns <- c("stratum_id", grouping$columns, "period", "cost_state", "currency_concept_id")
    dplyr::group_by(intervalCosts, dplyr::across(dplyr::all_of(columns)))
  }
  estimates <- dplyr::summarise(
    grouped,
    patients = dplyr::n_distinct(.data$subject_id),
    intervals = dplyr::n(),
    mapped_terminal_intervals = sum(.data$state_label != .data$cost_state),
    mapped_terminal_days = sum(.data$observed_days[.data$state_label != .data$cost_state]),
    observed_days = sum(.data$observed_days),
    intervals_without_records = sum(.data$cost_records == 0),
    cost_records = sum(.data$cost_records),
    missing_amounts = sum(.data$missing_amounts),
    known_recorded_cost = sum(.data$known_recorded_cost),
    total_cost = sum(.data$total_cost),
    .groups = "drop"
  )
  estimates$mean_cost_per_day <- estimates$total_cost / estimates$observed_days
  estimates$measure <- rep(measure, nrow(estimates))
  estimates <- estimates[order(match(estimates$period, periods$period),
                               estimates$cost_state, estimates$currency_concept_id), , drop = FALSE]
  if (!is.null(grouping)) {
    estimates <- estimates[order(match(estimates$stratum_id, grouping$registry$stratum_id),
                                 match(estimates$period, periods$period),
                                 estimates$cost_state, estimates$currency_concept_id), , drop = FALSE]
  }
  result <- structure(list(
    estimates = as.data.frame(estimates),
    intervalCosts = as.data.frame(intervalCosts),
    unallocatedCosts = unallocated,
    periods = periods,
    diagnostics = list(
      input_cost_records = nrow(costs),
      allocated_cost_records = allocatedCount,
      unallocated_cost_records = unallocatedCount,
      partially_allocated_cost_records = partialCount,
      allocated_cost_fragments = nrow(allocated),
      unallocated_cost_fragments = nrow(unallocated),
      missing_amounts = sum(is.na(costs$amount)),
      unknown_currency_records = sum(is.na(costs$currency_concept_id) |
        costs$currency_concept_id %in% c("", "0")),
      mapped_terminal_patients = length(unique(clinical$subject_id[mapped])),
      mapped_terminal_intervals = length(unique(clinical$.row_id[mapped])),
      mapped_terminal_days = sum(clinical$observed_days[mapped]),
      extraction = sourceDiagnostics
    ),
    metadata = list(measure = measure, currencies = currencies, periods = periods,
                    allocation = allocation, source_allocation = sourceAllocation,
                    cost_record_counts = "interval estimates count contributing record fragments; diagnostics count unique input records",
                    cost_scope = if (is.null(sourceCostScope)) "supplied_cost_records" else sourceCostScope,
                    source_visit_filter = sourceVisitFilter,
                    date_intervals = "inclusive", no_record = "zero recorded cost",
                    interval_counts = "fragments within each period; original intervals identified by .row_id",
                    unobserved_period_states = "absent; no cost estimate without observed days",
                    terminal_cost_map = terminalCostMap,
                    terminal_costs = if (length(terminalCostMap)) {
                      "explicit attribution of amounts and observed days; clinical labels unchanged"
                    } else "retained under the supplied cycle label",
                    mean_cost_denominator = "observed days attributed to cost_state")
  ), class = "tma_state_costs")
  if (allocation == "duration") result$allocatedCosts <- allocated
  if (!is.null(grouping)) {
    result$strata <- grouping$registry
    result$metadata$strata <- grouping$columns
    result$metadata$stratum_assignment <- "interval row values; costs and observed days grouped together"
  }
  result
}

.tmaTerminalCostMap <- function(terminalCostMap, prepared) {
  if (is.null(terminalCostMap)) return(character())
  if (!is.character(terminalCostMap) || !is.null(dim(terminalCostMap)) ||
      anyNA(terminalCostMap) || any(!nzchar(trimws(terminalCostMap)))) {
    stop("terminalCostMap must be a named character vector of nonempty destination states.",
         call. = FALSE)
  }
  if (!length(terminalCostMap)) return(character())
  sources <- names(terminalCostMap)
  if (is.null(sources) || anyNA(sources) || any(!nzchar(trimws(sources))) ||
      anyDuplicated(sources)) {
    stop("terminalCostMap must have unique, nonempty source-state names.", call. = FALSE)
  }
  invalidSources <- setdiff(sources, prepared$absorbingStates)
  if (length(invalidSources)) {
    stop("terminalCostMap sources must be declared absorbingStates in prepareTrajectories(): ",
         paste(invalidSources, collapse = ", "), ".",
         call. = FALSE)
  }
  invalidDestinations <- unique(unname(terminalCostMap[
    !terminalCostMap %in% prepared$states | terminalCostMap %in% prepared$absorbingStates]))
  if (length(invalidDestinations)) {
    stop("terminalCostMap destinations must be defined nonabsorbing states: ",
         paste(invalidDestinations, collapse = ", "),
         ". Include unobserved destinations in prepareTrajectories(states = ...).",
         call. = FALSE)
  }
  terminalCostMap
}

.tmaCostSegments <- function(prepared, periods, includeUnresolved = FALSE) {
  clinical <- if (includeUnresolved) prepared$data else {
    prepared$data[prepared$data$state_label != "OUT OF COHORT", , drop = FALSE]
  }
  origins <- prepared$patients$index_date[match(clinical$subject_id, prepared$patients$subject_id)]
  start <- as.numeric(clinical$state_start_date - origins)
  end <- as.numeric(clinical$state_end_date - origins) + 1
  empty <- clinical[FALSE, , drop = FALSE]
  empty$period <- character()
  segments <- list(empty)
  for (i in seq_len(nrow(periods))) {
    selected <- which(start < periods$end_day[[i]] & end > periods$start_day[[i]])
    if (!length(selected)) next
    segment <- clinical[selected, , drop = FALSE]
    clippedStart <- pmax(start[selected], periods$start_day[[i]])
    clippedEnd <- pmin(end[selected], periods$end_day[[i]])
    segment$state_start_date <- origins[selected] + clippedStart
    segment$state_end_date <- origins[selected] + clippedEnd - 1
    segment$observed_days <- clippedEnd - clippedStart
    segment$period <- periods$period[[i]]
    segments[[length(segments) + 1L]] <- segment
  }
  as.data.frame(dplyr::bind_rows(segments))
}
