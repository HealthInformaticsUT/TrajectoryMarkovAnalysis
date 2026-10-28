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
#' @param periodBreaks Follow-up day boundaries relative to each subject's index
#'   date, starting with 0 and ending with Inf. NULL pools all follow-up.
#' @param periodLabels Optional names for the periods between periodBreaks.
#' @return A list with state estimates, interval costs, unallocated cost records,
#'   periods, diagnostics and metadata. Each period and currency is summarised
#'   separately. Costs
#'   with missing amounts make the corresponding interval and state total
#'   unknown. known_recorded_cost reports only the observed monetary amounts.
#' @details An interval with no matching cost record has zero recorded cost.
#'   This is not evidence that all expenditure was captured. The mean cost per
#'   day divides the state total by observed days, including intervals without
#'   records. A missing amount produces an NA total and daily mean rather than
#'   being treated as zero. No confidence interval is calculated.
#'
#'   Periods include their starting day and exclude their ending day. Clinical
#'   intervals spanning a boundary are split: intervalCosts contains the clipped
#'   dates and observed days, retaining the original .row_id. Costs are assigned
#'   by their event date. The intervals and intervals_without_records estimates
#'   count fragments within each period; an original interval can contribute to
#'   multiple periods. Deduplicate .row_id to count original intervals across
#'   periods. Period-state combinations without observed days are absent, not
#'   estimated as zero. Period boundaries need not match a Markov cycle length.
#'
#'   START and EXIT are not clinical intervals. Costs in OUT OF COHORT rows,
#'   gaps or outside observation remain unallocated and are returned for review.
#'   For weekly input, the selected label describes the whole cycle. Costs in
#'   a death-labelled cycle are reported under that label. This function does
#'   not infer a previous living state or the order of events within a week.
#' @export
estimateStateCosts <- function(trajectories, costData,
                               periodBreaks = NULL, periodLabels = NULL) {
  prepared <- prepareTrajectories(trajectories)
  periods <- .tmaPeriods(periodBreaks, periodLabels)
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
  costs <- as.data.frame(costData)
  costs$subject_id <- .tmaSubjectIds(costs$subject_id)
  costs$event_date <- .tmaDates(costs$event_date, "event_date")
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
  rowsByPatient <- split(seq_len(nrow(intervals)), intervals$subject_id)
  costRowsByPatient <- split(seq_len(nrow(costs)), costs$subject_id)
  allocation <- rep(NA_integer_, nrow(costs))
  for (id in intersect(names(rowsByPatient), names(costRowsByPatient))) {
    rows <- rowsByPatient[[id]]
    eventRows <- costRowsByPatient[[id]]
    positions <- findInterval(as.numeric(costs$event_date[eventRows]),
                              as.numeric(intervals$state_start_date[rows]))
    candidates <- which(positions > 0L)
    if (!length(candidates)) next
    selected <- rows[positions[candidates]]
    inside <- costs$event_date[eventRows[candidates]] <= intervals$state_end_date[selected]
    allocation[eventRows[candidates[inside]]] <- selected[inside]
  }
  unresolved <- !is.na(allocation) & intervals$state_label[allocation] == "OUT OF COHORT"
  unresolved[is.na(unresolved)] <- FALSE
  reason <- rep(NA_character_, nrow(costs))
  reason[is.na(allocation)] <- "Outside a supplied interval"
  reason[!costs$subject_id %in% intervals$subject_id] <- "Patient not in trajectories"
  reason[unresolved] <- "Unresolved trajectory interval"
  allocation[unresolved] <- NA_integer_
  unallocated <- costs[is.na(allocation), , drop = FALSE]
  unallocated$reason <- reason[is.na(allocation)]

  allocated <- costs[!is.na(allocation), , drop = FALSE]
  allocated$.row_id <- intervals$.row_id[allocation[!is.na(allocation)]]
  amounts <- dplyr::summarise(
    dplyr::group_by(allocated, .data$.row_id, .data$period, .data$currency_concept_id),
    cost_records = dplyr::n(),
    missing_amounts = sum(is.na(.data$amount)),
    known_recorded_cost = sum(.data$amount, na.rm = TRUE),
    .groups = "drop"
  )
  clinical <- .tmaCostSegments(prepared, periods)
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
  estimates <- dplyr::summarise(
    dplyr::group_by(intervalCosts, .data$period, .data$state_label, .data$currency_concept_id),
    patients = dplyr::n_distinct(.data$subject_id),
    intervals = dplyr::n(),
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
                               estimates$state_label, estimates$currency_concept_id), , drop = FALSE]
  structure(list(
    estimates = as.data.frame(estimates),
    intervalCosts = as.data.frame(intervalCosts),
    unallocatedCosts = unallocated,
    periods = periods,
    diagnostics = list(
      input_cost_records = nrow(costs),
      allocated_cost_records = nrow(allocated),
      unallocated_cost_records = nrow(unallocated),
      missing_amounts = sum(is.na(costs$amount)),
      unknown_currency_records = sum(is.na(costs$currency_concept_id) |
        costs$currency_concept_id %in% c("", "0")),
      extraction = sourceDiagnostics
    ),
    metadata = list(measure = measure, currencies = currencies, periods = periods,
                    date_intervals = "inclusive", no_record = "zero recorded cost",
                    interval_counts = "fragments within each period; original intervals identified by .row_id",
                    unobserved_period_states = "absent; no cost estimate without observed days",
                    terminal_costs = "retained under the supplied cycle label")
  ), class = "tma_state_costs")
}

.tmaCostSegments <- function(prepared, periods) {
  clinical <- prepared$data[prepared$data$state_label != "OUT OF COHORT", , drop = FALSE]
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
