# Allocate interval charges without expanding events into individual days.
.tmaDurationCostAllocation <- function(costs, prepared, periods) {
  segmentInput <- prepared
  segmentInput$data <- prepared$data[c("subject_id", "state_label", "state_start_date",
                                      "state_end_date", ".row_id", "observed_days")]
  segments <- .tmaCostSegments(segmentInput, periods, includeUnresolved = TRUE)
  segments <- segments[order(segments$subject_id, segments$state_start_date), , drop = FALSE]
  starts <- as.numeric(costs$event_date)
  ends <- as.numeric(costs$event_end_date)
  durations <- ends - starts + 1
  segmentStarts <- as.numeric(segments$state_start_date)
  segmentEnds <- as.numeric(segments$state_end_date)
  rowsByPatient <- split(seq_len(nrow(segments)), segments$subject_id)
  eventsByPatient <- split(seq_len(nrow(costs)), costs$subject_id)
  empty <- data.frame(cost_row = integer(), segment_row = integer(),
                      start = double(), end = double(), days = double())
  pairs <- list(empty)
  for (id in intersect(names(rowsByPatient), names(eventsByPatient))) {
    rows <- rowsByPatient[[id]]
    events <- eventsByPatient[[id]]
    first <- findInterval(starts[events] - 1, segmentEnds[rows]) + 1L
    last <- findInterval(ends[events], segmentStarts[rows])
    sizes <- pmax(0L, last - first + 1L)
    if (!any(sizes)) next
    eventRows <- rep(events, sizes)
    segmentRows <- rows[rep(first, sizes) + sequence(sizes) - 1L]
    overlapStart <- pmax(starts[eventRows], segmentStarts[segmentRows])
    overlapEnd <- pmin(ends[eventRows], segmentEnds[segmentRows])
    pairs[[length(pairs) + 1L]] <- data.frame(
      cost_row = eventRows, segment_row = segmentRows,
      start = overlapStart, end = overlapEnd, days = overlapEnd - overlapStart + 1
    )
  }
  pairs <- dplyr::bind_rows(pairs)
  makeShares <- function(eventRows, days, start, end) {
    result <- costs[eventRows, , drop = FALSE]
    result$.cost_row_id <- eventRows
    result$original_amount <- result$amount
    result$allocation_days <- days
    result$allocation_fraction <- days / durations[eventRows]
    result$allocation_start_date <- as.Date(start, origin = "1970-01-01")
    result$allocation_end_date <- as.Date(end, origin = "1970-01-01")
    result$amount <- result$original_amount * result$allocation_fraction
    result
  }
  overlaps <- makeShares(pairs$cost_row, pairs$days, pairs$start, pairs$end)
  overlaps$.row_id <- segments$.row_id[pairs$segment_row]
  overlaps$period <- segments$period[pairs$segment_row]
  unresolved <- segments$state_label[pairs$segment_row] == "OUT OF COHORT"
  allocated <- overlaps[!unresolved, , drop = FALSE]
  unallocated <- overlaps[unresolved, , drop = FALSE]
  unallocated$reason <- rep("Unresolved trajectory interval", nrow(unallocated))

  # Gaps and time before/after follow-up keep their share of the original charge.
  coveredDays <- numeric(nrow(costs))
  if (nrow(pairs)) {
    covered <- rowsum(pairs$days, pairs$cost_row, reorder = FALSE)
    coveredDays[as.integer(rownames(covered))] <- covered[, 1]
  }
  remainder <- durations - coveredDays
  outsideRows <- which(remainder > 0)
  outside <- makeShares(outsideRows, remainder[outsideRows],
                         rep(NA_real_, length(outsideRows)), rep(NA_real_, length(outsideRows)))
  outside$.row_id <- rep(NA_integer_, nrow(outside))
  outside$period <- rep(NA_character_, nrow(outside))
  outside$reason <- rep("Outside a supplied interval", nrow(outside))
  outside$reason[!outside$subject_id %in% prepared$patients$subject_id] <- "Patient not in trajectories"
  unallocated <- as.data.frame(dplyr::bind_rows(unallocated, outside))
  allocated <- allocated[order(allocated$.cost_row_id, allocated$allocation_start_date), , drop = FALSE]
  unallocated <- unallocated[order(unallocated$.cost_row_id, unallocated$allocation_start_date), , drop = FALSE]
  rownames(allocated) <- rownames(unallocated) <- NULL
  list(allocated = allocated, unallocated = unallocated)
}
