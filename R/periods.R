.tmaPeriods <- function(periodBreaks = NULL, periodLabels = NULL,
                        cycleLength = NULL) {
  if (is.null(periodBreaks)) periodBreaks <- c(0, Inf)
  if (!is.numeric(periodBreaks) || is.complex(periodBreaks) ||
      inherits(periodBreaks, "integer64") || length(periodBreaks) < 2L ||
      anyNA(periodBreaks) || periodBreaks[1L] != 0 ||
      tail(periodBreaks, 1L) != Inf ||
      any(!is.finite(head(periodBreaks, -1L))) ||
      any(diff(periodBreaks) <= 0)) {
    stop("periodBreaks must be strictly increasing days starting at 0 and ending at Inf.",
         call. = FALSE)
  }
  finiteBreaks <- head(periodBreaks, -1L)
  if (any(finiteBreaks != trunc(finiteBreaks) | finiteBreaks >= 2^53)) {
    stop("Finite periodBreaks must be exact whole numbers of days.", call. = FALSE)
  }
  if (!is.null(cycleLength) && any(finiteBreaks %% cycleLength != 0)) {
    stop("Transition periodBreaks must align with cycleLength.", call. = FALSE)
  }
  nPeriods <- length(periodBreaks) - 1L
  if (is.null(periodLabels)) {
    periodLabels <- if (nPeriods == 1L) "All follow-up" else {
      paste0("Days [", finiteBreaks, ", ", tail(periodBreaks, -1L), ")")
    }
  }
  if (!is.character(periodLabels) || length(periodLabels) != nPeriods ||
      anyNA(periodLabels) || any(!nzchar(trimws(periodLabels))) ||
      anyDuplicated(periodLabels)) {
    stop("periodLabels must contain one unique, nonempty label per period.", call. = FALSE)
  }
  data.frame(period = periodLabels, start_day = finiteBreaks,
             end_day = tail(periodBreaks, -1L), stringsAsFactors = FALSE)
}

.tmaPeriodIndex <- function(elapsed, periods) {
  index <- findInterval(elapsed, periods$start_day)
  index[is.na(elapsed) | !is.finite(elapsed) | index == 0L] <- NA_integer_
  index
}
