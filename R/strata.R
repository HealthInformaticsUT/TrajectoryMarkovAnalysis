# Shared row-wise stratum registry. Joins compare columns separately, so values
# containing delimiters cannot collide with a different covariate combination.
.tmaStrata <- function(data, strata) {
  if (is.null(strata)) return(NULL)
  if (!is.character(strata) || !is.null(dim(strata)) || !length(strata) ||
      anyNA(strata) || any(!nzchar(trimws(strata))) || anyDuplicated(strata)) {
    stop("strata must be NULL or a character vector of unique, nonempty column names.",
         call. = FALSE)
  }
  strata <- unname(strata)
  reserved <- c("subject_id", "state_label", "state_start_date", "state_end_date",
                "index_date", "observation_end", "observed_days", ".row_id",
                "seq_ordinal", "stratum_id", "period", "from_state", "to_state",
                "state", "count", "denominator", "probability", "structural",
                "patients", "source_patients", "cost_state", "currency_concept_id",
                "measure", "cost_records", "missing_amounts", "known_recorded_cost",
                "total_cost", "mean_cost_per_day", "intervals", "intervals_without_records",
                "mapped_terminal_intervals", "mapped_terminal_days")
  if (any(strata %in% reserved)) {
    stop("strata contains reserved column names: ",
         paste(intersect(strata, reserved), collapse = ", "), ".", call. = FALSE)
  }
  absent <- setdiff(strata, names(data))
  if (length(absent)) {
    stop("Trajectory strata columns are missing: ", paste(absent, collapse = ", "),
         ".", call. = FALSE)
  }
  clinical <- if ("state_label" %in% names(data)) {
    !data$state_label %in% c("START", "EXIT", "OUT OF COHORT")
  } else rep(TRUE, nrow(data))
  for (column in strata) {
    value <- data[[column]]
    categorical <- is.factor(value) || is.character(value) || is.logical(value) ||
      (is.numeric(value) && !is.complex(value) && !inherits(value, "integer64"))
    if (!is.atomic(value) || !is.null(dim(value)) || !categorical) {
      stop("strata column '", column,
           "' must contain atomic character, factor, logical or finite numeric values.",
           call. = FALSE)
    }
    if (is.numeric(value) && any(!is.na(value) & !is.finite(value))) {
      stop("strata column '", column, "' must contain finite numeric values.", call. = FALSE)
    }
    blank <- if (is.character(value) || is.factor(value)) {
      !nzchar(trimws(as.character(value[clinical])))
    } else rep(FALSE, sum(clinical))
    if (anyNA(value[clinical]) || any(blank)) {
      stop("strata column '", column,
           "' must not contain missing or blank values on clinical rows.", call. = FALSE)
    }
  }
  registry <- as.data.frame(dplyr::distinct(data[clinical, strata, drop = FALSE]))
  if (nrow(registry)) {
    ordering <- lapply(registry, function(value) if (is.factor(value)) as.character(value) else value)
    registry <- registry[do.call(order, unname(ordering)), , drop = FALSE]
  }
  rownames(registry) <- NULL
  registry$stratum_id <- sprintf("s%d", seq_len(nrow(registry)))
  registry <- registry[c("stratum_id", strata)]
  matched <- dplyr::left_join(data[strata], registry, by = strata)
  list(columns = strata, registry = registry, row_ids = matched$stratum_id)
}

.tmaAddStratum <- function(data, registryRow) {
  labels <- registryRow[rep(1L, nrow(data)), , drop = FALSE]
  rownames(labels) <- NULL
  cbind(labels, data, row.names = NULL)
}
