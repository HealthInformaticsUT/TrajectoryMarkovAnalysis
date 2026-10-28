#' Extract observed costs for trajectory follow-up
#'
#' Resolves each OMOP cost domain against its own event table, preserving source
#' identifiers and currency. The supplied CDM is read only and remains connected.
#' Costs with missing amounts remain missing. No currency conversion is performed.
#'
#' @param cdm A CDMConnector database-backed `cdm_reference`.
#' @param trajectories C2T trajectory data or the result of `prepareTrajectories()`.
#' @param measure One of `total_charge`, `total_cost`, or `total_paid`.
#' @param domains Cost domain IDs to include. `NULL` selects every observed domain;
#'   unsupported observed domains cause an error rather than silent exclusion.
#' @return A data frame with `subject_id`, `event_date`, `amount`, `cost_id`,
#'   `cost_domain_id`, `cost_event_id`, `currency_concept_id`, and `measure`.
#'   The `diagnostics` attribute reports counts per domain. Unmatched event
#'   references are counted across the database for selected domains, since their
#'   patients cannot be identified. Other counts refer to selected patients.
#' @export
extractCosts <- function(cdm, trajectories, measure = "total_charge", domains = NULL) {
  if (!inherits(cdm, "cdm_reference")) {
    stop("`cdm` must be a CDMConnector cdm_reference.", call. = FALSE)
  }
  if (!is.character(measure) || length(measure) != 1L || is.na(measure) ||
      !measure %in% c("total_charge", "total_cost", "total_paid")) {
    stop("`measure` must be total_charge, total_cost, or total_paid.", call. = FALSE)
  }
  domain_map <- list(
    Drug = c("drug_exposure", "drug_exposure_id", "drug_exposure_start_date"),
    Visit = c("visit_occurrence", "visit_occurrence_id", "visit_start_date"),
    Procedure = c("procedure_occurrence", "procedure_occurrence_id", "procedure_date"),
    Device = c("device_exposure", "device_exposure_id", "device_exposure_start_date"),
    Measurement = c("measurement", "measurement_id", "measurement_date"),
    Observation = c("observation", "observation_id", "observation_date"),
    Specimen = c("specimen", "specimen_id", "specimen_date")
  )
  .tmaRequireCostTable(cdm, "cost", c(
    "cost_id", "cost_event_id", "cost_domain_id", "currency_concept_id", measure
  ))
  cost_table <- cdm[["cost"]]
  if (is.null(domains)) {
    observed <- dplyr::collect(dplyr::distinct(cost_table, .data$cost_domain_id))
    domains <- as.character(observed$cost_domain_id)
    if (anyNA(domains)) {
      stop("Observed costs have missing cost_domain_id; specify domains explicitly after reviewing them.",
           call. = FALSE)
    }
  }
  if (!is.character(domains) || anyNA(domains)) {
    stop("`domains` must be a character vector of cost domain IDs.", call. = FALSE)
  }
  domains <- unique(domains)
  unsupported <- setdiff(domains, names(domain_map))
  if (length(unsupported)) {
    stop("Unsupported cost domains: ", paste(unsupported, collapse = ", "),
         ". Specify supported domains explicitly after reviewing omitted costs.", call. = FALSE)
  }
  for (domain in domains) {
    spec <- domain_map[[domain]]
    .tmaRequireCostTable(cdm, spec[[1]], c(spec[[2]], "person_id", spec[[3]]))
  }
  prepared <- if (inherits(trajectories, "tma_trajectories")) {
    trajectories
  } else {
    prepareTrajectories(trajectories)
  }
  patients <- prepared$patients
  if (!is.data.frame(patients) ||
      !all(c("subject_id", "index_date", "observation_end") %in% names(patients))) {
    stop("Prepared trajectories must contain patient follow-up windows.", call. = FALSE)
  }
  patients$subject_id <- as.character(patients$subject_id)
  patients$index_date <- as.Date(patients$index_date)
  patients$observation_end <- as.Date(patients$observation_end)
  if (anyNA(patients[c("subject_id", "index_date", "observation_end")]) ||
      anyDuplicated(patients$subject_id) ||
      any(patients$observation_end < patients$index_date)) {
    stop("Patient windows must be complete, unique by subject_id, and ordered.", call. = FALSE)
  }
  result <- data.frame(
    subject_id = character(), event_date = as.Date(character()), amount = double(),
    cost_id = character(), cost_domain_id = character(), cost_event_id = character(),
    currency_concept_id = character(), measure = character(), stringsAsFactors = FALSE
  )
  diagnostics <- data.frame(
    cost_domain_id = character(), unmatched_event_references = double(),
    selected_patient_cost_rows = double(), missing_event_dates = double(),
    out_of_window = double(), extracted_rows = double(), missing_amounts = double(),
    stringsAsFactors = FALSE
  )
  result_chunks <- list(result)
  diagnostic_chunks <- list(diagnostics)
  chunks <- split(seq_len(nrow(patients)), ceiling(seq_len(nrow(patients)) / 100))
  for (domain in domains) {
    spec <- domain_map[[domain]]
    # Cast subject identifiers before filtering; keep event joins in the database's
    # native identifier types and cast their output before collection. This avoids
    # bigint precision loss and string mismatches such as integer 1 versus float 1.0.
    source <- dplyr::transmute(
      cdm[[spec[[1]]]], cost_event_id = .data[[spec[[2]]]],
      subject_id = as.character(.data$person_id), event_date = .data[[spec[[3]]]]
    )
    domain_cost <- dplyr::filter(cost_table, .data$cost_domain_id == domain)
    domain_cost <- dplyr::transmute(
      domain_cost, cost_id = as.character(.data$cost_id),
      cost_domain_id = as.character(.data$cost_domain_id),
      cost_event_id = .data$cost_event_id,
      currency_concept_id = as.character(.data$currency_concept_id),
      amount = .data[[measure]]
    )
    unmatched <- .tmaCostRowCount(dplyr::anti_join(
      domain_cost, dplyr::select(source, "cost_event_id"), by = "cost_event_id"
    ))
    domain_diagnostics <- data.frame(
      cost_domain_id = domain, unmatched_event_references = unmatched,
      selected_patient_cost_rows = 0, missing_event_dates = 0,
      out_of_window = 0, extracted_rows = 0, missing_amounts = 0,
      stringsAsFactors = FALSE
    )
    # No scratch tables: bound IN lists and date predicates to small chunks.
    for (indices in chunks) {
      windows <- patients[indices, , drop = FALSE]
      ids <- windows$subject_id
      selected_source <- dplyr::filter(source, .data$subject_id %in% ids)
      selected <- dplyr::inner_join(domain_cost, selected_source, by = "cost_event_id")
      counts <- dplyr::collect(dplyr::summarise(
        selected, n = dplyr::n(),
        missing_dates = sum(dplyr::if_else(is.na(.data$event_date), 1L, 0L), na.rm = TRUE)
      ))
      selected_count <- as.numeric(counts$n[[1]])
      missing_dates <- if (selected_count) as.numeric(counts$missing_dates[[1]]) else 0
      predicate <- .tmaCostWindowPredicate(windows)
      in_window <- dplyr::filter(selected, !!predicate)
      in_window <- dplyr::mutate(in_window, cost_event_id = as.character(.data$cost_event_id))
      extracted <- as.data.frame(dplyr::collect(in_window))
      extracted$event_date <- as.Date(extracted$event_date)
      extracted$amount <- as.numeric(extracted$amount)
      extracted$measure <- rep(measure, nrow(extracted))
      extracted <- extracted[, names(result), drop = FALSE]
      result_chunks[[length(result_chunks) + 1L]] <- extracted
      domain_diagnostics$selected_patient_cost_rows <-
        domain_diagnostics$selected_patient_cost_rows + selected_count
      domain_diagnostics$missing_event_dates <-
        domain_diagnostics$missing_event_dates + missing_dates
      domain_diagnostics$out_of_window <- domain_diagnostics$out_of_window +
        selected_count - missing_dates - nrow(extracted)
      domain_diagnostics$extracted_rows <-
        domain_diagnostics$extracted_rows + nrow(extracted)
      domain_diagnostics$missing_amounts <-
        domain_diagnostics$missing_amounts + sum(is.na(extracted$amount))
    }
    diagnostic_chunks[[length(diagnostic_chunks) + 1L]] <- domain_diagnostics
  }
  result <- as.data.frame(dplyr::bind_rows(result_chunks))
  diagnostics <- as.data.frame(dplyr::bind_rows(diagnostic_chunks))
  if (nrow(result)) {
    result <- result[order(result$subject_id, result$event_date,
                           result$cost_domain_id, result$cost_id), , drop = FALSE]
    rownames(result) <- NULL
  }
  attr(result, "measure") <- measure
  attr(result, "domains") <- domains
  attr(result, "diagnostics") <- diagnostics
  attr(result, "unmatched_diagnostics_scope") <- "database-wide selected domains"
  result
}

.tmaRequireCostTable <- function(cdm, name, columns) {
  if (!name %in% names(cdm)) {
    stop("The selected cost domains require CDM table `", name, "`.", call. = FALSE)
  }
  if (!inherits(cdm[[name]], "tbl_sql")) {
    stop("CDM table `", name, "` must be a supported database-backed lazy table.", call. = FALSE)
  }
  missing <- setdiff(columns, colnames(cdm[[name]]))
  if (length(missing)) {
    stop("CDM table `", name, "` is missing columns: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

.tmaCostRowCount <- function(table) {
  count <- dplyr::collect(dplyr::summarise(table, n = dplyr::n()))
  as.numeric(count$n[[1]])
}

.tmaCostWindowPredicate <- function(windows) {
  predicates <- lapply(seq_len(nrow(windows)), function(i) {
    id <- windows$subject_id[[i]]
    start <- windows$index_date[[i]]
    end <- windows$observation_end[[i]]
    rlang::expr(.data$subject_id == !!id &
                  .data$event_date >= !!start & .data$event_date <= !!end)
  })
  Reduce(function(a, b) rlang::expr((!!a) | (!!b)), predicates)
}
