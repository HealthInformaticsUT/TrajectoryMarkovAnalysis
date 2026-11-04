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
#'   Supported domains are Drug, Visit, Visit Detail, Procedure, Device,
#'   Measurement, Observation and Specimen.
#' @param visitFilter Optional named list with visitIds, visitConceptIds or
#'   eventConcepts. NULL retains all costs in follow-up. visitConceptIds selects
#'   visit_concept_id, not visit_type_concept_id. eventConcepts is a named list
#'   of exact standard concept IDs in Condition, Drug, Procedure, Device,
#'   Measurement or Observation. For example,
#'   list(eventConcepts = list(Condition = c(123, 456))).
#' @param allocation How costs will be assigned by `estimateStateCosts()`.
#'   `"start"` selects events starting within follow-up. `"duration"` selects
#'   events overlapping follow-up and keeps their full source dates and amounts
#'   for allocation across their inclusive duration.
#' @return A data frame with `subject_id`, `event_date`, `amount`, `cost_id`,
#'   `cost_domain_id`, `cost_event_id`, `currency_concept_id`, `measure`, and
#'   `visit_occurrence_id`. Visit IDs are text and can be missing in all-cost mode.
#'   Duration extraction also returns `event_end_date`. The `allocation`
#'   attribute records the choice for `estimateStateCosts()`.
#'   The `diagnostics` attribute reports counts per domain. Unmatched event
#'   references are counted across the database for selected domains, since their
#'   patients cannot be identified. Other counts refer to selected patients.
#'   In-window counts and visit exclusions refer to events starting within
#'   follow-up in start mode, or overlapping follow-up in duration mode.
#'   Missing visit links, unmatched or wrong-patient visit references, and valid
#'   visits not selected are reported separately. They are NA when no visit
#'   filter was requested. Attributes cost_scope and visit_filter retain the
#'   selection used.
#' @details Each visit filter vector matches any supplied ID. eventConcepts
#'   matches any concept across its supplied domains. Different filter fields
#'   must all match. Concepts are not expanded to descendants. An explicitly
#'   empty ID vector selects no visits, rather than removing that restriction.
#'
#'   A matching clinical record qualifies its visit only when its own date lies
#'   within the patient's trajectory follow-up. All selected-domain costs linked
#'   to that visit can then be included, not only the cost of the matching record.
#'   Their own events must meet the selected allocation window rule. A later qualifying
#'   record within follow-up can select earlier costs from the same visit. This
#'   is a retrospective visit selection, not prospective treatment eligibility.
#'
#'   Visit selection uses explicit visit_occurrence_id and patient links. No
#'   date-overlap matching is attempted. Repeated qualifying records do not
#'   multiply cost rows. Selected cost domains without a visit link raise an
#'   error in filtered mode. All-cost mode still supports those domains.
#'   Selecting visits does not change trajectory patients or observed days in
#'   estimateStateCosts(). Visit-associated costs are not necessarily caused by
#'   the qualifying condition. Bundled visit charges and itemised costs can
#'   overlap in the source, so select cost domains and measures accordingly.
#'
#'   Start allocation assigns the full amount to the source event date. Duration
#'   allocation uses start and end dates for Visit, Visit Detail, Drug and Device.
#'   Procedure, Measurement, Observation and Specimen remain single-day events.
#'   Required end-date columns must exist. Missing or reversed dates in cost
#'   records linked to selected patients cause an error before window or visit
#'   filtering. No dates are imputed. Source dates and amounts are not clipped
#'   to follow-up, so costs outside observed days can remain unallocated when
#'   passed to estimateStateCosts().
#'
#'   Visit Detail costs link to visit_detail_id and use visit_detail_start_date
#'   as the event date. Visit filters use the detail's parent
#'   visit_occurrence_id. visitConceptIds therefore selects the parent's
#'   visit_concept_id, not visit_detail_concept_id.
#' @export
extractCosts <- function(cdm, trajectories, measure = "total_charge", domains = NULL,
                           visitFilter = NULL, allocation = "start") {
  if (!inherits(cdm, "cdm_reference")) {
    stop("`cdm` must be a CDMConnector cdm_reference.", call. = FALSE)
  }
  if (!is.character(measure) || length(measure) != 1L || is.na(measure) ||
      !measure %in% c("total_charge", "total_cost", "total_paid")) {
    stop("`measure` must be total_charge, total_cost, or total_paid.", call. = FALSE)
  }
  if (!is.character(allocation) || length(allocation) != 1L || is.na(allocation) ||
      !allocation %in% c("start", "duration")) {
    stop("`allocation` must be start or duration.", call. = FALSE)
  }
  visitFilter <- .tmaCostVisitFilter(visitFilter, cdm)
  domain_map <- list(
    Drug = c("drug_exposure", "drug_exposure_id", "drug_exposure_start_date", "drug_exposure_end_date"),
    Visit = c("visit_occurrence", "visit_occurrence_id", "visit_start_date", "visit_end_date"),
    "Visit Detail" = c("visit_detail", "visit_detail_id", "visit_detail_start_date", "visit_detail_end_date"),
    Procedure = c("procedure_occurrence", "procedure_occurrence_id", "procedure_date"),
    Device = c("device_exposure", "device_exposure_id", "device_exposure_start_date", "device_exposure_end_date"),
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
    end_column <- if (length(spec) == 4L) spec[[4]] else spec[[3]]
    required <- c(spec[[2]], "person_id", spec[[3]],
                  if (allocation == "duration") end_column)
    .tmaRequireCostTable(cdm, spec[[1]], required)
    if (!is.null(visitFilter) && domain != "Visit" &&
        !"visit_occurrence_id" %in% colnames(cdm[[spec[[1]]]])) {
      stop("Visit-filtered costs require visit_occurrence_id in `", spec[[1]],
           "`. Exclude this domain explicitly through domains, or use all-cost mode. No date-based visit link is inferred.",
           call. = FALSE)
    }
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
    currency_concept_id = character(), measure = character(),
    visit_occurrence_id = character(), stringsAsFactors = FALSE
  )
  if (allocation == "duration") result$event_end_date <- as.Date(character())
  diagnostics <- data.frame(
    cost_domain_id = character(), unmatched_event_references = double(),
    selected_patient_cost_rows = double(), missing_event_dates = double(),
    out_of_window = double(), in_window_cost_rows = double(),
    missing_visit_links = double(), unmatched_visit_references = double(),
    visits_not_selected = double(), extracted_rows = double(), missing_amounts = double(),
    stringsAsFactors = FALSE
  )
  result_chunks <- list(result)
  diagnostic_chunks <- list(diagnostics)
  chunks <- split(seq_len(nrow(patients)), ceiling(seq_len(nrow(patients)) / 100))
  if (!is.null(visitFilter)) {
    visit_keys <- dplyr::distinct(dplyr::transmute(
      cdm[["visit_occurrence"]], subject_id = as.character(.data$person_id),
      visit_occurrence_id = as.character(.data$visit_occurrence_id)
    ))
    visit_keys <- dplyr::filter(visit_keys, !is.na(.data$visit_occurrence_id),
                               !.data$visit_occurrence_id %in% c("", "0"))
    visit_keys <- dplyr::mutate(visit_keys, .tma_valid_visit = 1L)
    selected_visits <- lapply(chunks, function(indices) {
      dplyr::mutate(.tmaSelectedCostVisits(cdm, patients[indices, , drop = FALSE], visitFilter),
                     .tma_selected_visit = 1L)
    })
  }
  for (domain in domains) {
    spec <- domain_map[[domain]]
    # Cast subject identifiers before filtering; keep event joins in the database's
    # native identifier types and cast their output before collection. This avoids
    # bigint precision loss and string mismatches such as integer 1 versus float 1.0.
    visit_column <- if (domain == "Visit") spec[[2]] else "visit_occurrence_id"
    visit_expression <- if (visit_column %in% colnames(cdm[[spec[[1]]]])) {
      rlang::expr(as.character(.data[[!!visit_column]]))
    } else rlang::expr(NA_character_)
    source <- dplyr::transmute(
      cdm[[spec[[1]]]], cost_event_id = .data[[spec[[2]]]],
      subject_id = as.character(.data$person_id), event_date = .data[[spec[[3]]]],
      visit_occurrence_id = !!visit_expression
    )
    if (allocation == "duration") {
      end_column <- if (length(spec) == 4L) spec[[4]] else spec[[3]]
      source <- dplyr::transmute(
        cdm[[spec[[1]]]], cost_event_id = .data[[spec[[2]]]],
        subject_id = as.character(.data$person_id), event_date = .data[[spec[[3]]]],
        event_end_date = .data[[end_column]], visit_occurrence_id = !!visit_expression
      )
    }
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
      out_of_window = 0, in_window_cost_rows = 0,
      missing_visit_links = if (is.null(visitFilter)) NA_real_ else 0,
      unmatched_visit_references = if (is.null(visitFilter)) NA_real_ else 0,
      visits_not_selected = if (is.null(visitFilter)) NA_real_ else 0,
      extracted_rows = 0, missing_amounts = 0,
      stringsAsFactors = FALSE
    )
    # No scratch tables: bound IN lists and date predicates to small chunks.
    for (chunk_index in seq_along(chunks)) {
      indices <- chunks[[chunk_index]]
      windows <- patients[indices, , drop = FALSE]
      ids <- windows$subject_id
      selected_source <- dplyr::filter(source, .data$subject_id %in% ids)
      selected <- dplyr::inner_join(domain_cost, selected_source, by = "cost_event_id")
      duration_check <- if (allocation == "duration") list(
        invalid_dates = rlang::expr(sum(dplyr::if_else(
          is.na(.data$event_date) | is.na(.data$event_end_date) |
            .data$event_end_date < .data$event_date, 1L, 0L), na.rm = TRUE))
      ) else list()
      counts <- dplyr::collect(dplyr::summarise(
        selected, n = dplyr::n(),
        missing_dates = sum(dplyr::if_else(is.na(.data$event_date), 1L, 0L), na.rm = TRUE),
        !!!duration_check
      ))
      selected_count <- as.numeric(counts$n[[1]])
      missing_dates <- if (selected_count) as.numeric(counts$missing_dates[[1]]) else 0
      if (allocation == "duration" && selected_count > 0 && counts$invalid_dates[[1]] > 0) {
        stop("Duration allocation found missing or reversed event dates for ",
             counts$invalid_dates[[1]], " selected-patient cost records in domain `", domain,
             "`. Review the source dates before extraction; no dates were imputed.", call. = FALSE)
      }
      predicate <- .tmaCostWindowPredicate(windows, allocation)
      in_window <- dplyr::filter(selected, !!predicate)
      window_count <- NULL
      if (!is.null(visitFilter)) {
        in_window <- dplyr::left_join(in_window, visit_keys,
                                      by = c("subject_id", "visit_occurrence_id"))
        in_window <- dplyr::left_join(in_window, selected_visits[[chunk_index]],
                                      by = c("subject_id", "visit_occurrence_id"))
        in_window <- dplyr::mutate(in_window, .tma_missing_visit =
          is.na(.data$visit_occurrence_id) | .data$visit_occurrence_id %in% c("", "0"))
        visit_counts <- dplyr::collect(dplyr::summarise(
          in_window, n = dplyr::n(),
          missing_links = sum(dplyr::if_else(.data$.tma_missing_visit, 1L, 0L), na.rm = TRUE),
          unmatched_links = sum(dplyr::if_else(!.data$.tma_missing_visit &
            is.na(.data$.tma_valid_visit), 1L, 0L), na.rm = TRUE),
          not_selected = sum(dplyr::if_else(!is.na(.data$.tma_valid_visit) &
            is.na(.data$.tma_selected_visit), 1L, 0L), na.rm = TRUE)
        ))
        window_count <- as.numeric(visit_counts$n[[1]])
        if (window_count > 0) {
          domain_diagnostics$missing_visit_links <- domain_diagnostics$missing_visit_links +
            as.numeric(visit_counts$missing_links[[1]])
          domain_diagnostics$unmatched_visit_references <- domain_diagnostics$unmatched_visit_references +
            as.numeric(visit_counts$unmatched_links[[1]])
          domain_diagnostics$visits_not_selected <- domain_diagnostics$visits_not_selected +
            as.numeric(visit_counts$not_selected[[1]])
        }
        in_window <- dplyr::filter(in_window, !.data$.tma_missing_visit,
                                    !is.na(.data$.tma_valid_visit), !is.na(.data$.tma_selected_visit))
      }
      in_window <- dplyr::mutate(in_window, cost_event_id = as.character(.data$cost_event_id))
      extracted <- as.data.frame(dplyr::collect(in_window))
      extracted$event_date <- as.Date(extracted$event_date)
      if (allocation == "duration") extracted$event_end_date <- as.Date(extracted$event_end_date)
      extracted$amount <- as.numeric(extracted$amount)
      extracted$visit_occurrence_id <- as.character(extracted$visit_occurrence_id)
      extracted$measure <- rep(measure, nrow(extracted))
      extracted <- extracted[, names(result), drop = FALSE]
      if (is.null(window_count)) window_count <- nrow(extracted)
      result_chunks[[length(result_chunks) + 1L]] <- extracted
      domain_diagnostics$selected_patient_cost_rows <-
        domain_diagnostics$selected_patient_cost_rows + selected_count
      domain_diagnostics$missing_event_dates <-
        domain_diagnostics$missing_event_dates + missing_dates
      domain_diagnostics$out_of_window <- domain_diagnostics$out_of_window +
        selected_count - missing_dates - window_count
      domain_diagnostics$in_window_cost_rows <- domain_diagnostics$in_window_cost_rows + window_count
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
  attr(result, "cost_scope") <- if (is.null(visitFilter)) "all_recorded_costs" else "selected_visits"
  attr(result, "visit_filter") <- visitFilter
  attr(result, "allocation") <- allocation
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

.tmaCostWindowPredicate <- function(windows, allocation = "start") {
  predicates <- lapply(seq_len(nrow(windows)), function(i) {
    id <- windows$subject_id[[i]]
    start <- windows$index_date[[i]]
    end <- windows$observation_end[[i]]
    if (allocation == "duration") {
      return(rlang::expr(.data$subject_id == !!id &
                          .data$event_end_date >= !!start & .data$event_date <= !!end))
    }
    rlang::expr(.data$subject_id == !!id &
                  .data$event_date >= !!start & .data$event_date <= !!end)
  })
  Reduce(function(a, b) rlang::expr((!!a) | (!!b)), predicates)
}
