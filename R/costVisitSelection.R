.tmaCostVisitEventMap <- function() {
  list(
    Condition = c("condition_occurrence", "condition_concept_id", "condition_start_date"),
    Drug = c("drug_exposure", "drug_concept_id", "drug_exposure_start_date"),
    Procedure = c("procedure_occurrence", "procedure_concept_id", "procedure_date"),
    Device = c("device_exposure", "device_concept_id", "device_exposure_start_date"),
    Measurement = c("measurement", "measurement_concept_id", "measurement_date"),
    Observation = c("observation", "observation_concept_id", "observation_date")
  )
}

.tmaCostVisitFilter <- function(visitFilter, cdm) {
  if (is.null(visitFilter)) return(NULL)
  keys <- c("visitIds", "visitConceptIds", "eventConcepts")
  supplied <- names(visitFilter)
  if (!is.list(visitFilter) || is.data.frame(visitFilter) ||
      !length(visitFilter) || is.null(supplied) || anyNA(supplied) ||
      any(!nzchar(supplied)) || anyDuplicated(supplied) ||
      any(!supplied %in% keys)) {
    stop("visitFilter must be a nonempty named list using visitIds, visitConceptIds or eventConcepts.",
         call. = FALSE)
  }
  if (any(vapply(visitFilter, is.null, logical(1)))) {
    stop("Omit unused visitFilter fields rather than supplying NULL.", call. = FALSE)
  }
  ids <- function(x, label, concept = FALSE) {
    if (!is.null(dim(x))) {
      stop(label, " must be an ID vector, not a table or matrix.", call. = FALSE)
    }
    value <- .tmaSubjectIds(x, label)
    if (concept && any(!grepl("^[1-9][0-9]*$", value))) {
      stop(label, " must contain positive whole concept IDs.", call. = FALSE)
    }
    unique(value)
  }
  if ("visitIds" %in% supplied) {
    visitFilter$visitIds <- ids(visitFilter$visitIds, "visitFilter$visitIds")
  }
  if ("visitConceptIds" %in% supplied) {
    visitFilter$visitConceptIds <- ids(visitFilter$visitConceptIds,
                                       "visitFilter$visitConceptIds", concept = TRUE)
  }
  eventMap <- .tmaCostVisitEventMap()
  if ("eventConcepts" %in% supplied) {
    events <- visitFilter$eventConcepts
    domains <- names(events)
    if (!is.list(events) || is.data.frame(events) || !length(events) ||
        is.null(domains) || anyNA(domains) || any(!nzchar(domains)) ||
        anyDuplicated(domains) || any(!domains %in% names(eventMap))) {
      stop("visitFilter$eventConcepts must be a nonempty named list using Condition, Drug, Procedure, Device, Measurement or Observation.",
           call. = FALSE)
    }
    for (domain in domains) {
      if (is.null(events[[domain]])) {
        stop("Omit unused eventConcepts domains rather than supplying NULL.", call. = FALSE)
      }
      events[[domain]] <- ids(events[[domain]], paste0("eventConcepts$", domain),
                              concept = TRUE)
      spec <- eventMap[[domain]]
      .tmaRequireCostTable(cdm, spec[[1]],
                           c("person_id", "visit_occurrence_id", spec[[2]], spec[[3]]))
    }
    visitFilter$eventConcepts <- events
  }
  visitColumns <- c("visit_occurrence_id", "person_id")
  if ("visitConceptIds" %in% supplied) visitColumns <- c(visitColumns, "visit_concept_id")
  .tmaRequireCostTable(cdm, "visit_occurrence", visitColumns)
  visitFilter
}

.tmaSelectedCostVisits <- function(cdm, windows, visitFilter) {
  visitSource <- cdm[["visit_occurrence"]]
  visits <- dplyr::transmute(
    visitSource,
    subject_id = as.character(.data$person_id),
    visit_occurrence_id = as.character(.data$visit_occurrence_id)
  )
  if ("visitConceptIds" %in% names(visitFilter)) {
    visits <- dplyr::transmute(
      visitSource,
      subject_id = as.character(.data$person_id),
      visit_occurrence_id = as.character(.data$visit_occurrence_id),
      visit_concept_id = as.character(.data$visit_concept_id)
    )
    conceptIds <- visitFilter$visitConceptIds
    visits <- dplyr::filter(visits, .data$visit_concept_id %in% conceptIds)
  }
  patientIds <- as.character(windows$subject_id)
  visits <- dplyr::filter(visits, .data$subject_id %in% patientIds)
  if ("visitIds" %in% names(visitFilter)) {
    visitIds <- visitFilter$visitIds
    visits <- dplyr::filter(visits, .data$visit_occurrence_id %in% visitIds)
  }
  visits <- dplyr::distinct(dplyr::select(visits, "subject_id", "visit_occurrence_id"))
  if (!nrow(windows) || !"eventConcepts" %in% names(visitFilter)) return(visits)

  eventMap <- .tmaCostVisitEventMap()
  matches <- lapply(names(visitFilter$eventConcepts), function(domain) {
    spec <- eventMap[[domain]]
    conceptIds <- visitFilter$eventConcepts[[domain]]
    events <- dplyr::transmute(
      cdm[[spec[[1]]]],
      subject_id = as.character(.data$person_id),
      visit_occurrence_id = as.character(.data$visit_occurrence_id),
      concept_id = as.character(.data[[spec[[2]]]]),
      event_date = .data[[spec[[3]]]]
    )
    events <- dplyr::filter(events, .data$subject_id %in% patientIds,
                            .data$concept_id %in% conceptIds)
    # Only evidence recorded during this patient's follow-up can select a visit.
    events <- dplyr::filter(events, !!.tmaCostWindowPredicate(windows))
    dplyr::distinct(dplyr::select(events, "subject_id", "visit_occurrence_id"))
  })
  # Repeated records and matches in several domains must not multiply costs.
  matchingVisits <- Reduce(dplyr::union, matches)
  dplyr::semi_join(visits, matchingVisits,
                   by = c("subject_id", "visit_occurrence_id"))
}
