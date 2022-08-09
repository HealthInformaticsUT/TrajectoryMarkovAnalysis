#' Prepare patientData dataframe for msm package
#'
#' This function prepares patientData dataframe for msm package
#'
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @keywords internal
#' @return A dataframe ready for using msm package
dataPreparation <- function(patientData) {
  # We leave out all subjects which are conflicting with the starting state
  dataTarget <- dplyr::filter(patientData, STATE == 'START')
  dataTarget <-
    dplyr::select(dataTarget, SUBJECT_ID, STATE_START_DATE)
  colnames(dataTarget) <- c("SUBJECT_ID", "REFERENCE_DATE")
  
  data <- merge(patientData, dataTarget, by = "SUBJECT_ID")
  
  data <- dplyr::filter(data, REFERENCE_DATE <= STATE_START_DATE)
  data <- dplyr::mutate(data,
                        TIME_IN_COHORT = as.numeric(difftime(
                          as.Date(STATE_START_DATE), as.Date(REFERENCE_DATE)
                        )) / 365.25)
  
  states <-
    c("START", setdiff(unique(data$STATE), c("START", "EXIT")), "END")
  
  n <- length(states)
  data$STATE_ID <-
    plyr::mapvalues(
      x = data$STATE,
      from = states,
      to = 1:n,
      warn_missing = FALSE
    )
  
  # We should make sure that the time_in_cohort column has differing values for each state for the same patient
  # for developement case, let's just create an artificial difference of 1 day for each colliding date
  data <- dplyr::arrange(data, SUBJECT_ID, TIME_IN_COHORT, STATE_ID)
  #data = dplyr::filter(data, SUBJECT_ID %in% c(1668,1669))
  paient_id <- NA
  last_patient_id <- data$SUBJECT_ID[1]
  last_observed_ts <- data$TIME_IN_COHORT[1]
  coef <- 1
  impact <- 1 / 365.25
  
  data$TIME_IN_COHORT <- round(data$TIME_IN_COHORT, 3)
  
  for (row in 2:nrow(data)) {
    patient_id <- data$SUBJECT_ID[row]
    if (patient_id != last_patient_id |
        last_observed_ts != data$TIME_IN_COHORT[row]) {
      last_patient_id <- patient_id
      last_observed_ts <- data$TIME_IN_COHORT[row]
      coef <- 1
    }
    else {
      last_observed_ts <- data$TIME_IN_COHORT[row]
      data$TIME_IN_COHORT[row] <-
        data$TIME_IN_COHORT[row] + impact * coef
      last_patient_id <- patient_id
      coef <- coef + 1
    }
  }
  data$STATE_ID <- as.numeric(data$STATE_ID)
  data <- dplyr::arrange(data, SUBJECT_ID, TIME_IN_COHORT, STATE_ID)
  data <-
    dplyr::select(
      data,
      SUBJECT_ID,
      STATE,
      STATE_ID,
      STATE_START_DATE,
      STATE_END_DATE,
      TIME_IN_COHORT
    )
  
  return(data)
}
