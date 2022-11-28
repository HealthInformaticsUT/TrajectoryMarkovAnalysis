################################################################################
#
# Functions which will query relevant data from database and mutate data  frames
# for better usage
#
################################################################################


#' This function adds personal data to subjects: age, gender
#'
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param connection Connection to database
#' @keywords internal
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
addPersonalData <- function(cohortData, connection) {
  data <- cohortData
  ##############################################################################
  #
  # Add state specific person data to each row
  #
  ##############################################################################
  sql_base <-
    loadRenderTranslateSql(
      dbms = dbms,
      "SELECT person_id, gender_concept_id, birth_datetime FROM @cdmSchema.person WHERE person_id in (%s);",
      cdmSchema = cdmSchema
    )
  idString <-
    toString(sprintf("%s", as.numeric(unique(data$SUBJECT_ID))))
  sql_q <- sprintf(sql_base, idString)
  personal_data <- DatabaseConnector::querySql(connection, sql_q)
  colnames(personal_data) <-
    c("SUBJECT_ID", "GENDER_CONCEPT_ID", "BIRTH_DATETIME")
  ##############################################################################
  #
  # Merge two datasets
  #
  ##############################################################################
  
  data_merged <-
    merge(x = data,
          y = personal_data,
          by = "SUBJECT_ID",
          all.x = TRUE)
  
  ##############################################################################
  #
  # Calculate age when entering state and time in cohort
  #
  ##############################################################################
  data_merged <- dplyr::mutate(data_merged, AGE = round(as.numeric(
    difftime(as.Date(STATE_START_DATE),
             as.Date(BIRTH_DATETIME),
             units = "days") / 365.25
  ), 3))
  data_merged <- dplyr::select(data_merged, !BIRTH_DATETIME)
  return(data_merged)
}


#' Get Markov state probabilities from raw data with defined state duration
#'
#' This function outputs a data.frame object which describes the movement of patients between the defined states and state duration
#'
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param stateCohorts The IDs of state cohorts
#' @param pathToResults Path to temporary folder for saving objects
#' @param statePriorityVector A vector of all the imported states in prioritized order, ascending
#' @param excludedStates A vector populated with excluded states
#' @keywords internal
getStohasticMatrix <- function(cohortData,
                               stateCohorts,
                               pathToResults = getwd(),
                               statePriorityVector = NULL,
                               studyName = 'THE-STUDY',
                               excludedStates = NULL) {
  tmp_data <- cohortData
  states <- as.character(c("START", setdiff(stateCohorts, c('START', 'EXIT')), "EXIT"))
  
  if (length(excludedStates) > 0) {
    # Excluding the explicitly chosen states
    states <- setdiff(states, excludedStates)
    tmp_data <- dplyr::filter(tmp_data, STATE %in% states)
  }
  
  
  classes <- 0:(length(states) - 1)
  tmp_data$CLASS <-
    plyr::mapvalues(
      x = tmp_data$STATE,
      from = states,
      to = classes,
      warn_missing = FALSE
    )
  tmp_data$CLASS <- as.numeric(tmp_data$CLASS)
  
  # Forward to Rcpp function
  M <- stochasticMatrix(
    matrixSize = length(states),
    states = states,
    discreteTrajectories = tmp_data
  )
  # We'll divide each row by it's sum and will get transfer probabilities
  M <- round(prop.table(as.matrix(M), 1), 4)
  # There is a possibility that we have NaN values (possible division with 0), therefore we will change them to zeros
  M[is.nan(M)] <- 0
  # As there is no states after "EXIT" we should explicitly state that from "EXIT" state the probability to transfer to "EXIT" state is 1
  M[nrow(M), ncol(M)] <- 1
  ParallelLogger::logInfo("Saving stochastic matrix!")
  save_object(M, path = paste(
    pathToResults,
    paste(
      "/tmp/databases/",
      studyName,
      "/",
      studyName,
      "_discrete_transition_matrix.rdata",
      sep = ""
    ),
    sep = ""
  ))
  ParallelLogger::logInfo(paste(
    "Saved to: ",
    pathToResults,
    paste(
      "/tmp/databases/",
      studyName,
      "/",
      studyName,
      "_discrete_transition_matrix.rdata",
      sep = ""
    ),
    sep = ""
  ))
  return(M)
}


#' Get patients' first state in the main cohort and their inclusion time
#'
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param excludedStates A vector populated with excluded states
#' @keywords internal
#' @return data.frame with four columns: unique SUBJECT_ID, FIRST_STATE, SUBJECT_START_DATE, SUBJECT_END_DATE
getFirstState <- function(cohortData, excludedStates = c()) {
  # Lets exclude "START" and "EXIT" states as we are not interested in these
  tmpDataState <-
    dplyr::filter(cohortData, !STATE %in% c("START", "EXIT", excludedStates))
  
  # Handling the case where we have 0 rows left
  if (nrow(tmpDataState) == 0) {
    return(tmpDataState)
  }
  
  tmpDataState <- dplyr::arrange(tmpDataState, STATE_START_DATE)
  tmpDataState <-
    tmpDataState[!duplicated(tmpDataState$SUBJECT_ID),]
  tmpDataState <- dplyr::select(tmpDataState, SUBJECT_ID, STATE)
  
  tmpDataDateMin <-
    dplyr::filter(cohortData, !STATE %in% c("START", "EXIT", excludedStates))#cohortData
  tmpDataDateMin$STATE_START_DATE <-
    as.Date(tmpDataDateMin$STATE_START_DATE)
  tmpDataDateMin <-
    stats::aggregate(tmpDataDateMin$STATE_START_DATE,
                     by = list(tmpDataDateMin$SUBJECT_ID),
                     min)
  colnames(tmpDataDateMin) <- c("SUBJECT_ID", "STATE_START_DATE")
  
  tmpData <- merge(tmpDataState,
                   tmpDataDateMin,
                   by.x = "SUBJECT_ID",
                   by.y = "SUBJECT_ID")
  
  tmpDataDateMax <-
    dplyr::filter(cohortData, !STATE %in% c("START", "EXIT", excludedStates)) #cohortData
  tmpDataDateMax$STATE_END_DATE <-
    as.Date(tmpDataDateMax$STATE_END_DATE)
  tmpDataDateMax <- stats::aggregate(tmpDataDateMax$STATE_END_DATE,
                                     by = list(tmpDataDateMax$SUBJECT_ID),
                                     max)
  colnames(tmpDataDateMax) <- c("SUBJECT_ID", "STATE_END_DATE")
  
  
  tmpData <-
    merge(tmpData, tmpDataDateMax, by.x = "SUBJECT_ID", by.y = "SUBJECT_ID")
  
  tmpData <-
    dplyr::select(.data = tmpData, SUBJECT_ID, STATE, STATE_START_DATE, STATE_END_DATE)
  colnames(tmpData) <- c("SUBJECT_ID",
                         "FIRST_STATE",
                         "SUBJECT_START_DATE",
                         "SUBJECT_END_DATE")
  tmpData <- dplyr::arrange(tmpData, FIRST_STATE)
  return(tmpData)
}


#' Get patients' first state statistics
#'
#' @param connection Connection to the database
#' @param dbms The Database dialect
#' @param cohortData Data retrieved from the getFirstState function
#' @param cdmTmpSchema Temp tables' schema
#' @keywords internal
#' @return A data.frame with each state's statistics
getFirstStateStatistics <- function(connection,
                                    dbms,
                                    cohortData,
                                    cdmTmpSchema,
                                    studyName = "THE-STUDY") {
  ParallelLogger::logInfo("Quering information about the costs of treatment according to patients first state")
  # Handling the case where we have 0 rows left
  if (nrow(cohortData) == 0) {
    table <- as.data.frame(cbind(NA, NA, NA, NA, NA))
    colnames(table) <- c('STATE',
                         'PERCENTAGE',
                         'MEAN CHARGE',
                         'MEAN COST',
                         'MEAN PAID')
    return(table)
  }
  
  entryPercentages <- prop.table(table(cohortData$FIRST_STATE))
  entryPercentages <- as.data.frame(entryPercentages)
  colnames(entryPercentages) <- c("STATE", "PERCENTAGE")
  
  # Let's query the total cost of the whole patient trajectory
  
  # First let's create a temp table with our data
  cohortData$SUBJECT_ID <- as.integer(cohortData$SUBJECT_ID)
  cohortData$FIRST_STATE <- as.character(cohortData$FIRST_STATE)
  cohortData$SUBJECT_START_DATE <-
    as.Date(cohortData$SUBJECT_START_DATE)
  cohortData$SUBJECT_END_DATE <-
    as.Date(cohortData$SUBJECT_END_DATE)
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = "tma_first_state",
    data = cohortData,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = TRUE,
    #    bulkLoad = TRUE,
    progressBar = TRUE
  )
  
  # Now let's query a complete table
  data <- DatabaseConnector::querySql(
    connection = connection,
    sql = SqlRender::translate(
      targetDialect = dbms,
      sql = SqlRender::render(
        sql = "SELECT tma_first_state.SUBJECT_ID AS SUBJECT_ID, tma_first_state.FIRST_STATE AS FIRST_STATE, sum(cost_person.total_charge) AS TOTAL_CHARGE, sum(cost_person.total_cost) AS TOTAL_COST, sum(cost_person.total_paid) AS TOTAL_PAID
FROM @cdmTmpSchema.cost_person
LEFT JOIN tma_first_state
  ON cost_person.person_id = tma_first_state.SUBJECT_ID
      WHERE cost_person.date BETWEEN tma_first_state.SUBJECT_START_DATE AND tma_first_state.SUBJECT_END_DATE
      GROUP BY tma_first_state.SUBJECT_ID, tma_first_state.FIRST_STATE;",
        cdmTmpSchema = cdmTmpSchema
      )
    )
  )
  data[is.na(data)] <- 0
  if (nrow(data) == 0) {
    mean_charge <- rep(NA, length(entryPercentages$STATE))
    mean_cost <- rep(NA, length(entryPercentages$STATE))
    mean_paid <- rep(NA, length(entryPercentages$STATE))
    table <-
      as.data.frame(cbind(entryPercentages, mean_charge, mean_cost, mean_paid))
    colnames(table) <- c('STATE',
                         'PERCENTAGE',
                         'MEAN CHARGE',
                         'MEAN COST',
                         'MEAN PAID')
    return(table)
  }
  save_object(data, path = paste(
    pathToResults,
    paste(
      "/tmp/databases/",
      studyName,
      "/",
      studyName,      "_first_state_statistics.csv",
      sep = ""
    ),
    sep = ""
  ))
  meanTotalCharge <-
    aggregate(data$TOTAL_CHARGE, list(data$FIRST_STATE), mean)
  colnames(meanTotalCharge) <- c("STATE", "MEAN_TOTAL_CHARGE")
  meanTotalCosts <-
    aggregate(data$TOTAL_COST, list(data$FIRST_STATE), mean)
  colnames(meanTotalCosts) <- c("STATE", "MEAN_TOTAL_COST")
  meanTotalPaid <-
    aggregate(data$TOTAL_PAID, list(data$FIRST_STATE), mean)
  colnames(meanTotalPaid) <- c("STATE", "MEAN_TOTAL_PAID")
  table <- cbind(
    entryPercentages,
    meanTotalCharge$MEAN_TOTAL_CHARGE,
    meanTotalCosts$MEAN_TOTAL_COST,
    meanTotalPaid$MEAN_TOTAL_PAID
  )
  colnames(table) <-
    c("STATE", "PERCENTAGE", "MEAN CHARGE", "MEAN COST", "MEAN PAID")
  save_object(table, path = paste(
    pathToResults,
    paste(
      "/tmp/databases/",
      studyName,
      "/",
      studyName,      "_first_state_statistics.txt",
      sep = ""
    ),
    sep = ""
  ))
  ParallelLogger::logInfo(paste(
    "Saved to: ",
    pathToResults,
    paste(
      "/tmp/databases/",
      studyName,
      "/",
      studyName,
      "_first_state_statistics.txt",
      sep = ""
    ),
    sep = ""
  ))
  return(table)
}



#' Get patients' state and trajectory cost statistics
#'
#' @param connection Connection to the database
#' @param dbms Database dialect
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param stateStatistics A (reactive) table for the retention of state statistics
#' @param cdmTmpSchema The temp tables' schema
#' @param cost_domains The selected cost domain ids
#' @param excludedStates A vector populated with excluded states
#' @keywords internal
#' @return A data.frame with each state's statistics
getStateStatistics <- function(connection,
                               dbms,
                               cohortData,
                               cdmTmpSchema,
                               stateStatistics = NULL,
                               studyName = 'THE-STUDY',
                               excludedStates = c(),
                               cost_domains =  c('Drug',
                                                 'Visit',
                                                 'Procedure',
                                                 'Device',
                                                 'Measurement',
                                                 'Observation',
                                                 'Specimen')) {
  ParallelLogger::logInfo("Quering information about the costs of treatment in a state")
  
  # Lets exclude "START" and "EXIT" states as we are not interested in these
  tmpDataState <-
    dplyr::filter(cohortData, !STATE %in% c("START", "EXIT", excludedStates))
  
  # Handling the case where there are zero rows left
  if (nrow(tmpDataState) == 0) {
    summaryTable <-  data.frame(matrix(ncol = 8, nrow = 0))
    colnames(summaryTable) <- c(
      "STATE",
      "PERCENTAGE",
      "MEAN CHARGE",
      "CI CHARGE",
      "MEAN COST",
      "CI COST",
      "MEAN PAID",
      "CI PAID"
    )
    
    return((list(
      "table" = summaryTable,
      "mean" = 0,
      "median" = 0
    )))
  }
  entryPercentages <- prop.table(table(tmpDataState$STATE))
  entryPercentages <- as.data.frame(entryPercentages)
  colnames(entryPercentages) <- c("STATE", "COUNT")
  
  # Let's query the total cost of the whole patient trajectory
  
  # First let's create a temp table with our data
  tmpDataState$SUBJECT_ID <- as.integer(tmpDataState$SUBJECT_ID)
  tmpDataState$FIRST_STATE <- as.character(tmpDataState$STATE)
  tmpDataState$STATE_START_DATE <-
    as.Date(tmpDataState$STATE_START_DATE)
  tmpDataState$STATE_END_DATE <-
    as.Date(tmpDataState$STATE_END_DATE)
  tmpDataState <- dplyr::select(tmpDataState,
                                SUBJECT_ID,
                                STATE,
                                STATE_START_DATE,
                                STATE_END_DATE)
  
  DatabaseConnector::insertTable(
    connection = connection,
    tableName = "tma_states",
    data = tmpDataState,
    dropTableIfExists = TRUE,
    createTable = TRUE,
    tempTable = TRUE,
    #    bulkLoad = TRUE,
    progressBar = TRUE
  )
  # Now let's query a complete table
  sql_s <- toString(sprintf("'%s'", cost_domains))
  #   data = DatabaseConnector::querySql(
  #     connection = connection,
  #     sql = SqlRender::translate(
  #       targetDialect = dbms,
  #       sql = sprintf(SqlRender::render(
  #         sql = "select total_cost_person.STATE_ AS STATE, AVG(total_cost_person.total_charge) AS MEAN_CHARGE,stddev(total_cost_person.total_charge) AS MEAN_CHARGE_STD, AVG(total_cost_person.total_cost) AS MEAN_COST, stddev(total_cost_person.total_cost) AS MEAN_COST_STD, AVG(total_cost_person.total_paid) AS MEAN_PAID, stddev(total_cost_person.total_paid) AS MEAN_PAID_STD
  # FROM
  # (SELECT tma_states.STATE AS STATE_, SUM(cost_person.total_charge)/(tma_states.STATE_END_DATE-tma_states.STATE_START_DATE+1) as TOTAL_CHARGE, SUM(cost_person.total_cost)/(tma_states.STATE_END_DATE-tma_states.STATE_START_DATE+1) AS TOTAL_COST, SUM(cost_person.total_paid)/(tma_states.STATE_END_DATE-tma_states.STATE_START_DATE+1) AS TOTAL_PAID
  # FROM @cdmTmpSchema.cost_person
  # LEFT JOIN tma_states
  #   ON cost_person.person_id = tma_states.SUBJECT_ID
  #       WHERE cost_person.date BETWEEN tma_states.STATE_START_DATE AND tma_states.STATE_END_DATE AND cost_person.total_charge IS NOT NULL AND cost_person.cost_domain_id IN (%s)
  #     GROUP BY cost_person.person_id, tma_states.STATE, tma_states.STATE_START_DATE,tma_states.STATE_END_DATE) as total_cost_person
  #     GROUP BY total_cost_person.STATE_;",
  #         cdmTmpSchema = cdmTmpSchema
  #       ),
  #       sql_s
  #       )
  #     )
  #   )
  
  data <- DatabaseConnector::querySql(
    connection = connection,
    sql = SqlRender::translate(
      targetDialect = dbms,
      sql = sprintf(
        SqlRender::render(
          sql = "SELECT tma_states.STATE AS STATE_P, SUM(cost_person.total_charge)/(tma_states.STATE_END_DATE-tma_states.STATE_START_DATE+1) as TOTAL_CHARGE,
        SUM(cost_person.total_cost)/(tma_states.STATE_END_DATE-tma_states.STATE_START_DATE+1) AS TOTAL_COST, SUM(cost_person.total_paid)/(tma_states.STATE_END_DATE-tma_states.STATE_START_DATE+1) AS TOTAL_PAID,
        cost_person.person_id AS PERSON_ID, SUM(cost_person.total_charge) AS TOTAL_STATE_CHARGE
FROM @cdmTmpSchema.cost_person
LEFT JOIN tma_states
  ON cost_person.person_id = tma_states.SUBJECT_ID
      WHERE cost_person.date BETWEEN tma_states.STATE_START_DATE AND tma_states.STATE_END_DATE AND cost_person.cost_domain_id IN (%s)
    GROUP BY cost_person.person_id, tma_states.STATE, tma_states.STATE_START_DATE,tma_states.STATE_END_DATE;",
          cdmTmpSchema = cdmTmpSchema
        ),
        sql_s
      )
    )
  )
  save_object(data, path = paste(
    pathToResults,
    paste("/tmp/databases/",studyName,"/", studyName, "_state_cost.csv", sep = ""),
    sep = ""
  ))
  ParallelLogger::logInfo(paste(
    "Saved to: ",
    pathToResults,
    paste("/tmp/databases/",studyName,"/", studyName, "_state_cost.csv", sep = ""),
    sep = ""
  ))
  # Overall trajectories charge statistics mean & median
  data_mm = readr::read_csv(paste(pathToResults,
                                  paste("/tmp/databases/",studyName,"/", studyName, "_first_state_statistics.csv", sep = ""),
                                  sep = ""), col_types = readr::cols())
  
  dataTrajectoryChargeCalculations <-
    dplyr::summarise(dplyr::group_by(
      dplyr::select(data, PERSON_ID, TOTAL_STATE_CHARGE),
      PERSON_ID
    ),
    CHARGE = sum(TOTAL_STATE_CHARGE))
  trajectoriesMeanCharge <-
    mean(data_mm$TOTAL_CHARGE,na.rm = TRUE)
  trajectoriesMedianCharge <-
    stats::median(data_mm$TOTAL_CHARGE,na.rm = TRUE)
  # Overall state chrage, cost, paid statistics
  uniqueStates <- unique(data$STATE_P)
  
  
  summaryTable <-  data.frame(matrix(ncol = 7, nrow = 0))
  for (state in uniqueStates) {
    state_data <- dplyr::filter(data, STATE_P == state)
    summaryTable <- rbind(summaryTable,
                          c(
                            state,
                            mean(state_data$TOTAL_CHARGE,na.rm = TRUE),
                            stats::sd(state_data$TOTAL_CHARGE,na.rm = TRUE),
                            mean(state_data$TOTAL_COST,na.rm = TRUE),
                            stats::sd(state_data$TOTAL_COST,na.rm = TRUE),
                            mean(state_data$TOTAL_PAID,na.rm = TRUE),
                            stats::sd(state_data$TOTAL_PAID,na.rm = TRUE)
                          ))
  }
  colnames(summaryTable) <-
    c(
      "STATE",
      "MEAN_CHARGE",
      "CHARGE_STD",
      "MEAN_COST",
      "COST_STD",
      "MEAN_PAID",
      "PAID_STD"
    )
  summaryTable[is.na(summaryTable)] <- 0
  stateStatistics <- summaryTable
  # Handling the case when database returns no rows
  if (nrow(summaryTable) == 0) {
    mean_charge <- rep(NA, length(entryPercentages$STATE))
    ci_charge <- rep(NA, length(entryPercentages$STATE))
    mean_cost <- rep(NA, length(entryPercentages$STATE))
    ci_cost <- rep(NA, length(entryPercentages$STATE))
    mean_paid <- rep(NA, length(entryPercentages$STATE))
    ci_paid <- rep(NA, length(entryPercentages$STATE))
    summaryTable <- as.data.frame(
      cbind(
        entryPercentages,
        mean_charge,
        mean_cost,
        ci_charge,
        mean_cost,
        ci_cost,
        mean_paid,
        ci_paid
      )
    )
    colnames(summaryTable) = c(
      "STATE",
      "PERCENTAGE",
      "MEAN CHARGE",
      "CI CHARGE",
      "MEAN COST",
      "CI COST",
      "MEAN PAID",
      "CI PAID"
    )
    
    return((list(
      "table" = summaryTable,
      "mean" = 0,
      "median" = 0
    )))
  }
  table <- cbind(
    stateStatistics$STATE,
    stateStatistics$MEAN_CHARGE,
    paste(
      "(",
      max(0, round(
        as.numeric(stateStatistics$MEAN_CHARGE) - 1.96 * as.numeric(stateStatistics$CHARGE_STD),
        2
      )),
      ", ",
      round(
        as.numeric(stateStatistics$MEAN_CHARGE) + 1.96 * as.numeric(stateStatistics$CHARGE_STD),
        2
      )
      ,
      ")"
    ),
    stateStatistics$MEAN_COST,
    paste(
      "(",
      max(0, round(
        as.numeric(stateStatistics$MEAN_COST) - 1.96 * as.numeric(stateStatistics$COST_STD),
        2
      )),
      ", ",
      round(
        as.numeric(stateStatistics$MEAN_COST) + 1.96 * as.numeric(stateStatistics$COST_STD),
        2
      )
      ,
      ")"
    ),
    stateStatistics$MEAN_PAID,
    paste(
      "(",
      max(0, round(
        as.numeric(stateStatistics$MEAN_PAID) - 1.96 * as.numeric(stateStatistics$PAID_STD),
        2
      )),
      ", ",
      round(
        as.numeric(stateStatistics$MEAN_PAID) + 1.96 * as.numeric(stateStatistics$PAID_STD),
        2
      )
      ,
      ")"
    )
  )
  table <- as.data.frame(table)
  colnames(table) <- c("STATE",
                       "MEAN CHARGE",
                       "CI CHARGE",
                       "MEAN COST",
                       "CI COST",
                       "MEAN PAID",
                       "CI PAID")
  table <- merge(
    x = entryPercentages,
    y = table,
    by.x = "STATE",
    by.y = "STATE",
    all.x = TRUE
  )
  colnames(table) <- c(
    "STATE",
    "PERCENTAGE",
    "MEAN CHARGE",
    "CI CHARGE",
    "MEAN COST",
    "CI COST",
    "MEAN PAID",
    "CI PAID"
  )
  save_object(table, path = paste(
    pathToResults,
    paste("/tmp/databases/",       studyName,
          "/",
          studyName, "_state_statistics.txt", sep = ""),
    sep = ""
  ))
  ParallelLogger::logInfo(paste(
    "Saved to: ",
    pathToResults,
    paste("/tmp/databases/",       studyName,
          "/",
          studyName, "_state_statistics.txt", sep = ""),
    sep = ""
  ))
  return(
    list(
      "table" = table,
      "mean" = trajectoriesMeanCharge,
      "median" = trajectoriesMedianCharge
    )
  )
}



################################################################################
#
# Profile view functions
#
################################################################################

#' Get selected profile ID patient data
#'
#' This function queries patient's data from ohdsi_cdm person table
#'
#' @param connection Connection to the database
#' @param dbms Database management system (postgresql, mysql etc.)
#' @param resultsSchema Name of the OMOP results schema
#' @param cdmSchema Name of the OMOP cdm schema
#' @param personId Person id value in person table
#' @keywords internal
#' @return A dataframe with selected patient's row data from OMOP database
getProfileData <- function(connection,
                           dbms,
                           resultsSchema,
                           cdmSchema,
                           personId) {
  ParallelLogger::logInfo("Quering information about the selected person")
  # Error handling
  if (!grepl("\\D", personId)) {
    personData <- DatabaseConnector::querySql(
      connection,
      loadRenderTranslateSql(
        dbms = dbms,
        sql = "SELECT birth_datetime, gender_concept_id, race_concept_id FROM @cdmSchema.person WHERE person_id = @personId",
        personId = personId,
        cdmSchema = cdmSchema
      )
    )
    personData$genderString <- ifelse(
      personData$GENDER_CONCEPT_ID == 8532,
      "Female",
      ifelse(personData$GENDER_CONCEPT_ID == 8507, "Male", "Unknown")
    )
    personData$birthtimeString <-
      toString(personData$BIRTH_DATETIME)
    return(personData)
  }
  else {
    personData <- cbind(c("NA"), c("NA"), c("NA"), c("NA"), c("NA"))
    colnames(personData) <- c(
      "BIRTH_DATETIME",
      "GENDER_CONCEPT_ID",
      "RACE_CONCEPT_ID",
      "genderString",
      "birthtimeString"
    )
    return(personData)
  }
}


#' Get patient's drug era cost
#'
#' This function queries patient's treatment cost
#'
#' @param connection Connection to database
#' @param dbms Database management system: postgresql, mysql etc.
#' @param patientId The id number of the patient of interest
#' @param startDate Starting date of the observation
#' @param endDate Ending date of the observation
#' @param domain Vector of treatment types of interest (Drug, Visit, Procedure, Device, Measurement, Observation, Specimen)
#' @param cdmTmpSchema temp tables schema
#' @keywords internal
#' @return Value of complete cost
getCost <- function(connection,
                    dbms,
                    patientId,
                    startDate,
                    endDate,
                    # Create a checkboxbutton box for selection
                    domain = c('Drug',
                               'Visit',
                               'Procedure',
                               'Device',
                               'Measurement',
                               'Observation',
                               'Specimen'),
                    cdmTmpSchema) {
  #ParallelLogger::logInfo("Quering information about the cost of the patients")
  sql_s <- toString(sprintf("'%s'", domain))
  sql_r <- SqlRender::render(
    sql = paste(
      "SELECT sum(total_cost) AS COST FROM @cdmTmpSchema.cost_person WHERE person_id = @patientId AND date BETWEEN",
      paste("'", "@startDate", "'", sep = "") ,
      "AND",
      paste("'", "@endDate", "'", sep = ""),
      "AND cost_domain_id IN (%s);"
    ),
    patientId = as.integer(patientId),
    startDate = startDate,
    endDate = endDate,
    cdmTmpSchema = cdmTmpSchema
  )
  sql <- sprintf(sql_r, sql_s)
  totalCost <- DatabaseConnector::querySql(connection,
                                           sql = SqlRender::translate(sql = sql, targetDialect = dbms))
  return(totalCost)
}
