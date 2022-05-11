################################################################################
#
# CLI implementation for TrajectoryMarkovAnalysis
#
################################################################################


#' This function outputs Markov model fitted from observed data
#' 
#' @param conn Connection to the database
#' @param dbms Database dialect
#' @param cdmSchema OHDSI OMOP CDM tabels' schema
#' @param cdmTmpSchema Table for creating temp tables
#' @param inputData Observed trajectories data outputted by Cohort2Trajectory package
#' @param modelType Markov model type can be "discrete" or "continuous"
#' @param studyName Customized study name
#' @param pathToResults The path where results will be saved
#' @param excludedStates States which have to be discarded from the study
#' @param costDomains Cost domains to include in cost analysis
#' 

TrajectoryMarkovAnalysis = function(conn,
                                    dbms,
                                    cdmSchema,
                                    cdmTmpSchema,
                                    inputData,
                                    modelType,
                                    studyName,
                                    pathToResults = getwd(),
                                    excludedStates = c(),
                                    costDomains = c('Drug',
                                                    'Visit',
                                                    'Procedure',
                                                    'Device',
                                                    'Measurement',
                                                    'Observation',
                                                    'Specimen')
                                    ) {
  
  
  ###############################################################################
  #
  # Creating mandatory directories if they do not exist
  #
  ###############################################################################
  
  createMandatorySubDirs(pathToResults)
  
  # Creating temp tables
  if (!DatabaseConnector::dbExistsTable(conn = conn,
                                        name = "cost_person",
                                        schema = cdmTmpSchema)) {
    generateTempTables(
      connection = conn,
      dbms = dbms,
      cdmSchema = cdmSchema,
      cdmTmpSchema = cdmTmpSchema
    )
    ParallelLogger::logInfo(paste("Table cost_person created to", cdmTmpSchema, "schema", sep = " "))
  }
  else {
    ParallelLogger::logInfo("Table cost_person already exists!")
  }
################################################################################
#
# Getting metadata from input dataframe
#
################################################################################
  inputData = as.data.frame(inputData)
  states = unique(inputData$STATE)
  ids = unique(inputData$STATE_ID)
  idStates = cbind(ids, states)
  colnames(idStates) = c("STATE_ID", "STATE")
  idStates = as.data.frame(idStates)
  
  markovModel = NULL
  
  ##############################################################################
  #
  # Discrete implementation
  #
  ##############################################################################
  if(modelType == "discrete") {
  
    markovModel = getStohasticMatrix(
    cohortData = inputData,
    stateCohorts = states,
    pathToResults = pathToResults,
    studyName = studyName,
    excludedStates = excludedStates
  )
    
    
    
  }
  
  ##############################################################################
  #
  # Continuous case
  #
  ##############################################################################
  else{
    qmatrixCMC <-
      matrix(1, nrow = length(states), ncol = length(states))
    diag(qmatrixCMC) <- 0
    qmatrixCMC[, 1] = 0
    qmatrixCMC[length(states),] = 0
    # Add correct labels
    idStates = dplyr::arrange(idStates, STATE_ID)
    
    colnames(qmatrixCMC) <- idStates$STATE
    rownames(qmatrixCMC) <- idStates$STATE
    
    markovModel =  msm::msm(
      STATE_ID ~ TIME_IN_COHORT,
      subject = SUBJECT_ID,
      data = inputData,
      qmatrix = qmatrixCMC,
      # Maybe add a checkbox for generation
      gen.inits = T,
      exacttimes = T,
      control = list(fnscale = 400000, maxit = 400)
    )
    ParallelLogger::logInfo("Continuous time Markov model calculated!")
    save_object(markovModel, path = paste(
      pathToResults,
      paste(
        "/tmp/models/",
        studyName,
        "_continuous_intensity_matrix.rdata",
        sep = ""
      ),
      sep = ""
    ))
    ParallelLogger::logInfo(paste(
      "Saved to: ",
      pathToResults,
      paste(
        "/models/",
        studyName,
        "_continuous_intensity_matrix.rdata",
        sep = ""
      ),
      sep = ""
    ))
  }
  
  
  
################################################################################
#
# State statistics
#
################################################################################
  stateStatisticsTable =  getStateStatistics(
    conn,
    dbms = dbms,
    cohortData = inputData,
    cdmTmpSchema = cdmTmpSchema,
    studyName = studyName,
    cost_domains = costDomains,
    excludedStates = excludedStates
  )

    tmpData = getFirstState(inputData,excludedStates)
      getFirstStateStatistics(
        connection = conn,
        dbms = dbms,
        cohortData = tmpData,
        cdmTmpSchema = cdmTmpSchema,
        studyName = studyName
      )
      
      # getStateStatistics(
      #   conn,
      #   dbms = dbms,
      #   cohortData = inputData,
      #   cdmTmpSchema = cdmTmpSchema,
      #   stateStatistics = stateStatistics,
      #   studyName = studyName,
      #   cost_domains = costDomains,
      #   excludedStates = excludedStates
      # )
      
      ################################################################################
      #
      # Delete temp tables & Disconnect database connection
      #
      ################################################################################
      
      ans = droppingTables()
      if (ans == "y") {
        dropRelation(
          connection = conn,
          dbms = dbms,
          schema = cdmTmpSchema,
          relationName = "cost_person"
        )
      }
      
      DatabaseConnector::disconnect(conn)
      ParallelLogger::logInfo("The database conncetion has been closed")
      
}
