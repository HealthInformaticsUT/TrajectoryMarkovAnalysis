################################################################################
#
# Functions for generating n trajectories (Generation tab)
#
################################################################################


#' Generate patient trajectories
#'
#' This function outputs a data.frame object which describes the movement of patients between the defined states and state duration
#'
#' @param transitionMatrix The transition matrix for states
#  @param startingProbabilities State starting probabilities derived from data
#' @param n Number of patients' trajectories to generate
#' @param minDate Smallest possible trajectory start date
#' @param maxDate Largest possible trajectory start date
#' @param maxOut Maximum nr of days patient can be out of cohort
#' @param pathToResults Path to temp folder for saving objects
#' @param generateCost Value [0,1] indicating whether to generate cost of states
#' @param statisticsTable Table retrieved from observed data for cost values
#' @param studyName  Customized study name
#' @export
generateDataDiscrete <- function(transitionMatrix,
                                 #startingProbabilities,
                                 n = 100,
                                 minDate = "1900-01-01",
                                 maxDate = "2021-12-31",
                                 maxOut = 183,
                                 stateDuration = 30,
                                 pathToResults = getwd(),
                                 generateCost = 0,
                                 statisticsTable = NULL,
                                 studyName = "") {
  genData <- list()
  
  ParallelLogger::logInfo(paste("Starting generation of ", n, " patients!"))
  
  for (patientId in 1:n) {
    startProbabilty <- runif(1)
    startStateIndex <-
      which.min(transitionMatrix["START",] < startProbabilty)
    startState <- colnames(transitionMatrix)[startStateIndex]
    
    startDate <-
      as.Date(minDate) + runif(1, min = 0, max = as.numeric(as.Date(maxDate) - as.Date(minDate)))
    # Adding "START" state
    tmpPatientInfo <- data.frame()
    newRow <- c(
      as.numeric(patientId),
      as.character("START"),
      as.character(startDate),
      as.character(startDate)
    )
    tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
    # Adding first state
    
    newRow <- c(
      as.numeric(patientId),
      startState,
      as.character(startDate),
      as.character(startDate + stateDuration)
    )
    tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
    lastState <- startState
    # a while loop
    outOfCohortDays <- 0
    i <- 0
    while (TRUE) {
      i <- i + 1
      # Uniform distribution value for determining the next state
      probability <- runif(1)
      nextStateIndex <-
        which.min(cumsum(transitionMatrix[lastState,]) < probability)
      nextState <- colnames(transitionMatrix)[nextStateIndex]
      
      if (nextState == "START") {
        next ## fix, in some cases there can be multiple START states generated (rounding bug)
      }
      lastState <- nextState
      ############################################################################
      #
      # If state is "EXIT" then add row and exit
      # If we have generated the state of "OUT OF COHORT" we check whether we have
      # passed the limit of "OUT OF COHORT" days, if so we stop the trajectory
      #
      ############################################################################
      if (nextState == "EXIT") {
        # Adding "EXIT" state
        newRow <- c(
          as.numeric(patientId),
          as.character("EXIT"),
          as.character(startDate + i * stateDuration),
          as.character(startDate + i * stateDuration + 1)
        )
        tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
        
        break
      }
      else if (nextState == "OUT OF COHORT") {
        outOfCohortDays <- outOfCohortDays + stateDuration
        # Adding OUT OF COHORT" state
        newRow <- c(
          as.numeric(patientId),
          as.character("OUT OF COHORT"),
          as.character(startDate + i * stateDuration),
          as.character(startDate + (i + 1) * stateDuration)
        )
        tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
        # Case when we have passed the limit of out-of-cohort days
        if (outOfCohortDays > maxOut) {
          # Adding "EXIT" state
          newRow <- c(
            as.numeric(patientId),
            as.character("EXIT"),
            as.character(startDate + (i + 1) * stateDuration),
            as.character(startDate + (i + 1) * stateDuration + 1)
          )
          tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
          
          # Ending loop as OUT OF COHORT days has maxed out
          break
        }
      }
      else {
        outOfCohortDays <- 0
        # Adding generated row if statements above not true
        
        newRow <- c(
          as.numeric(patientId),
          nextState,
          as.character(startDate + i * stateDuration),
          as.character(startDate + (i + 1) * stateDuration)
        )
        tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
        
        
      }
      
    }
    
    colnames(tmpPatientInfo) <-  c("SUBJECT_ID",
                                   "STATE",
                                   "STATE_START_DATE",
                                   "STATE_END_DATE")
    genData[[patientId]] <- tmpPatientInfo
  }
  
  genData <- do.call(rbind, genData)
  
  ##############################################################################
  #
  # Generating cost on the basis of data in cost table
  #
  ##############################################################################
  
  if (generateCost == 1) {
    costs <- c()
    cost <- NA
    for (state in genData$STATE) {
      if (state %in% c("START", "EXIT")) {
        cost <- 0
      }
      else {
        cost <-
          generateStateCost(observedStatistics = statisticsTable,
                            state,
                            duration = stateDuration)
      }
      costs <- c(costs, cost)
    }
    genData <- cbind(genData, costs)
    colnames(genData) <-  c("SUBJECT_ID",
                            "STATE",
                            "STATE_START_DATE",
                            "STATE_END_DATE",
                            "COST")
  }
  
  
  ParallelLogger::logInfo("Saving generated patients to a .csv file")
  save_object(
    object <- genData,
    path <- paste(
      pathToResults,
      "/tmp/databases/",
      studyName,
      "/",
      studyName,
      "generatedTrajectoriesDiscrete.csv",
      sep = ""
    )
  )
  ParallelLogger::logInfo(paste(
    "Saved to: ",
    paste(
      pathToResults,
      "/tmp/databases/",
      studyName,
      "/",
      studyName,
      "generatedTrajectoriesDiscrete.csv",
      sep = ""
    ),
    sep = ""
  ))
  return(genData)
}



################################################################################
#
# Functions for generating n trajectories (Generation tab)
#
################################################################################


#' Generate patient trajectories
#'
#' This function outputs a data.frame object which describes the movement of patients between the defined states and state duration
#'
#' @param model The markov model calculated using msm package
#' @param n Number of patients' trajectories to generate
#' @param minDate Smallest possible trajectory start date
#' @param maxDate Largest possible trajectory start date
#' @param pathToResults Path to temp folder for saving objects
#' @param generateCost Value [0,1] indicating whether to generate cost of states
#' @param statisticsTable Table retrieved from observed data for cost values
#' @param studyName  Customized study name
#' @export
generateDataContinuous <- function(model,
                                   n = 100,
                                   minDate = "1900-01-01",
                                   maxDate = "2021-12-31",
                                   pathToResults = getwd(),
                                   generateCost = 0,
                                   statisticsTable = NULL,
                                   studyName = "") {
  genData <- list()
  
  ParallelLogger::logInfo(paste("Starting generation of ", n, " patients!"))
  Q <- msm::qmatrix.msm(model)
  intensityMatrix <- data.frame(unclass(Q$estimates))
  stateLabels <- colnames(model$QmatricesSE$baseline)
  colnames(intensityMatrix) <- stateLabels
  rownames(intensityMatrix) <- stateLabels
  for (patientId in 1:n) {
    startState <- "START"
    # Creating a random trajectory start date from sepcified region
    startDate <-
      as.Date(minDate) + runif(1, min = 0, max = as.numeric(as.Date(maxDate) - as.Date(minDate)))
    stayYears <- rexp(1,-intensityMatrix[startState, startState])
    endDate <- startDate + 1 #as.integer(stayYears*365)
    tmpPatientInfo <- data.frame()
    newRow = c(
      as.numeric(patientId),
      startState,
      as.character(startDate),
      as.character(endDate)
    )
    tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
    lastState <- startState
    # a while loop until we hit the absorbing state
    while (TRUE) {
      probability <- runif(1)
      transitionMatrix <- msm::pmatrix.msm(model, t = stayYears)
      colnames(transitionMatrix) <- stateLabels
      rownames(transitionMatrix) <- stateLabels
      nextStateIndex <-
        which.min(cumsum(transitionMatrix[lastState,]) < probability)
      nextState <- colnames(transitionMatrix)[nextStateIndex]
      if (lastState == "START" & nextState == "EXIT") {
        next
      }
      startDate <- endDate
      stayYears <- if (nextState == "EXIT") {
        0.0
      }
      else {
        rexp(1,-intensityMatrix[nextState, nextState])
      }
      endDate <- startDate + as.integer(stayYears * 365.25)
      
      newRow <- c(
        as.numeric(patientId),
        nextState,
        as.character(startDate),
        as.character(endDate)
      )
      tmpPatientInfo <- rbind(tmpPatientInfo, newRow)
      if (nextState == "EXIT") {
        break
      }
      lastState <- nextState
    }
    
    colnames(tmpPatientInfo) <- c("SUBJECT_ID",
                                  "STATE",
                                  "STATE_START_DATE",
                                  "STATE_END_DATE")
    ############################################################################
    #
    # Multiple "START" states might be generated in sequence. Lets collide those
    # instances (rounding bug).
    #
    ############################################################################
    
    tmpPatientInfo_START <-
      dplyr::filter(tmpPatientInfo, STATE == "START")
    tmpPatientInfo_START$STATE_START_DATE <-
      min(tmpPatientInfo_START$STATE_START_DATE)
    tmpPatientInfo_START$STATE_END_DATE <-
      min(tmpPatientInfo_START$STATE_END_DATE)
    tmpPatientInfo_START <- tmpPatientInfo_START[1,]
    tmpPatientInfo <- rbind(tmpPatientInfo_START,
                            dplyr::filter(tmpPatientInfo, STATE != "START"))
    
    genData[[patientId]] <- tmpPatientInfo
  }
  
  genData <- do.call(rbind, genData)
  
  ##############################################################################
  #
  # Generating cost on the basis of data in cost table
  #
  ##############################################################################
  
  if (generateCost == 1) {
    costs <- c()
    for (row in 1:nrow(genData)) {
      state <- genData[row, "STATE"]
      if (state %in% c("START", "EXIT")) {
        cost <- 0
      }
      else {
        start <- genData[row, "STATE_START_DATE"]
        end <- genData[row, "STATE_END_DATE"]
        cost <-
          generateStateCost(
            observedStatistics = statisticsTable,
            state,
            duration = as.integer(as.Date(end) - as.Date(start))
          )
      }
      costs <- c(costs, cost)
    }
    genData <- cbind(genData, costs)
    colnames(genData) <- c("SUBJECT_ID",
                           "STATE",
                           "STATE_START_DATE",
                           "STATE_END_DATE",
                           "COST")
  }
  
  
  ParallelLogger::logInfo("Saving generated patients to a .csv file")
  save_object(
    object <- genData,
    path <- paste(
      pathToResults,
      "/tmp/databases/",
      studyName,
      "/",
      studyName,      "generatedTrajectoriesContinuous.csv",
      sep = ""
    )
  )
  ParallelLogger::logInfo(paste(
    "Saved to: ",
    paste(
      pathToResults,
      "/tmp/databases/",
      studyName,
      "/",
      studyName,
      "generatedTrajectoriesDiscrete.csv",
      sep = ""
    ),
    sep = ""
  ))
  return(genData)
  
}


################################################################################
#
# Function which returns a table for comparing generated and observed
# data descriptions
#
################################################################################


#' Compare two trajectory dataframes
#'
#' This function outputs a data table comparing two patient trajectory dataframes
#'
#' @param observedData A data.frame object which is output of Cohort2Trajectory package
#' @param generatedData The data frame of generated data
#' @keywords internal
compareTrajectoryData <- function(observedData, generatedData) {
  frame1 <-
    as.data.frame(round(table(observedData$STATE) / nrow(observedData), 5))
  frame2 <-
    as.data.frame(round(table(generatedData$STATE) / nrow(generatedData), 5))
  dataTable <- merge(frame1, frame2, by = "Var1")
  
  colnames(dataTable) <- c("STATE", "OBSERVED", "GENERATED")
  dataTable$OBSERVED <-
    as.character(formattable::percent(dataTable$OBSERVED, 4))
  dataTable$GENERATED <-
    as.character(formattable::percent(dataTable$GENERATED, 4))
  return(dataTable)
}

################################################################################
#
# Function which returns a matrix for comparing generated and observed
# data for Kaplan-Meier survival logRank tests
#
################################################################################


#' Compare two trajectory dataframes with logRank tests for all possible transfers
#'
#' This function outputs a matrix table comparing two patient trajectory dataframes with logRank tests
#'
#' @param observedDataA data.frame object which is output of Cohort2Trajectory package
#' @param generatedData the data frame of generated data
#' @keywords internal
compareTrajectoryDataLogRank <-
  function(observedData, generatedData) {
    allStates <- sort(unique(observedData$STATE))
    lrMatrix <- matrix(NA, length(allStates), length(allStates))
    colnames(lrMatrix) <- allStates
    rownames(lrMatrix) <- allStates
    for (startCohortId in allStates) {
      for (endCohortId in allStates) {
        test_survival1 <- kmDataPreparation(
          observedData,
          startCohortId,
          endCohortId,
          ageInterval = 0,
          selectedIntervals = NULL,
          survivalType = "nearest"
        )
        test_survival2 <- kmDataPreparation(
          generatedData,
          startCohortId,
          endCohortId,
          ageInterval = 0,
          selectedIntervals = NULL,
          survivalType = "nearest"
        )
        if (nrow(test_survival1) == 0 |
            nrow(test_survival2) == 0) {
          lrMatrix[startCohortId, endCohortId] = 1
          next
        }
        
        test_survival1$GROUP <- "Observed"
        test_survival2$GROUP <- "Generated"
        test_survival_combined <-
          rbind(test_survival1, test_survival2)
        
        surv_object <-
          survminer::surv_fit(survival::Surv(DATE, OUTCOME) ~ GROUP, data = test_survival_combined)
        p_value <-
          survminer::surv_pvalue(surv_object, test_survival_combined)$pval
        ##########################################################################
        # surv_object <-
        #   survival::Surv(time = test_survival_combined$DATE, event =  test_survival_combined$OUTCOME)
        # test_survival_combined$SURV_OBJECT = surv_object
        #
        # diff <-
        #   survival::survdiff(SURV_OBJECT ~ GROUP, data = test_survival_combined)
        # p_value = pchisq(diff$chisq, length(diff$n) - 1, lower.tail = FALSE)
        lrMatrix[startCohortId, endCohortId] <- if (is.na(p_value)) {
          0
        }
        else {
          p_value
        }
      }
    }
    return(lrMatrix)
  }



################################################################################
#
# Function which generates value (cost) from observed statistics
#
################################################################################
#' Compare two trajectory dataframes with logRank tests for all possible transfers
#'
#' This function outputs a value generated from observed data
#'
#' @param observedStatistics Statistics observed from data (mean, std) dataframe
#' @param state The state notation
#' @param duration Duration of the state
#' @keywords internal
generateStateCost <- function(observedStatistics, state, duration) {
  index <- which(observedStatistics[, 1] == state)
  
  # Beta distribution
  # mean = cost_data_statistics[index,2]$mean_charge/100000
  # sd=cost_data_statistics[index,3]$mean_charge_std/100000
  # alfa = mean*((mean*(1-mean))/sd^2-1)
  # beta = alfa*(1/mean-1)
  # cost = rbeta(1, shape1 = alfa, shape2=beta)
  
  # Normal distribution
  mean <- as.numeric(observedStatistics[index, 2])
  sd <- as.numeric(observedStatistics[index, 3])
  cost <- rnorm(1, mean = mean, sd = sd) * duration
  if (cost < 0) {
    cost <- 0
  }
  return(round(cost, 2))
}
