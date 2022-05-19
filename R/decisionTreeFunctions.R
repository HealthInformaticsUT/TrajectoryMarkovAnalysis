################################################################################
#
# This script contains functions for building a trajectory tree model
#
################################################################################

#' Get a dataframe of patients' trajectories
#'
#' This function outputs a data.frame object which describes the movement of patients between the defined states and state duration
#'
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param stateCohorts The IDs of state cohorts
#' @param pathToResults Path to temp folder for saving objects
#' @keywords internal
getDTreeProbabilities = function(cohortData,
                                 stateCohorts,
                                 pathToResults = getwd()
) {
  
  # all new here
  
  # TODO create dataframe from to count
  
  ## For the purpose of overview let's leave out the state Other as we will have this state implemented later in another style
  #stateTransitionTable = dplyr::filter(cohortData, STATE != "0")
  stateTransitionTable = cbind(c(NA,cohortData$SUBJECT_ID), c(NA,cohortData$STATE),c(cohortData$SUBJECT_ID,NA),c(cohortData$STATE, NA))
  colnames(stateTransitionTable) = c("SUBJECT_ID1", "STATE1","SUBJECT_ID2", "STATE2")
  TransitionTable = dplyr::select(dplyr::filter(as.data.frame(stateTransitionTable), SUBJECT_ID1 == SUBJECT_ID2),STATE1, STATE2)
  TransitionTable1 = dplyr::count(x = TransitionTable, STATE1, STATE2)
  TransitionTable2 = dplyr::count(x = TransitionTable, STATE1, )
  TransitionTable = merge(x = TransitionTable1, y = TransitionTable2, by = "STATE1")
  TransitionTable = dplyr::select(dplyr::mutate(TransitionTable, PROB = n.x/n.y), -n.x,-n.y)
  #TransitionTable$PROB = round(TransitionTable$PROB,4)
  colnames(TransitionTable) = c("FROM", "TO", "PROB")
  # Starting probs (1st layer)
  
  
  startingPointTable = dplyr::select(dplyr::filter(as.data.frame(stateTransitionTable), SUBJECT_ID1 != SUBJECT_ID2),STATE1, STATE2)
  startingPointTable$STATE1 = NA
  startingPointTable1 = dplyr::count(x = startingPointTable, STATE1, STATE2)
  startingPointTable2 = dplyr::count(x = startingPointTable, STATE1, )
  startingPointTable = merge(x = startingPointTable1, y = startingPointTable2, by = "STATE1")
  startingPointTable = dplyr::select(dplyr::mutate(startingPointTable, PROB = n.x/n.y), -n.x,-n.y)
  startingPointTable$PROB = round(startingPointTable$PROB,4)
  colnames(startingPointTable) = c("FROM", "TO", "PROB")
  
  stateTransitionTable = rbind(TransitionTable,startingPointTable)
  
  # Add customised labels as patients' state names
  stateTransitionTable$FROM_LABEL =stateTransitionTable$FROM
    
  
  stateTransitionTable$TO_LABEL = stateTransitionTable$TO
    
  return(stateTransitionTable)
  
}




#' Merge
#'
#' This function outputs a data.frame object which describes the movement of patients between the defined states and the probabilities
#' @param cohortData A data.frame object which is output of Cohort2Trajectory package
#' @param dTreeEdged A data.frame object which describes edge formulation
#' @param inputLayersDT A list of layers, which are included for decision tree in GUI
#' @param stateTransitionTable A data.frame object with data of the relevant probabilities
#' @param probabilityType Type of probability calculated
#' @keywords internal
mergeEdgeTransition = function(cohortData,
                               dTreeEdges,
                               stateTransitionTable,
                               inputLayersDT = NULL,
                               probabilityType = 1
) {
  cohortData_copy = cohortData
  dTreeEdges = dplyr::left_join(dTreeEdges, stateTransitionTable, by = c("from_label" = "FROM_LABEL", "to_label" = "TO_LABEL"))
  # If PROB = NA then let's change it to 0 && Let's present probabilities as percentage
  dTreeEdges$PROB[is.na(dTreeEdges$PROB)] = 0
  if (probabilityType == 1) {}
  else if (probabilityType == 2){
    summarydTreeEdges = dplyr::group_by(.data = dTreeEdges,from)
    summarydTreeEdges = dplyr::summarise(.data = summarydTreeEdges, PROB = PROB/sum(PROB))
    dTreeEdges$PROB = summarydTreeEdges$PROB
  }
  else if (probabilityType == 3 | probabilityType == 4){
    if(probabilityType == 4){
      # Lets remove all repetitions from cohortData_copy by analyzing STATE vector
      cohortData_copy = dplyr::filter(cohortData_copy, STATE!= dplyr::lag(STATE, default="1"))
    }
    # For starters let's state all the transfers we will need, ex 0-1, 0-2, 0-1-2 ...
    trajectories = data.frame(matrix(0,ncol = 3,
                                     nrow = nrow(dTreeEdges)+1))
    colnames(trajectories) = c("TRAJECTORIE", "COUNT", "PROB")
    
    g <- igraph::graph_from_data_frame(data.frame(dTreeEdges$from, dTreeEdges$to))
    
    l <- lapply(igraph::V(g), function(x) igraph::all_shortest_paths(g, from = dTreeEdges$from[1]))
    l <- lapply(l, function(x) x[[-2]])
    l <- unlist(l, recursive = F, use.names = F)
    paths <- lapply(1:(nrow(dTreeEdges)+1), function(x) igraph::as_ids(l[[x]]))
    paths <- lapply(1:(nrow(dTreeEdges)+1), function(n) plyr::mapvalues(
      x = paths[[n]],
      from = c(dTreeEdges$from,dTreeEdges$to),
      to = c(dTreeEdges$from_label,dTreeEdges$to_label),
      warn_missing = FALSE))
    
    paths = unlist(lapply(paths, paste, collapse = "%%,") )
    #paths
    trajectories$TRAJECTORIE = paths
    ## stringr::str_split(paths, pattern = "%,.,%")
    i = 0
    for (patientId in unique(cohortData_copy$SUBJECT_ID)) {
      i = i+1
      personData = dplyr::filter(cohortData_copy,  SUBJECT_ID == patientId)
      fullTrajectorie = paste(c(dTreeEdges$from_label[1],personData$STATE), collapse = "%%,")
      #Surpress warnings
      defaultW <- getOption("warn")
      options(warn = -1)
      #This might be very slow
      trajectories$COUNT[stringr::str_detect(fullTrajectorie, trajectories$TRAJECTORIE)] = trajectories$COUNT[stringr::str_detect(fullTrajectorie, trajectories$TRAJECTORIE)] + 1
      options(warn = defaultW)
    }
    trajectories$PROB = trajectories$COUNT/length(unique(cohortData_copy$SUBJECT_ID))
    dTreeEdges$PROB = trajectories$PROB[2:nrow(trajectories)]
    
  }
  dTreeEdges$PROB = round(dTreeEdges$PROB*100,2)
  return(dTreeEdges)
}

