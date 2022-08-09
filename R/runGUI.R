################################################################################
#
# Functions related to initiating connection with database
#
################################################################################

#' This function initiates the connection with database and starts Shiny application
#'
#' @param pathToDriver Path to a folder containing the JDBC driver JAR files. See downloadJdbcDrivers for instructions on how to download the relevant drivers.
#' @param pathToResults Path to target directory where results will be saved
#' @param dbms The type of DBMS running on the server. Valid values are: 'oracle','postgresql','redshift','sql server','pdw', 'netezza','bigquery','sqlite', 'sqlite extended','spark'
#' @param cdmSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmTmpSchema Schema for temporary tables
#' @param cdmResultsSchema Schema which has the information about the cohorts created in Atlas
#'
#' @export
runGUI <- function(connection,
                   connectionDetails,
                   pathToDriver = './Drivers',
                   dbms = "postgresql",
                   cdmSchema = "ohdsi_cdm",
                   cdmTmpSchema = "ohdsi_temp",
                   cdmResultsSchema = "ohdsi_results",
                   studyName = 'MarkovAnalysis',
                   pathToResults = NULL) {
  ################################################################################
  #
  # Creating global variables
  #
  ################################################################################
  dbms <<- dbms
  connectionDetails <<- connectionDetails
  conn <<- connection
  cdmSchema <<- cdmSchema
  cdmTmpSchema <<- cdmTmpSchema
  cdmResultsSchema <<- cdmResultsSchema
  if (!is.null(pathToResults)) {
    pathToResults <<- pathToResults
  }
  else {
    pathToResults <<- paste(getwd(), "/tmp", sep = "")
  }
  studyName <<- studyName
  
  ###############################################################################
  #
  # Creating mandatory directories if they do not exist
  #
  ###############################################################################
  
  createMandatorySubDirs(pathToResults)
  
  ################################################################################
  #
  # Opening shiny app
  #
  ################################################################################
  
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
  
  # Maximum value for uploading patient trajectory file
  options(shiny.maxRequestSize = 150 * 1024 ^ 2)
  shiny::runApp("./shiny")
  
  ################################################################################
  #
  # Delete temp tables & Disconnect database connection
  #
  ################################################################################
  
  ans <- droppingTables()
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
