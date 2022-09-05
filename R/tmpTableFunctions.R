################################################################################
#
# Functions which will create temporary tables needed for the study
#
################################################################################

#' Function for creating an index non-clustered/clustered, unique/non-unique
#'
#' @param connection Connection to the database (package DatabaseConnector)
#' @param dbms Database dialect
#' @param tableSchema Schema in which the targeted table resides
#' @param relationName Name of the targeted table
#' @param columnNames Columns on which to build the index on
#' @param label Name for the index
#' @param clustered Boolean for clustered index
#' @param tempTable Boolean indicating whether the table in a "TEMP" table, if so, no schema is specified
#' @keywords internal
createIndex <-
  function(connection,
           dbms = "postgresql",
           tableSchema = "",
           relationName,
           columnNames,
           label,
           clustered = FALSE,
           unique = FALSE,
           tempTable = FALSE) {
    #ParallelLogger::logInfo("Creating indeces")
    if (clustered) {
      ParallelLogger::logInfo(
        paste(
          "Start execution of: CREATE CLUSTERED INDEX ",
          label,
          " ON ",
          tableSchema ,
          "." ,
          relationName,
          "(",
          columnNames,
          ")",
          sep = ""
        )
      )
      colsString <-
        toString(sprintf("%s", columnNames))
      if (tempTable) {
        sql <- paste("CREATE CLUSTERED INDEX ",
                     label ,
                     " ON ",
                     relationName,
                     " (%s)",
                     sep = "")
        DatabaseConnector::executeSql(connection,
                                      SqlRender::translate(
                                        targetDialect = dbms,
                                        sql = sprintf(sql, colsString)
                                      ))
      }
      else {
        sql <- paste(
          "CREATE CLUSTERED INDEX ",
          label ,
          " ON ",
          tableSchema ,
          "." ,
          relationName,
          " (%s)",
          sep = ""
        )
        DatabaseConnector::executeSql(connection,
                                      SqlRender::translate(
                                        targetDialect = dbms,
                                        sql = sprintf(sql, colsString)
                                      ))
      }
      ParallelLogger::logInfo(
        paste(
          "CREATE CLUSTERED INDEX ",
          label,
          " ON ",
          tableSchema ,
          "." ,
          relationName,
          "(",
          columnNames,
          ") EXECUTED!",
          sep = ""
        )
      )
    }
    else {
      ParallelLogger::logInfo(
        paste(
          "Start execution of: CREATE INDEX ",
          label,
          " ON ",
          tableSchema ,
          "." ,
          relationName,
          "(",
          columnNames,
          ")",
          sep = ""
        )
      )
      if (tempTable) {
        colsString <-
          toString(sprintf("%s", columnNames))
        sql = paste("CREATE INDEX ",
                    label ,
                    " ON ",
                    relationName,
                    " (%s)",
                    sep = "")
        DatabaseConnector::executeSql(connection,
                                      SqlRender::translate(
                                        targetDialect = dbms,
                                        sql = sprintf(sql, colsString)
                                      ))
      }
      else {
        colsString <-
          toString(sprintf("%s", columnNames))
        sql = paste("CREATE INDEX ",
                    label ,
                    " ON ",
                    tableSchema ,
                    "." ,
                    relationName,
                    " (%s)",
                    sep = "")
        DatabaseConnector::executeSql(connection,
                                      SqlRender::translate(
                                        targetDialect = dbms,
                                        sql = sprintf(sql, colsString)
                                      ))
      }
      ParallelLogger::logInfo(
        paste(
          "CREATE INDEX ",
          label,
          " ON ",
          tableSchema ,
          "." ,
          relationName,
          "(",
          columnNames,
          ") EXECUTED!",
          sep = ""
        )
      )
    }
  }


#' Function for deleting temporary tables from user's db
#'
#' @param connection Connection to the database (package DatabaseConnector)
#' @param dbms Database dialect
#' @param schema Schema in which the targeted table resides
#' @param relationName Name of the targeted table which will be dropped
#' @keywords internal
dropRelation <-
  function(connection,
           dbms = "postgresql",
           schema = "",
           relationName) {
    ParallelLogger::logInfo(paste(
      "Start execution of: DROP TABLE IF EXISTS ",
      ifelse(
        schema == "",
        relationName,
        paste(schema,
              ".",
              relationName, sep = "")
      ),
      " !",
      sep = ""
    ))
    if (schema == "") {
      DatabaseConnector::executeSql(connection,
                                    SqlRender::translate(
                                      targetDialect = dbms,
                                      sql = SqlRender::render(sql = "DROP TABLE IF EXISTS @relationName CASCADE",
                                                              relationName = relationName)
                                    ))
    }
    else {
      DatabaseConnector::executeSql(connection,
                                    SqlRender::translate(
                                      targetDialect = dbms,
                                      sql = SqlRender::render(
                                        sql = "DROP TABLE IF EXISTS @cdmTmpSchema.@relationName CASCADE",
                                        cdmTmpSchema = schema,
                                        relationName = relationName
                                      )
                                    ))
    }
    
    ParallelLogger::logInfo(paste("DROP TABLE ",
                                  schema,
                                  ".",
                                  relationName,
                                  " EXECUTED!",
                                  sep = ""))
  }





#' Create temp tables for package
#'
#' This function creates all temp tables for running TrajectoryMarkovAnalysis package
#'
#' @param connection Connection to database
#' @param dbms Database management system: postgresql, mysql etc.
# @param cdmSchema Name of the OMOP cdm schema
#' @param cdmTmpSchema tmpSchema
#' @keywords internal
#' @return Boolean of success
generateTempTables <- function(connection,
                               dbms,
                               cdmSchema = "ohdsi_cdm",
                               cdmTmpSchema = "ohdsi_temp") {
  ParallelLogger::logInfo("Creating temp tables, this can take a while...")
  ##############################################################################
  #
  # Cost table with person_id's and approximate payment dates.
  # First of all, drop table if exists
  #
  ##############################################################################
  
  # Updated because some errors were occurring not checked
  dropRelation(
    connection = connection ,
    dbms = dbms,
    schema = cdmTmpSchema,
    relationName = "cost_person"
  )
  
  sql1 <- SqlRender::translate(
    targetDialect = dbms,
    sql = SqlRender::render(
      sql = "SELECT cost_id, cost.cost_event_id as cost_event_id, cost_domain_id, cost_type_concept_id, currency_concept_id, total_charge, total_cost, total_paid, pobs.person_id as person_id, pobs.date as date INTO @cdmTmpSchema.cost_person FROM @cdmSchema.cost LEFT JOIN (SELECT drug_exposure_id as cost_event_id, drug_exposure.person_id as person_id, drug_exposure_start_date as date FROM @cdmSchema.drug_exposure UNION SELECT visit_occurrence_id as cost_event_id, visit_occurrence.person_id as person_id, visit_start_date as date FROM @cdmSchema.visit_occurrence UNION SELECT procedure_occurrence_id as cost_event_id, procedure_occurrence.person_id as person_id, procedure_date as date FROM @cdmSchema.procedure_occurrence UNION SELECT device_exposure_id as cost_event_id, device_exposure.person_id as person_id, device_exposure_start_date as date FROM @cdmSchema.device_exposure UNION SELECT measurement_id as cost_event_id, measurement.person_id as person_id, measurement_date as date FROM @cdmSchema.measurement UNION SELECT observation_id as cost_event_id, observation.person_id as person_id, observation_date as date FROM @cdmSchema.observation UNION SELECT specimen_id as cost_event_id, specimen.person_id as person_id, specimen_date as date FROM @cdmSchema.specimen) pobs on cost.cost_event_id = pobs.cost_event_id;",
      cdmTmpSchema = cdmTmpSchema,
      cdmSchema = cdmSchema
    )
    
  )
  DatabaseConnector::executeSql(connection,
                                sql1)
  
  ParallelLogger::logInfo("Creation of table cost_person EXECUTED!")
  
  ##############################################################################
  #
  # Create index on columns person_id and date
  #
  ##############################################################################
  createIndex(
    connection = connection,
    dbms = dbms,
    tableSchema = cdmTmpSchema,
    relationName = "cost_person",
    columnNames = c("person_id", "date"),
    label = "ix_person_date_cost_person",
    clustered = FALSE,
    unique = FALSE,
    tempTable = FALSE
  )
  
  
}
