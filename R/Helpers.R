################################################################################
#
# Some regularly used functions to help code cleanness
#
################################################################################


#' Load and translate SQL file or an explicit SQL query to desired dialect.
#'
#' @param sql SQL file name or SQL query
#' @param warnOnMissingParameters Should a warning be raised when parameters provided to this function do not appear in the parameterized SQL that is being rendered? By default, this is TRUE.
#' @param output Should there be a .sql file created of the result
#' @param outputFile Name of the output file
#' @keywords internal
loadRenderTranslateSql <- function(sql,
                                   dbms = "postgresql",
                                   warnOnMissingParameters = TRUE,
                                   output = FALSE,
                                   outputFile,
                                   ...) {
  if (grepl('.sql', sql)) {
    pathToSql <- paste("inst/SQL/", sql, sep = "")
    parameterizedSql <-
      readChar(pathToSql, file.info(pathToSql)$size)[1]
  } else {
    parameterizedSql <- sql
  }
  
  renderedSql <-
    SqlRender::render(sql = parameterizedSql, warnOnMissingParameters = warnOnMissingParameters, ...)
  renderedSql <-
    SqlRender::translate(sql = renderedSql, targetDialect = dbms)
  
  if (output == TRUE) {
    SqlRender::writeSql(renderedSql, outputFile)
    writeLines(paste("Created file '", outputFile, "'", sep = ""))
  }
  
  return(renderedSql)
}

#' Function for finding NaN values in a data.frame object
#'
#' @param data SQL data.frame object
#' @keywords internal
is.nan.data.frame <- function(data) {
  do.call(cbind, lapply(data, is.nan))
}

#' Function for saving summary tables to path
#'
#' @param object Object to save
#' @param path Path to the file saved
#' @keywords internal
save_object <- function(object, path) {
  if (is.data.frame(object)) {
    utils::write.csv(object, path, row.names = FALSE)
  }
  else if (is.table(object)) {
    utils::write.table(object, file = path, row.names = FALSE)
  }
  else {
    save(object, file = path)
  }
}


#' Function for controlling whether patient exists in a cohort
#'
#' @param data A dataframe object with SUBJECT_ID values
#' @param id The subject ID
#' @keywords internal
idExists <- function(data, id) {
  if (as.character(id) %in% unique(as.character(data$SUBJECT_ID))) {
    return(TRUE)
  }
  return(FALSE)
}

#' Function for calculating overlap in date intervals
#'
#' @param dateStart Start of the boundary date interval
#' @param dateEnd End of the boundary date interval
#' @param controlStart Start of the date interval of interest
#' @param controlEnd End of the date interval of interest
#' @keywords internal
daysOverlap <- function(dateStart,
                        dateEnd,
                        controlStart,
                        controlEnd) {
  if (dateStart > controlStart &&
      dateEnd > controlEnd) {
    return(max(as.numeric(controlEnd - dateStart), 0))
  }
  else if (dateStart <= controlStart &&
           dateEnd > controlEnd) {
    return(max(as.numeric(controlEnd - controlStart) + 1, 0))
  }
  else if (dateStart <= controlStart &&
           dateEnd <= controlEnd) {
    return(max(as.numeric(dateEnd - controlStart) + 1, 0))
  }
  else if (dateStart > controlStart &&
           dateEnd <= controlEnd) {
    return(max(as.numeric(dateEnd - dateStart), 0))
  }
  else {
    return(0)
  }
}

#' Function for asking from user whether to drop the package-generated tables from database
#'
#' @keywords internal
droppingTables <- function() {
    ans <-
      as.character(readline(prompt = "Should we delete the cost_person table in temp schema? (y/n): "))
    cat("\n")
    return(ans)
}


#' Function which creates mandatory subdirectories and files to the pathToResults directory
#'
#' @param pathToResults Path to the package results
#' @keywords internal
createMandatorySubDirs <- function(pathToResults, databaseDescription) {
  dir.create(file.path(pathToResults, "tmp"), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/tmp', sep = ""), 'databases'), showWarnings = FALSE)
  dir.create(file.path(paste(pathToResults, '/tmp/databases', sep = ""), studyName), showWarnings = FALSE)
  #dir.create(file.path(paste(pathToResults, '/tmp', sep = ""), 'models'), showWarnings = FALSE)
  # Write database description file
  fileConn<-file((paste(pathToResults, '/tmp/databases/',studyName,'/description.md', sep = "")))
  writeLines(databaseDescription, fileConn)
  close(fileConn)
  
}
