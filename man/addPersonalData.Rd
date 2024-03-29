% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectoryFunctions.R
\name{addPersonalData}
\alias{addPersonalData}
\title{This function adds personal data to subjects: age, gender}
\usage{
addPersonalData(cohortData, connection)
}
\arguments{
\item{cohortData}{A data.frame object which is output of Cohort2Trajectory package}

\item{connection}{Connection to database}
}
\value{
A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
}
\description{
This function adds personal data to subjects: age, gender
}
\keyword{internal}
