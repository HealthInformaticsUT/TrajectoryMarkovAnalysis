% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectoryFunctions.R
\name{getFirstStateStatistics}
\alias{getFirstStateStatistics}
\title{Get patients' first state statistics}
\usage{
getFirstStateStatistics(
  connection,
  dbms,
  cohortData,
  cdmTmpSchema,
  studyName = "THE-STUDY"
)
}
\arguments{
\item{connection}{Connection to the database}

\item{dbms}{The Database dialect}

\item{cohortData}{Data retrieved from the getFirstState function}

\item{cdmTmpSchema}{Temp tables' schema}
}
\value{
A data.frame with each state's statistics
}
\description{
Get patients' first state statistics
}
\keyword{internal}
