% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rB3export.R
\name{rB3export}
\alias{rB3export}
\title{Write the dataframes of an rB3 object (list of dataframes) to separate csv files}
\usage{
rB3export(rB3in, startDate, endDate, varNames, src, qc, metadata, filePath)
}
\arguments{
\item{rB3in}{data frame to be standardized}

\item{startDate}{start date}

\item{endDate}{endDate}

\item{src}{write source data file}

\item{qc}{write qc data file}

\item{metadata}{write log and metadata files}

\item{filePath}{folder to which files will be writen, set relative to working dir}
}
\description{
Use args to select which dfs to export
}
\examples{
rB3export(rB3in, varNames = 'All', src = TRUE, qc = TRUE, metadata = TRUE)
}
\keyword{fileIO}
