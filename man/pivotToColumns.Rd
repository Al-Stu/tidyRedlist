% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyCriteria.R
\name{pivotToColumns}
\alias{pivotToColumns}
\title{Pivots output of criteriaToColumns functions and associates criteria with their
\code{scientificName} and \code{internalTaxonId}}
\usage{
pivotToColumns(splitCriteria, redCriteria)
}
\arguments{
\item{splitCriteria}{a tibble output by running a \code{criteriaToColumns()}
or \code{criteriaToColumnsL2()} on \code{redCriteria$redlistCriteria}}

\item{redCriteria}{a tibble with \code{redlistCriteria}, \code{internalTaxonId}
and \code{scientificName}, same length as splitCriteria}
}
\value{
tibble with columns \code{internalTaxonId}, \code{scientificName}
and \code{redlistCriteria} one row per non-NA criterion in splitCriteria
}
\description{
Pivots output of criteriaToColumns functions and associates criteria with their
\code{scientificName} and \code{internalTaxonId}
}
\section{Used in}{
 \code{\link{tidyCriteria}}
}

\examples{
criteriaToColumnsL2(resultFromPivotToColumns$redlistCriteria)
}
