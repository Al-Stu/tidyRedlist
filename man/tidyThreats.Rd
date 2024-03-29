% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyConcat.R
\name{tidyThreats}
\alias{tidyThreats}
\title{Splits \code{stressCode} and \code{stressName} into columns}
\usage{
tidyThreats(species_data)
}
\arguments{
\item{species_data}{a tibble of data downloaded from an IUCN Redlist search
and imported using \code{\link{importList}}}
}
\value{
\code{species_data} with \code{species_data[['threats']]} split into
tidy data
}
\description{
Splits \code{stressCode} and \code{stressName} into columns
}
\section{Used in}{
 \code{\link{tidyConcat}}
}

\examples{
tidyThreats(species_data)
}
