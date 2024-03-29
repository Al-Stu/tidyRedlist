% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzyMatch.R
\name{fuzzyMatchPairs}
\alias{fuzzyMatchPairs}
\title{finds best match for \code{x} in \code{y}}
\usage{
fuzzyMatchPairs(x, y, partial)
}
\arguments{
\item{x}{a character vector}

\item{y}{a character vector}

\item{partial}{a logical indicating whether the transformed x elements must exactly
match the complete y elements, or only substrings of these. The latter corresponds
to the approximate string distance used by \code{\link{agrep}} (by default).from
\code{\link{adist}}}
}
\value{
tibble with columns: \code{xPosition}, \cpde{xName},\code{yPosition}, \code{yName}
and \code{adist}
}
\description{
\code{fuzzyMatchPairs} finds the best matching pair for each value in vector x in vector y
}
\section{Used in}{
 \code{\link{SCImagoJournal}}
}

