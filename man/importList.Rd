% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listManagement.R
\name{importList}
\alias{importList}
\title{Export list of dataframes to any format, requires \link[rio] for non-csv filetypes}
\usage{
importList(directory, prefix, fileType)
}
\arguments{
\item{directory}{the directory the list is to be exported to}

\item{prefix}{the prefix included in all names of files to be imported,
'' if all files in directory}

\item{fileType}{the filetype of files to be imported, '' if not all one type}
}
\description{
Export list of dataframes to any format, requires \link[rio] for non-csv filetypes
}
\examples{
importList('/exported_files','species_data','csv')
}
