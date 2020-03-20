#-------------------------------------------------------------------------#
#--------------------------- TIDY SPECIES DATA ---------------------------#
#-------------------------------------------------------------------------#

#' Formats Redlist downloads into tidy data
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search and
#' imported using \code{\link{importList}}
#' @return a list with species
#' @examples
#' tidyCriteria(species_data[['redlistCriteria']])

tidySpeciesData <- function(species_data){
  species_data <- species_data %>%
    addTaxonId() %>%
    tidyAssessments() %>%
    tidyConcat %>%
    tidyNames()
  return(species_data)
}

#' Adds \code{internalTaxonId} OR \code{scientificName} column
#'
#' \code{addTaxonId} adds \code{internalTaxonId} OR \code{scientificName} column
#' to any dataframe in species_data missing one of these columns by using
#' dplyr::right_join on each data frame in list and \code{internalTaxonId} and
#' \code{scientificName}) selected from either the 'species_assessments' or
#' 'assessments' element
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search and
#' imported using \code{\link{importList}}
#' @return a list with species
#' \code{internalTaxonId}; \code{scientificName}; and \code{redlistCriteria}
#' split into three levels.
#' @examples
#' tidyCriteria(species_data[['redlistCriteria']])
addTaxonId <- function(species_data){
  if(any(grepl('species_assessments',names(species_data)))){
    result <- sapply(species_data, function(x) dplyr::right_join(x,
        dplyr::select(species_data[['species_assessments']],`internalTaxonId`,`scientificName`)))
  } else if(any(grepl('assessments',names(species_data)))){
    result <- sapply(species_data, function(x) dplyr::right_join(x,
        dplyr::select(species_data[['assessments']],`internalTaxonId`,`scientificName`)))
  } else {
    stop('Could not find source for internalTaxonId, check your list includes either "species data" or "assessments"')
  }
  result <- sapply(result, function(x) dplyr::select(x, `internalTaxonId`,`scientificName`,dplyr::everything()))
  return(result)
}

#' Joins all elements in a list
#'
#' \code{joinAll} uses dplyr::left_join() to merge all the elements in a list
#' of tibbles
#'
#' @param list the list to be joined, rows will be lost is all elements do not
#' have the same number of rows
#' @return a dataframe with nrow = nrow(list[[1]]) and all columns from all
#' elements of \code{list}
joinAll <- function(list){
  result <- list[[1]]
  for(i in 2:length(list)){
    result <- dplyr::left_join(result,list[[i]])
  }
  return(result)
}

#' Combines all line-per-species tibbles
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with all line-per-species tibbles combined into
#' \code{assessments}
#' @examples
#' tidyAssessments(species_data)
#' #' @section Used in: \code{\link{tidySpeciesData}}
tidyAssessments <- function(species_data){
  collate_list <- species_data[as.vector(sapply(species_data,nrow))==
                                 nrow(species_data[['assessments']])]
  species_data <- species_data[!as.vector(sapply(species_data,nrow))==
                                 nrow(species_data[['assessments']])]
  species_data[['assessments']] <- joinAll(collate_list)
  return(species_data)
}

#' Splits all columns with concatenated observations into tidy tibbles
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with tidy tibbles for systems, realms and
#' redlistCriteria and removes these columns from assessments
#' @examples
#' tidyConcat(species_data)
#' @section Used in: \code{\link{tidySpeciesData}}
tidyConcat <- function(species_data){
  internalTaxonId <- species_data[['assessments']]$internalTaxonId
  name <- c('systems','realm','redlistCriteria')
  columns <- dplyr::select(species_data[['assessments']],all_of(name))
  species_data[['assessments']] <- dplyr::select(species_data[['assessments']],-all_of(name))
  species_data <- c(species_data,
                    splitToTidy(columns = columns,IDs = internalTaxonId,pattern = c('[|]','[|]',';'),name = name))
  species_data[['redlistCriteria']] <- tidyCriteria(species_data[['redlistCriteria']])
}


