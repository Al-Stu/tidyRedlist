#-------------------------------------------------------------------------#
#----------------------------- TIDY NAMES --------------------------------#
#-------------------------------------------------------------------------#

#' Formats \code{common_names} and \code{synonyms} into a tidy tibble
#'
#' @param species_data a list of data downloaded from the IUCN Redlist and
#' imported using \code{\link{importList}}
#' @return Tibble with each name as a row and columns \code{internalTaxonId},
#' \code{name},\code{language},\code{source}
#' @examples
#' tidyNames(species_data)
#' @section Used in: \code{\link{tidySpeciesData}}
tidyNames <- function(species_data){
  species_data <- addTaxonId(species_data)
  common_names <- dplyr::select(species_data[['common_names']], `internalTaxonId`,
                                `scientificName`, `name`, `language`,`main`)
  species_data[['names']] <- species_data[['synonyms']] %>%
    tidySynonyms() %>%
    dplyr::mutate(language = 'scientific',
                  main = FALSE) %>%
    dplyr::bind_rows(common_names) %>%
    dplyr::mutate(source = 'RL')
  species_data[['names']] <- species_data[['assessments']] %>%
    dplyr::distinct(`scientificName`,.keep_all = TRUE) %>%
    dplyr::transmute(internalTaxonId = `internalTaxonId`,
                     scientificName = `scientificName`,
                     name = `scientificName`,
                     language = 'scientific',
                     main = TRUE,
                     source = 'RL') %>%
    dplyr::bind_rows(species_data[['names']])
  return(species_data)
}

tidySynonyms <- function(synonyms){
  cleanedSynonym <-  synonyms$name %>%
    strSplitSelect(',',1) %>%
    strSplitSelect('[A-Z][a-z]*$',1) %>%
    strSplitSelect('[(].*$',1) %>%
    stringr::str_trim() %>%
    gsub(pattern = ' spp[.]| ssp[.]',replacement = '')
  result <- dplyr::transmute(synonyms,
                     scientificName = `scientificName`,
                     name = cleanedSynonym)
  return(result)
}

#' Selects element from \code{\link{stringr::str_split}} list
#'
#' @inheritParams stringr::str_split
#' @param n the element you wish to keep, e.g. 1 to keep first element
#' @return vector with \code{n}th position of each split string
#' @examples
#' strSplitSelect(species_data[['synonyms']])
#' @section Used in: \code{\link{tidySynonyms}}
strSplitSelect <- function(x,pattern,n){
  result <- stringr::str_split(x,pattern = pattern) %>%
    sapply(function(x) x[n])
  return(result)
}
