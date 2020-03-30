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
#' @export
tidyNames <- function(species_data){
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

#' Formats \code{synonyms} into a tidy tibble
#'
#' @param species_data a list of data downloaded from the IUCN Redlist and
#' imported using \code{\link{importList}}
#' @return Tibble with each alternate scientific name as a row and columns
#' \code{scientificName} and \code{name}
#' @examples
#' tidySynonyms(species_data)
#' @section Used in: \code{\link{tidyNames}}
#'
#' @export
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
