#-------------------------------------------------------------------------#
#---------------------------- TIDY CREDITS -------------------------------#
#-------------------------------------------------------------------------#

#' Formats \code{credits} into tidy data
#'
#' @inheritParams tidySpeciesData
#' @return a tibble with \code{internalTaxonId}, \code{scientificName},
#' \code{type}, \code{name} and \code{organisation}.
#' @examples
#' tidyCredits(species_data)
#' @section Used in: \code{\link{tidyConcat}}
#'
tidyCredits <- function(species_data){
  species_data[['credits']] <- species_data %>%
    addTaxonId() %>%
    .[['credits']] %>%
    tidyr::separate_rows(c(`internalTaxonId`,`scientificName`,`value`,`type`),
                         sep = '[|]',
                         convert = TRUE
                         ) %>%
    dplyr::select(`internalTaxonId`,`scientificName`,`value`,`type`) %>%
    dplyr::mutate(organisation = `value` %>%
                    strSplitSelect('[(]|[)]',2),
                  value = `value` %>%
                    strSplitSelect('[(]|[)]',1) %>%
                    stringr::str_trim()
                  ) %>%
    dplyr::rename(role = `type`,
                  name = `value`)
  return(species_data)
}
