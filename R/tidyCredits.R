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
                    stringr::str_trim(),
                  name = cleanFullName(value)
                  ) %>%
    dplyr::rename(role = `type`,
                  fullName = `value`)
  species_data[['credits']]$name <- ifelse(species_data[['credits']]$fullName==species_data[['credits']]$organisation &
                                             !is.na(species_data[['credits']]$organisation) |
                                             species_data[['credits']]$role == 'Institutions',
                                           species_data[['credits']]$fullName,
                                           species_data[['credits']]$name)
  return(species_data)
}

#' cleans a vector of full names
#'
#' @param names a vector of names to be cleaned
#' @return a vector of cleaned names
#' @examples
#' tidyCredits(species_data)
#' @section Used in: \code{\link{tidyCredits}}
#'
cleanFullName <- function(names){
  split_names <- stringr::str_split(string = names, pattern = ' ')
  last_names <- c()
  initials <- c()
  for(i in 1:length(split_names)){
    de_or_del <- sapply(split_names[[i]], function(x) grepl(pattern = '[a-z]', x = substr(x,1,1)))
    if(any(de_or_del)){
      position <- grep(pattern = TRUE, x = de_or_del)[1]
      last_names[i] <- paste(split_names[[i]][position:length(de_or_del)], collapse = ' ')
      initials[i] <- firstNameToInitials(split_names[[i]][1:(position-1)])
    } else {
      last_names[i] <- split_names[[i]][length(de_or_del)]
      initials[i] <- firstNameToInitials(split_names[[i]][-length(de_or_del)])
    }
  }
  cleaned_name <- paste(last_names, initials, sep = ', ')
  return(cleaned_name)
}

#' converts a vector of names into initials
#'
#' @param names a vector of names to be cleaned
#' @return a value containing initials with no spaces or punctuation between
#' @section Used in: \code{\link{cleanFullName}}
#'
firstNameToInitials <- function(names){
  initials <- paste(names, collapse = ' ') %>%
    gsub(pattern = '[!-/:-@\\[-`{-~]', replacement = '') %>%
    stringr::str_split(pattern = ' ') %>%
    sapply(function(x) substr(x,1,1)) %>%
    paste(collapse = '')
  return(initials)
}
