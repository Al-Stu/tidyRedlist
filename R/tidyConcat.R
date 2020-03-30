#-------------------------------------------------------------------------#
#--------------------------- TIDY CONCATENATED ---------------------------#
#-------------------------------------------------------------------------#

#' Splits all columns with concatenated observations into tidy tibbles
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with tidy tibbles for systems, realms and
#' redlistCriteria and removes these columns from assessments
#' @examples
#' tidyConcat(species_data)
#' @section Used in: \code{\link{tidySpeciesData}}
#' @export
tidyConcat <- function(species_data){
  internalTaxonId <- species_data[['assessments']]$internalTaxonId
  name <- c('systems','realm','redlistCriteria')
  columns <- dplyr::select(species_data[['assessments']],all_of(name))
  species_data[['assessments']] <- dplyr::select(species_data[['assessments']],-all_of(name))
  species_data <- species_data %>%
    c(splitToTidy(columns = columns,
                  IDs = internalTaxonId,
                  pattern = c('[|]','[|]',';'),
                  name = name)) %>%
    addTaxonId() %>%
    tidyCredits() %>%
    tidyCriteria() %>%
    tidyCodes() %>%
    tidyThreats()
  return(species_data)
}

#' Splits \code{stressCode} and \code{stressName} into columns
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with \code{species_data[['threats']]} split into
#' tidy data
#' @examples
#' tidyThreats(species_data)
#' @section Used in: \code{\link{tidyConcat}}
#' @export
tidyThreats <- function(species_data){
  threats <- species_data %>%
    .[['threats']] %>%
    tidyr::separate_rows(`stressCode`,`stressName`,sep = '[|]')
  splitStressCode <- threats %>%
    dplyr::select(`stressCode`) %>%
    splitToColumns(pattern = '[.]')
  names(splitStressCode) <- paste('stressCode_l',c(1:ncol(splitStressCode)),sep='')
  species_data[['threats']] <- dplyr::bind_cols(threats,splitStressCode) %>%
    dplyr::select(`internalTaxonId`:`stressCode`,
                  colnames(splitStressCode),
                  dplyr::everything())
  return(species_data)
}

#' Splits all numeric multi-level codes into columns
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with columns added splitting codes into one level
#' per column
#' @examples
#' tidyCodes(species_data)
#' @section Used in: \code{\link{tidyConcat}}
#' @export
tidyCodes <- function(species_data){
  inc_code <- species_data %>%
    sapply(colnames) %>%
    grepl(pattern = 'code')
  collate_species_data <- species_data[inc_code]
  code_numeric <- collate_species_data %>%
    sapply(function(x) dplyr::select(x,`code`)) %>%
    grepl(pattern = '[0-9]')
  collate_species_data <- collate_species_data[code_numeric]
  split_code <- list()
  species_data_replacement <- list()
  for(i in 1:length(collate_species_data)){
    split_code[[i]] <- collate_species_data[[i]] %>%
      dplyr::select(`code`) %>%
      splitToColumns(pattern = '[.]')
    if(ncol(split_code[[i]])>1){
      names(split_code[[i]]) <- paste('code_l',c(1:ncol(split_code[[i]])),sep='')
      species_data_replacement[[names(collate_species_data)[i]]] <-
        dplyr::bind_cols(collate_species_data[[i]],split_code[[i]]) %>%
        dplyr::select(`internalTaxonId`:`code`,
                      colnames(split_code[[i]]),
                      dplyr::everything()) %>%
        .[,!grepl(pattern = 'code_l[0-9][0-9]',x = colnames(.))]
    }
  }
  result <- species_data[!grepl(x = names(species_data),
                                pattern = paste(names(species_data_replacement),
                                                sep='|',
                                                collapse='|'))] %>%
    c(species_data_replacement)
  return(result)
}

