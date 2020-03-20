#-------------------------------------------------------------------------#
#------------------------- TIDY REDLIST CRITERIA -------------------------#
#-------------------------------------------------------------------------#

#' Formats \code{redlistCriteria} into tidy data
#'
#' @param redCriteria a tibble with \code{redlistCriteria}, \code{internalTaxonId}
#' and \code{scientificName}.
#' @return Tibble with each species-criterion as a row and columns
#' \code{internalTaxonId}; \code{scientificName}; and \code{redlistCriteria}
#' split into three levels.
#' @examples
#' tidyCriteria(species_data[['redlistCriteria']])
#' @section Used in: \code{\link{tidyConcat}}
tidyCriteria <- function(redCriteria){
  check_col_titles <- any(grepl('redlistCriteria',colnames(redCriteria))) &
    any(grepl('internalTaxonId',colnames(redCriteria))) &
    any(grepl('scientificName',colnames(redCriteria)))
  if(!check_col_titles){
    stop('check column names and capitalisation: these should be redlistCriteria, \n
         internalTaxonId and scientificName as in raw Redlist data')
  }
  redCriteria$internalTaxonId <- as.character(redCriteria$internalTaxonId)
  to_columns <- criteriaToColumns(redCriteria$redlistCriteria,'[+]','^.','[0-9].*$')
  to_columns_pivot <-  pivotToColumns(redCriteria,to_columns)
  l2_to_columns <- criteriaToColumnsl2(input = to_columns_pivot$criterion,pattern = '')
  l2_to_columns_pivot <-  pivotToColumns(to_columns_pivot,l2_to_columns)
  l3_to_columns <- splitToColumns(l2_to_columns_pivot$criterion,pattern = '') %>%
    dplyr::bind_cols(l2_to_columns_pivot[,1]) %>%
    dplyr::right_join(redCriteria[,c(1,2)]) %>%
    dplyr::select(`internalTaxonId`,`scientificName`, everything()) %>%
    dplyr::rename(level1=`V1`,
                  level2=`V2`,
                  level3=`V3`) %>%
    dplyr::filter(!is.na(`level1`))
  return(l3_to_columns)
}

#' Formats a string of criteria of the same level 1 to one criterion per row
#'
#' @param input a column of Redlist criteria strings seperated by pattern parameter.
#' @param pattern the character or string used to seperate Redlist criteria
#' within input, in regex format.
#' @param firstHalf the string common to all criteria in a string but not
#' explicitly stated after the first, in A2cd this is A or '^[A-Z]'
#' @param secondHalf the string unique to that criteria in a string, in A2cd
#' this is 2cd or '[0-9].$'
#' @return tibble with ncol=max number of elements in string, each col named V1,V2 etc.
#' @examples
#' criteriaToColumns(species_data[['redlistCriteria']]$redlistCriteria,'[+]','^.','[0-9].*$')
#' @section Used in: \code{\link{tidyCriteria}}
#'
criteriaToColumns <- function(input,pattern,firstHalf,secondHalf){
  to_columns <- input %>%
    splitToColumns(pattern = pattern) %>%
    dplyr::mutate(letter = stringr::str_extract(V1,firstHalf),
                  V1 = stringr::str_extract(V1,secondHalf)) %>%
    pasteLastToAll()
  return(to_columns)
}

#' Formats a Redlist criterion into its sub-categories, dependency of tidyCriteria
#'
#' @param input a column of seperated Redlist criteria e.g. A2cd
#' @return tibble with ncol=max number of subcategories, each col named V1,V2 etc.
#' @examples
#' criteriaToColumnsL2(resultFromPivotToColumns$redlistCriteria)
#' @section Used in: \code{\link{tidyCriteria}}
#'
criteriaToColumnsL2 <- function(input){
  to_columns <- input %>%
    splitToColumns(pattern = '') %>%
    tidyr::unite(col='letter',`V1`,`V2`,sep='') %>%
    pasteFirstToAll()
  return(to_columns[,-1])
}

#' Pivots output of criteriaToColumns functions and associates criteria with their
#' \code{scientificName} and \code{internalTaxonId}
#'
#' @param redCriteria a tibble with \code{redlistCriteria}, \code{internalTaxonId}
#' and \code{scientificName}, same length as splitCriteria
#' @param splitCriteria a tibble output by running a \code{criteriaToColumns()}
#' or \code{criteriaToColumnsL2()} on \code{redCriteria$redlistCriteria}
#' @return tibble with columns \code{internalTaxonId}, \code{scientificName}
#' and \code{redlistCriteria} one row per non-NA criterion in splitCriteria
#' @examples
#' criteriaToColumnsL2(resultFromPivotToColumns$redlistCriteria)
#' @section Used in: \code{\link{tidyCriteria}}
#'
pivotToColumns <- function(redCriteria,splitCriteria){
  dplyr::bind_cols(splitCriteria[,-ncol(splitCriteria)], redCriteria[,1]) %>%
    dplyr::mutate(internalTaxonId = as.character(internalTaxonId)) %>%
    tidyr::pivot_longer(-`internalTaxonId`, values_to = 'criterion') %>%
    dplyr::select(c(`internalTaxonId`, `criterion`)) %>%
    dplyr::filter(!is.na(`criterion`))
}

#' Pastes the value in the last column of each row in front of each value in that row
#'
#' @param df a tibble with value to be pasted in the last column
#' @return a tibble with the value in the last column of each row in front of
#' each value in that row and last columns removed
#' @section Used in: \code{\link{criteriaToColumns}}
#'
pasteLastToAll <- function(df){
  for(i in 1:(ncol(df)-1)){
    for(j in 1:nrow(df)){
      if(!is.na(df[j,i])){
        df[j,i] <- paste(df[j,ncol(df)],df[j,i],sep='',collapse='')
      }
    }
  }
  return(df)
}

#' Pastes the value in the first column of each row in front of each value in that row
#'
#' @param df a tibble with value to be pasted in the first column
#' @return a tibble with the value in the last column of each row in front of
#' each value in that row and first column removed
#' @section Used in: \code{\link{criteriaToColumnsL2}}
#'
pasteFirstToAll <- function(df){
  for(i in 2:(ncol(df))){
    for(j in 1:nrow(df)){
      if(!is.na(df[j,i])){
        df[j,i] <- paste(df[j,1],df[j,i],sep='',collapse='')
      }
    }
  }
  return(df)
}
