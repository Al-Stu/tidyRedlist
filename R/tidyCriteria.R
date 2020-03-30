#-------------------------------------------------------------------------#
#------------------------- TIDY REDLIST CRITERIA -------------------------#
#-------------------------------------------------------------------------#

#' Formats \code{redlistCriteria} into tidy data
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return Tibble with each species-criterion as a row and columns
#' \code{internalTaxonId}; \code{scientificName}; and \code{redlistCriteria}
#' split into three levels.
#' @examples
#' tidyCriteria(species_data[['redlistCriteria']])
#' @section Used in: \code{\link{tidyConcat}}
#' @export
tidyCriteria <- function(species_data){
  redCriteria <- species_data$redlistCriteria
  check_col_titles <- any(grepl('redlistCriteria',colnames(redCriteria))) &
    any(grepl('internalTaxonId',colnames(redCriteria))) &
    any(grepl('scientificName',colnames(redCriteria)))
  if(!check_col_titles){
    stop('check column names and capitalisation: these should be redlistCriteria, \n
         internalTaxonId and scientificName as in raw Redlist data')
  }
  redCriteria$internalTaxonId <- as.character(redCriteria$internalTaxonId)
  l1_to_column <- criteriaToColumns(redCriteria$redlistCriteria,'[+]','^.','[0-9].*$') %>%
    pivotToColumns(redCriteria = redCriteria)
  l2_to_column <- l1_to_column %>%
    dplyr::select(`redlistCriteria`) %>%
    criteriaToColumnsL2() %>%
    pivotToColumns(redCriteria = l1_to_column)
  l3_to_columns <- splitToColumns(l2_to_column$redlistCriteria,pattern = '') %>%
    dplyr::bind_cols(l2_to_column[,-3]) %>%
    dplyr::select(`internalTaxonId`,`scientificName`, everything()) %>%
    dplyr::rename(level1 = `V1`,
                  level2 = `V2`,
                  level3 = `V3`,
                  level4 = `level`)
  species_data[['redlistCriteria']] <- l3_to_columns
  return(species_data)
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
#' @export
criteriaToColumns <- function(input,pattern,firstHalf,secondHalf){
  to_columns <- input %>%
    splitToColumns(pattern = pattern) %>%
    dplyr::mutate(letter = stringr::str_extract(V1,firstHalf),
                  V1 = stringr::str_extract(V1,secondHalf)) %>%
    pasteLastToAll() %>%
    dplyr::select(-letter)
  return(to_columns)
}

#' Formats a Redlist criterion into its sub-categories, dependency of tidyCriteria
#'
#' @param input a column of seperated Redlist criteria e.g. A2cd
#' @return tibble with ncol=max number of subcategories, each col named V1,V2 etc.
#' @examples
#' criteriaToColumnsL2(resultFromPivotToColumns$redlistCriteria)
#' @section Used in: \code{\link{tidyCriteria}}
#' @export
criteriaToColumnsL2 <- function(input){
  split_bracket <- input %>%
    splitToColumns(pattern = '[(]') %>%
    dplyr::mutate(V2 = `V2` %>%
                    strSplitSelect('[)]',1)
                  )
  to_columns <- split_bracket %>%
    dplyr::select(`V1`) %>%
    splitToColumns('') %>%
    tidyr::unite(col='letter',`V1`,`V2`,sep='') %>%
    pasteFirstToAll() %>%
    dplyr::mutate(level = split_bracket$V2)
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
#' @export

pivotToColumns <- function(splitCriteria,redCriteria){
  df <- dplyr::bind_cols(splitCriteria, redCriteria[,c(1,2)])
  v_columns <- grepl('V',colnames(df))
  to_pivot <- tidyr::unite(data = df[,v_columns],
                           col = 'redlistCriteria',
                           sep='|',
                           na.rm = TRUE) %>%
    dplyr::bind_cols(df[,!v_columns]) %>%
    tidyr::separate_rows(`redlistCriteria`, sep = '[|]') %>%
    dplyr::select(`internalTaxonId`,`scientificName`,`redlistCriteria`,dplyr::everything()) %>%
    dplyr::filter(`redlistCriteria`!='')
  return(to_pivot)
}

#' Pastes the value in the last column of each row in front of each value in that row
#'
#' @param df a tibble with value to be pasted in the last column
#' @return a tibble with the value in the last column of each row in front of
#' each value in that row and last columns removed
#' @section Used in: \code{\link{criteriaToColumns}}
#' @export
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
#' @export
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
