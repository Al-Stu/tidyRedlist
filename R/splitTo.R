#-------------------------------------------------------------------------#
#----------------------------- SPLIT TO ... ------------------------------#
#-------------------------------------------------------------------------#

#' Splits concatenated observations into multiple columns
#'
#' \code{splitToColumns} will take a vector/ column of concatenated observations
#' and split them into one observation per column, maintaining the rows they were
#' in before
#'
#' @param x the vector or column of concatenated observations to be split
#' @param pattern a regex representation of the characters used to split elements
#' in the concatenated column
#' @return a tibble with one row per concatenated observation, and a column for
#' each observation within a row
#' @examples
#' splitToColumns(species_data[['assessments']]$realms, '[|]')
#' @section Used in: \code{\link{splitToTidy}}
splitToColumns <- function(x,pattern){
  x <- as.matrix(x)
  split_x <- as.character(x) %>%
    stringr::str_split(pattern = pattern) %>%
    sapply(stringr::str_trim)
  if(class(split_x)=='list'){
    max_len <- max(sapply(split_x,length))
    ncol <- split_x %>% sapply(length) %>% max()
    split_df <- matrix(nrow = length(x),ncol = ncol)
    for(i in 1:max_len){
      split_df[,i] <- sapply(split_x, function(x) unlist(x)[i])
    }
    split_tibble <- tidyr::as_tibble(split_df)
  } else if(class(split_x)=='matrix'){
    split_tibble <- tidyr::as_tibble(t(split_x))
  } else if(class(split_x)=='vector'|class(split_x)=='character'){
    split_tibble <- split_x %>%
      as.matrix() %>%
      tidyr::as_tibble()
  }
  return(split_tibble)
}

#' Splits concatenated observations into multiple columns
#'
#' \code{splitToTidy} # will take (e.g. Palearctic|Afrotropical) a tibble of
#' concatenated columns and species IDs and will return a list with tibbles
#' with IDs and split results for each column
#'
#' @param columns the columns containing concatenated observations to be split
#' @param pattern a regex representation of the characters used to split elements
#' in the concatenated column
#' @param IDs the identifier for each row in x, e.g. internalTaxonId
#' @param name the names to be given to each tibble
#' @return a list with a tibble containing IDs and one result per row for each
#' column in \cpde{columns}
#' @examples
#' splitToTidy(c(species_data[['assessments']]$realms, species_data[['assessments']]$system),
#' c('[|]','[|]'), species_data[['assessments']]$internalTaxonId,c('realms','system'))
#'
splitToTidy <- function(columns,pattern,IDs,name){
  split_data <- list()
  for(i in 1:ncol(columns)){
    temp_id <- tidyr::tibble(internalTaxonId = IDs)
    temp <- columns[,i] %>%
      splitToColumns(pattern = pattern[i]) %>%
      dplyr::bind_cols(temp_id)
    split_data[[i]] <- tidyr::pivot_longer(temp,-ncol(temp), values_to = name[i]) %>%
      dplyr::select(c(`internalTaxonId`, name[i]))
    to_remove <- split_data[[i]] %>%
      dplyr::select(2) %>%
      is.na()
    split_data[[i]] <- dplyr::filter(split_data[[i]],!to_remove)
  }
  names(split_data) <- name
  return(split_data)
}
