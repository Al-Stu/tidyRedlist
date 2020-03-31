#-------------------------------------------------------------------------#
#------------------------------ FUZZY MATCH ------------------------------#
#-------------------------------------------------------------------------#

# FUZZYMATCHPAIRS
#
# it outputs a two-column dataframe

# author: alice stuart | date modified: 2020-0x-xx
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

#' finds best match for \code{x} in \code{y}
#'
#' \code{fuzzyMatchPairs} finds the best matching pair for each value in vector x in vector y
#'
#' @param x a character vector
#' @param y a character vector
#' @param partial a logical indicating whether the transformed x elements must exactly
#' match the complete y elements, or only substrings of these. The latter corresponds
#' to the approximate string distance used by \code{\link{agrep}} (by default).from
#' \code{\link{adist}}
#' @return tibble with columns: \code{xPosition}, \cpde{xName},\code{yPosition}, \code{yName}
#' and \code{adist}
#' @section Used in: \code{\link{SCImagoJournal}}
#' @export
fuzzyMatchPairs <- function(x, y, partial){
  # create a matrix with the Standard Levenshtein distance between the name fields of both sources
  dist.name<-adist(x,y, partial = partial, ignore.case = TRUE)
  # find pairs with minimum distance
  min.name<-apply(dist.name, 1, min)
  match.x.y<-NULL
  for(i in 1:nrow(dist.name)){
    y.i<-match(min.name[i],dist.name[i,])
    x.i<-i
    match.x.y<-rbind(data.frame(xPosition=x.i,
                                xName=x[x.i],
                                yPosition=y.i,
                                yName=y[y.i],
                                adist=min.name[i]),
                     match.x.y)
  }
  match.x.y <- dplyr::arrange(match.x.y,`xPosition`)
  return(match.x.y)
}

#' finds position of the best match for \code{x} in \code{y}
#'
#' @param x a character vector
#' @param y a character vector
#' @param partial a logical indicating whether the transformed x elements must exactly
#' match the complete y elements, or only substrings of these. The latter corresponds
#' to the approximate string distance used by \code{\link{agrep}} (by default).from
#' \code{\link{adist}}
#' @return ??
#' @section Used in: \code{\link{SCImagoJournal}}
fuzzymatchposition <- function(x,y){
  # create a matrix with the Standard Levenshtein distance between the name fields of both sources
  dist.name<-adist(x,y, partial = partial, ignore.case = TRUE)

  # find pairs with minimum distance
  min.name<-apply(dist.name, 1, min)

  result <- position(min.name,dist.name)

  return(result)
}
