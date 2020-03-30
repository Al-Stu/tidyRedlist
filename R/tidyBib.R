#-------------------------------------------------------------------------#
#-------------------------- TIDY BIBLIOGRAPHY ----------------------------#
#-------------------------------------------------------------------------#

#' Tidies redlist bibliographies into a tidy dataframe
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with an extra tibble for bibliography authors and
#' tidied bibliography entries
#' @examples
#' tidyBib(species_data)
#' @section Used in: \code{\link{tidySpeciesData}}
#' @export
tidyBib <- function(species_data){
  bibliography <- species_data[['references']] %>%
    dplyr::mutate(referenceId = paste('r', 1:nrow(.), sep = '')) %>%
    pageNumbers() %>%
    dateAccessed() %>%
    volumeIssue() %>%
    journalName() %>%
    createShortRef()
  authors <- tidyAuthors(bibliography)
  for(i in 1:nrow(bibliography)){
    bibliography$author[i] <- paste(authors$author[authors$referenceId==bibliography$referenceId[i]],
                                    collapse = '; ')
  }
  species_data[['references']] <- bibliography
  species_data[['reference_authors']] <- authors
  species_data <- species_data %>%
    speciesInTextRefs() %>%
    matchRefs(max_distance = 1)
  return(species_data)
}

#' Tidies authors of redlist bibliographies into a tidy dataframe
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus reference ID
#' @return tibble with \code{internalTaxonId}, \code{referenceId} and
#' \code{author} (cleaned author name)
#' @examples
#' tidyAuthors(bibliography)
#' @section Used in: \code{\link{tidyBib}}
#' @export
tidyAuthors <- function(bibliography){
  authors <- bibliography %>%
    dplyr::select(`internalTaxonId`,
                  `referenceId`,
                  `author`) %>%
    tidyr::separate_rows(`author`,sep = '[.], |; | and ') %>%
    dplyr::mutate(author = `author` %>%
                    gsub(pattern = '\\s|[.]',
                         replacement = '') %>%
                    gsub(pattern = ',',
                         replacement = ', '))
  return(authors)
}

#' Tidies redlist citations into a tidy dataframe
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus reference ID
#' @return tibble with \code{internalTaxonId}, \code{referenceId} and
#' \code{author} (cleaned author name)
#' @examples
#' tidyCitation(bibliography)
#' @section Used in: \code{\link{tidyBib}}
#' @export
tidyCitation <- function(bibliography){
    citation <- dplyr::select(bibliography, `citation`)
    tidy_bib <- bibliography %>%
      dplyr::select(`internalTaxonId`, `scientificName`, `referenceId`, `title`, `year`)
    return(tidy_bib)
}

#' Selects page numbers from citation if listed at end
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus reference ID
#' @return tibble with \code{pages},\code{startPage} and \code{endPage}
#' @examples
#' pageNumbers(bibliography)
#' @section Used in: \code{\link{tidyCitation}}
#' @export
pageNumbers <- function(bibliography){
  pages <- bibliography %>%
    dplyr::select(`citation`) %>%
    sapply(function(x) sub(".*:", "", x))
  pageNumber <- c()
  startPage <- c()
  endPage <- c()
  correct_pattern <- c()
  for(i in 1:nrow(bibliography)){
    correct_pattern[i] <- grepl(pattern = '[0-9]+[^0-9][0-9]+[.]$', x = pages[i])
    if(correct_pattern[i] == 1){
      pageNumber[i] <- stringr::str_extract(pages[i],'[0-9]+[^0-9][0-9]+[.]$') %>%
        gsub(pattern = '[.]',replacement = '')
      if(gsub(pattern = '[^0-9].*$', replacement = '', x = pageNumber[i])!=pageNumber[i]){
        startPage[i] <- gsub(pattern = '[^0-9].*$', replacement = '', x = pageNumber[i])
        endPage[i] <- gsub(pattern = '^.*[^0-9]', replacement = '', x = pageNumber[i])
      } else {
        pageNumber[i] <- NA
        startPage[i] <- NA
        endPage[i] <- NA
      }
    } else {
      pageNumber[i] <- NA
      startPage[i] <- NA
      endPage[i] <- NA
    }
  }
  page <- tidyr::tibble(pages = pageNumber,
                        startPage = startPage,
                        endPage = endPage)
  result <- dplyr::bind_cols(bibliography, page)
  return(result)
}

#' Selects access date
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus \code{referenceId} and results of
#' \code{\link{pageNumbers}}
#' @return tibble with \code{dayAccessed},\code{monthAccessed} and
#' \code{yearAccessed} if present
#' @examples
#' journal(bibliography)
#' @section Used in: \code{\link{tidyCitation}}
#' @export
dateAccessed <- function(bibliography){
  accessed <- bibliography %>%
    dplyr::select(`citation`) %>%
    sapply(function(x) gsub(pattern = '[0-9]+[^0-9][0-9]+[.]$',
                            replacement = '', x = x)) %>%
    stringr::str_extract(pattern = '[(][Accessed].*[)][.|:][ ]*$')
  date <- bibliography %>%
    dplyr::transmute(dayAccessed =  accessed %>%
                    strSplitSelect(pattern = '[ ]', n = 2) %>%
                    stringr::str_extract(pattern = '[0-9][0-9]'),
                  monthAccessed = accessed %>%
                    strSplitSelect(pattern = '[ ]', n = 3) %>%
                    stringr::str_extract(pattern = 'January|February|March|April|May|June|July|August|September|October|December'),
                  yearAccessed = accessed %>%
                    strSplitSelect(pattern = '[ ]', n = 4) %>%
                    stringr::str_extract(pattern = '[0-9][0-9][0-9][0-9]'))
  result <- dplyr::bind_cols(bibliography, date)
  return(result)
}

#' Selects volume and issue
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus \code{referenceId}
#' @return tibble with \code{volume} and \code{issue} if present
#' @examples
#' journal(bibliography)
#' @section Used in: \code{\link{tidyCitation}}
#' @export
volumeIssue <- function(bibliography){
  vol_iss <- bibliography %>%
    dplyr::select(`citation`) %>%
    sapply(function(x) gsub(pattern = '[0-9]+[^0-9][0-9]+[.]$',
                            replacement = '', x = x)) %>%
    gsub(pattern = '[(][Accessed].*[)][.|:][ ]*$', replacement = '') %>%
    stringr::str_extract(pattern = '[0-9]+[(].+[)]|[0-9]+[:]')
  volume <- bibliography %>%
    dplyr::transmute(volume = strSplitSelect(x = vol_iss,
                                             pattern = '[()]|[)]|[:]',
                                             n = 1),
                     issue = strSplitSelect(x = vol_iss,
                                             pattern = '[()]|[)]',
                                             n = 2))
  result <- dplyr::bind_cols(bibliography, volume)
  return(result)
}

#' Selects journal name
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus \code{referenceId}
#' @return tibble with \code{journalName} and \code{inSCImago}
#' @examples
#' journal(bibliography)
#' @section Used in: \code{\link{tidyCitation}}
#' @export
journalName <- function(bibliography){
  journal <- bibliography %>%
    dplyr::select(`citation`) %>%
    sapply(function(x) gsub(pattern = '[0-9]+[^0-9][0-9]+[.]$',
                            replacement = '', x = x)) %>%
    gsub(pattern = '[(][Accessed].*[)][.|:][ ]*$', replacement = '') %>%
    gsub(pattern = '[0-9]+[(].+[)]|[0-9]+[:]', replacement = '') %>%
    gsub(pattern = '[<][/][i][>]', replacement = '') %>%
    stringr::str_extract(pattern = '[<][i][>][A-z| |:|.]+$') %>%
    gsub(pattern = '[<][i][>]|[:]|[.]$', replacement = '') %>%
    stringr::str_trim()
  journal <- tidyr::tibble(journal = journal)
  result <- dplyr::bind_cols(bibliography, journal)
  return(result)
}

#' Searches for the journal names in \code{rankings}
#'
#' Note: this takes a while as there are many, many journals, and each has an abbreviation!
#'
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus \code{referenceId}
#' @return tibble with \code{journalName} and \code{inSCImago}
#' @examples
#' journal(bibliography)
#' @section Used in: \code{\link{journalName}}
#' @export
SCImagoJournal <- function(bibliography){
  SCImago <- tidyRedlist::rankings
  match <- fuzzyMatchPairs(bibliography$journal,SCImago$Title, partial = FALSE)
  SCImago_abbreviations <- dplyr::filter(SCImago, !is.na(`abbreviation`))
  match_abbrev <- fuzzyMatchPairs(x = tm::removePunctuation(bibliography$journal),
                                  y = SCImago_abbreviations$abbreviation,
                                  partial = FALSE) %>%
    dplyr::mutate(yName = SCImago_abbreviations$Title[`yPosition`])
  adist_diff <- match$adist - match_abbrev$adist
  overall_match <- rbind(match[adist_diff<=0,],match_abbrev[adist_diff>0,]) %>%
    dplyr::filter(!is.na(adist),adist<=1)
  result <- tidyr::tibble(journal = bibliography$journal,
                          inSCImago = FALSE,
                          SCImagoName = NA)
  result[c(overall_match$xPosition),2] <- TRUE
  result[is.na(result$journal),2] <- NA
  result[c(overall_match$xPosition),3] <- as.character(overall_match$yName)
  result <- dplyr::bind_cols(bibliography, dplyr::select(result,`inSCImago`,`SCImagoName`))
  return(result)
}

#' creates short ref for \code{bibliography} entries
#' @param bibliography an intermediate object in \code{\link{tidyBib}} made up
#' of \code{species_data[['references']]} plus \code{referenceId}
#' @return bibliography with added column \code{shortRef}
#' @examples
#' creatShortRef(bibliography)
#' @section Used in: \code{\link{journalName}}
#' @export
createShortRef <- function(bibliography){
  authors <- tidyAuthors(bibliography)
  bibliography <- dplyr::mutate(bibliography,
                                shortRef = NA)
  for(i in 1:nrow(bibliography)){
    temp_authors <- authors$author[authors$referenceId==bibliography$referenceId[i]] %>%
      strSplitSelect(pattern = ',', n = 1)
    if(length(temp_authors) == 1){
      bibliography$shortRef[i] <- paste(temp_authors,
                                        bibliography$year[i], sep = ' ')
    } else if (length(temp_authors) == 2){
      bibliography$shortRef[i] <- paste(temp_authors, collapse = ' and ') %>%
        paste(bibliography$year[i], sep = ' ')
    } else if (length(temp_authors) > 2){
      bibliography$shortRef[i] <- paste(temp_authors[1],
                                        bibliography$year[i],
                                        sep = ' et al ')
    }
  }
  return(bibliography)
}

#' Finds the in text references for each text in each species' redlist profile
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return \code{species_data} with an extra tibble \code{in_text}
#' @section Used in: \code{\link{}}
#' @export
speciesInTextRefs <- function(species_data){
  references <- tidyr::tibble(internalTaxonId = character(),
                              section = character(),
                              inText = character())
  for(i in 1:nrow(species_data[['assessments']])){
    species_refs <- species_data[['references']] %>%
      dplyr::filter(`internalTaxonId` == species_data[['assessments']]$internalTaxonId[i]) %>%
      dplyr::select(`referenceId`,`shortRef`)
    species_texts_tibble <- species_data[['assessments']] %>%
      dplyr::filter(`internalTaxonId` == species_data[['assessments']]$internalTaxonId[i]) %>%
      dplyr::select(`rationale`,`habitat`,`threats`,`population`,`range`,`conservationActions`)
    species_texts <- species_texts_tibble %>%
      as.matrix() %>%
      t() %>%
      as.vector()
    for(j in 1:length(species_texts)){
      temp_references <- tidyr::tibble(internalTaxonId = species_data[['assessments']]$internalTaxonId[i],
                                       section = names(species_texts_tibble)[j],
                                       inText = inTextRefs(species_texts[j]))
      references <- dplyr::bind_rows(references, temp_references)
    }
  }
  species_data[['in_text']] <- references
  return(species_data)
}

#' returns in text references
#'
#' \code{inTextRefs} searches for in text references using the regex pattern
#' '[(][^)]*[1-2][0-9][0-9][0-9][^(]*[)]'
#'
#' @param text the text to be searched for full text references
#' @return \code{refs} a vector of the full text references in \code{text}
#' @section Used in: \code{\link{}}
#' @export
inTextRefs <- function(text){
    refs <- stringr::str_extract_all(string = text,
                                     pattern = '[(][^)]*[1-2][0-9][0-9][0-9][a-z]?[)]') %>%
      unlist() %>%
      stringr::str_split(pattern = ',') %>%
      unlist() %>%
      gsub(pattern = '[(|)]', replacement = '') %>%
      stringr::str_trim()
  return(refs)
}

#' matches in text references with references from bibliography
#'
#' \code{matchRefs} uses \code{fuzzyMatchPairs} to find the best match for each
#' in text reference created by \code{\link{speciesInTextRefs} and stored in
#' \code{species_data} with in text references for the bibliography generated in
#' \code{createShortRef}
#'
#' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @param max_distance the maximum accespted standard levenschtien distance
#' between an in text reference and its match
#' @return \code{species_data} with two extra columns in \code{in_text}:
#' \code{bestMatch} and \code{referenceId}
#' @section Used in: \code{\link{}}
#' @export
matchRefs <- function(species_data, max_distance){
  distances <- refDistances(species_data)
  in_text <- dplyr::mutate(species_data[['in_text']],
                                             inBib = NA,
                                             bestMatch = NA,
                                             referenceId = NA)
  for(i in 1:nrow(distances)){
    if(!is.na(distances$adist[i])){
      if(distances$adist[i] <= max_distance){
        in_text[i,c(4:6)] <- c(TRUE,
                               distances$yName[i],
                               species_data[['references']]$referenceId[distances$yPosition[i]])
      } else {
        in_text[i,4] <- FALSE
      }
    }
  }
  species_data[['in_text']] <- in_text
  return(species_data)
}

#' fuzzy match references
#' #' @param species_data a tibble of data downloaded from an IUCN Redlist search
#' and imported using \code{\link{importList}}
#' @return tibble with \code{internaltaxonId}, \code{xPosition}, \code{xName},
#' \code{yPosition}, \code{yName} and \code{adist}
#' @section Used in: \code{\link{matchRefs}}
#' @export
refDistances <- function(species_data){
  references <- list()
  short_bib_refs <- list()
  match <- list()
  species_match <- tidyr::tibble(internalTaxonId = character(),
                                 xPosition = numeric(),
                                 xName = character(),
                                 yPosition = numeric(),
                                 yName = character(),
                                 adist = numeric())
  for(i in 1:nrow(species_data[['assessments']])){
    internalTaxonId <- species_data[['assessments']]$internalTaxonId[i]
    references[[i]] <- species_data[['in_text']][species_data[['in_text']]$internalTaxonId==internalTaxonId, ] %>%
      dplyr::mutate(inText = `inText` %>%
                      gsub(pattern = '[<][^<]+[>]', replacement = '')) %>%
      .$inText %>%
      gsub(pattern = '[!-/:-@\\[-`{-~]', replacement = '')
    short_bib_refs[[i]] <- species_data[['references']]$shortRef[species_data[['references']]$internalTaxonId==internalTaxonId] %>%
      gsub(pattern = '[<][^<]+[>]', replacement = '') %>%
      gsub(pattern = '[!-/:-@\\[-`{-~]', replacement = '')
    match[[i]] <- fuzzyMatchPairs(references[[i]], short_bib_refs[[i]], partial = FALSE) %>%
      dplyr::mutate(internalTaxonId = as.character(internalTaxonId))
    species_match <- dplyr::bind_rows(species_match,match[[i]])
  }
  return(species_match)
}
