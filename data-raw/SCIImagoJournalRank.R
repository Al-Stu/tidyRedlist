## import CSV downloaded from https://www.scimagojr.com/journalrank.php
rankings <- readr::read_csv('~/Documents/Masters/SCImagoJournalRank.csv') %>%
  tidyr::separate_rows(`Categories`, sep = ';') %>%
  dplyr::mutate(Categories = `Categories` %>%
                  gsub(pattern = '[(]Q[0-9][)]',replacement = '') %>%
                  stringr::str_trim(),
                journal = toupper(`Title`)) %>%
  dplyr::left_join(tidyRedlist::journalAbbreviations)


usethis::use_data(rankings, overwrite = TRUE)
