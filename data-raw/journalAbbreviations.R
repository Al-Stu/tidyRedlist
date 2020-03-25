## code to prepare `journalAbbreviations` dataset goes here

eCaps <- list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
rD <- RSelenium::rsDriver(extraCapabilities = eCaps)
remDr <- rD[["client"]]
remDr$navigate('https://images.webofknowledge.com/images/help/WOS/A_abrvjt.html')
Sys.sleep(0.5)
abbreviations <- tidyr::tibble(journal = character(),
                               abbreviation = character())
for(i in 3:27){
  text <- remDr$findElement(using = 'xpath', value = '/html/body/table/tbody/tr/td[2]/pre/dl')
  text <- unlist(text$getElementText())
  text_list <- text %>%
    stringr::str_split(pattern = '\\n') %>%
    unlist()
  temp_abbrev <- tidyr::tibble(journal = text_list[c(TRUE,FALSE)],
                               abbreviation = text_list[c(FALSE, TRUE)])
  abbreviations <- dplyr::bind_rows(abbreviations,temp_abbrev)
  next_letter <- remDr$findElement(using = 'xpath',
                                   value = paste0('/html/body/table/tbody/tr/td[2]/a[',i,']'))
  next_letter$clickElement()
  Sys.sleep(2)
}

remDr$close()
rD[["server"]]$stop()
rm(rD)
gc()

journalAbbreviations <- abbreviations[!duplicated(abbreviations$abbreviation)|
                                        !grepl(pattern = '[A-Z]', x = abbreviations$abbreviation),] %>%
  dplyr::mutate(journal = `journal` %>%
                  stringr::str_trim(),
                abbreviation = `abbreviation` %>%
                  stringr::str_trim()
                )

usethis::use_data(journalAbbreviations)
