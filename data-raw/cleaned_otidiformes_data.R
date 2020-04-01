## code to prepare `cleaned_otidiformes_data` dataset goes here
cleaned_otidiformes_data <- tidySpeciesData(raw_otidiformes_data)

usethis::use_data(cleaned_otidiformes_data, overwrite = TRUE)
