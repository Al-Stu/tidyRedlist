testthat::test_that("splitting name creates a column with class character", {
  test <- tidySynonyms(raw_otidiformes_data[['synonyms']])$name
  testthat::expect_true(class(test)=='character')
})

testthat::test_that("synonyms split keeps sci name", {
  pattern <- paste(raw_otidiformes_data[['synonyms']]$genusName,
                   raw_otidiformes_data[['synonyms']]$speciesName)
  string <- tidySynonyms(raw_otidiformes_data[['synonyms']])$name
  test <- sapply(string, function(x) grepl(pattern = pattern,x = x))
  testthat::expect_true(all(sa))
})
