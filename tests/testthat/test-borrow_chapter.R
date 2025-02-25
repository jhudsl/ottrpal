test_that("Borrow chapter", {

  parent_doc <- list.files(
    pattern = "parent_doc.Rmd$",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )

  rmarkdown::render(parent_doc)

})
