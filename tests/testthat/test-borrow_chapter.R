test_that("Borrow chapter", {

  parent_doc <- list.files(
    pattern = "parent_doc.Rmd$",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )

  rmarkdown::render(parent_doc)

  testthat::expect_true(
    file.exists(
      file.path(dirname(parent_doc), "parent_doc.html"))
  )
})
