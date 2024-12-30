
output_file <- file.path("check_reports", "spell_check_results.tsv")

test_that("Test spell checks for OTTR main", {

  rmd_dir <- setup_ottr_template(type = "rmd", render = FALSE)

  # Add in a spelling error
  writeLines("A spelling error: sauibguiabsduvbasuidbv", file.path(rmd_dir, "spell_test_error.md"))

  status <- check_spelling(rmd_dir)
  testthat::expect_true(status > 0)

  # Make sure the report exists
  testthat::expect_true(file.exists(file.path(rmd_dir, output_file)))
  results <- readr::read_tsv(file.path(rmd_dir, output_file))

  # It should be a data.frame
  testthat::expect_true(is.data.frame(results))
  clean_up()
})

test_that("Test spell checks for OTTR Quarto main", {
  qmd_dir <- setup_ottr_template(type = "quarto", render = FALSE)

  # Add in a spelling error
  writeLines("A spelling error: sauibguiabsduvbasuidbv", file.path(qmd_dir, "spell_test_error.md"))

  status <- check_spelling(qmd_dir)
  testthat::expect_true(status > 0)

  # Make sure the report exists
  testthat::expect_true(file.exists(file.path(qmd_dir, output_file)))
  results <- readr::read_tsv(file.path(qmd_dir, output_file))

  # It should be a data.frame
  testthat::expect_true(is.data.frame(results))
  clean_up()
})

test_that("Test spell checks for OTTR web", {
  rmd_web <- setup_ottr_template(type = "rmd_website", render = FALSE)

  # Add in a spelling error
  writeLines("A spelling error: sauibguiabsduvbasuidbv", file.path(rmd_web, "spell_test_error.md"))

  status <- check_spelling(rmd_web)
  testthat::expect_true(status > 0)

  # Make sure the report exists
  testthat::expect_true(file.exists(file.path(rmd_web, output_file)))
  results <- readr::read_tsv(file.path(rmd_web, output_file))

  # It should be a data.frame
  testthat::expect_true(is.data.frame(results))
  clean_up()
})

test_that("Test spell checks for OTTR Quarto web", {
  ## Test spell
  qmd_web <- setup_ottr_template(type = "quarto_website", render = FALSE)

  # Add in a spelling error
  writeLines("A spelling error: sauibguiabsduvbasuidbv", file.path(qmd_web, "spell_test_error.md"))

  status <- check_spelling(qmd_web)
  testthat::expect_true(status > 0)

  # Make sure the report exists
  testthat::expect_true(file.exists(file.path(qmd_web, output_file)))
  results <- readr::read_tsv(file.path(qmd_web, output_file))

  # It should be a data.frame
  testthat::expect_true(is.data.frame(results))
  clean_up()
})
