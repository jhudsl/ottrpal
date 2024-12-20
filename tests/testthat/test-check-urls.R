
output_file <- file.path("check_reports", "url_checks.tsv")

test_that("Test URL checks for OTTR main", {

   rmd_dir <- setup_ottr_template(type = "rmd", render = FALSE)

   status <- check_urls(rmd_dir)
   testthat::expect_true(status == 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(rmd_dir, output_file)))
   results <- readr::read_tsv(file.path(rmd_dir, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()

})

test_that("Test URL checks for OTTR Quarto main", {
   qmd_dir <- setup_ottr_template(type = "quarto", render = FALSE)

   status <- check_urls(qmd_dir)
   testthat::expect_true(status == 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(qmd_dir, output_file)))
   results <- readr::read_tsv(file.path(qmd_dir, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()
})

test_that("Test URL checks for OTTR web", {
   rmd_web <- setup_ottr_template(type = "rmd_website", render = FALSE)

   status <- check_urls(rmd_web, report_all = TRUE)
   testthat::expect_true(status == 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(rmd_web, output_file)))
   results <- readr::read_tsv(file.path(rmd_web, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()
})

test_that("Test URL checks for OTTR Quarto web", {
   ## Test URL
   qmd_web <- setup_ottr_template(type = "quarto_website", render = FALSE)

   status <- check_urls(qmd_web, report_all = TRUE)
   testthat::expect_true(status == 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(qmd_web, output_file)))
   results <- readr::read_tsv(file.path(qmd_web, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()
})
