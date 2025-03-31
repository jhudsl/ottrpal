
output_file <- file.path("check_reports", "url_checks.tsv")

test_that("Test URL checks for OTTR main", {
   testthat::skip_on_cran()
   rmd_dir <- setup_ottr_template(type = "rmd", render = FALSE)

   # Add in a URL error
   writeLines("A URL error: https://notawebsiteaaaaaaa.com", file.path(rmd_dir, "url_test_error.md"))

   status <- check_urls(rmd_dir)
   testthat::expect_true(status > 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(rmd_dir, output_file)))
   results <- readr::read_tsv(file.path(rmd_dir, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()

})

test_that("Test URL checks for OTTR Quarto main", {
   testthat::skip_on_cran()
   qmd_dir <- setup_ottr_template(type = "quarto", render = FALSE)

   # Add in a URL error
   writeLines("A URL error: https://notawebsiteaaaaaaa.com", file.path(qmd_dir, "url_test_error.md"))

   status <- check_urls(qmd_dir)
   testthat::expect_true(status > 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(qmd_dir, output_file)))
   results <- readr::read_tsv(file.path(qmd_dir, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()
})

test_that("Test URL checks for OTTR web", {
   testthat::skip_on_cran()

   rmd_web <- setup_ottr_template(type = "rmd_website", render = FALSE)

   # Add in a URL error
   writeLines("A URL error: https://notawebsiteaaaaaaa.com", file.path(rmd_web, "url_test_error.md"))

   status <- check_urls(rmd_web, report_all = TRUE)
   testthat::expect_true(status > 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(rmd_web, output_file)))
   results <- readr::read_tsv(file.path(rmd_web, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()
})

test_that("Test URL checks for OTTR Quarto web", {
   testthat::skip_on_cran()
   ## Test URL
   qmd_web <- setup_ottr_template(type = "quarto_website", render = FALSE)

   # Add in a URL error
   writeLines("A URL error: https://notawebsiteaaaaaaa.com", file.path(qmd_web, "url_test_error.md"))

   status <- check_urls(qmd_web, report_all = TRUE)
   testthat::expect_true(status > 0)

   # Make sure the report exists
   testthat::expect_true(file.exists(file.path(qmd_web, output_file)))
   results <- readr::read_tsv(file.path(qmd_web, output_file))

   # It should be a data.frame
   testthat::expect_true(is.data.frame(results))
   clean_up()
})
