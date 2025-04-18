test_that("Test Checks overall", {

  testthat::skip_on_cran()
  
  rmd_dir <- setup_ottr_template(type = "rmd", render = FALSE)

  ottr_check(path = rmd_dir, check_type = "urls")

  ottr_check(path = rmd_dir, check_type = "spelling")

  ottr_check(path = rmd_dir, check_type = "quiz_format")

  clean_up()
})
