## These tests make sure the infrastructure for setting up test OTTR repos is working
## Makes sure that the repos are downloaded, rendered and then cleaned up.

test_that("Rmd Rendering", {
  testthat::skip_on_cran()
  rmd_dir <- setup_ottr_template(dir = ".", type = "rmd")

  testthat::expect_true(dir.exists(rmd_dir))

  clean_up()

  testthat::expect_true(!dir.exists(rmd_dir))
})

test_that("Rmd Website Rendering", {
  testthat::skip_on_cran()
  rmd_web_dir <- setup_ottr_template(dir = ".", type = "rmd_website")

  testthat::expect_true(dir.exists(rmd_web_dir))

  clean_up()

  testthat::expect_true(!dir.exists(rmd_web_dir))
})


test_that("Quarto Rendering", {
  testthat::skip_on_cran()
  quarto_dir <- setup_ottr_template(dir = ".", type = "quarto")

  testthat::expect_true(dir.exists(quarto_dir))

  clean_up()

  testthat::expect_true(!dir.exists(quarto_dir))
})

test_that("Quarto Website Rendering", {
  testthat::skip_on_cran()
  quarto_web_dir <- setup_ottr_template(dir = ".", type = "quarto_website")

  testthat::expect_true(dir.exists(quarto_web_dir))

  clean_up()

  testthat::expect_true(!dir.exists(quarto_web_dir))
})
