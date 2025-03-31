
test_that("Test issue finder", {
  testthat::skip_on_cran()
  
  issue <- find_issue(text = "TEST:", repo_name = "jhudsl/ottrpal")

  testthat::expect_true(length(issue$id) > 0)
})
