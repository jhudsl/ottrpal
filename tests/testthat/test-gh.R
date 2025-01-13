
test_that("Test issue finder", {

  issue <- find_issue(text = "TEST:", repo_name = "jhudsl/ottrpal")

  testthat::expect_true(length(issue$id) > 0)
})
