if (Sys.getenv("GH_PAT") != "") {

test_that("Test issue finder", {

  find_issue(text = "TEST:", repo_name = "jhudsl/ottrpal")
})
}
