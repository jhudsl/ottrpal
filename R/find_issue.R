find_issue <- function(repo, github_pat) {
  # Argument check
  if (!is.character(repo)) {
    repo <- as.character(repo)
  }

  result <- httr::GET(
    paste0("https://api.github.com/repos/", repo, "/issues"),
    httr::add_headers(Authorization = paste0("Bearer ", github_pat)),
    httr::accept_json()
  )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  issue_exists <- length(grep('Broken URLs found in the course!', result_list$title))

  issue_exists
}
