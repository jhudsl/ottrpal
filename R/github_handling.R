#' Handler function for GET requests from GitHub
#' @description This is a function to get the GitHub user's info
#' @param token You can provide the Personal Access Token key directly or this function will attempt to grab a PAT that was stored using the `authorize("github")` function
#' @param url What is the URL endpoint we are attempting to grab here?
#' @return Information regarding a Github account
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
get_github <- function(token = NULL, url) {
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  # Github api get
  result <- httr::GET(
    url,
    httr::add_headers(Authorization = paste0("Bearer ", token)),
    httr::accept_json()
  )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  return(result_list)
}

#' Retrieve pages url for a repo
#'
#' Given an repository on GitHub, retrieve the pages URL for it.
#'
#' @param repo_name The full name of the repo to get chapters from. e.g. 'jhudsl/OTTR_Template'
#' @param token If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. Run `authorize("github")` to set this.
#' @param verbose TRUE/FALSE do you want more progress messages?
#' @param keep_json verbose TRUE/FALSE keep the json file locally?
#'
#' @return a data frame with the repository with the following columns:
#' data_level, data_path, chapt_name, url, repository name
#'
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @export
#'
#' @examples \dontrun{
#'
#' usethis::create_github_token()
#'
#' get_pages_url("jhudsl/Documentation_and_Usability")
#' }
get_pages_url <- function(repo_name,
                          token = NULL,
                          verbose = FALSE,
                          keep_json = FALSE) {
  page_url <- NA

  # Try to get credentials other way
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  # We can only retrieve pages if we have the credentials
  if (!grepl("Error", token[1])) {
    exists <- check_git_repo(
      repo_name = repo_name,
      token = token,
      verbose = FALSE
    )

    if (exists) {
      # Get repo info
      repo_info <- get_repo_info(
        repo_name = repo_name,
        token = token
      )

      # Declare URL
      url <- paste0("https://api.github.com/repos/", repo_name, "/pages")

      # Github api get
      response <- httr::GET(
        url,
        httr::add_headers(Authorization = paste0("token ", token)),
        httr::accept_json()
      )

      if (httr::http_error(response)) {
        if (verbose) {
          warning(paste0("url: ", url, " failed"))
        }
      } else {
        # Get content as JSON
        page_info <- httr::content(response, as = "parsed")

        page_url <- page_info$html_url
      }
    }
  }
  return(page_url)
}

#' Retrieve information about a github repo
#'
#' Given an repository on GitHub, retrieve the information about it from the
#' GitHub API and read it into R.
#'
#' @param repo_name The full name of the repo to get bookdown chapters from.
#' e.g. "jhudsl/OTTR_Template"
#' @param token If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. If none is supplied, then this will attempt to
#' grab from a git pat set in the environment with usethis::create_github_token().
#' @param verbose TRUE/FALSE do you want more progress messages?
#'
#' @return a data frame with the repository with the following columns:
#' data_level, data_path, chapt_name, url, repository name
#'
#' @importFrom httr GET
#' @importFrom httr accept_json
#' @importFrom httr authenticate
#' @importFrom gitcreds gitcreds_get
#' @import dplyr
#'
#' @export
#'
#' @examples \dontrun{
#'
#' repo_info <- get_repo_info("jhudsl/Documentation_and_Usability")
#' }
get_repo_info <- function(repo_name,
                          token = NULL,
                          verbose = FALSE) {
  # Try to get credentials other way
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  repo_info <- NA

  exists <- check_git_repo(
    repo_name = repo_name,
    token = token,
    verbose = FALSE,
    silent = TRUE
  )

  if (exists) {
    # Declare URL
    url <- paste0("https://api.github.com/repos/", repo_name)

    if (grepl("Error", token[1])) {
      # Github api get without authorization
      response <- httr::GET(
        url,
        httr::accept_json()
      )
    } else {
      # Github api get
      response <- httr::GET(
        url,
        httr::add_headers(Authorization = paste0("token ", token)),
        httr::accept_json()
      )
    }

    if (httr::http_error(response)) {
      warning(paste0("url: ", url, " failed"))
    }

    # Get content as JSON
    repo_info <- httr::content(response)
  } else {
    warning(paste0(repo_name, " could not be found with the given credentials."))
  }
  return(repo_info)
}

#' Check if a repository exists on GitHub
#'
#' Given a repository name, check with git ls-remote whether the repository exists and return a TRUE/FALSE
#'
#' @param repo_name the name of the repository, e.g. jhudsl/OTTR_Template
#' @param token A personal access token from GitHub. Only necessary if the
#' repository being checked is a private repository.
#' @param silent TRUE/FALSE of whether the warning from the git ls-remote
#' command should be echoed back if it does fail.
#' @param verbose TRUE/FALSE do you want more progress messages?
#' @param return_repo TRUE/FALSE of whether or not the output from git ls-remote
#' should be saved to a file (if the repo exists)
#'
#' @return A TRUE/FALSE whether or not the repository exists. Optionally the
#' output from git ls-remote if return_repo = TRUE.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' authorize("github")
#' check_git_repo("jhudsl/OTTR_Template")
#' }
check_git_repo <- function(repo_name,
                           token = NULL,
                           silent = TRUE,
                           return_repo = FALSE,
                           verbose = TRUE) {
  if (verbose) {
    message(paste("Checking for remote git repository:", repo_name))
  }
  # If silent = TRUE don't print out the warning message from the 'try'
  report <- ifelse(silent, suppressWarnings, message)

  # Try to get credentials other way
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  # Run git ls-remote
  if (!grepl("Error", token[1])) {
    # If token is supplied, use it
    test_repo <- report(
      try(system(paste0("git ls-remote https://", token, "@github.com/", repo_name),
        intern = TRUE, ignore.stderr = TRUE
      ))
    )
  } else {
    # Try to git ls-remote the repo_name given
    test_repo <- report
    try(system(paste0("git ls-remote https://github.com/", repo_name),
      intern = TRUE, ignore.stderr = TRUE
    ))
  }
  # If 128 is returned as a status attribute it means it failed
  exists <- ifelse(is.null(attr(test_repo, "status")), TRUE, FALSE)

  if (return_repo && exists) {
    # Make file name
    output_file <- paste0("git_ls_remote_", gsub("/", "_", repo_name))

    # Tell the user the file was saved
    message(paste("Saving output from git ls-remote to file:", output_file))

    # Write to file
    writeLines(exists, file.path(output_file))
  }

  return(exists)
}

#' Retrieve logs for a GitHub Action workflow
#'
#' Given an workflow job on GitHub Actions, download the logs file from the API
#'
#' @param repo_name The full name of the repo to get chapters from. e.g. 'jhudsl/OTTR_Template'
#' @param token If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. Run `authorize("github")` to set this.
#' @param run_id What is the run id of the github action being run?
#' @param attempt What attempt number are we trying to retrieve the logs from?
#' @param dest_file Where should the logs be saved to?
#' @param verbose TRUE/FALSE do you want more progress messages?
#' @param keep_json verbose TRUE/FALSE keep the json file locally?
#'
#' @return A zip file downloaded from the GitHub API of the logs for the run and attempt given.
#'
#' @importFrom httr
#'
#' @export
#'
#' @examples \dontrun{
#'
#' authorize("github")
#'
#' download_logs("jhudsl/OTTR_Template",
#'               run_id = 12345678,
#'               attempt = 1)
#' }
download_logs <- function(repo_name,
                         token = NULL,
                         run_id,
                         run_attempt =  1,
                         dest_file = "logs.zip",
                         verbose = FALSE,
                         keep_json = FALSE) {

  # Try to get credentials other way
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  # Declare URL
  url <- paste0("https://api.github.com/repos/",
  repo_name, "/actions/runs/",
  run_id, "/attempts/",
  run_attempt, "/logs")

  # Github api get
  result <- httr::GET(
    url,
    httr::add_headers(Authorization = paste0("Bearer ", token)),
    httr::accept_json()
  )

  if (result$status_code == 404) {
    warning("log not found")
    return(result)
  }

  download.file(result$all_headers[[1]]$headers$location,
                 destfile = dest_file)

  message("Logs downloaded to: ", dest_file)

  return(dest_file)
}


#' Trim up the Markdown linter log file for use
#'
#' Given an workflow job on GitHub Actions, download the logs file from the API where markdown-linter has been run
#' This function will trim it up to be a little more useful.
#'
#' @param log_file The path to the log zip file downloaded by using download_logs() function
#'
#' @export
#' @examples \dontrun{
#'
#' authorize("github")
#'
#' log_file <- download_logs("jhudsl/OTTR_Template",
#'               run_id = 12716577174,
#'               run_attempt = 1)
#'
#' md_df <- format_linter_log(log_file)
#'
#' }
format_linter_log <- function(log_file = "logs.zip") {

  unzip(log_file)

  md_log_file <- list.files(pattern = "DavidAnsonmarkdownlint", recursive = TRUE, full.names = TRUE)

  content <- readLines(md_log_file)
  content <- grep("##\\[error\\]", content, value = TRUE)

  trimmed_content <- stringr::word(content, sep = "##\\[error\\]", start = 2)

  file_id <- stringr::word(trimmed_content, sep = " ", start = 1)
  warning_message <- stringr::word(trimmed_content, sep = " ", start = 2)

  md_lint <- data.frame(file_id, warning_message)

  return(md_lint)
}
