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




#' Find an issue on GitHub with a particular title
#'
#' Given text and repository name, find if an issue exists.
#'
#' @param text What text to be searched for in the GitHub issues. Can be regex.
#' @param repo_name the name of the repository, e.g. jhudsl/OTTR_Template
#' @param token A personal access token from GitHub. Only necessary if the
#' repository being checked is a private repository.
#'
#' @return A TRUE/FALSE whether or not the issue with this text on this repository exists.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' authorize("github")
#'
#' find_issue(text = "TEST", repo_name = "jhudsl/ottrpal")
#'
#' }

find_issue <- function(text, repo_name, token = NULL) {

  if (!is.character(repo_name)) {
    repo <- as.character(repo_name)
  }
  # Try to get credentials other way
  if (is.null(token)) {
    # Get auth token
    token <- get_token(app_name = "github")
  }

  # Github api get
  result <- httr::GET(
    paste0("https://api.github.com/repos/", repo_name, "/issues"),
    #httr::add_headers(Authorization = paste0("Bearer ", token)),
    httr::accept_json()
    )

  if (httr::status_code(result) != 200) {
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- httr::content(result, "text")
  result_list <- jsonlite::fromJSON(result_content)

  issue_exists <- length(grep(text, result_list$title))

  # Print out the result
  write(issue_exists, stdout())

  return(result_list)
}
