#' Retrieve information about a github repo
#'
#' Given an repository on GitHub, retrieve the information about it from the
#' GitHub API and read it into R.
#'
#' @param repo_name The full name of the repo to get bookdown chapters from.
#' e.g. "jhudsl/OTTR_Template"
#' @param git_pat If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. If none is supplied, then this will attempt to
#' grab from a git pat set in the environment with usethis::create_github_token().
#' Authorization handled by \link[githubr]{get_git_auth}
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
#' @examples
#'
#' repo_info <- get_repo_info("jhudsl/Documentation_and_Usability")
get_repo_info <- function(repo_name,
                          git_pat = NULL,
                          verbose = FALSE) {
  repo_info <- NA

  exists <- check_git_repo(
    repo_name = repo_name,
    git_pat = git_pat,
    verbose = FALSE,
    silent = TRUE
  )

  if (exists) {
    # Declare URL
    url <- paste0("https://api.github.com/repos/", repo_name)

    # Try to get credentials other way
    auth_arg <- get_git_auth(git_pat = git_pat)

    git_pat <- try(auth_arg$password, silent = TRUE)

    if (grepl("Error", git_pat[1])) {
      # Github api get without authorization
      response <- httr::GET(
        url,
        httr::accept_json()
      )
    } else {
      # Github api get
      response <- httr::GET(
        url,
        httr::add_headers(Authorization = paste0("token ", git_pat)),
        httr::accept_json()
      )
    }

    if (httr::http_error(response)) {
      warning(paste0("url: ", url, " failed"))
    }

    # Get content as JSON
    repo_info <- httr::content(response, as = "parsed")
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
#' @param git_pat A personal access token from GitHub. Only necessary if the
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
#' @examples
#'
#' check_git_repo("jhudsl/OTTR_Template")
check_git_repo <- function(repo_name,
                           git_pat = NULL,
                           silent = TRUE,
                           return_repo = FALSE,
                           verbose = TRUE) {
  if (verbose) {
    message(paste("Checking for remote git repository:", repo_name))
  }
  # If silent = TRUE don't print out the warning message from the 'try'
  report <- ifelse(silent, suppressWarnings, message)

  # Try to get credentials
  auth_arg <- get_git_auth(git_pat = git_pat, quiet = !verbose)

  git_pat <- try(auth_arg$password, silent = TRUE)

  # Run git ls-remote
  if (!grepl("Error", git_pat[1])) {
    # If git_pat is supplied, use it
    test_repo <- report(
      try(system(paste0("git ls-remote https://", git_pat, "@github.com/", repo_name),
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

#' Handle GitHub PAT authorization
#'
#' Handle things whether or not a GitHub PAT is supplied.
#'
#' @param git_pat If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. If none is supplied, then this will attempt to
#' grab from a git pat set in the environment with usethis::create_github_token().
#' @param git_username Optional, can include username for credentials.
#' @param quiet Use TRUE if you don't want the warning about no GitHub credentials.
#'
#' @return Authorization argument to supply to curl OR a blank string if no
#' authorization is found or supplied.
#'
#' @export
#'
get_git_auth <- function(git_pat = NULL, git_username = "PersonalAccessToken", quiet = FALSE) {
  auth_arg <- NULL

  # If git pat is not provided, try to get credentials with gitcreds
  if (is.null(git_pat)) {

    # Try getting credentials
    auth_arg <- try(gitcreds::gitcreds_get(), silent = TRUE)

    if (grepl("Could not find any credentials", auth_arg[1])) {

      # Only if we're running this interactively
      if (interactive()) {
        # Set credentials if null
        auth_arg <- gitcreds::gitcreds_set()
      } else {
        if (!quiet) {
          message("Could not find git credentials, please set by running usethis::create_github_token(),
                    or directly providing a personal access token using the git_pat argument")
        }
      }
    }
  } else { # If git_pat is given, use it.
    # Set to Renviron file temporarily
    Sys.setenv(GITHUB_PAT = git_pat)

    # Put it in gitcreds
    auth_arg <- gitcreds::gitcreds_get()

    # Delete from Renviron file
    Sys.unsetenv("GITHUB_PAT")

    # Set up rest of token
    auth_arg$protocol <- "https"
    auth_arg$host <- "github.com"
    auth_arg$username <- git_username
  }

  # Check if we have authentication
  git_pat <- try(auth_arg$password, silent = TRUE)

  if (grepl("Error", git_pat[1])) {
    if (!quiet) {
      message("No github credentials found or provided; only public repositories will be successful.")
    }
  }

  return(auth_arg)
}
