#' Retrieve pages url for a repo
#'
#' Given an repository on GitHub, retrieve the pages URL for it.
#'
#' @param repo_name The full name of the repo to get bookdown chapters from.
#' e.g. "jhudsl/OTTR_Template"
#' @param git_pat If private repositories are to be retrieved, a github personal
#' access token needs to be supplied. If none is supplied, then this will attempt to
#' grab from a git pat set in the environment with usethis::create_github_token().
#' Authorization handled by \link[cow]{get_git_auth}
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
#' get_chapters("jhudsl/Documentation_and_Usability")
#' }
get_pages_url <- function(repo_name,
                          git_pat = NULL,
                          verbose = FALSE,
                          keep_json = FALSE) {
  page_url <- NA

  # Try to get credentials other way
  auth_arg <- get_git_auth(git_pat = git_pat, quiet = !verbose)

  git_pat <- try(auth_arg$password, silent = TRUE)

  if (grepl("Error", git_pat[1])) {
    warning("Cannot retrieve page info without GitHub credentials. Passing an NA.")
  }

  # We can only retrieve pages if we have the credentials
  if (!grepl("Error", git_pat[1])) {
    exists <- check_git_repo(
      repo_name = repo_name,
      git_pat = git_pat,
      verbose = FALSE
    )

    if (exists) {
      # Get repo info
      repo_info <- get_repo_info(
        repo_name = repo_name,
        git_pat = git_pat
      )

      # Declare URL
      url <- paste0("https://api.github.com/repos/", repo_name, "/pages")

      # Github api get
      response <- httr::GET(
        url,
        httr::add_headers(Authorization = paste0("token ", auth_arg$password)),
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
