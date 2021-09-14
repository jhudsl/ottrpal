# Originally from https://github.com/jhudsl/didactr/blob/f04239580c8132a48bb65af7c67648668f82af29/R/gs_convert.R
# Written by muschellij2

#' Authorize Application
#'
#' @param cache A logical value or a string. \code{TRUE} means to cache
#' using the default cache file \code{.httr-oauth}
#' @param use_oob use a local webserver for the OAuth dance
#' @param token_file If the \code{token} has been saved, use this file
#' to load the credentials.
#' @param language Should the language API be authorized
#' using your token (experimental, and most likely doesn't work)
#' @param token you can pass the token directly instead of the file
#' if you want
#'
#' @return The auth token, a Token class.
#' @export
#'
#' @importFrom httr oauth_endpoints oauth2.0_token
#' @importFrom googledrive drive_auth
#' @importFrom rgoogleslides authorize
#' @importFrom methods formalArgs
#' @examples \dontrun{
#' rgoogle_auth()
#' }
rgoogle_auth = function(
  token_file = NULL,
  cache = FALSE,
  language = FALSE,
  use_oob = FALSE,
  token = NULL) {

  if (is.null(token_file)) {
    token_file = tempfile(fileext = ".rds")
  }
  if (is.null(token)) {
    if (!file.exists(token_file)) {
      scope = c("https://www.googleapis.com/auth/drive",
                "https://www.googleapis.com/auth/youtube.force-ssl",
                "https://www.googleapis.com/auth/presentations")
      if (language) {
        scope = c(scope,
                  "https://www.googleapis.com/auth/cloud-language",
                  "https://www.googleapis.com/auth/cloud-platform")
      }
      token <- httr::oauth2.0_token(
        endpoint = httr::oauth_endpoints("google"),
        app = mooc_app(),
        scope = scope,
        cache = cache,
        use_oob = use_oob)
    } else {
      token = readRDS(token_file)
    }
  }

  # for tuber
  options(google_token = token)
  if (!file.exists(token_file)) {
    saveRDS(token, token_file)
  }
  if ("oauth_token" %in% methods::formalArgs(googledrive::drive_auth)) {
    drive_args = list(oauth_token = token_file)
    # googledrive::drive_auth(oauth_token = token_file)
  } else {
    drive_args = list(token = token)
    # googledrive::drive_auth(token = token)
  }
  do.call(googledrive::drive_auth, args = drive_args)
  rgoogleslides::authorize(token = token)
  if (language) {
    options(googleAuthR.client_id =  mooc_app()$key,
            googleAuthR.client_secret =  mooc_app()$secret)
    googleAuthR::gar_auth(token = token)
  }
  return(invisible(token))
}

#' Check authentication for rgoogleslides
#'
#' @param ... Arguments passed to _auth
#' @export
check_auth = function(...) {
  token = rgoogle_token(...)
  return(is.Token(token))
}

#' @export
rgoogle_token <- function(...) {
  token = getOption("google_token")
  if (is.Token(token)) {
    appname = token$app$appname
    if (appname != mooc_app()$appname) {
      token = rgoogle_auth(...)
    }
    args = list(...)
    token_file = args$token_file
    if (is.null(token_file)) {
      token_file = tempfile(fileext = ".rds")
    }
    saveRDS(token, token_file)
    if ("oauth_token" %in% formalArgs(googledrive::drive_auth)) {
      googledrive::drive_auth(oauth_token = token_file)
    } else {
      googledrive::drive_auth(token = token)
    }
    rgoogleslides::authorize(token = token)
    if (!is.null(args$language)) {
      if (args$language) {
        options(googleAuthR.client_id =  mooc_app()$key,
                googleAuthR.client_secret =  mooc_app()$secret)
        googleAuthR::gar_auth(token = token)
      }
    }
  } else {
    token = didactr_auth(...)
  }
  return(token)
}
