# Originally from https://github.com/jhudsl/didactr/blob/f04239580c8132a48bb65af7c67648668f82af29/R/gs_convert.R
# Written by muschellij2

#' Check authentication for rgoogleslides
#'
#' @param ... Arguments passed to didactr_auth
#' @export
check_didactr_auth = function(...) {
  token = didactr_token(...)
  return(is.Token(token))
}

#' @export
didactr_token <- function(...) {
  token = getOption("google_token")
  if (is.Token(token)) {
    appname = token$app$appname
    if (appname != mooc_app()$appname) {
      token = didactr_auth(...)
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
