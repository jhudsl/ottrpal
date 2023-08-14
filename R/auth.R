.tokenEnv <- new.env(parent = emptyenv())
.tokenEnv$Token <- NULL

# Set token to environment
set_token <- function(value) {
  .tokenEnv$Token <- value
  return(value)
}

# Get token from environment
get_token <- function() {
  .tokenEnv$Token
}

### Declare all the scopes
scopes_list <- c(
  "https://www.googleapis.com/auth/drive",
  "https://www.googleapis.com/auth/drive.file",
  "https://www.googleapis.com/auth/drive.readonly",
  "https://www.googleapis.com/auth/presentations",
  "https://www.googleapis.com/auth/presentations.readonly"
)


#' Authorize R package to access the Google Slides API
#' @description This is a function to authorize the R package to access the Google Slides API interactively.
#' @param token An output from \code{\link{oauth2.0_token}} to set as the authentication token.
#' @param cache Should the token be cached as an .httr-oauth file?
#' @param ... Additional arguments to send to \code{\link{oauth2.0_token}}
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize()
#' }
authorize <- function(token = NULL, cache = FALSE, ...) {
  if (!cache) {
    cache_it <- menu(c("Yes store credentials as .httr-oauth file", "No do not store credentials, I will re-run this authorize() in my next R session"))
    if (cache_it == 1) {
      message("You chose to cache your credentials, if you change your mind, just delete the .httr-oauth. Be careful not to push this file to GitHub or share it anywhere.")
    }
  } else {
    cache_it <- 1
  }
  if (is.null(token)) {
    token <- httr::oauth2.0_token(
      endpoint = app_set_up()$endpoint,
      app = app_set_up()$app,
      cache = cache_it == 1,
      scope = scopes_list,
      ...
    )
  }
  set_token(token)
  return(invisible(token))
}

#' Use secrets to authorize R package to access Google Slides API
#' @description This is a function to authorize the R package to access the Google Slides API. If no
#' client.id and client.secret is provided, the package would provide predefined values.
#' @param access_token Access token can be obtained from running authorize() interactively: token <-authorize(); token$credentials$access_token
#' @param refresh_token Refresh token can be obtained from running authorize() interactively: token <-authorize(); token$credentials$refresh_token
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @importFrom openssl aes_cbc_decrypt
#' @export
#' @examples \dontrun{
#'
#' token <- authorize()
#'
#' auth_from_secret(
#'   token$credentials$access_token,
#'   token$credentials$refresh_token
#' )
#' }
#'
auth_from_secret <- function(access_token = NULL, refresh_token = NULL) {

  # If no tokens are specified, we'll grab the default ones.
  if (is.null(access_token) | is.null(refresh_token)) {
    decrypted <- openssl::aes_cbc_decrypt(
      readRDS(encrypt_creds_user_path()),
      key = readRDS(key_encrypt_creds_path())
    )
    access_token <- unserialize(decrypted)[[1]]$access_token
    refresh_token <- unserialize(decrypted)[[1]]$refresh_token
  }

  credentials <- list(
    access_token = access_token,
    expires_in = 3599L,
    refresh_token = refresh_token,
    scope = scopes_list,
    token_type = "Bearer"
  )

  token <- httr::oauth2.0_token(
    endpoint = app_set_up()$endpoint,
    app = app_set_up()$app,
    scope = scopes_list,
    credentials = credentials
  )

  set_token(token)
  return(invisible(token))
}

# This sets up the app creds no matter which way authorization is called
app_set_up <- function() {
  decrypted <- openssl::aes_cbc_decrypt(
    readRDS(encrypt_creds_path()),
    key = readRDS(key_encrypt_creds_path())
  )
  app <- httr::oauth_app(
    appname = "ottrpal",
    key = unserialize(decrypted)$client_id,
    secret = unserialize(decrypted)$client_secret
  )
  endpoint <- httr::oauth_endpoints("google")

  return(list(app = app, endpoint = endpoint))
}
