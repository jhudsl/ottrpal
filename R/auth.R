# Make an empty environment where the token will be stored
.tokenEnv <- new.env(parent = emptyenv())

# For now the Token is gonna be NULL because we don't have it yet.
.tokenEnv$Token <- NULL

# A function to set token to environment
set_token <- function(value) {
  .tokenEnv$Token <- value
  return(value)
}

# A function to retrieve token from environment
get_token <- function() {
  .tokenEnv$Token
}


#' Authorize R package to access Google Slides API
#'
#' By providing a Google Cloud Client ID and Client Secret, you obtain an access
#' token from Google's OAuth 2.0 server. This access token is used to access the
#' Google Slides API. If you supply a token, this function will save it for
#' future use. For instructions on creating a Client ID and Client Secret, see
#' \url{https://www.hairizuan.com/rgoogleslides-using-your-own-account-client-id-and-secret/}.
#'
#' If this is your first time running authorize(), it will ask you if you want
#' to use a local file ('.httr-oauth') to cache the access token. If you say
#' "Yes", you will not have to run this function in future R sessions. Make sure
#' to provide ottrpal complete access to your Google Drive files and Google
#' Slides presentations.
#'
#' @param client_id Google Cloud Client ID
#' @param client_secret Google Cloud Client secret
#' @param token OAuth 2.0 Access Token
#' @param ... Additional arguments to [httr::oauth2.0_token()]
#'
#' @return A Token2.0 reference class (RC) object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate token from Client ID and Client Secret
#' authorize(client_id = "MY_CLIENT_ID", client_secret = "MY_CLIENT_SECRET")
#'
#' # Provides user-generated token
#' authorize(token = my_token)
#' }
authorize <- function(client_id = NULL,
                      client_secret = NULL,
                      token = NULL,
                      ...) {
  # client id or secret not provided
  if ((is.null(client_id) | is.null(client_secret)) & is.null(token)) {
    stop("Please generate a client secret and client key following these instructions:\n",
         "https://www.hairizuan.com/rgoogleslides-using-your-own-account-client-id-and-secret/")
  }
  if (is.null(token)) {
    # setup app
    app <- httr::oauth_app(appname = "googleslides",
                           key = client_id,
                           secret = client_secret)
    # google endpoints
    endpoint <- httr::oauth_endpoints("google")
    # generate token
    token <- httr::oauth2.0_token(endpoint = endpoint,
                                  app = app,
                                  scope = c("https://www.googleapis.com/auth/presentations",
                                            "https://www.googleapis.com/auth/drive.readonly"),
                                  ...)
  }
  set_token(token)
  invisible(token)
}
