
#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )
}
#' Get file path to an encrypted credentials RDS
encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt.rds",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )
}

#' Get file path to an default credentials RDS
encrypt_creds_user_path <- function() {
  list.files(
    pattern = "encrypted_default_user_creds.rds",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )
}


#' Get file path to an example quiz
#'
#' @return A file path to a markua markdown quiz example you can use for testing
#' @export
#' @examples \dontrun{
#'
#' # Find quiz path
#'
#' quiz_path <- markdown_quiz_path()
#' }
#'
markdown_quiz_path <- function() {
  list.files(
    pattern = "quiz.md",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}
