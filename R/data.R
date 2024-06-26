#' Download files from main OTTR_Template to test
#'
#' @param dir What relative file path should the files be downloaded
#' @param type Which OTTR repo are we downloading? Options are "rmd", "quarto", "rmd_website", "quarto_website"
#'
#' @return This downloads the main branch repo files from the respective repo for testing purposes
#' @export
download_ottr_template <- function(dir = "inst/extdata", type = "rmd") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  possible_types <- c("rmd", "quarto", "rmd_website", "quarto_website")

  if (!(type %in% possible_types)) {
    stop(
      "The `type` argument provided can only be one of these types: \n ",
      paste(possible_types, collapse = " ")
    )
  }

  url <- switch(type,
    rmd = "https://github.com/jhudsl/OTTR_Template/archive/refs/heads/main.zip",
    quarto = "https://github.com/fhdsl/ottr/archive/refs/heads/main.zip",
    rmd_website = "https://github.com/jhudsl/OTTR_Template_Website/archive/refs/heads/main.zip",
    quarto_website = "https://github.com/fhdsl/OTTR_Quarto_Website/archive/refs/heads/main.zip"
  )

  file_name <- switch(type,
    rmd = "OTTR_Template-main.zip",
    quarto = "OTTR_Quarto-main.zip",
    rmd_website = "OTTR_Template_Website-main.zip",
    quarto_website = "OTTR_Quarto_Website-main.zip"
  )

  file_path <- file.path(dir, file_name)

  if (!file.exists(file_path)) {
    download.file(url,
      destfile = file_path
    )

    unzip(file_path, exdir = dir)
  }
  output_dir <- stringr::str_remove(file.path(dir, file_name), ".zip")

  return(output_dir)
}

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

good_quiz_path <- function() {
  list.files(
    pattern = "quiz_good.md$",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )
}

#' Path to bad example quiz
#'
#' @export
#' @return The file path to an example bad quiz included in the package that will fail the quiz checks.
#'
#' @examples
#'
#' quiz_path <- bad_quiz_path()
bad_quiz_path <- function() {
  list.files(
    pattern = "quiz_bad.md$",
    recursive = TRUE,
    system.file("extdata", package = "ottrpal"),
    full.names = TRUE
  )
}
