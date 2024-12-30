#' Download and render files from main OTTR_Template to test
#'
#' @param dir What relative file path should the files be downloaded
#' @param type Which OTTR repo are we downloading? Options are "rmd", "quarto", "rmd_website", "quarto_website"
#' @param render Should the OTTR repo be rendered after downloading? Default is TRUE
#' @return This downloads the main branch repo files from the respective repo for testing purposes
#' @export
setup_ottr_template <- function(dir = ".", type, render = TRUE) {
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
    quarto = "https://github.com/fhdsl/OTTR_Quarto/archive/refs/heads/main.zip",
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
  }
  output_dir <- stringr::str_remove(file.path(dir, file_name), ".zip")

  if (!dir.exists(output_dir)) {
    unzip(file_path, exdir = dir)
  }

  ## Render it
  if (render) {
    if (type == "rmd") bookdown::render_book(output_dir)
    if (type == "rmd_website") rmarkdown::render_site(output_dir)

    if (type == "quarto" | type == "quarto_website") {
      quarto::quarto_render(output_dir, as_job = FALSE)
    }
    if (type == "quarto") {
      quarto::quarto_render(output_dir,
        metadata = list(sidebar = F, toc = F),
        quarto_args = c("--output-dir", "docs/no_toc/"),
        as_job = FALSE
      )
    }
  }
  return(output_dir)
}

#' Clean up OTTR_Template files used for testing
#'
#' @return Looks for dangling zips and directories downloaded for testing and removes them
#' @export
clean_up <- function() {
  dirs <- c(
    "OTTR_Template-main",
    "OTTR_Quarto-main",
    "OTTR_Template_Website-main",
    "OTTR_Quarto_Website-main"
  )

  zips <- paste0(dirs, ".zip")

  # Remove dirs and their files
  sapply(dirs, unlink, recursive = TRUE)

  # Which zips are out there?
  existing_zips <- list.files(pattern = paste0(zips, collapse = "|"))

  # Remove any dangling zips
  sapply(existing_zips, unlink)
}


#' Path to good example quiz
#'
#' @export
#' @return The file path to an example good quiz included in the package that should pass the quiz checks.
#'
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
