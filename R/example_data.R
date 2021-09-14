# C. Savonen 2021

#' Path to good example quiz
#'
#' @export
#'
#' @examples
#'
#' quiz_path <- good_quiz_path()
good_quiz_path <- function() {
  list.files(
    pattern = "quiz_good.md$",
    system.file("extdata", package = "leanbuild"),
    full.names = TRUE
  )
}

#' Path to bad example quiz
#'
#' @export
#'
#' @examples
#'
#' quiz_path <- bad_quiz_path()
bad_quiz_path <- function() {
  list.files(
    pattern = "quiz_bad.md$",
    system.file("extdata", package = "leanbuild"),
    full.names = TRUE
  )
}

#' Set up example repo files
#'
#' @param dest_dir The destination directory you would like the example repo files to be placed. By default is current directory.
#' @export
#'
#' @examples
#'
#' example_repo_setup()
example_repo_setup <- function(dest_dir = ".") {

  # Find example folder file
  zip_file <- list.files(
    pattern = "example-repo.zip",
    system.file("extdata", package = "leanbuild"),
    full.names = TRUE
  )

  # Unzip the folder
  unzip(zip_file, exdir = dest_dir)
}

#' Clean up example repo files
#'
#' @export
#'
#' @examples
#'
#' example_repo_cleanup()
#'
example_repo_cleanup <- function() {

  # Find example folder file
  zip_file <- list.files(
    pattern = "example-repo.zip",
    system.file("extdata", package = "leanbuild"),
    full.names = TRUE
  )

  # Retrieve list of files from original zip
  files_to_remove <- unzip(zip_file, list = TRUE)

  # Remove MACOSX bit if its there
  files_to_remove <- stringr::str_remove_all(files_to_remove$Name, "^_MACOSX/")

  message("Cleaning up and removing example repo files")

  # Now remove it all
  lapply(files_to_remove, function(file) {

    system(paste0("rm -r ", file))
  })
}

# save(bad_quiz, bad_quiz, file = "bad_quiz.RData")
# save(good_quiz, good_quiz, file = "good_quiz.RData")
