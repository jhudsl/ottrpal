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
    recursive = TRUE,
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
    recursive = TRUE,
    system.file("extdata", package = "leanbuild"),
    full.names = TRUE
  )
}

#' Set up example repo files
#'
#' @param dest_dir The destination directory you would like the example repo files to be placed. By default is current directory.
#' @export
#'
#' @importFrom fs dir_copy
#' @examples
#' # Run this to get the files we need
#' example_files <- leanbuild::example_repo_setup()
example_repo_setup <- function(dest_dir = ".") {
  bookdown_path <- list.files(
    pattern = "_bookdown.yml$",
    system.file("extdata/", package = "leanbuild"),
    full.names = TRUE
  )

  # Copy over whole directory
  fs::dir_copy(dirname(bookdown_path), dest_dir, overwrite = TRUE)

  copied_files <- list.files(dirname(bookdown_path), full.names = TRUE)

  return(copied_files)
}

#' Clean up example repo files
#'
#' @param files_to_remove List of example files to delete.
#' @param verbose TRUE/FALSE would you like progress messages?
#' @export
#'
#' @examples
#'
#' # Run this to get the files we need
#' example_files <- leanbuild::example_repo_setup()
#'
#' # Run this to delete them
#' example_repo_cleanup(files_to_remove = basename(example_files))
example_repo_cleanup <- function(files_to_remove, verbose = FALSE) {
  message("Cleaning up and removing example repo files")

  files_to_remove <- c(
    list.files("quizzes", recursive = TRUE, full.names = TRUE),
    list.files("docs", recursive = TRUE, full.names = TRUE),
    list.files("manuscript", recursive = TRUE, full.names = TRUE),
    list.files("resources", recursive = TRUE, full.names = TRUE),
    files_to_remove,
    "question_error_report.tsv",
    "Course_Name.rds"
  )

  lapply(files_to_remove, function(file2remove, verbose = verbose) {
    if (file.exists(file2remove)) {
      message(paste0("Removing: ", file2remove))
      file.remove(file2remove)
    }
  })
}
