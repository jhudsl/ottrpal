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
    system.file("extdata/quizzes", package = "leanbuild"),
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
    system.file("extdata/quizzes", package = "leanbuild"),
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
#'
#' example_repo_setup()
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
#' @export
#'
#' @examples
#'
#' example_repo_cleanup()
#'
example_repo_cleanup <- function(files_to_remove, verbose = FALSE) {

  message("Cleaning up and removing example repo files")

  files_to_remove <- grep(c("^docs$|^manuscript$|^resources$|^extdata$"),
                          basename(files), value = TRUE, invert = TRUE)
  files_to_remove <- c(files_to_remove,
                       list.files("docs", recursive = TRUE, full.names = TRUE),
                       list.files("manuscript",  recursive = TRUE, full.names = TRUE),
                       list.files("resources", recursive = TRUE, full.names = TRUE)
  )

  lapply(files_to_remove, function(file2remove, verbose = verbose) {
    if (file.exists(file2remove)) {
      message(paste0("Removing: ", file2remove))
      file.remove(file2remove)
    }
  })

}
