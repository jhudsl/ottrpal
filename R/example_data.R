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

  zip_file <- file.path(dest_dir, "example-repo.zip")

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir)
  }
  download.file(url = "https://github.com/jhudsl/DaSL_Course_Template_Leanpub/raw/main/example-repo.zip",
                destfile = zip_file)

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
example_repo_cleanup <- function(dir = ".") {

  # Find example folder file
  files_to_remove <- c(
    readLines(file.path(dir, "resources", "needed_leanbuild_files.txt")),
    "Course_Name.rds",
    "manuscript",
    "question_error_report.tsv",
    "packages.bib")


  message("Cleaning up and removing example repo files")

  # Now remove it all
  lapply(files_to_remove, function(file) {
    if (file.exists(file) | dir.exists(file)) {
      system(paste0("rm -r ", file))
    }
  })
}

# save(bad_quiz, bad_quiz, file = "bad_quiz.RData")
# save(good_quiz, good_quiz, file = "good_quiz.RData")
