.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use the authorize() function to begin. This gives the package the proper credentials to run.")
}

#' set_knitr_image_path is being deprecated
#' @return Message that says to no longer use this function
#' @export
set_knitr_image_path <- function() {
  message(
    "The set_knitr_image_path() function will be deprecated from ottrpal",
    "It is no longer needed. Please delete"
  )
}

utils::globalVariables(c(
  "num", "quiz_dir", "type_url", "file_name", "trimmed", "quiz",
  "quiz_path", "type", "q_num", "verbose", "chapt_title", "data_path", "image_dir",
  "convert_footnotes", "rmd_files", "root_dir", "found", "urls_status"
))

#' Find root of OTTR course provided
#' @param path Where should we be looking for a OTTR course. By default will look in current working directory.
#' @return Absolute file path to the course pointed to
#' @importFrom rprojroot find_root has_dir
#' @return Returns a absolute file path to where the course is
#' @export
#'
course_path <- function(path = ".") {
  # Find .git root directory
  root_dir <- rprojroot::find_root(has_dir(".github"), path = path)

  bookdown <- file.exists(file.path(root_dir, "_bookdown.yml"))
  quarto <- file.exists(file.path(root_dir, "_quarto.yml"))

  if (bookdown & quarto) stop("No OTTR course found in this repository. Looking for a _bookdown.yml or _quarto.yml file.")

  return(root_dir)
}

#' ottrpal checks
#'
#' @param path path to the bookdown or quarto course repository, must have a
#'   `.github` folder which will be used to establish the top of the repo.
#' @param output_dir A relative file path to the folder (existing or not) that the
#'   output check file should be saved to. Default is "check_reports"
#' @param resources_dir A relative file path to the folder (existing or not) that the
#'   dictionary.txt file and exclude_files.txt will be found. Default is "resources".
#'   If no dictionary.txt file and exclude_files.txt files are found, we will download one.
#' @param ... All additional arguments passed to the respective check
#' @return The result of the check being called
#' @export
ottr_check <- function(check_type,
                  path = ".",
                  output_dir = "check_reports",
                  resources_dir = "resources",
                  ...) {
  if (check_type == "spelling") {
    results <- check_spelling(
      path = path,
      output_dir = output_dir,
      resources_dir = resources_dir,
      ...
    )
  } else if (check_type == "urls") {
    results <- check_urls(
      path = path,
      output_dir = output_dir,
      resources_dir = resources_dir,
      ...
    )
  } else if (check_type == "quiz_format") {
    results <- check_quiz_dir(
      path = path,
      ...
    )
  }
  return(results)
}



#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
