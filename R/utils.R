.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use the authorize() function to begin. This gives the package the proper credentials to run.")
}


utils::globalVariables(c(
  "num", "quiz_dir", "type_url", "file_name", "trimmed", "quiz",
  "quiz_path", "type", "q_num", "verbose", "chapt_title", "data_path", "image_dir",
  "convert_footnotes", "rmd_files", "root_dir"
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
