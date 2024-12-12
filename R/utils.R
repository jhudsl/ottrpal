.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use the authorize() function to begin. This gives the package the proper credentials to run.")
}


utils::globalVariables(c(
  "num", "quiz_dir", "type_url", "file_name", "trimmed", "quiz",
  "quiz_path", "type", "q_num", "verbose", "chapt_title", "data_path", "image_dir",
  "convert_footnotes", "rmd_files"
))


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

