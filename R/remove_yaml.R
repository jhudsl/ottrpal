partition_yaml_front_matter <- function(input_lines) {
  # taken from rmarkdown package
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] >
      1) && grepl("^---\\s*$", input_lines[delimiters[1]])) {
      if (delimiters[1] == 1) {
        TRUE
      } else {
        all(grepl(
          "^\\s*(<!-- rnb-\\w*-(begin|end) -->)?\\s*$",
          input_lines[1:delimiters[1] - 1]
        ))
      }
    } else {
      FALSE
    }
  }
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    input_body <- c()
    if (delimiters[1] > 1) {
      input_body <- c(input_body, input_lines[1:delimiters[1] -
        1])
    }
    if (delimiters[2] < length(input_lines)) {
      input_body <- c(input_body, input_lines[-(1:delimiters[2])])
    }
    list(front_matter = front_matter, body = input_body)
  } else {
    list(front_matter = NULL, body = input_lines)
  }
}

#' Remove YAML header
#'
#' @param file file name of the markdown file
#'
#' @return A character vector of the text without the YAML header
#' @export
#'
#' @examples
#' file <- system.file("extdata/00_template.Rmd", package = "ottrpal")
#' out <- remove_yaml_header(file)
#' head(out)
remove_yaml_header <- function(file) {
  file <- readLines(file, warn = FALSE)
  out <- partition_yaml_front_matter(file)
  out <- out$body
  return(out)
}
