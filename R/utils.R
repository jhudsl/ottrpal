.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use the authorize() function to begin. This gives the package the proper credentials to run.")
}


utils::globalVariables(c(
  "num", "quiz_dir", "type_url", "file_name", "trimmed", "quiz",
  "quiz_path", "type", "q_num", "verbose", "chapt_title", "data_path", "image_dir",
  "convert_footnotes", "rmd_files"
))


#' Google Slides Helper Functions
#'
#' @param file markdown file for manuscript
#'
#' @return A scalar character vector
#' @export
#' @rdname gs_helpers
gs_id_from_slide <- function(file) {
  if (!file.exists(file)) {
    return(NA_character_)
  }
  x <- readLines(file, warn = FALSE)
  ind <- grep(x, pattern = "\\[(S|s)lides\\]")
  if (length(ind) > 0) {
    x <- x[ind]
  } else {
    x <- x[grep(x, pattern = ".*presentation/d/")]
  }
  if (!any(grepl("http", x))) {
    return(NA_character_)
  }
  x <- sub(".*\\(\\s*(http.*)\\s*\\).*", "\\1", x)
  x <- unlist(sapply(x, function(r) httr::parse_url(r)$path))
  x <- sub("/edit$", "", x)
  x <- sub("/export/.*", "", x)
  x <- basename(x)
  x <- stats::na.omit(x)
  x <- x[nchar(x) > 5]
  ux <- unique(x)
  if (length(ux) > 1) {
    warning(paste0(
      "Multiple sheets identified! Taking most frequent.",
      "  Please check ",
      file
    ))
    x <- sort(table(x), decreasing = TRUE)
    x <- names(x)[1]
    # x = x[1]
  } else {
    x <- ux
  }
  if (length(x) == 0 || grepl("\\(\\)", x)) {
    return(NA_character_)
  }
  if (nchar(x) < 10) {
    warning(paste0("ID extracted is ", x, ", seems short"))
  }
  return(x)
}

######################################
# this returns the actual links in the text
######################################
#' @export
#' @rdname gs_helpers
get_image_link_from_slide <- function(file) {
  x <- readLines(file, warn = FALSE)
  x <- grep(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", value = TRUE)
  x <- sub(x, pattern = "!\\[(.*)\\]\\(((resources/|)images.*)\\)", replacement = "\\1")
  # if (length(x) == 0) {
  #   return(NA)
  # }
  return(x)
}

######################################
# this returns the actual image filenames referenced
# we will check to see if all images referenced exist
######################################
#' @export
#' @rdname gs_helpers
get_image_from_slide <- function(file) {
  x <- readLines(file, warn = FALSE)
  x <- grep(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", value = TRUE)
  x <- sub(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", replacement = "\\1")
  # if (length(x) == 0) {
  #   return(NA)
  # }
  return(x)
}

is.Token <- function(token) {
  inherits(token, "Token") ||
    (inherits(token, "request") &&
      inherits(token$auth_token, "Token"))
}

png_url <- function(id, page_id) {
  paste0(
    "https://docs.google.com/presentation/d/",
    id, "/export/png?id=", id,
    "&pageid=", page_id
  )
}

download_png_urls <- function(urls) {
  res <- vapply(urls, function(url) {
    tfile <- tempfile(fileext = ".png")
    out <- httr::GET(
      url, httr::write_disk(tfile),
      httr::content_type(".png")
    )
    httr::stop_for_status(out)
    ctype <- out$headers$`content-type`
    ctype <- strsplit(ctype, " ")[[1]]
    ctype <- sub(";$", "", ctype)
    if (any(ctype == "text/html") &&
      !any(grepl("png", ctype))) {
      stop("Output is not a PNG!")
    }
    tfile
  }, FUN.VALUE = character(1))
  return(res)
}


add_footer <- function(rmd_path, footer_text = NULL) {
  if (is.null(footer_text)) {
    stop("Need character string in footer_text argument to append to end of file.")
  }
  footer_text <- paste0("\n", footer_text, collapse = "\n")
  write(as.character(footer_text),
    file = rmd_path,
    append = TRUE
  )
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

