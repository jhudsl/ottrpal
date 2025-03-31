#' Get Slide ID from URL
#'
#' @param x URL of slide
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x <- paste0(
#'   "https://docs.google.com/presentation/d/",
#'   "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8",
#'   "/edit#slide=id.g154aa4fae2_0_58"
#' )
#' get_slide_id(x)
get_slide_id <- function(x) {
  x <- sub(".*presentation/", "", x)
  x <- sub("/d/e", "/d", x) # if you publish by accident
  x <- sub("^(d|e)/", "", x)
  x <- strsplit(x, "/")[[1]]
  x <- x[!grepl("^(edit|pub|export|png)", x)]
  x <- x[nchar(x) > 5]
  x
}

#' Get Google Slide PNG URL
#'
#' @param url URL to Google Slide
#'
#' @return A character vector of URLs
#' @export
#'
#' @examples
#' url <- paste0(
#'   "https://docs.google.com/presentation/d/",
#'   "12DPZgPteQBwgal6kSPP58zhPhjZ7QSPZLe3NkA8M3eo/edit",
#'   "#slide=id.gc8648f14c3_0_397&t=4"
#' )
#' id <- get_slide_id(url)
#' gs_png_url(url)
gs_png_url <- function(url) {
  id <- get_slide_id(url)
  slide_id <- get_slide_page(url)
  gs_png_id(id, slide_id)
}

gs_png_id <- function(id, slide_id) {
  if (any(grepl("^id[.]", slide_id))) {
    warning(
      "slide ids usually don't have format of id.gc*, ",
      "you should likely remove the id."
    )
  }
  paste0(
    "https://docs.google.com/presentation/d/",
    id,
    "/export/png?id=", id,
    "&pageid=", slide_id
  )
}

#' @export
#' @rdname gs_png_url
get_slide_page <- function(url) {
  parsed <- httr::parse_url(url)
  slide_id <- parsed$query$pageid
  if (length(slide_id) == 0 || nchar(slide_id) == 0) {
    fragment <- parsed$fragment
    slide_id <- sub("slide=(.*)", "\\1", fragment)
    slide_id <- sub("&.*", "", slide_id)
    slide_id <- sub("^id[.]", "", slide_id)
  }
  stopifnot(length(slide_id) > 0 && nchar(slide_id) > 0)
  slide_id
}

#' @export
#' @rdname gs_png_url
#' @param output_dir path to output png
#' @param overwrite should the slide PNG be overwritten?
gs_png_download <- function(url, output_dir = ".", overwrite = TRUE) {
  id <- get_slide_id(url)
  slide_id <- get_slide_page(url)
  url <- gs_png_url(url)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(output_dir, paste0(id, "_", slide_id, ".png"))
  if (!file.exists(outfile) || overwrite) {
    download.file(url, destfile = outfile, quiet = FALSE)
  }
  stopifnot(file.exists(outfile))
  outfile
}

#' @export
#' @rdname gs_png_url
#' @param ... for \code{include_slide}, options passed to
#' [knitr::include_graphics()]
include_slide <- function(url,
                          output_dir = knitr::opts_chunk$get("fig.path"),
                          overwrite = TRUE, ...) {
  outfile <- gs_png_download(url, output_dir, overwrite = overwrite)
  knitr::include_graphics(outfile, ...)
}

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
