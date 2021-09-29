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
    curl::curl_download(url, destfile = outfile, quiet = FALSE)
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

#' Download Google Slides File
#'
#' @param id Identifier of Google slides presentation, passed to
#' \code{\link{get_slide_id}}
#' @param out_type output type of file to download. Usually
#' `pdf` or `pptx`
#'
#' @note This downloads presentations if they are public and also try to make
#' sure it does not fail on large files
#' @return Downloaded file (in temporary directory)
#' @export
download_gs_file <- function(id, out_type = "pptx") {
  id <- as.character(id)
  id <- get_slide_id(id)
  url <- type_url(id = id, page_id = NULL, type = out_type)

  tfile <- tempfile(fileext = paste0(".", out_type))
  result <- httr::GET(url, httr::write_disk(tfile))
  warn_them <- FALSE
  fr_header <- result$headers$`x-frame-options`
  if (!is.null(fr_header)) {
    if (all(fr_header == "DENY")) {
      warn_them <- TRUE
    }
  }
  if (httr::status_code(result) >= 300) {
    warn_them <- TRUE
  }
  # don't write something if not really a pptx
  ctype <- result$headers$`content-type`
  if (httr::status_code(result) >= 400 &&
    !is.null(ctype) && grepl("html", ctype)) {
    file.remove(tfile)
  }
  if (grepl("ServiceLogin", result$url)) {
    warn_them <- TRUE
  }
  # if (result$times["redirect"] > 0) {
  #   warn_them = TRUE
  # }
  if (warn_them) {
    warning(
      paste0(
        "This presentation may not be available, ",
        "did you turn link sharing on?"
      )
    )
  }
  tfile
}
