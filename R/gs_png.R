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
#' id <- ariExtra::get_slide_id(url)
#' gs_png_url(url)
gs_png_url <- function(url) {
  id <- ariExtra::get_slide_id(url)
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
  id <- ariExtra::get_slide_id(url)
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
