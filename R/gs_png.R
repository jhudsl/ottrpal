#' Get Presentation ID from URL
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
#' get_presentation_id(x)
get_presentation_id <- function(x) {
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
#' id <- get_presentation_id(url)
#' gs_png_url(url)
gs_png_url <- function(url) {
  presentation_id <- get_presentation_id(url)
  slide_id <- get_slide_id(url)
  gs_png_id(presentation_id, slide_id)
}

# Get URL to download slide as PNG
gs_png_id <- function(presentation_id, slide_id) {
  if (any(grepl("^id[.]", slide_id))) {
    warning(
      "slide ids usually don't have format of id.gc*, ",
      "you should likely remove the id."
    )
  }
  paste0(
    "https://docs.google.com/presentation/d/",
    presentation_id,
    "/export/png?id=", presentation_id,
    "&pageid=", slide_id
  )
}

#' Extract Slide ID from URL
#' @export
#' @rdname gs_png_url
get_slide_id <- function(url) {
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

#' Download a slide from Google Slides
#' @export
#' @rdname gs_png_url
#' @param output_dir path to output png
#' @param overwrite should the slide PNG be overwritten?
gs_png_download <- function(url, output_dir = ".", overwrite = TRUE) {
  presentation_id <- get_presentation_id(url)
  slide_id <- get_slide_id(url)
  url <- gs_png_url(url)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  outfile <- file.path(output_dir, paste0(presentation_id, "_", slide_id, ".png"))
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
  # Get speaker notes for ALL slides
  all_speaker_notes <- get_object_id_notes(url)

  # Get slide speaker notes
  slide_id <- get_slide_id(url)
  slide_speaker_notes <- all_speaker_notes[all_speaker_notes$id == slide_id, "notes"]

  alt_text <<- slide_speaker_notes
  outfile <- gs_png_download(url, output_dir, overwrite = overwrite)
  knitr::include_graphics(outfile, ...)
}
