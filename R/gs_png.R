#' Title
#'
#' @param id
#' @param slide_id
#'
#' @return
#' @export
#'
#' @examples
#' url = paste0("https://docs.google.com/presentation/d/",
#' "12DPZgPteQBwgal6kSPP58zhPhjZ7QSPZLe3NkA8M3eo/edit",
#' "#slide=id.gc8648f14c3_0_397&t=4")
#' id = ariExtra::get_slide_id(url)
#' slide_id = get_slide_page(url)
#' gs_png_url(id, slide_id)
gs_png_url = function(id, slide_id) {
  if (any(grepl("^id[.]", slide_id))) {
    warning("slide ids usually don't have format of id.gc*, ",
            "you should likely remove the id.")
  }
  paste0("https://docs.google.com/presentation/d/",
         id,
         "/export/png?id=", id,
         "&pageid=", slide_id)
}

get_slide_page = function(url) {
  parsed = httr::parse_url(url)
  slide_id = parsed$query$pageid
  if (nchar(slide_id) == 0) {
    fragment = parsed$fragment
    slide_id = sub("slide=(.*)", "\\1", fragment)
    slide_id = sub("&.*", "", slide_id)
    slide_id = sub("^id[.]", "", slide_id)
  }
  stopifnot(nchar(slide_id) > 0)
  slide_id
}

#' @export
#' @param output_dir path to output png
gs_png_download = function(id, slide_id, output_dir = ".") {
  url = gs_png_url(id, slide_id)
  outfile = file.path(output_dir, paste0(id, "_", slide_id, ".png"))
  curl::curl_download(url, destfile = outfile, quiet = FALSE)
  stopifnot(file.exists(outfile))
  outfile
}

