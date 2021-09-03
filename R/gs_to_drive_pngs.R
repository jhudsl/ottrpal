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
download_gs_file = function(id, out_type = "pptx") {
  id = as.character(id)
  id = get_slide_id(id)
  url = type_url(id = id, page_id = NULL, type = out_type)

  tfile = tempfile(fileext = paste0(".", out_type))
  result = httr::GET(url, httr::write_disk(tfile))
  warn_them = FALSE
  fr_header = result$headers$`x-frame-options`
  if (!is.null(fr_header)) {
    if (all(fr_header == "DENY")) {
      warn_them = TRUE
    }
  }
  if (httr::status_code(result) >= 300) {
    warn_them = TRUE
  }
  # don't write something if not really a pptx
  ctype = result$headers$`content-type`
  if (httr::status_code(result) >= 400 &&
      !is.null(ctype) && grepl("html", ctype)) {
    file.remove(tfile)
  }
  if (grepl("ServiceLogin", result$url)) {
    warn_them = TRUE
  }
  # if (result$times["redirect"] > 0) {
  #   warn_them = TRUE
  # }
  if (warn_them) {
    warning(
      paste0(
        "This presentation may not be available, ",
        "did you turn link sharing on?")
    )
  }
  tfile
}

#' Convert Google Slides to PNGs and Upload to Drive folder
#'
#' @param id ID or URL to Google slides presentation.
#' @param folder_name folder to upload to on Google Drive.  If no folder
#' exists, then one will be created.  Also a `dribble` can be passed
#' @param verbose print diagnostic messages, higher numbers give more output
#' @param overwrite Overwrite the PNGs if they exist, passed to
#' \code{\link{drive_upload}}
#'
#' @return A list of results
#' @export
#'
#' @examples
#' \dontrun{
#' url <- paste0(
#'   "https://docs.google.com/presentation/d/",
#'   "1ywZbtFacZK0UIsnt2g-sheC9du_rw_7XZ1FX4rRt27M",
#'   "/export/png?",
#'   "id=1ywZbtFacZK0UIsnt2g-sheC9du_rw_7XZ1FX4rRt27M&pageid=g325fd519ca_0_5"
#' )
#'
#' out <- gs_to_drive_pngs(url)
#' }
gs_to_drive_pngs <- function(
                             id,
                             folder_name = "leanpub_pngs",
                             verbose = TRUE,
                             overwrite = TRUE) {
  id <- get_slide_id(id)
  # pdf_file = download_gs_file(id = path, out_type = "pdf")
  if (!requireNamespace("didactr", quietly = TRUE)) {
    stop(
      "please install didactr for gs_to_drive_pngs: \n",
      "remotes::install_github('jhudsl/didactr')"
    )
  }
  result <- didactr::gs_convert(
    id,
    PPTX = FALSE,
    use_gs_pngs = FALSE,
    use_gs_ids = TRUE
  )

  images <- file.path(
    dirname(result$images),
    paste0("id=", id, "&page_id=", basename(result$images))
  )
  file.rename(result$images, images)

  if (googledrive::is_dribble(folder_name)) {
    trans_fol <- folder_name
  } else {
    # folder_name = id
    trans_fol <- googledrive::drive_find(
      pattern = folder_name,
      type = "folder", n_max = 100,
      verbose = TRUE
    )
    if (nrow(trans_fol) == 0) {
      trans_fol <- googledrive::drive_mkdir(
        folder_name,
        verbose = verbose
      )
    }
    if (nrow(trans_fol) > 1) {
      warning("Multiple folders found, choosing first one")
      print(trans_fol[1, ])
    }
    trans_fol <- trans_fol[1, ]
  }

  output <- lapply(images, function(image) {
    out <- googledrive::drive_upload(
      media = image,
      path = trans_fol,
      name = basename(image),
      overwrite = overwrite,
      verbose = verbose > 1
    )
  })
  result$images <- images
  output <- do.call(rbind, output)
  output$url <- sapply(output$drive_resource, `[[`, "webViewLink")
  result$upload_output <- output
  result
}
