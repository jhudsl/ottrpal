
#' Set image path for `knitr`
#'
#' @param verbose print out what the figure path is
#'
#' @return NULL
#' @export
set_knitr_image_path = function(verbose = FALSE) {
  fp <- knitr::fig_path()
  fp <- dirname(fp)
  fp <- paste0("resources/images/", fp, "/")
  if (verbose) {
    message(paste0("figpath is ", fp))
  }
  knitr::opts_chunk$set(fig.path = fp)
}
