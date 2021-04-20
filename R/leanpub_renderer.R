#' Renderer for Leanpub for `knitr`
#'
#' @param x an object
#' @param options options for the thing
#'
#' @return A string
#' @export
leanpub_renderer <- function(x, options) {
  if (inherits(x, "knit_image_paths")) {
    # Alternative text
    alt = options$alt = paste0('"', options$alt, '"')
    opts_list = list(
      alt = options$alt
    )

    if (!is.null(options$full_bleed)) {
      options$full_bleed = as.logical(options$full_bleed)
    }
    if (!is.null(options$fullbleed) && is.null(options$full_bleed)) {
      options$full_bleed = as.logical(options$fullbleed)
    }
    opts_list$fullbleed = options$full_bleed

    if (!is.null(options$fig.align)) {
      options$fig.align = tolower(options$fig.align)
      if (options$fig.align == "center") {
        options$fig.align = "middle"
      }
    }
    opts_list$align = options$fig.align

    if (!is.null(options$float)) {
      options$float = tolower(options$float)
    }
    # we don't want to override defaults
    nullpaste0 = function(x) {
      if (is.null(x)) {
        return(NULL)
      }
      paste0('"', x, '"')
    }
    options$caption = nullpaste0(options$caption)
    options$class = nullpaste0(options$class)

    direct_copy_list = c("type", "caption", "class", "format", "float")
    for (ival in direct_copy_list) {
      opts_list[[ival]] = options[[ival]]
    }

    opts_list$width = options$out.width
    opts_list$height = options$out.height
    opts_list = mapply(function(x, y) {
      paste0(x, ": ", y)
    }, names(opts_list), opts_list, SIMPLIFY = TRUE)
    opts_list = paste(opts_list, collapse = ", ")
    opts_list = paste0("{", opts_list, "}\n")


    x = paste0(opts_list,
               '![', alt ,'](', x, ')')
    # class(x) = setdiff(class(x), "knit_image_paths")
    class(x) = "knit_asis"
  }
  knitr::knit_print(x, options=options)
  # could also knit_print multiple things with another `knit_print` call
}
