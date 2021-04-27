
get_html_element = function(x, element = "img") {
  x = paste(x, collapse = "\n")
  doc = xml2::read_html(x)
  nodes = rvest::html_nodes(doc, xpath = paste0("//", element))
  nodes
}
get_html_attr = function(x, element = "img", name = "src") {
  x = get_html_element(x, element = element)
  rvest::html_attr(x, name)
}

get_iframe_attr = function(x, name = "src") {
  x = get_html_attr(x, element = "iframe", name = name)
}

get_img_attr = function(x, name = "src") {
  x = get_html_attr(x, element = "img", name = name)
}

get_iframe_src = get_iframe_attr
get_img_src = get_img_attr

get_iframe_alt = function(x) {
  get_iframe_attr(x, name = "alt")
}

get_img_alt = function(x) {
  get_img_attr(x, name = "alt")
}



find_iframe = function(x) {
  regex = paste0('<iframe')
  grepl(regex,  x )
}


find_img = function(x) {
  regex = paste0('<img')
  xx = grepl(regex, x = x)
}

# don't need this as
# https://leanpub.com/markua/read#leanpub-auto-adding-a-link-around-an-image
# find_double = function(x) {
#   x = "[![](resources/images/unnamed-chunk-3-1.png)](http://www.youtube.com/embed/9bZkp7q19f0?rel=0)"
#   regex = "\\[!\\["
#   # image_tag <- "{alt: 'an image', width=80%}"
# }
na_empty = function(x) {
  x[is.na(x)] = ""
  x
}

split_style = function(x) {
  xx = strsplit(x, ";")[[1]]
  xx = strsplit(xx, ":")
  xx = lapply(xx, trimws)
  xx = lapply(xx, gsub, pattern = "\\s+", replacement = " ")
  att = sapply(xx, function(x) x[1])
  xx = lapply(xx, function(x) x[2:length(x)])
  names(xx) = att
  xx
}

get_margin = function(x) {
  out = split_style(x)
  margin = out$margin
  if (length(margin) > 0) {
    margin = strsplit(margin, " ")[[1]]
  }
  margin = c(margin, rep("0", length = 4-length(margin)))
  names(margin) = c("top", "right", "bottom", "left")
  margin
}



replace_single_html = function(file) {
  stopifnot(length(file) == 1 && file.exists(file))
  x = readLines(file, warn = FALSE)
  image_index = find_img(x)
  images = x[image_index]
  attributes = c("src", "alt", "height", "width", "style")
  # style="display: block; margin: auto;" is center
  image_attributes = lapply(images, function(x) {
    out = lapply(attributes, function(name) {
      na_empty(get_img_attr(x = x, name = name))
    })
    names(out) = attributes
    out$margin = get_margin(out$style)
    out
  })

  build_image = function(src, alt, height, width, align) {

  }

}



#' Replace HTML and other Tags in Leanpub Markdown
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param verbose print diagnostic messages
#'
#' @return A list of output files and diagnostics
#' @export
#'
#' @examples
replace_html = function(path = "manuscript",
                        verbose = TRUE) {
  md_files = list.files(path = path, pattern = "[.]md$", ignore.case = TRUE,
                        full.names = TRUE)
  md_files = lapply(md_files, replace_single_html)

}
