
#' Check Leanpub Course or Book
#'
#' @param path path to the Leanpub book/course
#' @param verbose print diagnostic messages
#'
#' @return A list of output files and diagnostics
#' @export
#'
leanpub_check = function(path = ".",
                         verbose = TRUE) {
  if (verbose) {
    message("Checking the Book.txt files")
  }
  check_book_txt(path = path)
  extra_book_result = check_extra_md_files(path = path)

  if (verbose) {
    message("Checking if HTML is present")
  }
  html_result = full_html_check(path = path)
  attribute_result = full_attribute_check(path = path)

  files = get_md_files(path)

  L = list(
    extra_book_result = extra_book_result,
    html_result = html_result,
    attribute_result = attribute_result,
  )
  return(L)
}

check_book_txt = function(path = ".") {
  file = file.path(path, "Book.txt")
  # if Book.txt not there, fail
  stopifnot(file.exists(file))
  x = readLines(file, warn = FALSE)
  files = file.path(path, x)
  fe = file.exists(files)
  if (!all(fe)) {
    msg = paste(x[fe], collapse = ", ")
    msg = paste0("Book.txt has files specified, but missing: ", msg)
    stop(msg)
  }
  return(NULL)
}
get_md_files = function(path) {
  files = list.files(pattern = ".md$", ignore.case = TRUE, path = path)
  files
}

check_extra_md_files = function(path = ".") {
  files = get_md_files(path)
  bn = basename(files)
  file = file.path(path, "Book.txt")
  # if Book.txt not there, fail
  stopifnot(file.exists(file))
  x = readLines(file, warn = FALSE)
  sd = setdiff(files, x)
  if (length(sd) > 0) {
    sd = paste(sd, collapse = ", ")
    warning(
      "Markdown files exist in path but are not in Book.txt, may be left out ",
      sd)
    return(FALSE)
  }
  return(FALSE)
}

full_html_check = function(path = ".") {
  files = get_md_files(path)
  out = lapply(files, check_html)
  names(out) = basename(files)
  html_result = any(sapply(out, function(x) x$result))
  if (any(html_result)) {
    tag_list = mapply(function(x, name) {
      if (!x$result) return(NULL)
      paste0("file ", name, ": ", paste(x$tags_found, collapse = ", "))
    }, SIMPLIFY = FALSE)
    tag_list = c(unlist(tag_list))
    tag_list = paste(tag_list, collapse = "\n\n")
    message(tag_list)
    warning(tag_list)
    return(FALSE)
  }
  return(TRUE)
}

check_html = function(x) {
  if (length(x) == 1 && file.exists(x)) {
    x = readLines(x, warn = FALSE)
  }
  # taken from dput(names(shiny::tags))
  tags_to_check = c(
    "a", "abbr", "address", "animate", "animateMotion", "animateTransform",
    "area", "article", "aside", "audio", "b", "base", "bdi", "bdo",
    "blockquote", "body", "br", "button", "canvas", "caption", "circle",
    "cite", "clipPath", "code", "col", "colgroup", "color-profile",
    "command", "data", "datalist", "dd", "defs", "del", "desc", "details",
    "dfn", "dialog", "discard", "div", "dl", "dt", "ellipse", "em",
    "embed", "eventsource", "feBlend", "feColorMatrix", "feComponentTransfer",
    "feComposite", "feConvolveMatrix", "feDiffuseLighting", "feDisplacementMap",
    "feDistantLight", "feDropShadow", "feFlood", "feFuncA", "feFuncB",
    "feFuncG", "feFuncR", "feGaussianBlur", "feImage", "feMerge",
    "feMergeNode", "feMorphology", "feOffset", "fePointLight", "feSpecularLighting",
    "feSpotLight", "feTile", "feTurbulence", "fieldset", "figcaption",
    "figure", "filter", "footer", "foreignObject", "form", "g", "h1",
    "h2", "h3", "h4", "h5", "h6", "hatch", "hatchpath", "head", "header",
    "hgroup", "hr", "html", "i", "iframe", "image", "img", "input",
    "ins", "kbd", "keygen", "label", "legend", "li", "line", "linearGradient",
    "link", "main", "map", "mark", "marker", "mask", "menu", "meta",
    "metadata", "meter", "mpath", "nav", "noscript", "object", "ol",
    "optgroup", "option", "output", "p", "param", "path", "pattern",
    "picture", "polygon", "polyline", "pre", "progress", "q", "radialGradient",
    "rb", "rect", "rp", "rt", "rtc", "ruby", "s", "samp", "script",
    "section", "select", "set", "slot", "small", "solidcolor", "source",
    "span", "stop", "strong", "style", "sub", "summary", "sup", "svg",
    "switch", "symbol", "table", "tbody", "td", "template", "text",
    "textarea", "textPath", "tfoot", "th", "thead", "time", "title",
    "tr", "track", "tspan", "u", "ul", "use", "var", "video", "view",
    "wbr")
  agrepl = function(...) {
    any(grepl(...))
  }
  start_strings = paste("<\\s*", tags_to_check, ".*>")
  start_res = sapply(start_strings, function(pattern) {
    agrepl(x = x, pattern = pattern)
  })
  end_strings = paste("<\\s*/", tags_to_check, ".*>")
  end_res = sapply(start_strings, function(pattern) {
    agrepl(x = x, pattern = pattern)
  })
  res = start_res | end_res
  tag_df = tibble::tibble(
    tag = tags_to_check,
    start_present = start_res,
    end_present = end_res,
    present = res)
  L = list(
    result = any(res),
    tag_data = tag_df
  )
  if (any(res)) {
    L$tags_found = tag_df$tag[tag_df$present]
  }
  L
}

is_html_present = function(x) {
  out = check_html(x)
  out$result
}



make_sure_attributes_above = function(x) {
  if (length(x) == 1 && file.exists(x)) {
    x = readLines(x, warn = FALSE)
  }
  index = lag_attribute = lag_trimmed = trimmed = link = link_index = NULL
  rm(list = c("link_index", "link", "trimmed", "lag_trimmed",
              "lag_attribute", "index"))
  df = tibble::tibble(
    x = x,
    index = 1:length(x),
    trimmed = trimws(x)
  ) %>%
    dplyr::mutate(
      link = grepl(trimmed, pattern = "^!\\s*\\[.*\\]\\s*\\("),
      attributes = grepl(pattern = "^\\{", trimmed),
      lag_trimmed = dplyr::lag(trimmed, n = 1),
      lag_attribute = grepl(pattern = "^\\{", lag_trimmed)
    )
  if (!any(df$link)) {
    return(TRUE)
  }
  bad = df %>%
    dplyr::filter(link & !lag_attribute)
  if (nrow(bad) > 0) {
    bad_lines = df %>%
      dplyr::filter(index %in% c(bad$index, bad$index-1)) %>%
      dplyr::select(x, index) %>%
      as.data.frame()
    warning("Links without attributes in the last")
    print(bad_lines)
    return(FALSE)
  }
  return(TRUE)
}

# See if any filenames are duplicated in resources/ and resources/images/
# just PNGs
# list.files()
# if image specified, see if image actually exists *somewhere*

full_attribute_check = function(path = ".") {
  files = get_md_files(path)
  out = lapply(files, make_sure_attributes_above)

}














