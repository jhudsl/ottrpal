.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Use the authorize() function to begin. This gives the package the proper credentials to run.")
}


utils::globalVariables(c(
  "num", "quiz_dir", "type_url", "file_name", "trimmed", "quiz",
  "quiz_path", "type", "q_num", "verbose", "chapt_title", "data_path", "image_dir",
  "convert_footnotes", "rmd_files"
))


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

test_url <- function(url, ignore_urls) {

  if (url %in% ignore_urls) {
    message(paste0("Ignoring: ", url))
    return("ignored")
  }

  message(paste0("Testing: ", url))

  url_status <- try(httr::GET(url), silent = TRUE)

  # Fails if host can't be resolved
  status <- ifelse(suppressMessages(grepl("Could not resolve host", url_status)), "failed", "success")

  if (status == "success") {
    # Fails if 404'ed
    status <- ifelse(try(url_status$status_code, silent = TRUE) == 404, "failed", "success")
  }

  return(status)
}

get_urls <- function(file, ignore_urls) {

  message(paste("##### Testing URLs from file:", file))

  # Read in a file and return the urls from it
  content <- readLines(file)

  # Set up the possible tags
  html_tag <- "<a href="
  include_url_tag <- "include_url\\("
  include_slide_tag <- "include_slide\\("
  markdown_tag <- "\\[.*\\]\\(http[s]?.*\\)"
  markdown_tag_bracket <- "\\[.*\\]: http[s]?"
  http_gen <- "http[s]?"
  url_pattern <- "[(|<]?http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  # Other patterns
  nested_parens <- "\\((.*)\\((.*)\\)(.*)\\)"
  outermost_parens <- "^\\((.*)\\)(.*)$"

  # Collect the different kinds of tags in a named vector
  all_tags <- c(html = html_tag,
                knitr = include_url_tag,
                ottrpal = include_slide_tag,
                markdown = markdown_tag,
                markdown_bracket = markdown_tag_bracket,
                other_http = http_gen)

  url_list <- sapply(all_tags, grep, content, value = TRUE)
  url_list$other_http <- setdiff(url_list$other_http, unlist(url_list[-6]))

  # Extract the urls only of each type
  if (length(url_list$html) > 0 ){
    url_list$html <- sapply(url_list$html, function(html_line) {
      head(rvest::html_attr(rvest::html_nodes(rvest::read_html(html_line), "a"), "href"))
    })
    url_list$html <- unlist(url_list$html)
  }
  url_list$knitr <- stringr::word(url_list$knitr, sep = "include_url\\(\"|\"\\)", 2)
  url_list$ottrpal <- stringr::word(url_list$ottrpal, sep = "include_slide\\(\"|\"\\)", 2)

  # Check markdown for parentheticals outside of [ ]( )
  parens_index <- sapply(url_list$markdown, stringr::str_detect, nested_parens)

  if (length(parens_index) >= 1) {
    # Break down to parenthetical only
    url_list$markdown[parens_index] <- stringr::str_extract(url_list$markdown[parens_index], nested_parens)
    # Remove parentheticals outside [ ]( )
    url_list$markdown[parens_index] <- stringr::word(stringr::str_replace(url_list$markdown[parens_index], outermost_parens, "\\1"), sep = "\\]", 2)

    url_list$markdown[!parens_index] <- stringr::word(url_list$markdown[!parens_index], sep = "\\]", 2)
    url_list$markdown <- grep("http", url_list$markdown, value = TRUE)
  }
  if (length(url_list$markdown_bracket) > 0 ){
    url_list$markdown_bracket <- paste0("http", stringr::word(url_list$markdown_bracket, sep = "\\]: http", 2))
  }
  url_list$other_http <- stringr::word(stringr::str_extract(url_list$other_http, url_pattern), sep = "\\]", 1)

  # Remove parentheses only if they are on the outside
  url_list$other_http <- stringr::word(stringr::str_replace(url_list$other_http, outermost_parens, "\\1"), sep = "\\]", 1)
  url_list$markdown <- stringr::word(stringr::str_replace(url_list$markdown, outermost_parens, "\\1"), sep = "\\]", 1)

  # Remove `< >`
  url_list$other_http <- stringr::word(stringr::str_replace(url_list$other_http, "^<(.*)>(.*)$", "\\1"), sep = "\\]", 1)

  # If after the manipulations there's not actually a URL, remove it.
  url_list <- lapply(url_list, na.omit)

  # collapse list
  urls <- unlist(url_list)

  # Remove trailing characters
  urls <- gsub("\\'\\:$|\\'|\\:$|\\.$", "", urls)

  if (length(urls) > 0 ){
    # Remove trailing characters
    urls_status <- sapply(urls, test_url, ignore_urls)
    url_df <- data.frame(urls, urls_status, file)
    return(url_df)
  } else {
    message("No URLs found")
  }
}


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
