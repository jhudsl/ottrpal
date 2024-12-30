#' Check URLs of all md,rmd, and qmd files
#'
#' @param path path to the bookdown or quarto course repository, must have a
#'   `.github` folder which will be used to establish the top of the repo.
#' @param output_dir A relative file path to the folder (existing or not) that the
#'   output check file should be saved to. Default is "check_reports"
#' @param resources_dir A relative file path to the folder (existing or not) that the
#'   ignore_urls.txt file and exclude_files.txt will be found. Default is "resources".
#'   If no ignore_urls.txt file and exclude_files.txt files are found, we will download one.
#' @param report_all Should all URLs that were tested be returned? Default is FALSE
#'   meaning only broken URLs will be reported in the url_checks.tsv file.
#' @return A file will be saved that lists the broken URLs will be saved to the specified output_dir.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rprojroot find_root has_dir
#' @importFrom tidyr unnest separate
#' @importFrom readr write_tsv
#'
#' @examples
#'
#' rmd_dir <- setup_ottr_template(dir = ".", type = "rmd", render = FALSE)
#'
#' check_urls(rmd_dir)
#'
#' # If there are broken URLs they will be printed in a list at "check_reports/url_checks.tsv"
#'
#' qmd_dir <- setup_ottr_template(dir = ".", type = "qmd", render = FALSE)
#'
#' check_urls(qmd_dir)
#'
check_urls <- function(path = ".",
                       output_dir = "check_reports",
                       resources_dir = "resources",
                       report_all = FALSE) {
  # Find .git root directory
  root_dir <- rprojroot::find_root(path = path, rprojroot::has_dir(".github"))

  resources_dir <- file.path(root_dir, resources_dir)
  output_dir <- file.path(root_dir, output_dir)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(resources_dir)) {
    dir.create(resources_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_file <- file.path(output_dir, "url_checks.tsv")
  ignore_urls_file <- file.path(resources_dir, "ignore-urls.txt")
  exclude_file <- file.path(resources_dir, "exclude_files.txt")

  # Read in ignore urls file if it exists
  if (file.exists(ignore_urls_file)) {
    ignore_urls <- readLines(ignore_urls_file)
  } else {
    ignore_urls <- ""
  }

  # Read in ignore urls file if it exists
  if (file.exists(exclude_file)) {
    exclude_file <- readLines(exclude_file)
  } else {
    exclude_file <- ""
  }

  # Only declare `.md` files but not the ones in the style-sets directory
  files <- list.files(path = root_dir, pattern = "md$", full.names = TRUE, recursive = TRUE)

  if (exclude_file[1] != "") files <- grep(paste0(exclude_file, collapse = "|"), files, invert = TRUE, value = TRUE)

  # Run this for all Rmds
  all_urls <- lapply(files, get_urls)

  # Write the file
  all_urls_df <- dplyr::bind_rows(all_urls)

  if (nrow(all_urls_df) > 0) {
    if (!report_all) {
      all_urls_df <- all_urls_df %>%
        dplyr::filter(urls_status == "failed") %>%
        readr::write_tsv(output_file)
    }
  } else {
    all_urls_df <- data.frame(errors = NA)
  }

  # Print out how many spell check errors
  write(nrow(all_urls_df), stdout())

  # Save spell errors to file temporarily
  readr::write_tsv(all_urls_df, output_file)

  message(paste0("Saved to: ", output_file))

  return(nrow(all_urls_df))
}


#' Test a URL
#'
#' @param url A single URL that will be checked whether it is real.
#' @param ignore_url A vector of URLs which to ignore.
#'
#' @return a logical TRUE/FALSE for whether the URL is legitimate.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET
#'
#' @examples /dontrun {
#'
#' # This should print out "failed"
#' test_url("https://notawebsiteaaaaaaa.com")
#'
#'
#' # This should print out "success"
#' test_url("https://github.com")
#' }
#'
test_url <- function(url, ignore_urls = "") {
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


#' Identify and collect URLs in a single rmd/qmd/md file
#'
#' @param file A file path to a rmd/qmd/md file that contains URLs to be check
#' @param ignore_url A vector of URLs which to ignore.
#'
#' @return a data.frame of all the URLs identified in the given rmd/qmd/md file
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes read_html html_attr
#' @import stringr
#' @importFrom stats na.omit
#' @importFrom utils head
#'
#'
#' @examples
#'
#' # Add in a URL error
#  writeLines("A URL error: https://notawebsiteaaaaaaa.com", "url_test_error.md")
#'
#' get_urls("url_test_error.md")
#'
get_urls <- function(file, ignore_urls = "") {
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
  all_tags <- c(
    html = html_tag,
    knitr = include_url_tag,
    ottrpal = include_slide_tag,
    markdown = markdown_tag,
    markdown_bracket = markdown_tag_bracket,
    other_http = http_gen
  )

  url_list <- sapply(all_tags, grep, content, value = TRUE)
  url_list$other_http <- setdiff(url_list$other_http, unlist(url_list[-6]))

  # Extract the urls only of each type
  if (length(url_list$html) > 0) {
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
  if (length(url_list$markdown_bracket) > 0) {
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
  urls <- gsub("\\'\\:$|\\'|\\:$|\\.$|\\)$|\\,$", "", urls)

  # Remove URLs that are in the ignore
  if (ignore_urls[1] != "") urls <- grep(paste0(ignore_urls, collapse = "|"), urls, invert = TRUE, value = TRUE)

  if (length(urls) > 0) {
    # Remove trailing characters
    urls_status <- sapply(urls, test_url, ignore_urls = ignore_urls)
    url_df <- data.frame(urls, urls_status, file)
    return(url_df)
  } else {
    message("No URLs found")
  }
}
