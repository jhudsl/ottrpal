#' Check URLs and create a summary of the URL checks
#'
#' @return A data frame containing three columns: `urls`, `urls_status`, `file`
#' @export
url_check <- function() {
  # Find .git root directory
  root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

  output_file <- file.path(root_dir, 'check_reports', 'url_checks.tsv')
  # If check_reports folder doesn't exist, create
  if (!dir.exists('check_reports')) {
    dir.create('check_reports')
  }

  # Declare ignore_urls file
  ignore_urls_file <- system.file("extdata", "checks", "ignore-urls.txt", package = "ottrpal")
  # Declare exclude_files.txt
  exclude_file <- system.file("extdata", "checks", "exclude_files.txt", package = "ottrpal")

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
  files <- list.files(pattern = 'md$', full.names = TRUE, recursive = TRUE)
  if( exclude_file[1] != "") files <- grep(paste0(exclude_file, collapse = "|"), files, invert = TRUE, value = TRUE)


  # Run this for all Rmds
  all_urls <- lapply(files, get_urls, ignore_urls)

  # Write the file
  all_urls_df <- dplyr::bind_rows(all_urls) %>%
    dplyr::filter(!(urls %in% ignore_urls))

  if (nrow(all_urls_df) > 0) {
    all_urls_df <- all_urls_df %>%
      dplyr::filter(urls_status == "failed") %>%
      readr::write_tsv(output_file)
  } else {
    all_urls_df <- data.frame(errors = NA)
  }

  # Print out how many spell check errors
  write(nrow(all_urls_df), stdout())

  # Save spell errors to file temporarily
  readr::write_tsv(all_urls_df, output_file)
  message(paste0("Saved to: ", output_file))
}
