#' OTTRfy your repository
#' @description This script downloads all the files and sets up the folders you need to
#' OTTR-fy a repository that has markdown or R Markdown files
#' @param path What's the file path we are making an OTTR. Needs to be a directory
#' @param type Can be "rmd", "quarto" "rmd_web", or "quarto_web" depending on what
#' kind of OTTR site you'd like to make
#' @param overwrite TRUE or FALSE existing files should be overwritten?
#' @return Information regarding a Github account
#' @export
#'
#' @examples \dontrun{
#'
#' ottrfy(type = "rmd")
#'
#' ottrfy(type = "quarto")
#'
#' ottrfy(type = "rmd_web")
#'
#' ottrfy(type = "quarto_web")
#'
#' }
#'
ottrfy <- function(path = ".", type = "rmd", overwrite = FALSE) {

  stopifnot(
    "type must be one of rmd, quarto, rmd_web or quarto_web" = type %in%
    c("rmd", "quarto", "rmd_web", "quarto_web")
  )

  # Find .git root directory
  root_dir <- file.path(rprojroot::find_root(rprojroot::has_dir(path)), basename(path))

  always_needed_files <- c(
    ".github/workflows/check-url.yml",
    ".github/workflows/pull_request.yml",
    ".github/workflows/delete-preview.yml",
    "config_automation.yml",
    "resources/dictionary.txt",
    "resources/ignore-urls.txt",
    "resources/exclude_files.txt"
  )

  special_files <- switch(type,
    rmd = c("index.Rmd", "_bookdown.yml", "_output.yml", "assets/big-image.html",
            "assets/big-image.html", "assets/footer.html", "book.bib",
            ".github/workflows/render-all.yml", "assets/open-new-tab.html"),
    quarto = c("index.qmd", "_quarto.yml", "references.bib",
               ".github/workflows/render-all.yml", "img/favicon.ico", "img/logo.png"),
    rmd_web = c("_site.yml", "styles.css", ".github/workflows/render-site.yml",
                "resources/header.html"),
    quarto_web = c("_quarto.yml", "styles.css", ".github/workflows/render-site.yml")
  )

  base_url <- switch(type,
    rmd = "https://raw.githubusercontent.com/jhudsl/OTTR_Template/refs/heads/main/",
    quarto = "https://raw.githubusercontent.com/fhdsl/OTTR_Quarto/refs/heads/main/",
    rmd_web = "https://raw.githubusercontent.com/jhudsl/OTTR_Template_Website/refs/heads/main/",
    quarto_web = "https://raw.githubusercontent.com/fhdsl/OTTR_Quarto_Website/refs/heads/main/"
  )

  all_files_needed <- c(always_needed_files, special_files)

  # Set up a file list with the destination locations as the names
  url_to_files <- paste0(base_url, all_files_needed)
  names(url_to_files) <- file.path(root_dir, all_files_needed)

  # Download the file in the respective place
  for (index in 1:length(url_to_files)) {
    dest_folder <- dirname(names(url_to_files)[index])
    if (!dir.exists(dest_folder)) {
      dir.create(dest_folder, recursive = TRUE)
    }
    # If overwrite is set to true go ahead and download
    if (overwrite) {
      download.file(url = url_to_files[index], destfile = names(url_to_files)[index], method = "wget")
    } else {
      # otherwise we have to check if the file exists first
      if (!file.exists(names(url_to_files)[index])) {
        download.file(url = url_to_files[index], destfile = names(url_to_files)[index], method = "wget")
      } else {
        message(names(url_to_files)[index], ": Already exists and overwrite = FALSE so skipping download of this file.")
      }
    }
  }

  if (type == "quarto") {
    dir.create(file.path("resources", "images", "figure"), recursive = TRUE, showWarnings= FALSE)
  }

}
