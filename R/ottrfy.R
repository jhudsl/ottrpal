#' OTTRfy your repository
#' @description This script downloads all the files and sets up the folders you need to
#' OTTR-fy a repository that has markdown or R Markdown files
#' @param path What's the file path we are making an OTTR. Needs to be a directory
#' @param type Can be "rmd", "quarto" "rmd_web", or "quarto_web" depending on what
#' kind of OTTR site you'd like to make
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
ottrfy <- function(path = ".", type = "rmd", git_commit = TRUE, overwrite = FALSE) {

  stopifnot(
    "type must be one of rmd, quarto, rmd_web or quarto_web" = type %in%
    c("rmd", "quarto", "rmd_web", "quarto_web")
  )
  if (git_commit) {
    system(paste("cd", path))
    system("git checkout -b 'robot/ottr-fy'")
  }

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

  if (git_commit) {
    system("git add .")
    system("git config commit.gpgsign false")
    system("git commit -m 'Add ottr-fying files'")
    system("git push --set-upstream origin robot/ottr-fy")
  }
}


#' Update chapters
#' @description This updates the chapter list in the _bookdown.yml or _quarto.yml
#' Default behavior is to sort the chapters but have index come first.
#' @param path What's the file path to where we are updating chapters?
#' @param ignore_files Are there qmd or rmd files that are not chapters and should be ignored?
#' @return an updated _bookdown.yml or _quarto.yml with all chapters.
#' @export
#'
#' @examples \dontrun{
#'
#' update_chapters()
#'
#' }
#'
update_chapters <- function(path = ".", ignore_files = NULL) {

  chapters <- list.files(path = path, pattern = "qmd$|rmd$", full.names = TRUE, ignore.case = TRUE)

  if (!is.null(ignore_files)) {
    chapters <- grep(chapters, ignore_files, invert = TRUE, value = TRUE)
  }

  index_exists <- list.files(path = path, pattern = "index.[q|R]md")
  if (length(index_exists) > 0) {
    chapters <- c(index_exists, grep(index_exists, sort(basename(chapters)), value = TRUE, invert = TRUE))
  }

  # Update the file
  chapter_yml_file <- list.files(path = path, pattern = "_quarto.yml|_bookdown.yml", recursive = TRUE, full.names = TRUE)
  chapter_yml <- yaml::read_yaml(chapter_yml_file)

  if ("rmd_files" %in% names(chapter_yml)) chapter_yml$rmd_files <- chapters

  if ("book" %in% names(chapter_yml)) {
    chapter_yml$book$chapters <- yaml::as.yaml(lapply(chapters, function(x) x), indent = 4)
    chapter_yml$knitr$opts_chunk$fig.path <- yaml::as.yaml("resources/images/figure/")
  }
  yaml::write_yaml(chapter_yml, chapter_yml_file)


  message("yml file updated with new chapters: ", chapter_yml_file)

  invisible(chapter_yml)
}
