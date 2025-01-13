
#' OTTRfy your repository
#' @description This script downloads all the files and sets up the folders you need to
#' OTTR-fy a repository that has markdown or R Markdown files
#' @param path What's the file path we are making an OTTR
#' @param type Essentials
#' @return Information regarding a Github account
#' @export
#'
#' examples \dontrun{
#'
#'}
#'
ottrfy <- function(path = ".") {

system("git checkout -b 'robot/ottr-fy'")

# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

bookdown_base_url <- "https://raw.githubusercontent.com/jhudsl/OTTR_Template/main/"
quarto_base_url <- "https://raw.githubusercontent.com/fhdsl/OTTR_Quarto/main/"

needed_files <- c(
  ".github/workflows/pull_request.yml",
  ".github/workflows/render-all.yml",
  ".github/workflows/delete-preview.yml",
  "_bookdown.yml",
  "_output.yml",
  "book.bib",
  "config_automation.yml",
  "assets/big-image.html",
  "assets/footer.html",
  "resources/dictionary.txt",
  "resources/ignore-urls.txt",
  "resources/exclude_files.txt"
  )

bookdown_files <- c("_bookdown.yml", "_output.yml", "assets/big-image.html", "assets/footer.html", "book.bib")
quarto_files <- c("_quarto.yml", "_output.yml", "assets/big-image.html", "assets/footer.html", "book.bib")

# If this is bookdown, we don't want to copy over the bookdown.yml or output.yml files
if (opt$bookdown) {
  needed_files <- setdiff(needed_files,
  c("_bookdown.yml", "_output.yml", "assets/big-image.html", "assets/footer.html", "book.bib"))
}

# Set up a file list with the destination locations as the names
url_to_files <- paste0(base_url, needed_files)
names(url_to_files) <-  file.path(root_dir, needed_files)

# Download the file in the respective place
for (index in 1:length(url_to_files)) {
  dest_folder <- dirname(names(url_to_files)[index])
  if (!dir.exists(dest_folder)){
    dir.create(dest_folder, recursive = TRUE)
  }
  download.file(url = url_to_files[index], destfile = names(url_to_files)[index])
}

system("git add .")
system("git config commit.gpgsign false")
system("git commit -m 'Add ottr-fying files'")
system("git push --set-upstream origin robot/ottr-fy")

}
