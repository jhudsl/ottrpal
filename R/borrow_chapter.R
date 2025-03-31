#' Borrow/link a chapter from another bookdown course
#'
#' @description If you have two courses that the content and topics overlap,
#' you may want to share written material between the two.
#' But, if you copy and paste to share material this would create a maintenance
#' problem because if you update one you will need to remember to copy over the
#' other! 
#' To borrow a chapter from another course, create an .Rmd as you normally would,
#'  with an [`H1` title](https://www.markdownguide.org/basic-syntax/) if you wish.
#' Then in a code chunk, use cow::borrow_chapter() to have the content from an
#' Rmd from another repository knitted into the Rmd.
#'
#' @param doc_path A file path of markdown or R Markdown
#' document of the chapter in the repository you are retrieving it from that
#' you would like to include in the current document. e.g "docs/intro.md" or
#' "intro.md"
#' @param repo_name A character vector indicating the repo name of where you are
#'  borrowing from. e.g. "jhudsl/OTTR_Template/".
#' For a Wiki of a repo, use "wiki/jhudsl/OTTR_Template/"
#' If nothing is provided, will look for local file.
#' @param remove_h1 If TRUE Remove all h1 headers.
#' @param tag_replacement An optional list of tags that need to be replaced in
#' the child document upon bringing it into the render.
#' @param branch Default is to pull from main branch, but need to declare if
#' other branch is needed.
#' @param token A personal access token from GitHub. Only necessary if the
#' repository being checked is a private repository.
#' @param base_url it's assumed this is coming from github so it is by default
#' 'https://raw.githubusercontent.com/'
#' @param dest_dir A file path where the file should be stored upon arrival to
#' the current repository.
#'
#' @return An Rmarkdown or markdown is knitted into the document from another repository
#'
#' @importFrom knitr opts_knit
#' @importFrom knitr knit_child
#' @importFrom knitr current_input
#' @importFrom utils download.file
#' @importFrom rprojroot find_root
#' @importFrom stringr str_replace_all
#' @export
#'
#' @examples \dontrun{
#'
#' # In an Rmarkdown document:
#'
#' # For a file in another repository:
#' # ```{r, echo=FALSE, results='asis'}
#' borrow_chapter(
#'   doc_path = "docs/02-chapter_of_course.md",
#'   repo_name = "jhudsl/OTTR_Template"
#' )
#' # ```
#'
#' # For a local file:
#' # ```{r, echo=FALSE, results='asis'}
#' borrow_chapter(doc_path = "02-chapter_of_course.Rmd")
#' # ```
#'
#' tag_replacement_list <- list(
#'   "{A_TAG}" = "replacement here",
#'   "{TEMPLATE_URL}" = "https://www.ottrproject.org/",
#'   "{SECOND_TAG}" = "some other replacement here"
#' )
#'
#' # For replacing tags
#' # ```{r, echo=FALSE, results='asis'}
#' # borrow_chapter(
#' #  doc_path = "02-chapter_of_course.Rmd",
#' #  tag_replacement = tag_replacement_list,
#' # )
#' # )
#' # ```
#' }
borrow_chapter <- function(doc_path,
                           repo_name = NULL,
                           remove_h1 = FALSE,
                           tag_replacement = NULL,
                           branch = "main",
                           token = NULL,
                           base_url = "https://raw.githubusercontent.com",
                           dest_dir = file.path("resources", "other_chapters")) {
  # Declare file names
  # normalize relative to Rmd file calling this
  doc_path <- file.path(doc_path)
  doc_name <- basename(doc_path)

  # Create folder if it doesn't exist
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_file <- file.path(dest_dir, doc_name)

  if (!is.null(repo_name)) {
    # Is this a wiki page?
    is_wiki <- grepl("^wiki\\/", repo_name)

    # There's not remote branches for wiki
    if (is_wiki) {
      branch <- ""
    }
    # check_git_repo() does not work for wiki pages
    if (!is_wiki) {
      exists <- check_git_repo(
        repo_name = repo_name,
        token = token,
        verbose = FALSE,
        silent = TRUE
      )
      if (!exists) {
        warning(paste(repo_name, "was not found in GitHub. If it is a private repository, make sure your credentials have been provided"))
      }
    }

    # Piece together URL
    full_url <- file.path(base_url, repo_name, branch, doc_path)

    # Download it
    response <- try(download.file(full_url, destfile = dest_file, quiet = TRUE))

    # Let us know if the url didn't work
    if (grepl("Error", response)) {
      stop(
        "URL failed: ", full_url,
        "\n Double check doc_path and repo_name (and branch if set)"
      )
    }
  } else {
    file.copy(from = doc_path, to = dest_file)
  }

  # Remove leanbuild::set_knitr_image_path() from downloaded file
  file_contents <- readLines(doc_path)
  file_contents <- gsub("leanbuild::set_knitr_image_path\\(\\)", "", file_contents)

  # If remove_header = TRUE
  if (remove_h1) {
    file_contents <- file_contents[-grep("^#[^#]", file_contents)]
  }

  ### Do the replacment tags

  if (!is.null(tag_replacement)) {
    tag_df <-
      data.frame(
        tag = names(tag_replacement),
        replacement = unlist(tag_replacement)
      )

    for (tag in 1:nrow(tag_df)) {
      file_contents <- stringr::str_replace_all(
        str = file_contents,
        replacement = tag_df$replacement[tag],
        pattern = fixed(tag_df$tag[tag])
      )
    }
  }
  writeLines(file_contents, dest_file)

  # Get parent directory
  parent_dir <- knitr::opts_knit$get("output.dir")

  # Set the root directory based on the parent directory that this is being called at
  knitr::opts_knit$set(root.dir = parent_dir)

  # Knit it in
  result <- knitr::knit_child(dest_file,
    options = list(
      echo = FALSE,
      results = "asis"
    ),
    quiet = TRUE
  )
  cat(result, sep = "\n")
}
