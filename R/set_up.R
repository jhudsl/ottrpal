#' Load in Bookdown specifications from _bookdown.yml
#'
#' @param path  Where to look for the _bookdown.yml file. Passes to the bookdown_file() function. By default looks in current directory
#'
#' @return The yaml contents using yaml::yaml.load_file()
#' @export

get_bookdown_spec <- function(path = ".") {

  # Get the file path to _bookdown.yaml
  file_path <- bookdown_file(path = path)

  # Read in yaml
  suppressWarnings({
    yaml_contents <- yaml::yaml.load_file(file_path)
  })

  return(yaml_contents)
}

#' Find main Bookdown directory
#'
#' @param path  Where to look for the file. By default looks in current directory.
#'
#' @return Returns the directory where the _bookdown.yml is contained.
#' @export

bookdown_path <- function(path = ".") {

  # See what unzip is being used
  operating_system <- Sys.info()[1]

  path <- rprojroot::find_root(rprojroot::has_file("_bookdown.yml"), path = file.path(path))

  return(path)
}

#' Find file path to _bookdown.yml
#'
#' @param path  Where to look for the _bookdown.yml file. Passes to the bookdown_file() function. By default looks in current directory
#'
#' @return The file path to _bookdown.yml
#' @export
#'
bookdown_file <- function(path = ".") {
  root_dir <- bookdown_path(path = file.path(path))
  file_path <- file.path(root_dir, "_bookdown.yml")

  return(file_path)
}

#' Get file paths all Rmds in the bookdown directory
#'
#' @param path  Where to look for the _bookdown.yml file. Passes toget_bookdown_spec() function. By default looks in current directory
#'
#' @return The file paths to Rmds listed in the _bookdown.yml file.
#' @export
#'
bookdown_rmd_files <- function(path = ".") {
  spec <- get_bookdown_spec(file.path(path))

  files <- spec$rmd_files
  if (is.null(files) || all(is.na(files)) || length(files) == 0) {
    warning(
      "No bookdown specification of the files, using ",
      "list.files(pattern ='.Rmd')"
    )
    root_dir <- bookdown_path(path = file.path(path))
    files <- list.files(
      pattern = "[.]Rmd", ignore.case = TRUE,
      path = root_dir, full.names = FALSE
    )
  }
  return(files)
}

#' Declare file path to docs/ folder
#'
#' @param path  Where to look for the _bookdown.yml file. Passes toget_bookdown_spec() function. By default looks in current directory
#'
#' @return The file paths to Rmds listed in the _bookdown.yml file.
#' @export
#'
bookdown_destination <- function(path = ".") {

  # Find _bookdown.yml
  root_dir <- bookdown_path(path = file.path(path))

  # Get specs from _bookdown.yml
  spec <- get_bookdown_spec(path = file.path(path))

  # Find output directory declared in the bookdown.yml
  output_dir <- spec$output_dir

  # If none specified, assume its called docs/
  if (is.null(output_dir)) {
    output_dir <- "docs"
  }
  # Get the full file path
  full_output_dir <- file.path(root_dir, output_dir)

  # If the output dir doesn't exist, make it
  dir.create(full_output_dir, showWarnings = FALSE, recursive = TRUE)

  # Declare full paths
  full_output_dir <- normalizePath(full_output_dir, winslash = "/")

  return(full_output_dir)
}

copy_resources <- function(path = ".",
                           images_dir = "resources",
                           output_dir = "manuscript") {

  # Get file path to bookdown.yml
  path <- bookdown_path(path)

  # Assume image directory is `resources/images`
  res_image_dir <- file.path(path, images_dir)

  # Create the directory if it doesn't exist
  dir.create(res_image_dir, showWarnings = FALSE, recursive = TRUE)

  manuscript_image_dir <- file.path(output_dir, images_dir)

  dir.create(manuscript_image_dir, showWarnings = FALSE, recursive = TRUE)

  manuscript_image_dir <- normalizePath(manuscript_image_dir)

  if (dir.exists(res_image_dir)) {
    R.utils::copyDirectory(res_image_dir, manuscript_image_dir, recursive = TRUE, overwrite = TRUE)
  }
}

copy_docs <- function(path = ".", output_dir = "manuscript") {
  path <- bookdown_destination(path)
  R.utils::copyDirectory(path, file.path(output_dir), recursive = TRUE, overwrite = TRUE)
}

copy_bib <- function(path = ".", output_dir = "manuscript") {
  path <- bookdown_path(path)
  files <- list.files(path = path, full.names = TRUE, pattern = ".bib$")
  if (length(files) > 0) {
    file.copy(files, file.path(output_dir), overwrite = TRUE)
  }
}

copy_quizzes <- function(quiz_dir = "quizzes", output_dir = "manuscript") {
  quiz_dir <- file.path(quiz_dir)

  if (!is.null(quiz_dir)) {
    if (!dir.exists(quiz_dir)) {
      warning(paste(
        "The quiz directory specified by quiz_dir:", quiz_dir, "does not exist.",
        "If you don't have quizzes, set quiz_dir = NULL"
      ))
    }
    quizzes <- list.files(path = file.path(quiz_dir), full.names = TRUE, pattern = "\\.md$")
    if (length(quizzes) > 0) {
      fs::file_copy(quizzes, file.path(output_dir, basename(quizzes)),
        overwrite = TRUE
      )
    }
  }
}

#' Set up Manuscript folder for Leanpub publishing
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param clean_up TRUE/FALSE the old output directory should be deleted and
#' everything created fresh.
#' @param render if `TRUE`, then [bookdown::render_book()] will be run on each Rmd.
#' @param verbose print diagnostic messages
#' @param remove_resources_start remove the word `resources/` at the front
#' of any image path.
#' @param run_quiz_checks TRUE/FALSE run quiz checks
#' @param make_book_txt Should [ottrpal::bookdown_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
#' @param quiz_dir directory that contains the quiz .md files that should be
#' checked and incorporated into the Book.txt file. If you don't have quizzes,
#' set this to NULL
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#' @param embed is this being run by bookdown_to_embed_leanpub? TRUE/FALSE
#' @return A list of output files and diagnostics
#' @export
#'
set_up_leanpub <- function(path = ".",
                           clean_up = FALSE,
                           render = TRUE,
                           output_dir = "manuscript",
                           make_book_txt = FALSE,
                           quiz_dir = "quizzes",
                           run_quiz_checks = FALSE,
                           remove_resources_start = FALSE,
                           verbose = TRUE,
                           footer_text = NULL,
                           embed = NULL) {
  if (clean_up) {
    message(paste("Clearing out old version of output files:", output_dir))

    file.remove(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # If output directory doesn't exist, make it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Declare regex for finding rmd files
  rmd_regex <- "[.][R|r]md$"

  # Get the path to the _bookdown.yml
  path <- bookdown_path(path)

  if (verbose) {
    message(paste0("Looking for bookdown file in ", path))
  }
  rmd_files <- bookdown_rmd_files(path = path)

  if (verbose) {
    message(paste0(c("Processing these files: ", rmd_files), collapse = "\n"))
  }

  if (render) {
    if (verbose) {
      message("Rendering the Book")
    }
    # Get the index file path
    index_file <- grep("index", rmd_files, ignore.case = TRUE, value = TRUE)

    index_file <- normalizePath(index_file)

    if (length(index_file) == 0 || is.na(index_file)) {
      index_file <- rmd_files[1]
    }
    message(paste("index_file is", index_file))

    if (rmarkdown::pandoc_version() >= "2.11") {
      output_format <- bookdown::gitbook(pandoc_args = "--citeproc")
      output_format$pandoc$args <- c(output_format$pandoc$args, "--citeproc")
    } else {
      warning("Pandoc version is not greater than 2.11 so citations will not be able to be rendered properly")
      output_format <- NULL
    }
    bookdown::render_book(
      input = index_file,
      output_format = output_format,
      clean_envir = FALSE
    )
  }

  # We only need to copy these things if we are not doing embed
  if (!embed) {
    if (verbose) message("Copying Resources")
    copy_resources(path, output_dir = output_dir)

    if (verbose) message("Copying docs files")
    copy_docs(path, output_dir = output_dir)

    if (verbose) message("Copying bib files")
    copy_bib(path, output_dir = output_dir)
  }

  if (!is.null(quiz_dir)) {
    #### Run quiz checks
    if (run_quiz_checks) {
      message("Checking quizzes")
      quiz_checks <- check_quizzes(
        quiz_dir,
        verbose = verbose
      )
    }
    if (verbose) message("Copying quiz files")
    copy_quizzes(
      quiz_dir = quiz_dir,
      output_dir = output_dir
    )
  }
}
