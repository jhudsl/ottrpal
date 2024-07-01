#' Load in yaml specifications from _bookdown.yml or _quarto.yml
#'
#' @param path  Where to look for the yaml spec file. By default looks in current directory.
#'
#' @return The yaml contents using yaml::yaml.load_file()
#' @export

get_yaml_spec <- function(path = ".") {

  root_dir <- course_path(path = file.path(path))

  file_path <- list.files(pattern = "_bookdown.yml|_quarto.yml", full.names = TRUE)

  # Read in yaml
  suppressWarnings({
    yaml_contents <- yaml::yaml.load_file(file_path)
  })

  return(yaml_contents)
}

#' Find main course git directory
#'
#' @param path  Where to look for the file. By default looks in current directory.
#'
#' @return Returns the directory where the .git folder is contained.
#' @export
course_path <- function(path = ".") {
  # See what unzip is being used
  operating_system <- Sys.info()[1]

  path <- rprojroot::find_root(rprojroot::has_dir(".git"), path = file.path(path))

  return(path)
}

#' Get file paths to all qmds or rmds in the course website directory
#'
#' @param path  Where to look for the _bookdown.yml or _quarto.yml file. Passes to get_yaml_spec() function. By default looks in current directory
#'
#' @return The file paths to rmds or wmds listed in the _bookdown.yml or _quarto.yml file.
#' @export
#'
qrmd_files <- function(path = ".") {
  spec <- get_yaml_spec(file.path(path))

  rmd_files <- spec$rmd_files
  qmd_files <- grep(".qmd", unlist(spec$book$chapters), value = TRUE)

  if (length(rmd_files) > 0 && length(qmd_files) > 0) stop("Both qmd and rmd files are found. Not sure what format to expect")

  # Make files whichever ones exist here
  if (length(rmd_files) > 0) files <- rmd_files
  if (length(qmd_files) > 0) files <- qmd_files

  if (length(rmd_files) == 0 && length(qmd_files) == 0) {
    warning(
      "No rmd or qmd files found specified in the _quarto.yml or _bookdown.yml file. Going to try to find files in the repo based on suffix"
    )
    root_dir <- course_path(path = file.path(path))
    files <- list.files(
      pattern = "[.]Rmd$|[.]qmd$", ignore.case = TRUE,
      path = root_dir, full.names = FALSE
    )
  }

  # If we don't find files STOP
  if (length(files) == 0) stop("No rmd/qmd files found in the repo")

  return(files)
}

#' Declare file path to docs/ folder
#'
#' @param path  Where to look for the _bookdown.yml or _quarto.yml file. Passes to get_yaml_spec() function. By default looks in current directory
#'
#' @return The file paths to rmds or qmds listed in the _bookdown.yml or _quarto.ymlfile.
#' @export
#'
output_destination <- function(path = ".") {
  # Find .git folder which indicates the top of the repo
  root_dir <- course_path(path = file.path(path))

  # Get specs from _bookdown.yml or _quarto.yml
  spec <- get_yaml_spec(path = file.path(path))

  # Find output directory declared in the bookdown.yml
  output_dir <- spec$output_dir

  # If that didn't work we're working with quarto so look for that
  if (is.null(output_dir)) output_dir <- spec$project$`output-dir`

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
  path <- course_path(path)

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
  path <- output_destination (path)
  R.utils::copyDirectory(path, file.path(output_dir), recursive = TRUE, overwrite = TRUE)
}

copy_bib <- function(path = ".", output_dir = "manuscript") {
  path <- course_path(path)
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
#' @param path path to the top of course repository (looks for .git folder)
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param clean_up TRUE/FALSE the old output directory should be deleted and
#' everything created fresh.
#' @param render If NULL will not be run. If "quarto" or "bookdown" then the respective render type will be run
#' @param verbose print diagnostic messages
#' @param remove_resources_start remove the word `resources/` at the front
#' of any image path.
#' @param run_quiz_checks TRUE/FALSE run quiz checks
#' @param make_book_txt Should [ottrpal::course_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
#' @param quiz_dir directory that contains the quiz .md files that should be
#' checked and incorporated into the Book.txt file. If you don't have quizzes,
#' set this to NULL
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#' @return A list of output files and diagnostics
#' @export
#'
set_up_leanpub <- function(path = ".",
                           clean_up = FALSE,
                           output_dir = "manuscript",
                           make_book_txt = FALSE,
                           quiz_dir = "quizzes",
                           run_quiz_checks = FALSE,
                           remove_resources_start = FALSE,
                           verbose = TRUE,
                           footer_text = NULL) {


  # Check that render is something we can use
  if (!is.null(render)) {
    if (!render %in% c("quarto", "bookdown")) stop("`render` argument invalid. ")
  }

  if (clean_up) {
    message(paste("Clearing out old version of output files:", output_dir))

    file.remove(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # If output directory doesn't exist, make it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!is.null(render)) {
    # Declare regex for finding rmd files
    rmd_regex <- "[.][q|R|r]md$"

    # Get the path to the _bookdown.yml or _quarto.yml
    path <- course_path(path)

    if (verbose) {
      message(paste0("Looking for bookdown or quarto md files in ", path))
    }
    md_files <- qrmd_files(path = path)

    if (verbose) {
      message(paste0(c("Processing these files: ", rmd_files), collapse = "\n"))
    }

    if (render == "bookdown") {
      if (verbose) {
        message("Rendering bookdown Book")
      }
      # Get the index file path
      index_file <- grep("index", rmd_files, ignore.case = TRUE, value = TRUE)

      index_file <- normalizePath(index_file)

      if (length(index_file) == 0 || is.na(index_file)) {
        index_file <- md_files[1]
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
    if (render == "quarto") {
      if (verbose) {
        message("Rendering Quarto Book")
      }
      # Get the index file path
      index_file <- grep("index", rmd_files, ignore.case = TRUE, value = TRUE)

      index_file <- normalizePath(index_file)

      if (length(index_file) == 0 || is.na(index_file)) {
        index_file <- md_files[1]
      }
      message(paste("index_file is", index_file))

      quarto::quarto_render('.')
    }

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
