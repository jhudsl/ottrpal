# C. Savonen 2021

#' Create TOC-less course website for use in Coursera or Leanpub
#'
#' Create a version of the course that does not have a TOC and has quizzes in the Coursera yaml format.
#' This is only needed to be used on Bookdown courses. Quarto has a simple command for this.
#'
#' @param path path to the bookdown or quarto course repository, must have a `_bookdown.yml` or `_quarto.yml` file
#' @param output_dir A folder (existing or not) that the TOC-less Bookdown for Coursera files should be saved. By default is file.path("docs", "coursera")
#' @param output_yaml A output.yml file to be provided to bookdown. By default is "_output.yml"
#' @param convert_quizzes TRUE/FALSE whether or not to convert quizzes. Default is TRUE
#' @param input_quiz_dir A path to a directory of Leanpub-formatted quiz md files. By default assumes "quizzes" and looks in current directory.
#' @param output_quiz_dir A folder (existing or not) where the coursera quizzes should be saved. By default is "coursera_quizzes".
#' @param verbose Would you like the progress messages? TRUE/FALSE
#'
#' @return A folder of coursera ready quiz files and html chapter files saved to output directories specified.
#' @export
#' @rdname coursera
#'
#' @importFrom utils download.file
#'
render_without_toc <- function(path = ".",
                               output_dir = file.path("docs", "no_toc"),
                               output_yaml = "_output.yml",
                               convert_quizzes = FALSE,
                               input_quiz_dir = "quizzes",
                               output_quiz_dir = "coursera_quizzes",
                               verbose = TRUE) {
  # Find root directory by finding `.git` folder
  root_dir <- course_path()

  # Output files:
  output_dir <- file.path(root_dir, output_dir)

  ###### Check we have the files we need ######
  # Create output folder if it does not exist
  if (!dir.exists(output_dir)) {
    message(paste0("Creating output folder: ", output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ###### Declare all the file paths relative to root directory ######
  # Input files:
  toc_close_css <- file.path(root_dir, "assets", "toc_close.css")

  if (!file.exists(toc_close_css)) {
    download.file("https://raw.githubusercontent.com/jhudsl/ottrpal/master/inst/extdata/toc_close.css",
      destfile = toc_close_css
    )
  }
  output_yaml_file <- file.path(root_dir, output_yaml)

  # Make sure we have that file
  if (!file.exists(toc_close_css)) {
    stop(paste0("Could not find: ", toc_close_css))
  }
  # Make sure we know where the output yaml is
  if (!file.exists(output_yaml_file)) {
    stop(paste0("Could not find: ", output_yaml_file))
  }

  # Clean out old files if they exist
  old_files <- list.files(output_dir, pattern = c("html$", "md$"), full.names = TRUE)
  if (length(old_files) > 0) {
    file.remove(old_files)
  }

  ###### Copy over needed directories ######
  # Copy these directories over if they don't exist in the output folder
  needed_directories <- c("assets", "resources")

  if (verbose) {
    message(paste0(c("Needed directories being copied:"), collapse = "\n"))
  }

  # Do the copying
  lapply(needed_directories, function(needed_dir) {
    if (verbose) {
      message(needed_dir)
    }
    if (!dir.exists(needed_dir)) {
      stop(paste0("Needed directory:", needed_dir, "does not exist in the current path."))
    }
    if (!dir.exists(file.path(output_dir, needed_dir))) {
      fs::dir_copy(needed_dir, file.path(output_dir, needed_dir), overwrite = TRUE)
    }
  })

  # Slightly different path for the libs folder
  libs_path <- file.path("docs", "libs")
  if (!dir.exists(file.path(output_dir, "libs"))) {
    if (verbose) {
      message(file.path("docs", "libs"))
    }
    fs::dir_copy(libs_path, file.path(output_dir, "libs"), overwrite = TRUE)
  }

  ###### Copy over CSS file ######
  # Retrieve yaml file specs
  output_yaml_lines <- yaml::yaml.load_file(output_yaml_file)

  # Copy over css file(s) that's specified
  org_css_file <- output_yaml_lines$`bookdown::gitbook`$css

  # Check if there are multiple .css
  if (length(org_css_file) > 1) {
    # Read all .css
    css_files_read <- sapply(org_css_file, readLines)

    # Make a "mega .css" and write
    if (verbose) {
      message("Combining .css files")
    }
    css_lines_cat <- rbind(unlist(css_files_read))
    css_file <- file.path(output_dir, org_css_file[1])
    writeLines(css_lines_cat, css_file)
  } else {
    css_file <- file.path(output_dir, org_css_file)

    # Write it as "style.css"
    fs::file_copy(org_css_file,
      css_file,
      overwrite = TRUE
    )
  }

  ###### Now do the rendering! ######
  message("Render bookdown without TOC")

  # Do the render
  bookdown::render_book(
    input = "index.Rmd",
    output_yaml = output_yaml_file,
    output_dir = output_dir
  )

  # Read in TOC closing CSS lines
  toc_close_css_lines <- readLines(toc_close_css)

  # Using suppressWarnings() because "incomplete final line"
  full_css <- suppressWarnings(
    readLines(css_file)
  )

  # Write to "style.css"
  writeLines(append(full_css, toc_close_css_lines), css_file)

  # Only convert the quizzes if set to TRUE
  if (convert_quizzes) {
    if (!dir.exists(input_quiz_dir)) {
      stop(
        "convert_quizzes = TRUE but the specified input_quiz_dir: ",
        input_quiz_dir,
        " cannot be found."
      )
    }
    convert_coursera_quizzes(
      input_quiz_dir = input_quiz_dir,
      output_quiz_dir = output_quiz_dir,
      verbose = verbose
    )
  }
}
