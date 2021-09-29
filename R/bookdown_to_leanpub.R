
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

  if (!dir.exists(quiz_dir)) {
    stop(paste(
      "The quiz directory specified by quiz_dir:", quiz_dir, "does not exist.",
      "If you don't have quizzes, set quiz_dir = NULL"
    ))
  }
  quizzes <- list.files(path = file.path(quiz_dir), full.names = TRUE, pattern = "\\.md$")
  if (length(files) > 0) {
    fs::file_copy(quizzes, file.path(output_dir, basename(quizzes)),
      overwrite = TRUE
    )
  }
}



#' Convert Bookdown to Leanpub
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param render if `TRUE`, then [bookdown::render_book()] will be run on each Rmd.
#' @param verbose print diagnostic messages
#' @param remove_resources_start remove the word `resources/` at the front
#' of any image path.
#' @param run_quiz_checks TRUE/FALSE run quiz checks
#' @param make_book_txt Should [leanbuild::bookdown_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
#' @param quiz_dir directory that contains the quiz .md files that should be
#' checked and incorporated into the Book.txt file. If you don't have quizzes,
#' set this to NULL
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#'
#' @return A list of output files and diagnostics
#' @export
#'
bookdown_to_leanpub <- function(path = ".",
                                render = TRUE,
                                output_dir = "manuscript",
                                make_book_txt = FALSE,
                                quiz_dir = "quizzes",
                                run_quiz_checks = FALSE,
                                remove_resources_start = FALSE,
                                verbose = TRUE,
                                footer_text = NULL) {

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

  # may be irrelevant since copy_docs does everything
  if (verbose) {
    message("Copying Resources")
  }
  copy_resources(path, output_dir = output_dir)

  if (verbose) {
    message("Copying Docs folder")
  }

  copy_docs(path, output_dir = output_dir)
  if (verbose) {
    message("Copying docs files")
  }

  copy_bib(path, output_dir = output_dir)
  if (verbose) {
    message("Copying bib files")
  }

  bib_files <- list.files(pattern = "[.]bib$")
  if (length(bib_files) > 0) {
    pandoc_args <- paste0("--bibliography=", path.expand(normalizePath(bib_files)))
  } else {
    pandoc_args <- NULL
  }

  # run_env = new.env()
  md_files <- sub(rmd_regex, ".md", rmd_files, ignore.case = TRUE)
  md_files <- file.path(output_dir, basename(md_files))

  for (file in md_files) {
    if (verbose > 1) {
      message("Replacing HTML for ", file)
    }
    infile <- normalizePath(file)

    infile <- replace_single_html(infile,
      verbose = verbose > 1,
      remove_resources_start = remove_resources_start,
      footer_text = footer_text
    )

    if (length(bib_files) > 0) {
      if (verbose > 1) {
        message("Making references for ", file)
      }
      writeLines(simple_references(infile, bib_files, add_reference_header = TRUE),
        con = infile, sep = "\n"
      )
    }
  }
  if (!is.null(quiz_dir)) {
    #### Run quiz checks
    if (run_quiz_checks) {
      message("Checking quizzes")
      quiz_checks <- check_quizzes(quiz_dir,
        verbose = verbose
      )
    }
    copy_quizzes(
      quiz_dir = quiz_dir,
      output_dir = output_dir
    )
    if (verbose) {
      message("Copying quiz files")
    }
  }

  out <- NULL
  if (make_book_txt) {
    if (verbose > 1) {
      message("Running bookdown_to_book_txt")
    }
    bookdown_to_book_txt(
      path = path,
      output_dir = output_dir,
      quiz_dir = quiz_dir,
      verbose = verbose
    )
    out <- book_txt_file <- file.path(output_dir, "Book.txt")
  } else {
    # If false, look for Book.txt file to copy to output folder.
    book_txt_file <- file.path(path, "Book.txt")

    if (file.exists(book_txt_file)) {
      # Copy over an existing book.txt file if it exists
      file.copy(from = book_txt_file, to = file.path(output_dir, "Book.txt"))

      out <- book_txt_file <- file.path(output_dir, "Book.txt")
    } else {
      # If none exists and make_book_txt is false: stop.
      stop(paste0(
        "Book.txt file does not exist in the main directory: ", path, "and make_book_txt is set to FALSE.",
        "There is no Book.txt file. Leanpub needs one. Either make one and place it in the directory path or ",
        "use make_book_txt = TRUE and one will be generated for you."
      ))
    }
  }
  message(paste(
    "Leanpub ready files are saved to",
    output_dir,
    "Go to https://leanpub.com/ to publish them using the GitHub writing mode."
  ))
}


#' Create Book.txt file from files existing in quiz directory
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param quiz_dir Where are the quizzes stored? Default looks for folder called "quizzes".
#' @param verbose print diagnostic messages
#'
#' @return A list of quiz and chapter files in order in a file called Book.txt -- How Leanpub wants it.
#' @export
#'
bookdown_to_book_txt <- function(path = ".",
                                 output_dir = "manuscript",
                                 quiz_dir = "quizzes",
                                 verbose = TRUE) {
  # Establish path
  path <- bookdown_path(path)

  rmd_regex <- "[.][R|r]md$"

  # Extract the names of the Rmd files (the chapters)
  rmd_files <- bookdown_rmd_files(path = path)

  # Find the quiz files in the quiz directory
  quiz_files <- list.files(pattern = "\\.md$", quiz_dir)

  # Put files in one vector
  all_files <- c(rmd_files, quiz_files)

  # Make a vector specifying the file type: quiz or not
  file_type <- c(
    rep("non-quiz", length(rmd_files)),
    rep("quiz", length(quiz_files))
  )

  # Put all files in one data.frame
  all_files <- data.frame(
    file_name = all_files,
    file_type
  ) %>%
    dplyr::mutate(
      # Use this so we don't have to fiddle with case senstivity for the next step
      lower_filename = tolower(file_name),
      # Get the number from the file name and that will be the order
      num = stringr::str_extract(file_name, "([0-9]+)"),
      num = dplyr::case_when(
        # Put index file first and about file last
        lower_filename == "index.rmd" ~ "0",
        lower_filename == "about.rmd" ~ as.character(length(all_files)),
        TRUE ~ num
      ),
      num = as.numeric(num)
    ) %>%
    # Put quizzes in order!
    dplyr::arrange(num, file_type) %>%
    dplyr::pull(file_name)

  # Declare output file name
  book_txt <- file.path(output_dir, "Book.txt")

  if (verbose) {
    message(paste0("Autogenerated Book.txt saved to: ", book_txt))
  }
  # need to fix about quiz
  writeLines(all_files, con = book_txt)
}
