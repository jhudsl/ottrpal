
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
  
  rprojroot::find_root(rprojroot::has_file("_bookdown.yml"), path = path)
  
  path <- dirname(here::here("_bookdown.yml"))
  
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
  
  root_dir <- bookdown_path(path = path)
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
  
  spec <- get_bookdown_spec(path)
  
  files <- spec$rmd_files
  if (is.null(files) || all(is.na(files)) || length(files) == 0) {
    warning(
      "No bookdown specification of the files, using ",
      "list.files(pattern ='.Rmd')"
    )
    root_dir <- bookdown_path(path = path)
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
  root_dir <- bookdown_path(path = path)
  
  # Get specs from _bookdown.yml
  spec <- get_bookdown_spec(path = path)
  
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

#' Copying directory contents
#'
#' @param from  Where the files to copy are located. 
#' @param to  Where the files to copy are to be copied should go to.  
#'
#' @return The file paths to Rmds listed in the _bookdown.yml file. 
#' @export
#'
copy_directory_contents <- function(from, to) {
  
  file_list <- list.files(
    path = from, full.names = TRUE, all.files = TRUE,
    recursive = TRUE
  )
  file.copy(file_list, to, recursive = TRUE, overwrite = TRUE)
}

copy_resources <- function(path = ".",
                           images_dir = "resources/images",
                           output_dir = "manuscript") {
  
  # Get file path to bookdown.yml
  path <- bookdown_path(path)
  
  # Assume image directory is `resources/images`
  res_image_dir <- file.path(path, images_dir )
  
  # Creat the directory if it doesn't exist
  dir.create(res_image_dir, showWarnings = FALSE, recursive = TRUE)
  
  manuscript_image_dir <- file.path(output_dir, images_dir )
  
  dir.create(manuscript_image_dir, showWarnings = FALSE, recursive = TRUE)
  
  manuscript_image_dir <- normalizePath(manuscript_image_dir)
  if (file.exists(res_image_dir)) {
    copy_directory_contents(res_image_dir, manuscript_image_dir)
  }
}

copy_docs <- function(path = ".", output_dir = "manuscript") {
  path <- bookdown_destination(path)
  R.utils::copyDirectory(path, output_dir, recursive = TRUE, overwrite = TRUE)
}

copy_bib <- function(path = ".", output_dir = "manuscript") {
  path <- bookdown_path(path)
  files <- list.files(path = path, full.names = TRUE, pattern = ".bib$")
  if (length(files) > 0) {
    file.copy(files, output_dir, overwrite = TRUE)
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
#' @param make_book_txt Should [leanbuild::bookdown_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
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
    
    output_format <- bookdown::gitbook(pandoc_args = "--citeproc")
    # output_format$pandoc$to = output_format$pandoc$from
    output_format$pandoc$args <- c(output_format$pandoc$args, "--citeproc")
    bookdown::render_book(input = index_file, 
                          output_format = output_format, 
                          clean_envir = FALSE)
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
    message("Copying bib files")
  }
  copy_bib(path, output_dir = output_dir)
  # FIXME Can also use bookdown_rmd_files
  # rmd_files = list.files(pattern = rmd_regex)


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
  out <- NULL
  if (make_book_txt) {
    if (verbose > 1) {
      message("Running bookdown_to_book_txt")
    }
    out <- bookdown_to_book_txt(
      path = path,
      output_dir = output_dir,
      verbose = verbose
    )
  }
  L <- list(
    output_files = md_files,
    full_output_files = normalizePath(md_files, winslash = "/")
  )
  L$book_txt_output <- out
  return(L)
}


#' Convert Bookdown to Leanpub
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param verbose print diagnostic messages
#'
#' @return A list of output files in order, the book text file name, and diagnostics
#' @export
bookdown_to_book_txt <- function(
                                 path = ".",
                                 output_dir = "manuscript",
                                 verbose = TRUE) {
  index <- full_file <- NULL
  rm(list = c("full_file", "index"))

  path <- bookdown_path(path)

  rmd_regex <- "[.][R|r]md$"
  rmd_files <- bookdown_rmd_files(path = path)
  md_files <- sub(rmd_regex, ".md", rmd_files, ignore.case = TRUE)
  md_df <- tibble::tibble(
    file = md_files,
    index = seq_along(md_files)
  )
  quiz_files <- paste0("quiz-", md_files)
  bad_quiz_files <- paste0("quiz_", md_files)
  if (any(file.exists(file.path(output_dir, bad_quiz_files)))) {
    warning(
      "Naming convention for quizzes is quiz-, not quiz_",
      ", please correct"
    )
  }
  # add 0.5 so it's after the correct md file
  quiz_df <- tibble::tibble(
    file = quiz_files,
    index = seq_along(quiz_files) + 0.5
  )
  bad_quiz_df <- tibble::tibble(
    file = bad_quiz_files,
    index = seq_along(bad_quiz_files) + 0.5
  )
  quiz_df <- dplyr::bind_rows(quiz_df, bad_quiz_df)
  rm(list = c("bad_quiz_files", "bad_quiz_df"))
  quiz_df <- quiz_df %>%
    dplyr::arrange(index)
  df <- dplyr::bind_rows(md_df, quiz_df)
  rm(list = c("quiz_df"))

  df <- df %>%
    dplyr::arrange(index)
  df <- df %>%
    dplyr::mutate(full_file = file.path(output_dir, file))
  df <- df %>%
    dplyr::filter(file.exists(full_file))

  book_txt <- file.path(output_dir, "Book.txt")
  # need to fix about quiz
  writeLines(df$file, book_txt)
  L <- list(
    md_order = df,
    book_file = book_txt
  )
  return(L)
}
