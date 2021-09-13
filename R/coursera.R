# C. Savonen 2021

# Need magrittr
`%>%` <- dplyr::`%>%`

find_end_of_prompt <- function(start_prompt_index, type_vector) {
  # We want to see if the next line is where the answers start
  end_prompt_index <- start_prompt_index + 1

  # See if the end of the prompt is in the same line
  end_prompt <- grepl("answer", type_vector[end_prompt_index])

  # Keep looking in each next line until we find it.
  if (end_prompt == FALSE) {
    while (end_prompt == FALSE) {
      # Add one
      end_prompt_index <- end_prompt_index + 1

      # Look in next line
      end_prompt <- grepl("answer", type_vector[end_prompt_index])

      if (end_prompt_index == length(type_vector) && end_prompt == FALSE) {
        stop(paste("Searched end of file and could not find end of prompt that starts at line:", start_prompt_index))
      }
    }
  } else {
    end_prompt_index <- start_prompt_index
  }
  return(end_prompt_index)
}

#' Convert Leanpub md quiz to Coursera yaml quiz
#'
#' Convert a Leanpub-formatted md quiz file to a Coursera-formatted yaml quiz file in preparation for uploading to Coursera.
#'
#' @param quiz_path A path to a quiz .md file to be converted.
#' @param output_quiz_dir An existing folder where you would like the new version of the quiz to be saved.
#' Default is the directory of the quiz_path provided
#' @param verbose Would you like the progress messages?
#'
#' @return A Coursera-ready quiz file saved to the output directory specified as a yaml.
#' @export convert_quiz
#'
#' @examples
#'
#' # Provide path to quiz to convert
#' convert_quiz(leanbuild::good_quiz_path)
#'
#'
convert_quiz <- function(quiz_path,
                         output_quiz_dir = dirname(quiz_path),
                         verbose = TRUE) {
  # Print out which quiz we're converting
  message(paste("Converting quiz:", quiz_path))

  output_filename <- file.path(output_quiz_dir, paste0(basename(quiz_path), ".yml"))

  ### First read lines for each quiz
  # Put it as a data.frame:
  quiz_lines_df <- parse_quiz_df(readLines(quiz_path), remove_tags = TRUE)

  # Add in proper Coursera yaml mappings based on the parsing
  quiz_lines_df <- quiz_lines_df %>%
    # Now for updating based on type!
    dplyr::mutate(updated_line = dplyr::case_when(
      type %in% c("prompt", "single_line_prompt") ~ stringr::str_replace(original, "^\\?", "  prompt:"),
      type %in% c("extended_prompt", "end_prompt") ~ paste0("    ", original),
      grepl("answer", type) ~ stringr::str_replace(original, "^[[:alpha:]]\\)", "    - answer:"),
      TRUE ~ original
    ))

  # Declare the correct answers by which ones are the first ones listed
  correct_answers <- quiz_lines_df %>%
    dplyr::filter(type == "correct_answer") %>%
    dplyr::distinct(question, .keep_all = TRUE)

  # Identify other correct answers that we don't want to keep
  remove_answers <- setdiff(
    dplyr::filter(quiz_lines_df, type == "correct_answer")$index,
    correct_answers$index
  )

  # Remove any correct answers that aren't those
  quiz_lines_df <- quiz_lines_df %>%
    dplyr::filter(!(index %in% remove_answers))

  # Turn updated lines into a named vector
  updated_quiz_lines <- quiz_lines_df$updated_line
  names(updated_quiz_lines) <- quiz_lines_df$type

  ### Add specs for coursera
  # Add typeName before prompt starts:
  updated_quiz_lines <- R.utils::insert(updated_quiz_lines,
    ats = which(names(updated_quiz_lines) %in% c("prompt", "single_line_prompt")),
    values = "- typeName: multipleChoice"
  )

  ### Add "  options:" before beginning of answer options
  updated_quiz_lines <- R.utils::insert(updated_quiz_lines,
    ats = which(names(updated_quiz_lines) %in% c("end_prompt", "single_line_prompt")) + 1,
    values = "  options:"
  )

  # Add shuffleoptions: true after prompt ends
  updated_quiz_lines <- R.utils::insert(updated_quiz_lines,
    ats = which(names(updated_quiz_lines) %in% c("end_prompt", "single_line_prompt")) + 1,
    values = "  shuffleOptions: true"
  )

  # Need to add "isCorrect: true" one line below correct value lines
  updated_quiz_lines <- R.utils::insert(updated_quiz_lines,
    ats = which(names(updated_quiz_lines) == "correct_answer") + 1,
    values = "      isCorrect: true"
  )

  # Need to add "isCorrect: false" one line below incorrect value lines
  updated_quiz_lines <- R.utils::insert(updated_quiz_lines,
    ats = which(names(updated_quiz_lines) == "wrong_answer") + 1,
    values = "      isCorrect: false"
  )

  # Remove other lines
  updated_quiz_lines <- updated_quiz_lines[-grep("other", names(updated_quiz_lines))]

  # Add extra line in between each question
  updated_quiz_lines <- R.utils::insert(updated_quiz_lines,
    ats = grep("typeName:", updated_quiz_lines),
    values = ""
  )

  # Trim trailing space
  updated_quiz_lines <- trimws(updated_quiz_lines, which = "right")

  # Add extra line in between each question
  updated_quiz_lines <- stringr::str_remove(updated_quiz_lines, ":$")

  # Return the options : though
  updated_quiz_lines <- stringr::str_replace(updated_quiz_lines, "  options", "  options:")

  ### Write new file with .yml at end of file name and put in coursera dir
  writeLines(updated_quiz_lines, con = output_filename)

  # Put message
  message(paste("Converted quiz saved to:", output_filename))
}

#' Convert Leanpub md quiz to Coursera yaml quiz
#'
#' @param input_quiz_dir A path to a directory of leanpub formatted quiz md files. By default assumes "quizzes" and looks in current directory.
#' @param output_quiz_dir A folder (existing or not) that the new coursera converted quizzes should be saved to. By default saves to "coursera_quizzes".
#' @param verbose Would you like the progress messages: TRUE/FALSE?
#'
#' @return A folder of coursera ready quiz files saved to the output directory specified as a yamls.
#' @export
#'
#' @examples
#'
#' # Set up a directory with a quiz in it for this example
#' tdir <- tempfile()
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#' file.copy(from = leanbuild::good_quiz_path,
#'           to = file.path(tdir, basename(leanbuild::good_quiz_path)))
#'
#' # Provide path to directory of quizzes
#' convert_coursera_quizzes(tdir)
#'
convert_coursera_quizzes <- function(input_quiz_dir = "quizzes",
                                     output_quiz_dir = "coursera_quizzes",
                                     verbose = TRUE) {

  # Create directory if it is not yet created
  if (!dir.exists(output_quiz_dir)) {
    dir.create(output_quiz_dir, recursive = TRUE)
  }

  # List quiz paths
  leanpub_quizzes <- list.files(
    pattern = (".md"),
    ignore.case = TRUE,
    path = input_quiz_dir,
    full.names = TRUE
  )

  if (length(leanpub_quizzes) < 1) {
    stop(paste0("No quiz .md files found in your specified path dir of: ", quiz_path))
  }

  # Run the thing!
  lapply(leanpub_quizzes,
    convert_quiz,
    verbose = verbose,
    output_quiz_dir = output_quiz_dir
  )
}

#' Create TOC-less Bookdown for use in Coursera
#'
#' Create a version of Leanpub that does not have a TOC and has quizzes in the Coursera yaml format. Requires Bookdown output files including "assets", "resources", and "libs".
#'
#' @param output_dir a folder (existing or not) that the TOC-less Bookdown for Coursera files should be saved. By default is file.path("docs", "coursera")
#' @param convert_quizzes TRUE/FALSE whether or not to convert quizzes. Default is TRUE
#' @param input_quiz_dir a path to a directory of leanpub formatted quiz md files. By default assumes "quizzes" and looks in current directory.
#' @param output_quiz_dir a folder (existing or not) where the coursera quizzes should be saved. By default is "coursera_quizzes".
#' @param verbose would you like the progress messages?
#'
#' @return a folder of coursera ready quiz files saved to the output directory specified as a yamls.
#' @export
#' @rdname coursera
#'
#'
render_coursera <- function(
  output_dir = file.path("docs", "coursera"),
  convert_quizzes = FALSE,
  input_quiz_dir = "quizzes",
  output_quiz_dir = "coursera_quizzes",
  verbose = TRUE) {

  # Find root directory by finding `_bookdown.yml` file
  root_dir <- bookdown_path()

  # Create output folder if it does not exist
  if (!dir.exists(output_dir)) {
    message(paste0("Creating output folder: ", output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (convert_quizzes) {
    if (!dir.exists(input_quiz_dir)){
      stop("convert_quizzes = TRUE but the specified input_quiz_dir: ",
           input_quiz_dir,
           " cannot be found.")
    }
    convert_coursera_quizzes(input_quiz_dir = input_quiz_dir,
                             output_quiz_dir = output_quiz_dir,
                             verbose = verbose)
  }

  # Clean out old files if they exist
  old_files <- list.files(output_dir, pattern = c("html$", "md$"), full.names = TRUE)
  if (length(old_files) > 0) {
    file.remove(old_files)
  }

  # Copy these directories over if they don't exist in the output folder
  needed_directories <- c("assets", "resources")

  if (verbose) {
    message(paste0(c("Needed directories being copied:"), collapse = "\n"))
  }

  # Copy over needed directories
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

  # Retrieve list of Rmd files from the _bookdown.yml
  output_yaml <- yaml::yaml.load_file(file.path(root_dir, "_output.yml"))

  # Change CSS file to coursera special one
  coursera_css <- gsub("\\.css", "_coursera.css", output_yaml$`bookdown::gitbook`$css)
  output_yaml$`bookdown::gitbook`$css <- coursera_css

  # Write this new coursera yml
  yaml::write_yaml(output_yaml, file.path(output_dir, "_output_coursera.yml"))

  message("Render bookdown without TOC for Coursera")

  # Do the render
  bookdown::render_book(
    input = "index.Rmd",
    output_yaml = file.path(output_dir, "_output_coursera.yaml"),
    output_dir = output_dir,
    clean_envir = FALSE
  )

  # Get specified style name
  style_css <- file.path(coursera_css)

  # Read in TOC closing CSS lines
  toc_close_css <- readLines(style_css)

  # Using suppressWarnings() because "incomplete final line"
  full_css <- suppressWarnings(readLines(file.path(output_dir, "assets", "style.css")))

  # Write to "style.css"
  writeLines(append(full_css, toc_close_css), file.path(output_dir, "assets", "style.css"))
}
