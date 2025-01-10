# C. Savonen 2025

utils::globalVariables(c("question", "original", "n", "metadata_check", "index", "ignore_coursera"))


#' Check all quizzes' formatting for Leanpub
#'
#' @param path path to the bookdown or quarto course repository, must have a
#'   `.github` folder which will be used to establish the top of the repo.
#' @param quiz A relative file path to the folder (existing or not) that contains the quizzes
#' in Leanpub format. Default is "quizzes".
#' @param output_dir A relative file path to the folder (existing or not) that the
#'   output check file should be saved to. Default is "check_reports"
#' @param resources_dir A relative file path to the folder (existing or not) that the
#'   ignore_urls.txt file and exclude_files.txt will be found. Default is "resources".
#'   If no ignore_urls.txt file and exclude_files.txt files are found, we will download one.
#' @param report_all Should all URLs that were tested be returned? Default is FALSE
#'   meaning only broken URLs will be reported in the url_checks.tsv file.
#' @return A file will be saved that lists the broken URLs will be saved to the specified output_dir.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rprojroot find_root has_dir
#' @importFrom tidyr unnest separate
#' @importFrom readr write_tsv
#'
#' @examples \dontrun{
#'
#' rmd_dir <- setup_ottr_template(dir = ".", type = "rmd", render = FALSE)
#'
#' check_quiz_dir(rmd_dir)
#'
#' # If there are broken URLs they will be printed in a list at 'question_error_report.tsv'
#'
#' qmd_dir <- setup_ottr_template(dir = ".", type = "quarto", render = FALSE)
#'
#' check_quiz_dir(qmd_dir)
#' }
check_quiz_dir <- function(path = ".",
                       quiz_dir =  "quizzes",
                       output_dir = "check_reports",
                       resources_dir = "resources",
                       report_all = FALSE) {
  # Find .git root directory
  root_dir <- rprojroot::find_root(path = path, rprojroot::has_dir(".github"))

  resources_dir <- file.path(root_dir, resources_dir)
  output_dir <- file.path(root_dir, output_dir)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(resources_dir)) {
    dir.create(resources_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_file <- file.path(output_dir, 'question_error_report.tsv')
  ignore_urls_file <- file.path(resources_dir, "ignore-urls.txt")
  exclude_file <- file.path(resources_dir, "exclude_files.txt")

  ottrpal::check_quizzes(quiz_dir = file.path(root_dir, quiz_dir), write_report = TRUE, verbose = TRUE)

  if (file.exists("question_error_report.tsv")) {
    quiz_errors <- readr::read_tsv("question_error_report.tsv")

    file.copy('question_error_report.tsv', file.path(root_dir, 'check_reports'))
    file.remove('question_error_report.tsv')

    # Print out how many quiz check errors
    write(nrow(quiz_errors), stdout())

  } else {
    quiz_errors <- data.frame()

    # Print out how many quiz check errors
    write("1", stdout())
  }

  # Save question errors to file
  readr::write_tsv(quiz_errors, output_file)

  message(paste0("Saved to: ", output_file))
}


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
#' @examples \dontrun{
#'
#' quiz_path <- good_quiz_path()
#'
#' # Provide path to quiz to convert
#' convert_quiz(quiz_path)
#' }
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
#'
#' file.copy(
#'   from = good_quiz_path(),
#'   to = file.path(tdir, basename(good_quiz_path()))
#' )
#'
#' # Provide path to directory of quizzes
#' convert_coursera_quizzes(tdir)
#'
#' system("rm -r coursera_quizzes")
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

#' Parse quiz into a data.frame
#'
#' @param quiz_lines A character vector of the contents of the markdown
#' file obtained from readLines()
#' @param remove_tags TRUE/FALSE remove tags and empty lines?
#' @return A data frame containing a type column which indicates what type of line each is.
#'
#' @export
#'
#' @examples \dontrun{
#'
#' # Use readLines() to read in a quiz
#' quiz_path <- good_quiz_path()
#' quiz_lines <- readLines(quiz_path)
#'
#' # Can use this to parse the quiz into a data.frame
#' quiz_df <- parse_quiz_df(quiz_lines)
#' }
parse_quiz_df <- function(quiz_lines, remove_tags = FALSE) {
  quiz_df <- tibble::tibble(
    original = quiz_lines,
    trimmed = trimws(quiz_lines, which = "left"),
    index = 1:length(quiz_lines)
  ) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        # Find starts to questions:
        grepl("^\\?", quiz_lines) ~ "prompt",
        # Find which lines are the wrong answer options
        grepl("^[[:lower:]]{1}\\)", quiz_lines) ~ "wrong_answer",
        # Find which lines are the correct answer options
        grepl("^[[:upper:]]{1}\\)", quiz_lines) ~ "correct_answer",
        # Find the tags
        grepl("^\\{", quiz_lines) ~ "tag",
        # Mark empty lines
        nchar(quiz_lines) == 0 ~ "empty",
        # Mark which lines have links
        grepl("\\!\\[|http", quiz_lines) ~ "link",
        # Mark as a fill in the blank
        grepl("^\\!", quiz_lines) ~ "fill_in_blank_answer",
        # Mark everything else as "other
        TRUE ~ "other"
      ),
      # Assign each a question number
      question = cumsum(type == "prompt")
    )

  ###### Find extended prompts
  # Get the starts of prompts
  start_prompt_indices <- which(quiz_df$type == "prompt")

  # Find the line which the footnote ends at
  end_prompt_indices <- sapply(start_prompt_indices,
    find_end_of_prompt,
    type_vector = quiz_df$type
  )

  # Rename "other" as also part of prompts
  for (index in 1:length(start_prompt_indices)) {
    if (start_prompt_indices[index] != end_prompt_indices[index]) {
      # Mark things as a part of prompts
      quiz_df$type[(start_prompt_indices[index] + 1):(end_prompt_indices[index] - 1)] <- "extended_prompt"

      # Mark the end of prompts
      quiz_df$type[end_prompt_indices[index] - 1] <- "end_prompt"
    } else {
      quiz_df$type[start_prompt_indices[index]] <- "single_line_prompt"
    }
  }

  if (remove_tags) {
    quiz_df <- quiz_df %>%
      dplyr::filter(!(type %in% c("tag", "empty")))
  } else {
    # Adjust that tags are actually the start of the next question
    quiz_df$question[which(quiz_df$type == "tag")] <- quiz_df$question[which(quiz_df$type == "tag")] + 1
  }

  return(quiz_df)
}

#' Extract meta fields from a tag
#'
#' @param tags A single tag or vector of tags to extract the fields from.
#' @return A named vector indicating the field and entry associated with it.
#'
#' @export extract_meta
#'
#' @examples
#'
#' ### Simple example
#' tag <- "{quiz, id: quiz_name_here, attempts: 10}"
#'
#' # Extract metadata tags
#' meta <- extract_meta(tag)
#'
#' ### Example using a file
#' quiz_path <- good_quiz_path()
#' quiz_lines <- readLines(quiz_path)
#'
#' # Put this in a data.frame so we can identify the content
#' quiz_df <- parse_quiz_df(quiz_lines)
#'
#' # Extract the tags
#' tags <- quiz_df %>%
#'   dplyr::filter(type == "tag") %>%
#'   dplyr::pull("original")
#'
#' # Extract metadata tags
#' meta <- extract_meta(tags)
extract_meta <- function(tags) {
  # trim whitespace
  tags <- trimws(tags)

  # Remove brackets
  tags <- stringr::str_remove_all(tags, "^\\{quiz, |\\}$")

  # Split by commas
  tags <- strsplit(tags, ",")

  # Parse each tag
  meta <- lapply(tags, parse_q_tag)

  # Make it a named list
  meta <- unlist(meta)

  return(meta)
}

#' Parse apart a tag
#'
#' @param tag A single tag to extract from
#' @return A named vector indicating the field and entry associated with it.
#'
#' @export
#' @examples
#'
#' tag <- "{quiz, id: quiz_name_here, attempts: 10}"
#' parse_q_tag(tag)
parse_q_tag <- function(tag) {
  # Trim whitespace
  tag <- trimws(tag)

  # Get rid of empty lines
  tag <- tag[tag != ""]

  # Split by commas
  tag <- strsplit(tag, ":")

  parsed_tag <- lapply(tag, function(field) {
    field <- trimws(field)
    if (length(field) > 1) {
      field_name <- field[1]
      field_spec <- field[2]

      parsed_field <- field_spec
      names(parsed_field) <- field_name
    } else {
      parsed_field <- field
    }
    return(parsed_field)
  })
  return(parsed_tag)
}

#' Parse Quiz and Other Checking Functions
#'
#' @param quiz_lines A character vector of the contents of the markdown
#' file obtained from readLines()
#' @param quiz_name A character vector indicating the name of the quiz.
#' @param verbose Would you like progress messages? TRUE/FALSE
#' @return A list of elements, including a `data.frame` and metadata
#' for questions
#' @export
#'
#' @examples
#'
#' quiz_lines <- c(
#'   "{quiz, id: quiz_00_filename}",
#'   "### Lesson Name quiz",
#'   "{choose-answers: 4}",
#'   "? What do you think?",
#'   "",
#'   "C) The answer to this one",
#'   "o) Not the answer",
#'   "o) Not the answer either",
#'   "C) Another correct answer",
#'   "m) Mandatory different answer",
#'   "",
#'   "{/quiz}"
#' )
#' quiz_specs <- parse_quiz(quiz_lines)
#' check_quiz_attributes(quiz_specs)
parse_quiz <- function(quiz_lines,
                       quiz_name = NULL,
                       verbose = FALSE) {
  answer <- meta <- repeated <- question <- number <- NULL
  rm(list = c("number", "question", "repeated", "answer", "meta"))

  # Extract only the lines of the actual quiz
  extracted_quiz <- extract_quiz(quiz_lines)

  # Quiz should have at least two lines
  if (length(extracted_quiz$quiz_lines) < 2) {
    stop("Quiz file is empty, double check file contents.")
  }

  # Extract the main quiz metadata
  quiz_meta <- extract_meta(extracted_quiz$quiz_tag)

  # Put this in a data.frame so we can identify the content
  quiz_df <- parse_quiz_df(extracted_quiz$quiz_lines)

  #### Extract metadata
  # Extract the tags
  tags <- quiz_df$original[quiz_df$type == "tag"]

  # Extract metadata tags
  question_meta <- extract_meta(tags)

  # Put the info we need in a list
  quiz_info <- list(
    data = quiz_df,
    question_metadata = question_meta,
    quiz_tag = extracted_quiz$quiz_tag,
    quiz_metadata = quiz_meta
  )
  return(quiz_info)
}


#' Extract lines of the quiz
#'
#' @param quiz_lines A quiz's contents read in with readLines()
#'
#' @return the lines of the quiz that actually contain of the content of the quiz.
#' @export
#' @rdname parse_quiz
extract_quiz <- function(quiz_lines) {
  start <- grep("^\\s*\\{\\s*quiz", quiz_lines)
  end <- grep("^\\s*\\{\\s*/\\s*quiz", quiz_lines)

  if (length(start) == 0) {
    warning("Quiz should start with a { } tag and end with {\\quiz}.")
  }
  if (length(end) == 0) {
    warning("Could not find end tag of quiz; should end with: {\\quiz}.")
  }
  # Extract main quiz tag
  quiz_tag <- quiz_lines[start]

  # Keep only those lines:
  quiz_lines <- quiz_lines[(start + 1):(end - 1)]

  return(list(
    quiz_lines = quiz_lines,
    quiz_tag = quiz_tag
  ))
}

#' Check Quiz Attributes
#'
#' @param quiz_specs The output from [ottrpal::parse_quiz].
#' @param quiz_name A character string indicating the name of the quiz being checked.
#' @param verbose Would you like progress messages? TRUE/FALSE
#'
#' @return A logical
#' @export
#'
#'
check_quiz_attributes <- function(quiz_specs, quiz_name = NULL, verbose = TRUE) {
  # Assume good until otherwise
  metadata_msg <- "good"

  # Set up as tibble
  quiz_metadata <- quiz_specs$quiz_metadata

  # These are the accepted quiz attributes
  quiz_attributes <- c(
    "version",
    "attempts",
    "case-sensitive",
    "id",
    "points",
    "random-choice-order",
    "random-question-order",
    "start-at",
    "version"
  )

  # Find if there are any attributes in names(question_meta) that are unsupported
  unsupported_attributes <- !(names(quiz_metadata) %in% quiz_attributes)

  # If any are unsupported give a warning
  if (any(unsupported_attributes)) {
    # Get the attribute
    unsupported_attributes <- names(quiz_metadata)[unsupported_attributes]

    # Build message
    metadata_msg <- paste0(
      quiz_name, " has attributes that aren't relevant for quizzes: ",
      paste(unsupported_attributes, collapse = ", ")
    )
    # Now print it out
    warning(metadata_msg)
  }

  return(metadata_msg)
}
#' Check a question's attributes
#'
#' This is ran automatically by [ottrpal::check_all_questions] for all questions.
#' It checks that the attributes specified are accepted ones by Leanpub.
#'
#' @param question_df a data.frame obtained from [ottrpal::parse_quiz_df] and dplyr::group_split(question).
#' @param quiz_name inherited from parse
#' @param verbose print diagnostic messages
#' @return Will return a warning for any quiz question attributes used that are not supported.
#' @export
#'
check_quiz_question_attributes <- function(question_df,
                                           quiz_name = NULL,
                                           verbose = TRUE) {
  # Assume good until shown otherwise
  attr_msg <- "good"

  # Extract the tags
  question_meta <- question_df$original[question_df$type == "tags"]

  # These are the accepted question attributes
  quiz_question_attributes <- c(
    "choose-answers",
    "points",
    "random-choice-order"
  )

  # Find if there are any attributes in names(question_meta) that are unsupported
  unsupported_attributes <- !(names(question_meta) %in% quiz_question_attributes)

  # If any are unsupported give a warning
  if (any(unsupported_attributes)) {
    # Get the attribute
    unsupported_attributes <- names(question_meta)[unsupported_attributes]

    attr_msg <- paste0(
      quiz_name, " has attributes that aren't relevant for questions: ",
      paste(unsupported_attributes, collapse = ", ")
    )
    # Now print it out
    warning(attr_msg)
  }

  return(attr_msg)
}

#' Check all quiz questions
#'
#' Takes output from [ottrpal::parse_quiz] and runs checks on each question in a quiz by calling [ottrpal::check_question] for each question.
#' First splits questions into their own data frame. Returns a list of messages/warnings about each question's set up.
#'
#' @param quiz_specs quiz_specs which is output from [ottrpal::parse_quiz].
#' @param quiz_name The name of the quiz being checked.
#' @param verbose Whether progress messages should be given.
#' @param ignore_coursera Coursera doesn't like `!` or `:` in the quizzes. Do not convert quizzes to coursera and ignore ! and : in question prompts that would not be allowed in Leanpub quizzes when converted to a Coursera quiz. Default is to ignore Coursera compatibility.
#'
#' @return A list of the output from [ottrpal::check_question] with messages/warnings regarding each question and each check.
#'
#' @export check_all_questions
#'
#' @examples \dontrun{
#'
#' # Using good quiz md example
#'
#' quiz_path <- good_quiz_path()
#' good_quiz <- readLines(quiz_path)
#' good_quiz_specs <- parse_quiz(good_quiz)
#' good_quiz_checks <- check_all_questions(good_quiz_specs)
#'
#' # Using bad quiz md example
#'
#' bad_quiz <- readLines(bad_quiz_path())
#' bad_quiz_specs <- parse_quiz(bad_quiz)
#' bad_quiz_checks <- check_all_questions(bad_quiz_specs)
#' }
check_all_questions <- function(quiz_specs, quiz_name = NA, verbose = TRUE, ignore_coursera = TRUE) {
  # Remove header part and split into per question data frames
  question_dfs <- quiz_specs$data %>%
    dplyr::filter(question > 0) %>%
    dplyr::group_split(question)

  # Get prompt names
  question_names <- quiz_specs$data %>%
    dplyr::filter(type == "prompt") %>%
    dplyr::pull(original)

  # Remove beginning format
  question_names <- stringr::str_remove(question_names, "^\\? ")

  # Run checks on each question
  question_checks <- lapply(
    question_dfs,
    check_question,
    quiz_name = quiz_name,
    ignore_coursera = ignore_coursera
  )

  # Add names to question check list
  names(question_checks) <- question_names

  question_checks <- dplyr::bind_rows(question_checks, .id = "question_names")

  return(question_checks)
}

#' Check Quiz Question Set Up
#'
#' Check quiz question set up to see if it is compliant with Leanpub and Coursera needs.
#' Based off of [Markua guide](https://leanpub.com/markua/read#leanpub-auto-quizzes-and-exercises).
#' Is called by [ottrpal::check_all_questions] and run for each question.
#'
#' @param question_df Which is an individual question's data frame after being parse from
#' @param quiz_name The name of the quiz the question is from
#' @param verbose Whether progress messages should be given
#' @param ignore_coursera Coursera doesn't like `!` or `:` in the quizzes. Do not convert quizzes to coursera and ignore ! and : in question prompts that would not be allowed in Leanpub quizzes when converted to a Coursera quiz. Default is to ignore Coursera compatibility
#'
#' @return A list of messages/warnings regarding each check for the given question.
#'
#' @export check_question
#'
#' @examples \dontrun{
#'
#' # Use readLines to read in a quiz
#' quiz_path <- good_quiz_path()
#' quiz_lines <- readLines(quiz_path)
#'
#' # Use group_split to get the questions
#' questions_df <- parse_quiz(quiz_lines)$data %>%
#'   dplyr::group_split(question)
#'
#' good_quiz_checks <- check_question(questions_df[[2]])
#' }
check_question <- function(question_df, quiz_name = NA, verbose = TRUE, ignore_coursera = TRUE) {
  # Things are considered innocent until proven guilty
  colon_msg <- tot_ans_msg <- cor_ans_msg <- inc_ans_msg <- exclam_msg <- "good"
  colon_index <- tot_ans_index <- cor_ans_index <- inc_ans_index <- exclam_index <- NA

  # Get index for reporting purposes
  question_start_index <- min(question_df$index)

  # Get prompt if its there
  if (!any(grepl("^\\?", question_df$original))) {
    warning("Could not find prompt for question. Question prompts start line with '?'")
  }
  prompt <- question_df$original[question_df$type == "prompt"]

  prompt <- stringr::str_remove(prompt, "^\\? ")

  # Piece together a quiz identity
  quiz_identity <- paste0(substr(prompt, 0, 20), " ... in quiz: ", quiz_name)

  # Only run this if there is an actual prompt and start to the question
  if (verbose) {
    message(paste0("Checking question: ", quiz_identity))
  }

  ###### Check for no-no symbols:
  colon_index <- question_df %>%
    dplyr::filter(type != "tag", grepl("\\:", original)) %>%
    dplyr::pull(index)

  if (length(colon_index) > 0) {
    # Collapse in case there are multiple infractions
    colon_index <- paste0(colon_index, collapse = ", ")

    colon_msg <- paste0(
      "Colon detected in question on lines: ",
      paste0(colon_index, collapse = ", "),
      " in question starting with:", quiz_identity
    )
    if (!ignore_coursera) {
      warning(colon_msg)
    }
  } else {
    colon_index <- NA
  }

  ###### Parse out and check answer choices
  num_answers <- question_df %>%
    dplyr::group_by(question, type) %>%
    dplyr::count() %>%
    dplyr::filter(grepl("answer", type))

  # Get the counts for each kind of answer:
  correct_answers <- num_answers %>%
    dplyr::filter(type == "correct_answer") %>%
    dplyr::pull(n)

  wrong_answers <- num_answers %>%
    dplyr::filter(type == "wrong_answer") %>%
    dplyr::pull(n)

  fill_in <- num_answers %>%
    dplyr::filter(type == "fill_in_blank_answer") %>%
    dplyr::pull(n)

  total_answers <- sum(num_answers$n)

  # Now warn us if anything is fishy:
  if (length(total_answers) == 0) {
    tot_ans_msg <- paste0("No detected answer options provided for ", quiz_identity)
    warning(tot_ans_msg)
    tot_ans_index <- question_start_index
  }

  if (length(correct_answers) == 0 & length(fill_in) == 0) {
    cor_ans_msg <- paste0("No correct answers provided for ", quiz_identity)
    warning(cor_ans_msg)
    cor_ans_index <- question_start_index
  }

  if (length(wrong_answers) == 0 & length(fill_in) == 0) {
    inc_ans_msg <- paste0("No incorrect answer options provided for ", quiz_identity)
    warning(inc_ans_msg)
    inc_ans_index <- question_start_index
  }

  #### If choose answer, make sure that there are more answers than specified in tag
  if ("tag" %in% question_df$type) {
    # Retrieve lines with tags
    tags <- question_df$original[question_df$type == "tag"]

    # Extract metadata
    question_meta <- extract_meta(tags)

    # Check the attributes
    attr_msg <- check_quiz_question_attributes(question_df,
      quiz_name = quiz_name
    )

    if ("choose-answers" %in% names(question_meta)) {
      choose_answers_num <- as.numeric(question_meta[names(question_meta) == "choose-answers"])

      if (choose_answers_num > total_answers) {
        choos_ans_msg <- paste0(
          "choose-answers number is greater than the number of answers provided in: ",
          quiz_identity
        )
        choos_ans_index <- question_start_index
        warning(choos_ans_msg)
      }
    } else {
      # If choose answers isn't used then put NA for this check
      choos_ans_msg <- NA
    }
  } else {
    # If attributes weren't declared then put NA for this check
    attr_msg <- NA
  }
  #### Check answer formats:
  exclam_index <- question_df %>%
    dplyr::filter(grepl("answer", type)) %>%
    dplyr::filter(grepl("\\:", original)) %>%
    dplyr::pull(index)

  if (length(exclam_index) > 0) {
    # Collapse in case there are multiple infractions
    exclam_index <- paste0(exclam_index, collapse = ", ")

    exclam_msg <- paste0(
      "Exclamation point detected in answer for: ", quiz_identity
    )
    if (!ignore_coursera) {
      warning(exclam_msg)
    }
  } else {
    exclam_index <- NA
  }

  # Put these together in a helpful way
  if (!ignore_coursera) {
    warning_msg <- c(
      colon_msg,
      tot_ans_msg,
      cor_ans_msg,
      inc_ans_msg,
      exclam_msg
    )
    related_index <- c(
      as.numeric(colon_index),
      as.numeric(tot_ans_index),
      as.numeric(cor_ans_index),
      as.numeric(inc_ans_index),
      as.numeric(exclam_index)
    )
  } else {
    warning_msg <- c(
      tot_ans_msg,
      cor_ans_msg,
      inc_ans_msg
    )
    related_index <- c(
      as.numeric(tot_ans_index),
      as.numeric(cor_ans_index),
      as.numeric(inc_ans_index)
    )
  }
  # Store all warning messages as a list; they will say "good" if nothing is detected as wrong
  question_result <- data.frame(
    quiz = rep(quiz_name, length(related_index)),
    warning_msg,
    related_index
  ) %>%
    # Now filter out the good ones
    dplyr::filter(warning_msg != "good")

  return(question_result)
}

#' Check all quizzes in a directory
#'
#' Check the formatting of all quizzes in a given directory.
#'
#' @param path path to the top of course repository (looks for .github folder)
#' @param quiz_dir A path to a directory full of quizzes that should all be checked with [ottrpal::check_all_quizzes].
#' @param verbose print diagnostic messages
#' @param write_report TRUE/FALSE save warning report to a CSV file?
#' @param ignore_coursera Coursera doesn't like `!` or `:` in the quizzes. Do not convert quizzes to coursera and ignore ! and : in question prompts that would not be allowed in Leanpub quizzes when converted to a Coursera quiz. Default is to ignore Coursera compatibility
#'
#' @return A list checks performed on each quiz
#' @importFrom readr write_tsv
#'
#' @export check_quizzes
#'
#' @examples \dontrun{
#'
#'
#' ## Make a temporary quiz directory
#' quiz_dir <- dirname(good_quiz_path())
#'
#' ## Now check the quizzes in that directory
#' all_quiz_results <- check_quizzes(quiz_dir = quiz_dir)
#' }
check_quizzes <- function(path = ".",
                          quiz_dir = "quizzes",
                          write_report = TRUE,
                          verbose = TRUE,
                          ignore_coursera = TRUE) {
  files <- list.files(
    pattern = "\\.md",
    ignore.case = TRUE,
    path = quiz_dir,
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop(paste0("No quizzes found at given path:", quiz_dir))
  }

  all_quiz_results <- lapply(files, function(quiz_path) {
    check_quiz(quiz_path, verbose = verbose, ignore_coursera = ignore_coursera)
  })

  # Name the results with the file names
  names(all_quiz_results) <- basename(files)

  # Only extract the question checks
  question_checks <- lapply(all_quiz_results, function(quiz_report) {
    quiz_report$question_checks
  })

  # Make into one data.frame
  question_report <- dplyr::bind_rows(question_checks) %>%
    dplyr::arrange("quiz")

  if (write_report) {
    if (nrow(question_report) > 0) {
      message("\n Question error report saved to:", file.path(quiz_dir, "question_error_report.tsv"))
      readr::write_tsv(question_report,
        file = file.path(quiz_dir, "question_error_report.tsv")
      )
    } else {
      message("\n No question errors to report!")
    }
  }
  return(question_report)
}

#' Check Quiz
#'
#' For a file path to a quiz, check whether it is properly formatted for Leanpub.
#'
#' @param quiz_path A file path to a quiz markdown file
#' @param verbose print diagnostic messages? TRUE/FALSE
#' @param ignore_coursera Coursera doesn't like `!` or `:` in the quizzes. Do not convert quizzes to coursera and ignore ! and : in question prompts that would not be allowed in Leanpub quizzes when converted to a Coursera quiz. Default is to ignore Coursera compatibility
#'
#' @return A list of checks. "good" means the check passed. Failed checks will report where it failed.
#'
#' @export check_quiz
#'
#' @examples \dontrun{
#'
#' # Take a look at a good quiz's checks:
#' quiz_path <- good_quiz_path()
#' good_checks <- check_quiz(quiz_path)
#'
#' # Take a look at a failed quiz's checks:
#' quiz_path <- bad_quiz_path()
#' failed_checks <- check_quiz(quiz_path)
#' }
check_quiz <- function(quiz_path, verbose = TRUE, ignore_coursera = TRUE) {
  if (verbose) {
    message(paste0("\n Checking quiz: ", quiz_path))
  }
  # Read in quiz
  quiz_lines <- readLines(quiz_path)

  # Have a name for this quiz
  quiz_name <- basename(quiz_path)

  # Parse the quiz
  quiz_specs <- parse_quiz(quiz_lines,
    quiz_name = quiz_name
  )

  # Check main quiz attributes
  meta_checks <- check_quiz_attributes(quiz_specs,
    quiz_name = quiz_name
  )

  # Check each question
  question_checks <- check_all_questions(
    quiz_specs,
    quiz_name = quiz_name,
    verbose = verbose,
    ignore_coursera = ignore_coursera
  )


  return(list(
    quiz_name = quiz_name,
    parsed_quiz = quiz_specs,
    meta_checks = meta_checks,
    question_checks = question_checks
  ))
}
