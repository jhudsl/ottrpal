
# These will be existing paths to grab from
#' @export
good_quiz_path <- list.files(pattern = "quiz_good.md",
                        system.file('extdata', package = 'leanbuild'),
                        full.names = TRUE)
#' @export
bad_quiz_path <- list.files(pattern = "quiz_bad.md",
                        system.file('extdata', package = 'leanbuild'),
                        full.names = TRUE)

#' Parse quiz into a data.frame
#'
#' @param quiz_lines A character vector of the contents of the markdown
#' file obtained from readLines()
#' @param remove_tags TRUE/FALSE remove tags and empty lines?
#' @param verbose Would you like progress messages? TRUE/FALSE
#' @return A data frame containing a type column which indicates what type of line each is.
#' @export

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

#' @param question_df Which is an individual question's data frame after being parse from
#' @param verbose Whether progress messages should be given
#'
extract_meta <- function(tags) {

  # trim whitespace
  tags <- trimws(tags)

  # Remove brackets
  tags <- stringr::str_remove_all(tags, "^\\{|\\}$")

  # Split by commas
  tags <- strsplit(tags, ",")

  # Parse each tag
  meta <- lapply(tags, parse_q_tag)

  # Make it a named list
  meta <- unlist(meta)

  return(meta)
}

# For example, {id: "this is , my id", number: 2}
parse_q_tag <- function(tag) {

  # Trim whitespace
  tag <- trimws(tag)

  # Get rid of empty lines
  tag <- tag[tag != ""]

  # Split by colons
  tag <- strsplit(tag, ":")

  individual_tags <- sapply(tag, function(r) {
    if (length(r) <= 1) {
      return(r)
    }
    r[2] <- paste(r[2:length(r)], collapse = ":")
    r <- r[1:2]
    r <- trimws(r)
    nr <- r[1]
    r <- r[2]
    names(r) <- nr
    nr <- as.list(r)
    r
  })
  individual_tags <- as.list(individual_tags)
  if (length(individual_tags) == 0) {
    individual_tags <- NULL
  }
  individual_tags
}

#' Parse Quiz and Other Checking Functions
#'
#' @param quiz A character vector of the contents of the markdown
#' file obtained from readLines()
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
#'
#'
#' # quiz_lines <- readLines("quizzes/quiz_ch1.md")
parse_quiz <- function(quiz_lines, quiz_name = NULL, verbose = FALSE) {
  answer <- meta <- repeated <- question <- number <- NULL
  rm(list = c("number", "question", "repeated", "answer", "meta"))

  # Extract only the lines of the actual quiz
  quiz_lines <- extract_quiz(quiz_lines)

  # Quiz should have at least two lines
  if (length(quiz_lines) < 2) {
    stop("Quiz file is empty, double check file contents.")
  }

  # Quiz meta data is in first line (after using extract_quiz)
  full_quiz_spec <- quiz_meta <- quiz_lines[1]

  # Remove the first part of the quiz tag
  quiz_meta <- sub("\\{\\s*quiz(,|)", "{", quiz_meta)

  # remove the "/quiz"
  quiz_lines <- quiz_lines[2:(length(quiz_lines) - 1)]

  # Put this in a data.frame so we can identify the content
  quiz_df <- parse_quiz_df(quiz_lines)

  #### Extract metadata
  # Extract the tags
  tags <- quiz_df$original[quiz_df$type == "tag"]

  # Extract metadata tags
  meta <- extract_meta(tags)

  # Extract the main quiz metadata
  quiz_meta <- extract_meta(quiz_meta)[[1]]

  # Put the info we need in a list
  quiz_info <- list(
    data = quiz_df,
    question_metadata = meta,
    quiz_tag = full_quiz_spec,
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
    stop("Quiz should start with a { } tag and end with {\\quiz}.")
  }
  if (length(end) == 0) {
    stop("Could not find end tag of quiz; should end with: {\\quiz}.")
  }
  # Keep only those lines:
  quiz_lines <- quiz_lines[start:end]

  return(quiz_lines)
}

#' Check Quiz Information
#'
#' @param quiz The output from [leanbuild::parse_quiz]
#'
#' @return A logical
#' @export
#'
#' @examples
#'
#' quiz <- c(
#'   "{quiz, id: quiz_00_filename, choose-answers: 4}",
#'   "### Lesson Name quiz",
#'   "{choose-answers: 4, attempts: 25}",
#'   "? What do you think?",
#'   "C) The answer to this one",
#'   "o) Not the answer",
#'   "o) Not the answer either",
#'   "C) Another correct answer",
#'   "m) Mandatory different answer",
#'   "{/quiz}"
#' )
#' quiz_specs <- parse_quiz(quiz)
#' check_quiz_attributes(quiz_specs)
#' check_quiz_question_attributes(quiz_specs)
#'
#' quiz <- c(
#'   "{quiz, id: quiz_00_filename, choose-answers: 4}",
#'   "### Lesson Name quiz",
#'   "{choose-answers: 4, attempts: 25}",
#'   "",
#'   "? What do you think?",
#'   "! The answer to this one",
#'   "{/quiz}"
#' )
#' quiz_specs <- parse_quiz(quiz)
#' check_quiz_attributes(quiz_specs)
#' check_quiz_question_attributes(quiz_specs)
#'
#' @rdname parse_quiz

check_quiz_attributes <- function(quiz_specs, quiz_name = NULL, verbose = TRUE) {

  # Set up as tibble
  quiz_metadata <- tibble::as_tibble(quiz_specs$quiz_metadata)

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

    # Now print it out
    warning(paste0(
      quiz_name, " has attributes that aren't relevant for quizzes: ",
      paste(unsupported_attributes, collapse = ", ")
    ))
  }

  return(TRUE)
}

#' @export
#' @rdname parse_quiz
#' @param verbose print diagnostic messages
check_quiz_question_attributes <- function(question_df, quiz_name = NULL, verbose = TRUE) {

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

#' Check All Quiz Questions
#'
#' Takes output from [leanbuild::parse_quiz] and runs checks on each question in a quiz by calling [leanbuild::check_question] for each question.
#' First splits questions into their own data frame. Returns a list of messages/warnings about each question's set up.
#'
#' @param quiz_specs quiz_specs which is output from [leanbuild::parse_quiz].
#' @param quiz_name The name of the quiz being checked.
#' @param verbose Whether progress messages should be given.
#'
#' @return A list of the output from [leanbuild::check_question] with messages/warnings regarding each question and each check.
#'
#' @export check_all_questions
#'
#' @examples
#'
#' ## Simple one question example:
#' quiz <- c(
#'   "{quiz, id: quiz_00_filename, choose-answers: 4}",
#'   "### Lesson Name quiz",
#'   "{choose-answers: 4, attempts: 25}",
#'   "? What do you think?",
#'   "C) The answer to this one",
#'   "o) Not the answer",
#'   "o) Not the answer either",
#'   "C) Another correct answer",
#'   "m) Mandatory different answer",
#'   "{/quiz}"
#' )
#' quiz_specs <- parse_quiz(quiz)
#' quiz_checks <- check_all_questions(quiz_specs)
#'
#'
#' ## A more complicated example using good quiz md example
#'
#' good_quiz <- readLines(leanbuild::good_quiz_path)
#' good_quiz_specs <- parse_quiz(good_quiz)
#' check_all_questions(good_quiz_specs)
#'
#' ## A more complicated example using good quiz md example
#'
#' bad_quiz <- readLines(leanbuild::bad_quiz_path)
#' bad_quiz_specs <- parse_quiz(bad_quiz)
#' check_all_questions(bad_quiz_specs)
#'
check_all_questions <- function(quiz_specs, quiz_name = NULL, verbose = TRUE) {

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
    quiz_name = quiz_name
  )

  # Add names to question check list
  names(question_checks) <- question_names

  return(question_checks)
}

#' Check Quiz Question Set Up
#'
#' Check quiz question set up to see if it is compliant with Leanpub and Coursera needs.
#' Based off of [Markua guide](https://leanpub.com/markua/read#leanpub-auto-quizzes-and-exercises).
#' Is called by [leanbuild::check_all_questions] and run for each question.
#'
#' @param question_df Which is an individual question's data frame after being parse from
#' @param quiz_name The name of the quiz the question is from
#' @param verbose Whether progress messages should be given
#'
#' @return A list of messages/warnings regarding each check for the given question.
#'
#' @export check_question
#'
check_question <- function(question_df, quiz_name = NULL, verbose = TRUE) {

  # Things are considered innocent until proven guilty
  colon_msg <- tot_ans_msg <- cor_ans_msg <- inc_ans_msg <- exclam_msg <- "good"

  # Get prompt
  prompt <- question_df$original[question_df$type == "prompt"]

  prompt <- stringr::str_remove(prompt, "^\\? ")

  # Piece together a quiz identity
  quiz_identity <- paste0(substr(prompt, 0, 20), " of quiz: ", quiz_name)

  # Only run this if there is an actual prompt and start to the question
  if (verbose) {
    message(paste0("Checking question: ", quiz_identity))
  }

  ###### Check for no-no symbols in prompt:
  full_prompt <- question_df %>%
    dplyr::filter(grepl("prompt", type))

  # Look for colons
  colon <- stringr::str_detect(full_prompt$original, "\\:")

  if (any(colon)) {
    colon_msg <- paste0(
      "Colon detected in question prompt for: ", quiz_identity,
      "\n Get rid of colon -- will mess up formatting in Coursera."
    )
    warning(colon_msg)
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

  total_answers <- sum(num_answers$n)

  # Now stop if anything is fishy:
  if (total_answers == 0) {
    tot_ans_msg <- paste0("No detected answer options provided for ", quiz_identity)
    warning(tot_ans_msg)
  }

  if (correct_answers == 0) {
    cor_ans_msg <- paste0("No correct answers provided for ", quiz_identity)
    warning(cor_ans_msg)
  }

  if (wrong_answers == 0) {
    inc_ans_msg <- paste0("No incorrect answer options provided for ", quiz_identity)
    warning(inc_ans_msg)
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
  exclam <- stringr::str_detect(question_df$original, "\\!")

  if (any(exclam)) {
    exclam_msg <- paste0(
      "Exclamation point detected in answer for: ", quiz_identity,
      "\n Get rid of exclamation -- will mess up formatting in Leanpub."
    )
    warning(exclam_msg)
  }

    # Store all warning messages as a list; they will say "good" if nothing is detected as wrong
  question_result <- list(
    attributes = attr_msg,
    no_colons = colon_msg,
    total_answers = tot_ans_msg,
    correct_answers = cor_ans_msg,
    incorrect_answers = inc_ans_msg,
    no_exclamations = exclam_msg
  )

  return(question_result)
}

#' Check Quizzes
#'
#' @param path either a path to the directory of quizzes or a full path to a
#' quiz markdown file
#' @param verbose print diagnostic messages
#'
#' @return A list of logical indicators
#' @export
#'
#' @examples
#'
#' quiz <- c(
#'   "{quiz, id: quiz_00_filename}",
#'   "### Lesson Name quiz",
#'   "{choose-answers: 4}",
#'   "? What do you think?",
#'   "C) The answer to this one",
#'   "o) Not the answer",
#'   "o) Not the answer either",
#'   "C) Another correct answer",
#'   "m) Mandatory different answer",
#'   "{/quiz}"
#' )
#'
#' ## Make a temporary quiz directory
#' tdir <- tempfile()
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#' tfile <- tempfile(pattern = "quiz_", fileext = ".md", tmpdir = tdir)
#' writeLines(quiz, tfile)
#'
#' ## Now check the quizzes in that directory
#' check_quizzes(path = tdir)
#'
check_quizzes <- function(path = "quizzes",
                          verbose = TRUE) {
  files <- list.files(
    pattern = "\\.md",
    ignore.case = TRUE,
    path = path,
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop(paste0("No quizzes found at given path:", path))
  }

  result <- lapply(files, function(quiz_path) {
    if (verbose) {
      message("Checking ", quiz_path)
    }
    check_quiz(quiz_path, verbose = verbose)
  })

  names(result) <- files
  result <- sapply(result, function(quiz) {
    all(quiz$quiz_answer_output & quiz$quiz_spec_output)
  })
  return(result)
}

#' Check Quizzes
#'
#' @param quiz_path a full path to a quiz markdown file
#' @param verbose print diagnostic messages
#'
#' @return A list of logical indicators
#' @export
#'
#' @examples
#'
#' check_list <- check_quiz(leanbuild::good_quiz_path)
#'
check_quiz <- function(quiz_path, verbose = TRUE) {
  if (verbose) {
    message(paste0("Checking quiz: ", quiz_path))
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
    verbose = verbose
  )

  return(list(
    quiz_name = quiz_name,
    parsed_quiz = quiz_specs,
    meta_checks = meta_checks,
    question_checks = question_checks
  ))
}
