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
    dplyr::mutate(type = dplyr::case_when(
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

  quiz_df <- quiz_df %>%
    # Now for updating based on type!
    dplyr::mutate(updated_line = dplyr::case_when(
      type %in% c("prompt", "single_line_prompt") ~ stringr::str_replace(quiz_lines, "^\\?", "  prompt:"),
      type %in% c("extended_prompt", "end_prompt") ~ paste0("    ", quiz_lines),
      grepl("answer", type) ~ stringr::str_replace(quiz_lines, "^[[:alpha:]]\\)", "    - answer:"),
      TRUE ~ quiz_lines
    ))

  if (remove_tags) {
    quiz_df <- quiz_df %>%
      dplyr::filter(!(type %in% c("tag", "empty")))
  }

  return(quiz_df)
}

# note this is not for general metadata
# For example, {id: "this is , my id", number: 2}
# will fail in this example
extract_meta <- function(quiz) {
  quiz <- trimws(quiz)
  quiz <- sub("^\\{", "", quiz)
  quiz <- sub("\\}$", "", quiz)
  quiz <- strsplit(quiz, ",")
  out <- lapply(quiz, function(xx) {
    xx <- trimws(xx)
    xx <- xx[!xx %in% ""]
    xxx <- strsplit(xx, ":")
    xxx <- sapply(xxx, function(r) {
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
    xxx <- as.list(xxx)
    if (length(xxx) == 0) {
      xxx <- NULL
    }
    xxx
    # xxx = unlist(c(xxx))
  })
  out
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
#' out <- parse_quiz(quiz_lines)
#' check_quiz_attributes(out)
#'
#'
# quiz_lines <- readLines("quizzes/quiz_ch1.md")

parse_quiz <- function(quiz_lines, verbose = FALSE) {

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
  # Find those lines
  meta <- quiz_df$trimmed[quiz_df$type == "tag"]

  # Extract the items in the meta
  meta <- extract_meta(meta)

  # Extract the main quiz metadata
  quiz_meta <- extract_meta(quiz_meta)[[1]]

  # Put the info we need in a list
  quiz_info <- list(
    data = quiz_df,
    question_metadata = meta,
    original_quiz_specification = full_quiz_spec,
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
#' out <- parse_quiz(quiz)
#' check_quiz_attributes(out)
#' check_quiz_question_attributes(out)
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
#' out <- parse_quiz(quiz)
#' check_quiz_attributes(out)
#' check_quiz_question_attributes(out)
#' @rdname parse_quiz

check_quiz_attributes <- function(quiz, verbose = TRUE) {

  # If quiz has not been parsed, parse it
  if (!is.list(quiz)) {
    quiz <- parse_quiz(quiz)
  }
  quiz_metadata <- quiz$quiz_metadata
  quiz_metadata <- tibble::as_tibble(quiz_metadata)

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

  result <- TRUE
  if (NROW(quiz_metadata) > 0) {
    sd <- setdiff(colnames(quiz_metadata), quiz_attributes)
    if (length(sd) > 0) {
      msg <- paste0(
        "quiz has attributes not specified as appropriate for quizzes:",
        paste(sd, collapse = ", ")
      )
      if (verbose) {
        message(msg)
      }
      warning(msg)
      result <- FALSE
    }
  }
  return(result)
}

#' @export
#' @rdname parse_quiz
#' @param verbose print diagnostic messages
check_quiz_question_attributes <- function(quiz, verbose = TRUE) {
  type <- answer <- meta <- repeated <- question <- number <- NULL
  rm(list = c(
    "number", "question", "repeated", "answer",
    "meta", "type"
  ))

  out <- quiz$data
  quiz_question_attributes <- c(
    "choose-answers",
    "points",
    "random-choice-order"
  )

  if (is.null(out)) {
    return(TRUE)
  }
  result <- TRUE
  out <- out %>%
    dplyr::filter(question >= 1)
  out <- split(out, out$question)
  out <- lapply(out, function(r) {
    question_name <- unique(paste0("question_", r$question))
    if (verbose > 1) {
      message(question_name)
    }
    meta <- r %>%
      dplyr::filter(type == "metadata")
    meta <- extract_meta(meta$original)
    meta <- dplyr::bind_rows(lapply(meta, tibble::as_tibble))
    if (NROW(meta) > 0) {
      sd <- setdiff(colnames(meta), quiz_question_attributes)
      if (length(sd) > 0) {
        msg <- paste0(
          question_name,
          " has attributes that aren't relevant for questions: ",
          paste(sd, collapse = ", ")
        )
        if (verbose) {
          message(msg)
        }
        warning(msg)
        result <<- FALSE
      }
    }
    r <- r %>%
      dplyr::filter(type == "answer")
    if (NROW(r) == 0) {
      result <<- FALSE
      msg <- paste0(question_name, " has no listed answers")
      if (verbose) {
        message(msg)
      }
      warning(msg)
    }
    return(NULL)
  })
  return(result)
}

#' @export
#' @rdname parse_quiz
check_attributes <- function(quiz, verbose = TRUE) {
  if (is.character(quiz)) {
    quiz <- parse_quiz(quiz)
  }
  if (is.list(quiz) && "data" %in% names(quiz)) {
    quiz <- quiz$data
  }
  index <- original <- lead_type <- type <- NULL
  rm(list = c("lead_type", "type", "original", "index"))
  bad <- quiz %>%
    dplyr::mutate(lead_type = dplyr::lead(type)) %>%
    dplyr::filter(type == "metadata" & !lead_type %in% "question")
  if (NROW(bad) > 0) {
    bad <- bad %>%
      dplyr::select(original, index) %>%
      as.data.frame()
    msg <- paste0(
      "Attributes with the next line ",
      "not being a question!  Some may be ",
      "false positives if images are in quizzes"
    )
    if (verbose) {
      message(msg)
    }
    warning(msg)
    print(bad)
    return(FALSE)
  }
  return(TRUE)
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
#' tdir <- tempfile()
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#' tfile <- tempfile(pattern = "quiz_", fileext = ".md", tmpdir = tdir)
#' writeLines(quiz, tfile)
#' check_quizzes(path = tdir)
#'
#' check_quiz(quiz)
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
check_quiz <- function(quiz_path, verbose = TRUE) {

  if (verbose) {
    message(paste0("Checking quiz: ", quiz_path))
  }
  # Read in quiz
  quiz_lines <- readLines(quiz_path)

  # Parse the quiz
  quiz_specs <- parse_quiz(quiz_lines)

  quiz_spec_output <- check_quiz_attributes(quiz_specs)

  quiz_answer_output <- check_quiz_question_attributes(
    quiz_specs,
    verbose = verbose
  )
  quiz_attribute_output <- check_attributes(quiz_specs)

  # Make sure choose answers is > than the number of answers you give. lol
  # Make sure that if you say "choose-answers" you use the C) m) o) notation
  # Make sure if you don't use choose answers but instead use : a, b,c,d you delete the "choose-answers" tag
  # Don't have exclamation points in answers.
  # Make sure there's at least one right answer!
  # Make sure the top of the quiz tag looks something like: {quiz, id: quiz_why_doc, attempts: 10}
  # Make sure all quizzes are listed in Book.txt (I think John made a check for this part already).


  return(list(
    quiz_df = out,
    quiz_answer_output = quiz_answer_output,
    quiz_question_follow_attribute = quiz_attribute_output,
    quiz_spec_output = quiz_spec_output
  ))
}
