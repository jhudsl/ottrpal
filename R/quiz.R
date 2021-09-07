
# Make sure choose answers is > than the number of answers you give. lol
# Make sure that if you say "choose-answers" you use the C) m) o) notation
# Make sure if you don't use choose answers but instead use : a, b,c,d you delete the "choose-answers" tag
# Don't have exclamation points in answers.
# Make sure there's at least one right answer!
# Make sure the top of the quiz tag looks something like: {quiz, id: quiz_why_doc, attempts: 10}
# Make sure all quizzes are listed in Book.txt (I think John made a check for this part already).


find_question <- function(quiz) {
  grepl("^\\?", quiz)
}

extract_number <- function(quiz) {
  bad <- !find_question(quiz)
  out <- gsub("^\\?(\\d*)\\s*.*", "\\1", quiz)
  out[bad] <- NA
  out
}

find_answer <- function(quiz) {
  grepl("^([[:alpha:]]\\)|!)", quiz)
}


find_metadata <- function(quiz) {
  grepl("^\\{", quiz)
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
#' @param quiz A single filename or a vector of the contents of the markdown
#' file
#'
#' @return A list of elements, including a `data.frame` and metadata
#' for questions
#' @export
#'
#' @examples
#'
#' quiz <- c(
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
#' out <- parse_quiz(x)
#' check_quiz_attributes(out)

quiz <- "../DaSL_Course_Template_Leanpub/quizzes/quiz_ch1.md"
parse_quiz <- function(quiz) {
  if (length(quiz) == 1 && file.exists(quiz)) {
    quiz <- readLines(quiz, warn = FALSE)
  }
  answer <- meta <- repeated <- question <- number <- NULL
  rm(list = c("number", "question", "repeated", "answer", "meta"))

  # Extract only the lines of the actual quiz
  quiz <- extract_quiz(quiz)

  # Quiz should have at least two lines
  if (length(quiz) < 2) {
    stop(paste("Quiz file: ", quiz, " is empty, double check file contents."))
  }

  # Quiz meta data is in first line (after using extract_quiz)
  full_quiz_spec <- quiz_meta <- quiz[1]

  # Remove the first part of the quiz tag
  quiz_meta <- sub("\\{\\s*quiz(,|)", "{", quiz_meta)

  # remove the "/quiz"
  quiz <- quiz[2:(length(quiz) - 1)]

  # Put this in a data.frame so we can identify the content
  quiz_df <- tibble::tibble(
    original = quiz,
    trimmed = trimws(quiz, which = "left"),
    index = 1:length(quiz)
  ) %>%
    # Find which lines are which kinds of items
    dplyr::mutate(
      question = find_question(trimmed),
      answer = find_answer(trimmed),
      number = extract_number(trimmed),
      meta = find_metadata(trimmed),
      number = ifelse(number == "", NA, number),
      repeated = duplicated(number) & !is.na(number)
    ) %>%
    dplyr::select(-number)

  # Find out how many lines of each type
  types <- quiz_df %>%
    dplyr::select(answer, meta, question) %>%
    rowSums(na.rm = TRUE)

  # If one line contains more than one type of thing, then stop
  if (all(types > 1)) {
    # Find which line is a multiple type
    line <- which(types > 1)
    stop(paste0("Quiz parsing error. Line #:", line, " of ", quiz, " is unclear what type of item it is."))
  }

  quiz_df <- quiz_df %>%
    dplyr::mutate(
      type = dplyr::case_when(
        question ~ "question",
        answer ~ "answer",
        meta ~ "metadata",
        trimws(trimmed) == "" ~ "spacing",
        TRUE ~ "markdown"
      )
    ) %>%
    dplyr::mutate(
      question = ifelse(repeated, FALSE, question)
    ) %>%
    # Assign each line to a question number
    dplyr::mutate(question = cumsum(question)) %>%
    # assign the question number to the next line, as it should be the question
    dplyr::mutate(question = ifelse(meta, dplyr::lead(question), question)) %>%
    # Remove the answer and meta columns
    dplyr::select(-answer, -meta)

  #### Extract metadata
  # Find those lines
  meta <- quiz_df$trimmed[quiz_df$type == "metadata"]

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
#' @param quiz a file path to a quiz file or a quiz's contents read in with readLines()
#'
#' @return the lines of the quiz that actually contain of the content of the quiz.
#' @export
#' @rdname parse_quiz
extract_quiz <- function(quiz) {
  quiz_index <- find_quiz_indices(quiz)
  ind <- seq(quiz_index[1], quiz_index[2])

  return(quiz[ind])
}
#' Retrieve quiz index range
#'
#' @param quiz a file path to a quiz file or a quiz's contents read in with readLines()
#'
#' @return the indices that indicate the beginning and end of the quiz itself.
#' Looks for the quiz tag.
#' @export
#' @rdname parse_quiz
#'
find_quiz_indices <- function(quiz) {
  if (length(quiz) == 1 && file.exists(quiz)) {
    quiz <- readLines(quiz, warn = FALSE)
  }
  start <- grep("^\\s*\\{\\s*quiz", quiz)
  end <- grep("^\\s*\\{\\s*/\\s*quiz", quiz)
  stopifnot(
    (length(start) == 1 & length(end) == 1) |
      (length(start) == 0 & length(end) == 0)
  )
  out <- c(start, end)
  if (length(out) == 0) {
    out <- NULL
  }
  out
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
#' out <- parse_quiz(x)
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
#' out <- parse_quiz(x)
#' check_quiz_attributes(out)
#' check_quiz_question_attributes(out)
#' @rdname parse_quiz
check_quiz_attributes <- function(quiz, verbose = TRUE) {
  if (is.character(quiz)) {
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



quiz_md_files <- function(path = "manuscript") {
  files <- list.files(
    pattern = "quiz.*[.]md", ignore.case = TRUE,
    path = path, full.names = FALSE
  )
  return(files)
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
#' check_quiz(x)
check_quizzes <- function(path = "manuscript",
                          verbose = TRUE) {
  owd <- getwd()
  setwd(path)
  on.exit({
    setwd(owd)
  })
  files <- quiz_md_files(path = path)
  if (length(files) == 0) {
    return(TRUE)
  }
  result <- lapply(files, function(quiz) {
    if (verbose) {
      message("Checking ", quiz)
    }
    check_quiz(quiz, verbose = verbose)
  })
  names(result) <- files
  result <- sapply(result, function(quiz) {
    all(quiz$quiz_answer_output & quiz$quiz_spec_output)
  })
  return(result)
}

#' @export
#' @rdname check_quizzes
check_quiz <- function(path, verbose = TRUE) {
  out <- parse_quiz(path)
  quiz_spec_output <- check_quiz_attributes(out)
  quiz_answer_output <- check_quiz_question_attributes(
    out,
    verbose = verbose
  )
  quiz_attribute_output <- check_attributes(out)
  return(list(
    quiz_df = out,
    quiz_answer_output = quiz_answer_output,
    quiz_question_follow_attribute = quiz_attribute_output,
    quiz_spec_output = quiz_spec_output
  ))
}

