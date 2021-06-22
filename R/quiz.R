find_question = function(x) {
  grepl("^\\?", x)
}

extract_number = function(x) {
  bad = !find_question(x)
  out = gsub("^\\?(\\d*)\\s*.*", "\\1", x)
  out[bad] = NA
  out
}

find_answer = function(x) {
  grepl("^([[:alpha:]]\\)|!)", x)
}


find_metadata = function(x) {
  grepl("^\\{", x)
}

# note this is not for general metadata
# For example, {id: "this is , my id", number: 2}
# will fail in this example
extract_meta = function(x) {
  x = trimws(x)
  x = sub("^\\{", "", x)
  x = sub("\\}$", "", x)
  x = strsplit(x, ",")
  out = lapply(x, function(xx) {
    xx = trimws(xx)
    xx = xx[ !xx %in% "" ]
    xxx = strsplit(xx, ":")
    xxx = sapply(xxx, function(r) {
      if (length(r) <= 1) {
        return(r)
      }
      r[2] = paste(r[2:length(r)], collapse = ":")
      r = r[1:2]
      r = trimws(r)
      nr = r[1]
      r = r[2]
      names(r) = nr
      nr = as.list(r)
      r
    })
    xxx = as.list(xxx)
    if (length(xxx) == 0) {
      xxx = NULL
    }
    xxx
    # xxx = unlist(c(xxx))
  })
  out
}


#' Parse Quiz and Other Checking Functions
#'
#' @param x A single filename or a vector of the contents of the markdown
#' file
#'
#' @return A list of elements, including a `data.frame` and metadata
#' for questions
#' @export
#'
#' @examples
#'
#' x = c('{quiz, id: quiz_00_filename}',
#' "### Lesson Name quiz",
#' "{choose-answers: 4}",
#' "? What do you think?",
#' "",
#' "C) The answer to this one",
#' "o) Not the answer",
#' "o) Not the answer either",
#' "C) Another correct answer",
#' "m) Mandatory different answer",
#' "",
#' "{/quiz}")
#' out = parse_quiz(x)
#' check_quiz_attributes(out)
parse_quiz = function(x) {
  if (length(x) == 1 && file.exists(x)) {
    x = readLines(x, warn = FALSE)
  }
  answer = meta = repeated = question = number = NULL
  rm(list = c("number", "question", "repeated", "answer", "meta"))

  df = extract_quiz(x)


  if (length(df) == 0) {
    return(NULL)
  }
  stopifnot(length(df) >= 2)
  quiz_meta = df[1]
  full_quiz_spec = quiz_meta
  quiz_meta = sub("\\{\\s*quiz(,|)", "{", quiz_meta)

  # remove the "/quiz"
  df = df[2:(length(df)-1)]
  df = tibble::tibble(
    original = df,
    x = trimws(df, which = "left"),
    index = 1:length(df)
  )
  # df = df %>%
  # dplyr::filter(!x %in% "")
  df = df %>%
    dplyr::mutate(
      question = find_question(x),
      answer = find_answer(x),
      number = extract_number(x),
      meta = find_metadata(x),
      number = ifelse(number == "", NA, number),
      repeated = duplicated(number) & !is.na(number)
    )
  df = df %>%
    dplyr::select(-number)
  types = df %>%
    dplyr::select(answer, meta, question)
  types = rowSums(types, na.rm = TRUE)
  stopifnot(all(types <= 1))
  df = df %>%
    dplyr::mutate(
      type = dplyr::case_when(
        question ~ "question",
        answer ~ "answer",
        meta ~ "metadata",
        trimws(x) == "" ~ "spacing",
        TRUE ~ "markdown"
      ))
  # trying to caputure ?1 and ?1 for multiple questions
  df = df %>%
    dplyr::mutate(
      question = ifelse(repeated, FALSE, question)
    )
  df = df %>%
    dplyr::mutate(question = cumsum(question))
  # assign the question number to the next line, as it should be the question
  df = df %>%
    dplyr::mutate(question = ifelse(meta, dplyr::lead(question), question))
  df = df %>%
    dplyr::select(-answer, -meta)
  meta = df$x[df$type == "metadata"]
  meta = extract_meta(meta)
  quiz_meta = extract_meta(quiz_meta)[[1]]
  L = list(
    data = df,
    question_metadata = meta,
    original_quiz_specification = full_quiz_spec,
    quiz_metadata = quiz_meta
  )
  L
}




#' @export
#' @rdname parse_quiz
extract_quiz = function(x) {
  xx = find_quiz_indices(x)
  ind = seq(xx[1], xx[2])
  x[ind]
}

#' @export
#' @rdname parse_quiz
find_quiz_indices = function(x) {
  if (length(x) == 1 && file.exists(x)) {
    x = readLines(x, warn = FALSE)
  }
  start = grep("^\\s*\\{\\s*quiz", x)
  end = grep("^\\s*\\{\\s*/\\s*quiz", x)
  stopifnot(
    (length(start) == 1 & length(end) == 1) |
      (length(start) == 0 & length(end) == 0)
  )
  out = c(start, end)
  if (length(out) == 0) {
    out = NULL
  }
  out
}


#' Check Quiz Information
#'
#' @param x The output from [leanbuild::parse_quiz]
#'
#' @return A logical
#' @export
#'
#' @examples
#'
#' x = c('{quiz, id: quiz_00_filename, choose-answers: 4}',
#' "### Lesson Name quiz",
#' "{choose-answers: 4, attempts: 25}",
#' "? What do you think?",
#' "C) The answer to this one",
#' "o) Not the answer",
#' "o) Not the answer either",
#' "C) Another correct answer",
#' "m) Mandatory different answer",
#' "{/quiz}")
#' out = parse_quiz(x)
#' check_quiz_attributes(out)
#' check_quiz_question_attributes(out)
#'
#' x = c('{quiz, id: quiz_00_filename, choose-answers: 4}',
#' "### Lesson Name quiz",
#' "{choose-answers: 4, attempts: 25}",
#' "",
#' "? What do you think?",
#' "! The answer to this one",
#' "{/quiz}")
#' out = parse_quiz(x)
#' check_quiz_attributes(out)
#' check_quiz_question_attributes(out)
#'
#' @rdname parse_quiz
check_quiz_attributes = function(x, verbose = TRUE) {
  if (is.character(x)) {
    x = parse_quiz(x)
  }
  quiz_metadata = x$quiz_metadata
  quiz_metadata = tibble::as_tibble(quiz_metadata)


  quiz_attributes = c("version",
                      "attempts",
                      "case-sensitive",
                      "id",
                      "points",
                      "random-choice-order",
                      "random-question-order",
                      "start-at",
                      "version")

  result = TRUE
  if (NROW(quiz_metadata) > 0) {
    sd = setdiff(colnames(quiz_metadata), quiz_attributes)
    if (length(sd) > 0) {
      msg = paste0("quiz has attributes not specified as appropriate for quizzes:",
                   paste(sd, collapse = ", "))
      if (verbose) {
        message(msg)
      }
      warning(msg)
      result = FALSE
    }
  }
  return(result)
}

#' @export
#' @rdname parse_quiz
#' @param verbose print diagnostic messages
check_quiz_question_attributes = function(x, verbose = TRUE) {
  type = answer = meta = repeated = question = number = NULL
  rm(list = c("number", "question", "repeated", "answer",
              "meta", "type"))

  out = x$data
  quiz_question_attributes = c("choose-answers",
                               "points",
                               "random-choice-order")

  if (is.null(out)) {
    return(TRUE)
  }
  result = TRUE
  out = out %>%
    dplyr::filter(question >= 1)
  out = split(out, out$question)
  out = lapply(out, function(r) {
    question_name = unique(paste0("question_", r$question))
    if (verbose > 1) {
      message(question_name)
    }
    meta = r %>%
      dplyr::filter(type == "metadata")
    meta = extract_meta(meta$original)
    meta = dplyr::bind_rows(lapply(meta, tibble::as_tibble))
    if (NROW(meta) > 0) {
      sd = setdiff(colnames(meta), quiz_question_attributes)
      if (length(sd) > 0) {
        msg = paste0(question_name,
                     " has attributes that aren't relevant for questions: ",
                     paste(sd, collapse = ", "))
        if (verbose) {
          message(msg)
        }
        warning(msg)
        result <<- FALSE
      }
    }
    r = r %>%
      dplyr::filter(type == "answer")
    if (NROW(r) == 0) {
      result <<- FALSE
      msg = paste0(question_name, " has no listed answers")
      if (verbose) {
        message(msg)
      }
      warning(msg)
    }
    return(NULL)
  })
  return(result)
}



quiz_md_files = function(path = "manuscript") {
  files = list.files(pattern = "quiz.*[.]md", ignore.case = TRUE,
                     path = path, full.names = FALSE)
  return(files)
}

#' @export
#' @rdname parse_quiz
check_attributes = function(x, verbose = TRUE) {
  if (is.character(x)) {
    x = parse_quiz(x)
  }
  if (is.list(x) && "data" %in% names(x)) {
    x = x$data
  }
  index = original = lead_type = type = NULL
  rm(list = c("lead_type", "type", "original", "index"))
  bad = x %>%
    dplyr::mutate(lead_type = dplyr::lead(type)) %>%
    dplyr::filter(type == "metadata" & !lead_type %in% "question")
  if (NROW(bad) > 0) {
    bad = bad %>%
      dplyr::select(original, index) %>%
      as.data.frame()
    msg = paste0(
      "Attributes with the next line ",
      "not being a question!  Some may be ",
      "false positives if images are in quizzes")
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
#' x = c('{quiz, id: quiz_00_filename}',
#' "### Lesson Name quiz",
#' "{choose-answers: 4}",
#' "? What do you think?",
#' "C) The answer to this one",
#' "o) Not the answer",
#' "o) Not the answer either",
#' "C) Another correct answer",
#' "m) Mandatory different answer",
#' "{/quiz}")
#' tdir = tempfile()
#' dir.create(tdir, showWarnings = FALSE, recursive = TRUE)
#' tfile = tempfile(pattern = "quiz_", fileext = ".md", tmpdir = tdir)
#' writeLines(x, tfile)
#' check_quizzes(path = tdir)
#'
#' check_quiz(x)
check_quizzes = function(path = "manuscript",
                         verbose = TRUE) {
  owd = getwd()
  setwd(path)
  on.exit({
    setwd(owd)
  })
  files = quiz_md_files(path = path)
  if (length(files) == 0) return(TRUE)
  result = lapply(files, function(x) {
    if (verbose) {
      message("Checking ", x)
    }
    check_quiz(x, verbose = verbose)
  })
  names(result) = files
  result = sapply(result, function(x) {
    all(x$quiz_answer_output & x$quiz_spec_output)
  })
  return(result)
}

#' @export
#' @rdname check_quizzes
check_quiz = function(path, verbose = TRUE) {
  out = parse_quiz(path)
  quiz_spec_output = check_quiz_attributes(out)
  quiz_answer_output = check_quiz_question_attributes(
    out, verbose = verbose)
  quiz_attribute_output = check_attributes(out)
  return(list(
    quiz_df = out,
    quiz_answer_output = quiz_answer_output,
    quiz_question_follow_attribute = quiz_attribute_output,
    quiz_spec_output = quiz_spec_output
  ))
}


## Anything that starts with a `{` the next line should be a question
#
# add SO MUHC verbosity to everything
