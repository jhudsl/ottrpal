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

#' Parse Quiz
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
#'
parse_quiz = function(x) {

  answer = meta = repeated = question = number = NULL
  rm(list = c("number", "question", "repeated", "answer", "meta"))
  df = extract_quiz(x)


  if (length(df) == 0) {
    return(NULL)
  }
  stopifnot(length(df) >= 2)
  quiz_meta = df[1]

  # remove the "/quiz"
  df = df[2:(length(df)-1)]
  df = tibble::tibble(
    original = df,
    x = trimws(df, which = "left")
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
  L = list(
    data = df,
    metadata = meta
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



