check_img = function(x) {
  # NONE OF THESE
  "</img>"
}

find_quiz_indices = function(x) {
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
