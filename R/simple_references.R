#' Run Simple References
#'
#' @param x A filename of a markdown or Rmarkdown file, or the full output
#' from `readLines` on that file
#' @param bib_files bibliography files to use for pandoc
#' @param add_reference_header Should the `## References` header be added at the
#' end of the output?
#'
#' @return A character vector of the file, with references subbed in
#' @export
simple_references <- function(x, bib_files, add_reference_header = FALSE) {
  if (length(x) == 1 && file.exists(x)) {
    x <- readLines(x, warn = FALSE)
  }
  # indices = grepl("\\[\\s*[@]", x)
  indices <- grepl("@", x)
  # [@bookdown2016 #bookdown2016]
  all_refs <- x[indices]
  # out = x
  bad_string <- "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
  result <- lapply(all_refs, function(out) {
    stopifnot(!any(grepl(bad_string, out)))
    out <- c(out, "", bad_string)
    tfile <- tempfile(fileext = ".md")
    writeLines(out, tfile, sep = "\n")
    pandoc_args <- c(
      paste0("--bibliography=", path.expand(normalizePath(bib_files))),
      "--wrap=none"
    )
    # o1 = tempfile(fileext = ".md")
    # rmarkdown::pandoc_convert(input = tfile,
    #                           to = "markdown_strict",
    #                           output = o1,
    #                           from = "markdown",
    #                           citeproc = FALSE,
    #                           options = NULL)
    o2 <- tempfile(fileext = ".md")
    rmarkdown::pandoc_convert(
      input = tfile,
      to = "markdown_strict",
      output = o2,
      citeproc = TRUE,
      options = pandoc_args
    )
    o2_text <- readLines(o2)
  })
  all_references <- lapply(result, function(o2_text) {
    references <- grep(bad_string, o2_text)
    stopifnot(length(references) == 1)
    if (references == length(o2_text)) {
      return(NULL)
    }
    references <- o2_text[(references + 1):length(o2_text)]
    references <- references[!references %in% ""]
    references
  })
  # dangerous
  final_refs <- unique(unlist(unique(all_references)))
  if (length(final_refs) > 0 && all(nchar(final_refs) > 0)) {
    final_refs <- paste0(final_refs, "\n\n")
  }

  all_output <- lapply(result, function(o2_text) {
    references <- grep(bad_string, o2_text)
    stopifnot(length(references) == 1)
    stopifnot(o2_text[references - 1] == "")
    out <- o2_text[-((references - 1):length(o2_text))]
  })
  all_output <- mapply(function(x, y, z) {
    if (is.null(z)) {
      out <- x
    } else {
      out <- y
    }
    paste(out, collapse = "\n")
  }, all_refs, all_output, all_references, SIMPLIFY = TRUE)
  all_output <- c(unlist(all_output))
  out_x <- x
  out_x[indices] <- all_output

  out_x <- c(
    out_x, "",
    if (add_reference_header && length(final_refs) > 0) "## References",
    final_refs
  )
  out_x
}
