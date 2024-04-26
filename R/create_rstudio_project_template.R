create_rstudio_project_template <- function(path, ...) {
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # For each file in the ottr repo,
  # Step 1. Collect inputs and paste together as 'Parameter: Value'
  dots <- list(...)
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0(key, ": ", val)
  })

  # Step 2. Collect into single text string
  contents <- paste(
    paste(header, collapse = "\n"),
    paste(text, collapse = "\n"),
    sep = "\n"
  )

  # Step 3. Write to file
  writeLines(contents, con = file.path(path, "FILE_NAME_HERE"))

  # Repeat Steps 1~3 for each file
}
