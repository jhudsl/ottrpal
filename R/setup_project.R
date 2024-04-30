setup_project <- function(path, ...) {

  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Move boilerplate files into path
  # Vector of filenames to be copied
  boilerplate_file <- c("index.qmd")

  # Function to copy each file
  copy_files <- function(file_name) {
    source_path <- system.file(paste0("ottr/", file_name), package = "ottrpal")
    destination_path <- paste0(path, "/", file_name)
    file.copy(source_path, destination_path)
  }

  # Apply the function to each file in the vector
  lapply(boilerplate_file, copy_files)

  # collect inputs
  dots <- list(...)
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0(key, ": ", val)
  })

  # collect into single text string
  contents <- paste(
    paste(header, collapse = "\n"),
    paste(text, collapse = "\n"),
    sep = "\n"
  )

  # write to index file
  # writeLines(contents, con = file.path(path, "INDEX"))
}
