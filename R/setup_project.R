setup_project <- function(path, ...) {

  # create directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # create img folder within path
  dir.create(paste0(path, "/img"), recursive = TRUE, showWarnings = FALSE)
  # create img/box-images folder within path
  dir.create(paste0(path, "/img/box-images"), recursive = TRUE, showWarnings = FALSE)

  # Move boilerplate files into path

  # Vector of filenames to be copied (non-images)
  boilerplate_file <- c("index.qmd", "intro.qmd", "404.qmd",
                        "favicon.ico", "logo.png", "style.css",
                        "references.bib", "_quarto.yml")

  # Function to copy each file
  copy_files <- function(file_name) {
    source_path <- system.file(paste0("ottr/", file_name), package = "ottrpal")
    destination_path <- paste0(path, "/", file_name)
    file.copy(source_path, destination_path)
  }

  # Apply the function to each file in the vector
  lapply(boilerplate_file, copy_files)

  # Vector of filenames to be copied (non-images)
  boilerplate_file_img <- c("img/logo.png", "img/box-images/note.png", "img/box-images/warning.png",
                            "img/box-images/github.png", "img/box-images/dictionary.png",
                            "img/box-images/thinking_face.png", "img/box-images/under_construction.png")

  # Apply the function to each file in the vector
  lapply(boilerplate_file_img, copy_files)

  # collect inputs
  dots <- list(...)
  text <- lapply(seq_along(dots), function(i) {
    key <- names(dots)[[i]]
    val <- dots[[i]]
    paste0(key, ": ", val)
  })
}
