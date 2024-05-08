setup_project <- function(path, ...) {
  # collect inputs
  dots <- list(...)

  # create directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # create img folder within  path
  dir.create(paste0(path, "/img"), recursive = TRUE, showWarnings = FALSE)
  # create img/box-images folder within path
  dir.create(paste0(path, "/img/box-images"), recursive = TRUE, showWarnings = FALSE)

  # Move boilerplate files into path

  # Function to copy each file
  copy_files <- function(file_name, style_set) {
    if (style_set == "FHDaSL") {
      style_set <- "fhdasl"
    } else if (style_set == "AnVIL") {
      style_set <- "anvil"
    } else {
      style_set <- "gdscn"
    }

    source_path <- system.file(paste0("ottr/", style_set, "/",  file_name), package = "ottrpal")
    destination_path <- paste0(path, "/", file_name)
    file.copy(source_path, destination_path)
  }


  # Vector of filenames to be copied (non-images)
  boilerplate_file <- c("index.qmd", "intro.qmd", "404.qmd",
                       "style.css", "references.bib", "_quarto.yml")
  # Apply the function to each file in the vector
  lapply(boilerplate_file, copy_files, dots[[1]])


  # Vector of filenames to be copied (images)
  boilerplate_file_img <- c("img/logo.png", "img/favicon.ico",
                            "img/box-images/note.png", "img/box-images/warning.png",
                            "img/box-images/github.png", "img/box-images/dictionary.png",
                            "img/box-images/thinking_face.png", "img/box-images/under_construction.png")
  # Apply the function to each file in the vector
  lapply(boilerplate_file_img, copy_files, dots[[1]])
}
