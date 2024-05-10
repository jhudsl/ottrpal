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
    } else if (style_set == "GDSCN") {
      style_set <- "gdscn"
    } else {
      style_set <- "custom"
    }

    source_path <- system.file(paste0("style-sets/", style_set, "/",  file_name), package = "ottrpal")
    destination_path <- paste0(path, "/", file_name)
    file.copy(source_path, destination_path)
  }


  # Vector of filenames to be copied (non-images)
  boilerplate_file <- c("index.qmd", "intro.qmd", "404.qmd",
                       "style.css", "references.bib", "_quarto.yml")
  # Apply the function to each file in the vector
  lapply(boilerplate_file, copy_files, dots$style_set)

  path_quarto_yml <- paste0(path, "/", "_quarto.yml")

  quarto_yml <- yaml::read_yaml(path_quarto_yml)
  quarto_yml$book$title <- dots$title
  quarto_yml$book$author <- dots$author
  quarto_yml$book$`repo-url` <- dots$repo_url

  if (dots$style_set == "Custom") {
     file.copy(paste0(normalizePath(dirname(dots$logo)), "/logo.png"), paste0(path, "/img/", "logo.png"))
  }

  write(yaml::as.yaml(quarto_yml, handlers = list(logical = yaml::verbatim_logical)),
              path_quarto_yml)

  # Vector of filenames to be copied (images)
  boilerplate_file_img <- c("img/logo.png", "img/favicon.ico",
                            "img/box-images/note.png", "img/box-images/warning.png",
                            "img/box-images/github.png", "img/box-images/dictionary.png",
                            "img/box-images/thinking_face.png", "img/box-images/under_construction.png")
  # Apply the function to each file in the vector
  lapply(boilerplate_file_img, copy_files, dots$style_set)
}
