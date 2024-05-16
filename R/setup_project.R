#' Template Function to Initiate a New Quarto-Based OTTR Course
#'
#' This function will be invoked by RStudio to create a new RStudio Project
#' containing boilerplates files required to start a new Quarto-based OTTR course.
#'
#' For more details, refer to the RStudio Project Templates link:
#' https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html
#'
#' @param path Path to newly created project
#' @param ... User inputs
#' @noRd
setup_project_quarto <- function(path, ...) {
  # collect inputs
  dots <- list(...)

  # create directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # create img folder within  path
  dir.create(file.path(path, "img"), recursive = TRUE, showWarnings = FALSE)
  # create img/box-images folder within path
  dir.create(file.path(path, "img", "box-images"), recursive = TRUE, showWarnings = FALSE)
  # create .github folder within path
  dir.create(file.path(path, ".github"), recursive = TRUE, showWarnings = FALSE)
  # create .github/workflows folder within path
  dir.create(file.path(path, ".github", "workflows"), recursive = TRUE, showWarnings = FALSE)

  # Vector of filenames to be copied
  boilerplate_file <- c("index.qmd", "intro.qmd", "404.qmd",
                       "style.css", "references.bib", "_quarto.yml", "config_automation.yml")
  # Apply the function to each file in the vector
  lapply(boilerplate_file, copy_files, dots$style_set, path)

  path_quarto_yml <- file.path(path, "_quarto.yml")

  quarto_yml <- yaml::read_yaml(path_quarto_yml)
  quarto_yml$book$title <- dots$title
  quarto_yml$book$author <- dots$author
  quarto_yml$book$`repo-url` <- dots$repo_url

  # For Custom Style set, user provides logo
  if (dots$style_set == "Custom") {
     file.copy(file.path(normalizePath(dirname(dots$logo)), "logo.png"), file.path(path, "img", "logo.png"))
  }

  write(yaml::as.yaml(quarto_yml, handlers = list(logical = yaml::verbatim_logical)),
              path_quarto_yml)

  # Vector of filenames to be copied (images)
  boilerplate_file_img <- c("img/logo.png", "img/favicon.ico",
                            "img/box-images/note.png", "img/box-images/warning.png",
                            "img/box-images/github.png", "img/box-images/dictionary.png",
                            "img/box-images/thinking_face.png", "img/box-images/under_construction.png")
  # Apply the function to each file in the vector
  lapply(boilerplate_file_img, copy_files, dots$style_set, path)

  # Vector of filenames to be copied (Github Actions)
  boilerplate_file_gha <- c(".github/workflows/pull_request.yml", ".github/workflows/delete-preview.yml",
                            ".github/workflows/render-all.yml")
  # Apply the function to each file in the vector
  lapply(boilerplate_file_gha, copy_files, dots$style_set, path)
}

# Utility function to copy each file
copy_files <- function(file_name, style_set, project_path) {
  if (style_set == "FHDaSL") {
    style_set <- "fhdasl"
  } else if (style_set == "ITN") {
    style_set <- "itn"
  } else if (style_set == "AnVIL") {
    style_set <- "anvil"
  } else if (style_set == "GDSCN") {
    style_set <- "gdscn"
  } else {
    style_set <- "custom"
  }

  source_path <- system.file(file.path("style-sets", style_set, file_name), package = "ottrpal")
  destination_path <- file.path(project_path, file_name)
  file.copy(source_path, destination_path)
}
