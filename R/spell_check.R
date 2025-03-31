#' Check spelling of all md, rmd, and qmd files
#'
#' @param path path to the bookdown or quarto course repository, must have a
#'   `.github` folder which will be used to establish the top of the repo.
#' @param output_dir A relative file path to the folder (existing or not) that the
#'   output check file should be saved to. Default is "check_reports"
#' @param resources_dir A relative file path to the folder (existing or not) that the
#'   dictionary.txt file and exclude_files.txt will be found. Default is "resources".
#'   If no dictionary.txt file and exclude_files.txt files are found, we will download one.
#' @param file_pattern A file pattern should we be looking for for the files whose
#'   spelling should be tested. Default is "md$". Regex interpreted.
#' @return A file will be saved that lists the broken URLs will be saved to the specified output_dir.
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom spelling spell_check_files
#' @importFrom rprojroot find_root has_dir
#' @importFrom tidyr unnest separate
#' @importFrom readr write_tsv
#'
#' @examples \dontrun {
#'
#' rmd_dir <- setup_ottr_template(dir = ".", type = "rmd", render = FALSE)
#'
#' check_spelling(rmd_dir)
#'
#' # If there are broken URLs they will be printed in a list at "check_reports/url_checks.tsv"
#'
#' qmd_dir <- setup_ottr_template(dir = ".", type = "quarto", render = FALSE)
#'
#' check_spelling(qmd_dir)
#' }
check_spelling <- function(path = ".",
                           output_dir = "check_reports",
                           resources_dir = "resources",
                           file_pattern = "md$") {
  # Find .git root directory
  root_dir <- rprojroot::find_root(path = path, rprojroot::has_dir(".github"))

  resources_dir <- file.path(root_dir, resources_dir)
  output_dir <- file.path(root_dir, output_dir)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(resources_dir)) {
    dir.create(resources_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_file <- file.path(output_dir, "spell_check_results.tsv")
  dictionary_file <- file.path(resources_dir, "dictionary.txt")
  exclude_file <- file.path(resources_dir, "exclude_files.txt")

  # Read in dictionary file if it exists
  if (file.exists(dictionary_file)) {
    dictionary <- readLines(dictionary_file)
  } else {
    dictionary <- ""
  }

  # Read in ignore urls file if it exists
  if (file.exists(exclude_file)) {
    exclude_file <- readLines(exclude_file)
  } else {
    exclude_file <- ""
  }

  # Only declare `.Rmd` files but not the ones in the style-sets directory
  files <- list.files(path = path, pattern = file_pattern, recursive = TRUE, full.names = TRUE)

  if (exclude_file[1] != "") files <- grep(paste0(exclude_file, collapse = "|"), files, invert = TRUE, value = TRUE)

  tryCatch(
    expr = {
      # Run spell check
      sp_errors <- spelling::spell_check_files(files, ignore = dictionary)

      if (nrow(sp_errors) > 0) {
        sp_errors <- sp_errors %>%
          data.frame() %>%
          tidyr::unnest(cols = found) %>%
          tidyr::separate(found, into = c("file", "lines"), sep = ":")
      } else {
        sp_errors <- data.frame(errors = NA)
      }
    },
    error = function(e) {
      stop("Spell check did not work. Check that your dictionary is formatted correctly. You cannot have special characters (non-ASCII) in the dictionary.txt file. You need to use HTML formatting (e.g., Din&eacute;) for these.")
    }
  )

  # Print out how many spell check errors
  write(nrow(sp_errors), stdout())

  # Save spell errors to file temporarily
  readr::write_tsv(sp_errors, output_file)

  message(paste0("Saved to: ", output_file))

  return(as.numeric(nrow(sp_errors)))
}
