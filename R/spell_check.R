spell_check <- function(variables) {

  # Find .git root directory
  root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

  # Set up output file directory
  output_file <- file.path(root_dir, 'check_reports', 'spell_check_results.tsv')

  if (!dir.exists('check_reports')) {
    dir.create('check_reports')
  }

  # Read in dictionary
  dict_file <- file.path(root_dir, 'resources', 'dictionary.txt')
  dictionary <- readLines(dict_file)

  # Declare exclude_files.txt
  exclude_file <- file.path(root_dir, 'resources', 'exclude_files.txt')

  # Read in exclude_files.txt if it exists
  if (file.exists(exclude_file)) {
    exclude_file <- readLines(exclude_file)
  } else {
    exclude_file <- ""
  }

  # Only declare `.Rmd` files but not the ones in the style-sets directory
  files <- list.files(pattern = 'md$', recursive = TRUE, full.names = TRUE)

  if( exclude_file[1] != "") files <- grep(paste0(exclude_file, collapse = "|"), files, invert = TRUE, value = TRUE)

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
    error = function(e){
      message("Spell check did not work. Check that your dictionary is formatted correctly. You cannot have special characters (e.g., Diné) in the dictionary.txt file. You need to use HTML formatting (e.g., Din&eacute;) for these.")
    }
  )

  # Print out how many spell check errors
  write(nrow(sp_errors), stdout())

  # Save spell errors to file temporarily
  readr::write_tsv(sp_errors, output_file)

  message(paste0("Saved to: ", output_file))

}
