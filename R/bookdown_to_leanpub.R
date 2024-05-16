#' Convert Bookdown to Leanpub
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param render if `TRUE`, then [bookdown::render_book()] will be run on each Rmd.
#' @param verbose print diagnostic messages
#' @param remove_resources_start remove the word `resources/` at the front
#' of any image path.
#' @param run_quiz_checks TRUE/FALSE run quiz checks
#' @param make_book_txt Should [ottrpal::bookdown_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
#' @param quiz_dir directory that contains the quiz .md files that should be
#' checked and incorporated into the Book.txt file. If you don't have quizzes,
#' set this to NULL
#' @param clean_up TRUE/FALSE the old output directory should be deleted and
#' everything created fresh.
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#'
#' @return A list of output files and diagnostics
#' @export
#'
bookdown_to_leanpub <- function(path = ".",
                                render = TRUE,
                                output_dir = "manuscript",
                                make_book_txt = FALSE,
                                quiz_dir = "quizzes",
                                run_quiz_checks = FALSE,
                                remove_resources_start = FALSE,
                                verbose = TRUE,
                                footer_text = NULL,
                                clean_up = FALSE) {
  # Run the set up
  set_up_leanpub(
    path = path,
    embed = FALSE,
    clean_up = clean_up,
    render = render,
    output_dir = output_dir,
    make_book_txt = make_book_txt,
    quiz_dir = quiz_dir,
    run_quiz_checks = run_quiz_checks,
    remove_resources_start = remove_resources_start,
    verbose = verbose,
    footer_text = footer_text
  )

  # Establish path
  path <- bookdown_path(path)

  rmd_regex <- "[.][R|r]md$"

  # Extract the names of the Rmd files (the chapters)
  rmd_files <- bookdown_rmd_files(path = path)

  bib_files <- list.files(pattern = "[.]bib$")

  if (length(bib_files) > 0) {
    pandoc_args <- paste0("--bibliography=", path.expand(normalizePath(bib_files)))
  } else {
    pandoc_args <- NULL
  }

  # run_env = new.env()
  md_files <- sub(rmd_regex, ".md", rmd_files, ignore.case = TRUE)
  md_files <- file.path(output_dir, basename(md_files))

  for (file in md_files) {
    if (verbose > 1) {
      message("Replacing HTML for ", file)
    }
    infile <- normalizePath(file)

    infile <- replace_single_html(infile,
      verbose = verbose > 1,
      remove_resources_start = remove_resources_start,
      footer_text = footer_text
    )

    if (length(bib_files) > 0) {
      if (verbose > 1) {
        message("Making references for ", file)
      }
      writeLines(simple_references(infile, bib_files, add_reference_header = TRUE),
        con = infile, sep = "\n"
      )
    }
  }
  ####################### Book.txt creation ####################################
  out <- NULL
  if (make_book_txt) {
    if (verbose > 1) {
      message("Running bookdown_to_book_txt")
    }
    bookdown_to_book_txt(
      md_files = rmd_files,
      output_dir = output_dir,
      quiz_dir = quiz_dir,
      verbose = verbose
    )
    out <- book_txt_file <- file.path(output_dir, "Book.txt")
  } else {
    # If false, look for Book.txt file to copy to output folder.
    book_txt_file <- file.path(path, "Book.txt")

    if (file.exists(book_txt_file)) {
      # Copy over an existing book.txt file if it exists
      file.copy(from = book_txt_file, to = file.path(output_dir, "Book.txt"))

      out <- book_txt_file <- file.path(output_dir, "Book.txt")
    } else {
      # If none exists and make_book_txt is false: stop.
      stop(paste0(
        "Book.txt file does not exist in the main directory: ", path, "and make_book_txt is set to FALSE.",
        "There is no Book.txt file. Leanpub needs one. Either make one and place it in the directory path or ",
        "use make_book_txt = TRUE and one will be generated for you."
      ))
    }
  }
  ############################# Wrapping up ####################################
  message(paste(
    "Leanpub ready files are saved to",
    output_dir,
    "Go to https://leanpub.com/ to publish them using the GitHub writing mode."
  ))
}

#' Convert Bookdown to Embed version of Leanpub
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param chapt_img_key File path to a TSV whose contents are the chapter urls (`url`),
#' the chapter titles (`chapt_title`), the file path to the image to be used for the chapter (`img_path`).
#' Column names `url`, `chapt_title`, and `img_path` must be used.
#' If no chapter title column supplied, the basename of the url will be used,
#' If no image column supplied, default image used.
#' @param bookdown_index The file path of the rendered bookdown index.html file
#' @param base_url The base url of where the chapters are published -- the url to provide to the iframe in Leanpub
#' e.g. https://jhudatascience.org/OTTR_Template/coursera
#' @param default_img A google slide link to the default image to be used for all chapters
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param render if `TRUE`, then [bookdown::render_book()] will be run on each Rmd.
#' @param verbose print diagnostic messages
#' @param remove_resources_start remove the word `resources/` at the front
#' of any image path.
#' @param run_quiz_checks TRUE/FALSE run quiz checks
#' @param make_book_txt Should [ottrpal::bookdown_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
#' @param quiz_dir directory that contains the quiz .md files that should be
#' checked and incorporated into the Book.txt file. If you don't have quizzes,
#' set this to NULL
#' @param clean_up TRUE/FALSE the old output directory should be deleted and
#' everything created fresh.
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#'
#' @return A directory of output files in a folder 'manuscript' for publishing on Leanpub.
#' @export
#'
#' @examples \dontrun{
#'
#' ottrpal::bookdown_to_embed_leanpub(
#'   base_url = "https://jhudatascience.org/OTTR_Template/",
#'   make_book_txt = TRUE,
#'   quiz_dir = NULL
#' )
#' }
bookdown_to_embed_leanpub <- function(path = ".",
                                      chapt_img_key = NULL,
                                      bookdown_index = file.path(base_url, "index.html"),
                                      base_url = NULL,
                                      clean_up = FALSE,
                                      default_img = NULL,
                                      render = TRUE,
                                      output_dir = "manuscript",
                                      make_book_txt = FALSE,
                                      quiz_dir = "quizzes",
                                      run_quiz_checks = FALSE,
                                      remove_resources_start = FALSE,
                                      verbose = TRUE,
                                      footer_text = "") {
  # Run the set up
  set_up_leanpub(
    path = path,
    embed = TRUE,
    clean_up = clean_up,
    render = render,
    output_dir = output_dir,
    make_book_txt = make_book_txt,
    quiz_dir = quiz_dir,
    run_quiz_checks = run_quiz_checks,
    remove_resources_start = remove_resources_start,
    verbose = verbose
  )

  # If TSV chapter image key file is specified read it in
  if (!is.null(chapt_img_key)) {
    message(paste("Reading in a chapt_img_key TSV file:", chapt_img_key))
    chapt_df <- readr::read_tsv(chapt_img_key)
  } else {
    # If its not supplied, create it from the get_chapters function
    message("Creating a chapt_img_key TSV file")

    if (is.null(base_url)) {
      stop("No base_url is supplied and no chapt_img_key file was supplied. Need one or the other.")
    }
    chapt_df <- get_chapters(
      bookdown_index = paste0(base_url, "index.html"),
      base_url = base_url
    ) %>%
      dplyr::mutate(chapt_title = gsub("\\:|\\?|\\&|\\!|\\'", "", chapt_title))
  }

  # If there's no img_path supplied, then use a default image for each.
  if (!("img_path" %in% colnames(chapt_df))) {
    # If no default image is supplied
    if (is.null(default_img)) {
      default_img <- "https://docs.google.com/presentation/d/1jEUxUY1qXDZ3DUtvTU6NCc6ASG5nx4Gwczv5aAglYX4/edit#slide=id.p"
    }
    # Set up location to store default image
    img_dir <- file.path(output_dir, "embed_chapt_images")

    if (!dir.exists(img_dir)) {
      dir.create(img_dir, recursive = TRUE)
    }

    # Download default image
    chapt_df$img_path <- gs_png_download(url = default_img, output_dir = img_dir, overwrite = TRUE)
  }

  md_output_files <- chapt_df %>%
    # Make the data.frame be in the same order
    dplyr::select(dplyr::any_of("url"), dplyr::any_of("chapt_title"), dplyr::any_of("img_path")) %>%
    # Run it make_embed_markdown on each row
    purrr::pmap(~ make_embed_markdown(url = ..1, chapt_title = ..2, img_path = ..3, footer_text = footer_text))

  ####################### Book.txt creation ####################################
  out <- NULL
  if (make_book_txt) {
    if (verbose) message("Running bookdown_to_book_txt")
    md_files <- basename(unlist(md_output_files))

    bookdown_to_book_txt(
      md_files = md_files,
      output_dir = output_dir,
      quiz_dir = quiz_dir,
      verbose = verbose
    )
    out <- book_txt_file <- file.path(output_dir, "Book.txt")
  } else {
    # If false, look for Book.txt file to copy to output folder.
    book_txt_file <- file.path(path, "Book.txt")

    if (file.exists(book_txt_file)) {
      # Copy over an existing book.txt file if it exists
      file.copy(from = book_txt_file, to = file.path(output_dir, "Book.txt"))

      out <- book_txt_file <- file.path(output_dir, "Book.txt")
    } else {
      # If none exists and make_book_txt is false: stop.
      stop(paste0(
        "Book.txt file does not exist in the main directory: ", path, "and make_book_txt is set to FALSE.",
        "There is no Book.txt file. Leanpub needs one. Either make one and place it in the directory path or ",
        "use make_book_txt = TRUE and one will be generated for you."
      ))
    }
  }

  ############################# Wrapping up ####################################
  message(paste(
    "Leanpub ready files are saved to",
    output_dir,
    "Go to https://leanpub.com/ to publish them using the GitHub writing mode."
  ))
}

#' Create Book.txt file from files existing in quiz directory
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param md_files vector of file path of the md's to be included
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param quiz_dir Where are the quizzes stored? Default looks for folder called "quizzes".
#' @param verbose print diagnostic messages
#'
#' @return A list of quiz and chapter files in order in a file called Book.txt -- How Leanpub wants it.
#' @export
#'
bookdown_to_book_txt <- function(path = ".",
                                 md_files = NULL,
                                 output_dir = "manuscript",
                                 quiz_dir = "quizzes",
                                 verbose = TRUE) {
  # If md_files are not specified, then try to get them
  if (is.null(md_files)) {
    # Establish path
    path <- bookdown_path(path)

    rmd_regex <- "[.][R|r]md$"

    # Extract the names of the Rmd files (the chapters)
    md_files <- bookdown_rmd_files(path = path)
  }

  if (!is.null(quiz_dir)) {
    # Find the quiz files in the quiz directory
    quiz_files <- list.files(pattern = "\\.md$", quiz_dir)

    # Put files in one vector
    all_files <- c(md_files, quiz_files)

    # Make a vector specifying the file type: quiz or not
    file_type <- c(
      rep("non-quiz", length(md_files)),
      rep("quiz", length(quiz_files))
    )
  } else {
    all_files <- md_files
    file_type <- rep("non-quiz", length(md_files))
  }
  # Put all files in one data.frame
  all_files <- data.frame(
    file_name = all_files,
    file_type
  ) %>%
    dplyr::mutate(
      # Use this so we don't have to fiddle with case senstivity for the next step
      lower_filename = tolower(file_name),
      # Get the number from the file name and that will be the order
      num = stringr::str_extract(file_name, "([0-9]+)"),
      num = dplyr::case_when(
        # Put index file first and about file last
        lower_filename == "index.rmd" ~ "0",
        lower_filename == "about.rmd" ~ as.character(length(all_files)),
        TRUE ~ num
      ),
      num = as.numeric(num),
      file_name = gsub(".Rmd$", ".md", file_name)
    ) %>%
    # Put quizzes in order!
    dplyr::arrange(num, file_type) %>%
    dplyr::pull(file_name)

  # Declare output file name
  book_txt <- file.path(output_dir, "Book.txt")

  if (verbose) {
    message(paste0("Autogenerated Book.txt saved to: ", book_txt))
  }
  # need to fix about quiz
  writeLines(all_files, con = book_txt)
}


#' Make Leanpub file that has embed webpage of a chapter
#'
#' @param url The url to the chapter that is to be embed
#' @param chapt_title Title of chapter to be used as file name and printed on iframe
#' @param width_spec How wide should the iframe be in pixels?
#' @param height_spec How high should the iframe be in pixels?
#' @param img_path File path to image to use for poster
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param verbose print diagnostic messages
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#' @return A markdown file with an iframe of the provided chapter
#'
#' @export
make_embed_markdown <- function(url,
                                chapt_title,
                                width_spec = 800,
                                height_spec = 600,
                                img_path,
                                output_dir = "manuscript",
                                verbose = TRUE,
                                footer_text = "") {
  # Arguments:
  #   url: The url to the chapter
  #   chapt_title: The title of the chapter to be used as a header
  #   img: file path to the image to be used in the preview
  #
  # Returns: A markdown document ready for Leanpub and the image copied to the manuscript folder

  chapt_file_name <- gsub(" ", "-", chapt_title)

  # Declare output file
  output_file <- file.path(output_dir, paste0(chapt_file_name, ".md"))

  file_contents <- c(
    paste("#", chapt_title),
    " ",
    paste0(
      "{type: iframe, title:", chapt_title,
      ", width:", width_spec,
      ", height:", height_spec,
      ", poster:", img_path, "}"
    ),
    paste0("![](", url, ")"),
    " ",
    footer_text,
    " "
  )

  write(file_contents, file = output_file)

  manuscript_dir <- file.path(output_dir, dirname(img_path))

  if (!dir.exists(manuscript_dir)) {
    dir.create(manuscript_dir, recursive = TRUE)
  }

  file.copy(from = img_path, to = file.path(output_dir, img_path), overwrite = TRUE)

  if (verbose) {
    message(paste0("Output saved to: ", output_file))
  }

  return(output_file)
}

#' Make Leanpub file that has embed webpage of a chapter
#'
#' @param bookdown_index The file path of the rendered bookdown index.html file
#' @param base_url The base url of where the chapters are published -- the url to provide to the iframe in Leanpub
#' e.g. https://jhudatascience.org/OTTR_Template/coursera
#'
#' @return A data.frame of the chapter urls and their titles that are to be ported to Leanpub.
#' This can be passed to
#'
#' @export
#'
get_chapters <- function(bookdown_index = file.path("docs", "index.html"),
                         base_url = NULL) {
  # Read in html
  index_html <- suppressWarnings(try(xml2::read_html(bookdown_index)))

  # Extract chapter nodes the Rmd way
  nodes <- rvest::html_nodes(index_html, xpath = paste0("//", 'li[@class="chapter"]'))

  # If the Rmd way didn't work, lets try the quarto way
  if (length(nodes) < 0) {
    # Get the sidebar stuff
    nodes <- rvest::html_nodes(index_html, xpath = paste0("//", 'div[@class="sidebar-item-container"]'))

    # We only want chapters
    nodes <- nodes[grep("chapter",as.character(nodes))]

    # Extract chapter nodes from the sidebar
    chapt_titles <- nodes %>%
      rvest::html_nodes('span.chapter-title') %>%
      rvest::html_text()

    data_level <- nodes %>%
      rvest::html_nodes('span.chapter-number') %>%
      rvest::html_text()

    data_path <- nodes %>%
      rvest::html_nodes('a.sidebar-item-text.sidebar-link') %>%
      rvest::html_attr('href') %>%
      stringr::str_remove("^\\.\\/")

    chapt_data <- data.frame(chapt_titles, data_level, data_path)

  } else {
    chapt_data <- rvest::html_attrs(nodes) %>%
      dplyr::bind_rows() %>%
      dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE)) %>%
      dplyr::mutate(
        chapt_title = stringr::word(rvest::html_text(nodes), sep = "\n", 1),
        url = paste0(base_url, data_path)
      ) %>%
      dplyr::select(url, chapt_title) %>%
      as.data.frame() %>%
      dplyr::distinct(url, .keep_all = TRUE)
  }

  if (nrow(chapt_data) < 1) {
    stop(paste("Chapter retrieval from:", bookdown_index, "Failed."))
  }

  return(chapt_data)
}
