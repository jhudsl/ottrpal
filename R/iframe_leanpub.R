#' Convert Website Course to Leanpub
#'
#' @param path path to the bookdown or quarto course repository, must have a `_bookdown.yml` or `_quarto.yml` file
#' @param chapt_img_key File path to a TSV whose contents are the chapter urls (`url`),
#' the chapter titles (`chapt_title`), the file path to the image to be used for the chapter (`img_path`).
#' Column names `url`, `chapt_title`, and `img_path` must be used.
#' If no chapter title column supplied, the basename of the url will be used,
#' If no image column supplied, default image used.
#' @param html_page The file path of the rendered index.html file
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
#' @param make_book_txt Should [ottrpal::course_to_book_txt()] be run
#' to create a `Book.txt` in the output directory?
#' @param quiz_dir directory that contains the quiz .md files that should be
#' checked and incorporated into the Book.txt file. If you don't have quizzes,
#' set this to NULL
#' @param footer_text Optionally can add a bit of text that will be added to the
#' end of each file before the references section.
#'
#' @return A directory of output files in a folder 'manuscript' for publishing on Leanpub.
#' @export
#'
#' @examples \dontrun{
#'
#' ottrpal::website_to_embed_leanpub(
#'   base_url = "https://jhudatascience.org/OTTR_Template/",
#'   make_book_txt = TRUE,
#'   quiz_dir = NULL
#' )
#' }
website_to_embed_leanpub <- function(path = ".",
                                      chapt_img_key = NULL,
                                      render = NULL,
                                      html_page = file.path(base_url, "index.html"),
                                      base_url = NULL,
                                      default_img = NULL,
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
      html_page = paste0(base_url, "index.html"),
      base_url = file.path(base_url, "no_toc/")
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
    if (verbose) message("Running course_to_book_txt")
    md_files <- basename(unlist(md_output_files))

    course_to_book_txt(
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
#' @param path path to the bookdown or quarto course repository, must have a `_bookdown.yml` or `_quarto.yml` file
#' @param md_files vector of file path of the md's to be included
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param quiz_dir Where are the quizzes stored? Default looks for folder called "quizzes".
#' @param verbose print diagnostic messages
#'
#' @return A list of quiz and chapter files in order in a file called Book.txt -- How Leanpub wants it.
#' @export
#'
course_to_book_txt <- function(path = ".",
                                 md_files = NULL,
                                 output_dir = "manuscript",
                                 quiz_dir = "quizzes",
                                 verbose = TRUE) {
  # If md_files are not specified, then try to get them
  if (is.null(md_files)) {
    # Establish path
    path <- course_path(path)

    rmd_regex <- "[.][R|r]md$"

    # Extract the names of the Rmd files (the chapters)
    md_files <- qrmd_files(path = path)
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
#' @param html_page The file path of the rendered index.html file. It can be a url
#' @param base_url The base url of where the chapters are published -- the url to provide to the iframe in Leanpub
#' e.g. https://jhudatascience.org/OTTR_Template/coursera
#'
#' @return A data.frame of the chapter urls and their titles that are to be ported to Leanpub.
#' This can be passed to
#'
#' @export
#'

get_chapters <- function(html_page = file.path("docs", "index.html"),
                         base_url = ".") {
  # Read in html
  index_html <- suppressWarnings(try(xml2::read_html(html_page)))

  # Extract chapter nodes the Rmd way
  nodes <- rvest::html_nodes(index_html, xpath = paste0("//", 'li[@class="chapter"]'))

  # If the Rmd way didn't work, lets try the quarto way
  if (length(nodes) < 1) {
    # Get the sidebar stuff
    nodes <- rvest::html_nodes(index_html, xpath = paste0("//", 'div[@class="sidebar-item-container"]'))

    # We only want chapters
    nodes <- nodes[grep("chapter",as.character(nodes))]

    # Extract chapter nodes from the sidebar
    chapt_title <- nodes %>%
      rvest::html_nodes('span.chapter-title') %>%
      rvest::html_text()

    data_level <- nodes %>%
      rvest::html_nodes('span.chapter-number') %>%
      rvest::html_text()

    data_path <- nodes %>%
      rvest::html_nodes('a.sidebar-item-text.sidebar-link') %>%
      rvest::html_attr('href') %>%
      stringr::str_remove("^\\.\\/")

    chapt_data <- data.frame(chapt_title, data_level, url = paste0(base_url, "/", data_path))

  } else {
    chapt_data <- rvest::html_attrs(nodes) %>%
      dplyr::bind_rows() %>%
      dplyr::rename_with(~ gsub("-", "_", .x, fixed = TRUE)) %>%
      dplyr::mutate(
        chapt_title = stringr::word(rvest::html_text(nodes), sep = "\n", 1),
        url = paste0(base_url, "/", data_path)
      ) %>%
      dplyr::select(url, chapt_title) %>%
      as.data.frame() %>%
      dplyr::distinct(url, .keep_all = TRUE)
  }

  if (nrow(chapt_data) < 1) {
    stop(paste("Chapter retrieval from:", html_page, "Failed."))
  }

  return(chapt_data)
}

#' A function to make screenshots from an OTTR bookdown website
#' @description This function creates screenshots of course chapters that are stored in a created output directory
#'
#' @param git_pat required argument; a Git secret -- see https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens for more info
#' @param repo required argument; GitHub repository name, e.g., jhudsl/OTTR_Template
#' @param output_dir default is "resources/chapt_screen_images"; Output directory where the chapter's screen images should be stored. For OTTR courses, don't change this unless you've changed the downstream functions accordingly.
#' @param base_url default is NULL; rendered bookdown URL where screenshots are taken from, if NULL, the function will use the repo_name and and git_pat to find the base_url
#' @param path default is to look for OTTR files in current directory based on existence of .github. But if you'd like to run this in a different path, you can point to that file path.
#' @return the file path for file where chapter urls are saved
#'
#' @import dplyr
#' @importFrom webshot2 webshot
#' @importFrom magrittr %>%
#' @importFrom rprojroot find_root has_dir
#'
#' @author Candace Savonen
#'
#' @export
#'
#' @examples \dontrun{
#'
#'  make_screenshots(Sys.getenv("secrets.GH_PAT"), "jhudsl/OTTR_Template")
#'
#' }
make_screenshots <- function(git_pat,
                             repo,
                             output_dir = file.path("resources", "chapt_screen_images"),
                             base_url = NULL,
                             path = "."){

  # Find .git root directory
  root_dir <- find_root(has_dir(".github"), path = path)

  output_folder <- file.path(output_dir)
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  if (is.null(base_url)){
    base_url <- ottrpal::get_pages_url(repo_name = repo, git_pat = git_pat) #what if these arguments are still NULL/not supplied?
    base_url <- gsub("/$", "", base_url)
  }

  # Collect all the chapter pages for the url given
  chapt_df <- ottrpal::get_chapters(html_page = file.path(root_dir, "docs", "index.html"),
                                    base_url = base_url)

  # Now take screenshots for each
  file_names <- lapply(chapt_df$url, function(url){
    file_name <- gsub(".html",
                      ".png",
                      file.path(output_folder, basename(url))
    )

    # Get rid of special characters because leanpub no like
    file_name <- gsub(":|?|!|\\'",
                      "",
                      file_name
    )

    # Take the screenshot
    webshot(url, file = file_name)

    return(file_name)
  })

  # Save file of chapter urls and file_names
  chapt_df <- chapt_df %>%
    dplyr::mutate(img_path = unlist(file_names))

  chapt_df %>%
    readr::write_tsv(file.path(output_folder, "chapter_urls.tsv"))

  message(paste("Image Chapter key written to: ", file.path(output_folder, "chapter_urls.tsv")))

  return(file.path(output_folder, "chapter_urls.tsv"))

}
