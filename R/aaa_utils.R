
# get script path and number of paragraphs
paragraph_from_script <- function(x) {
  if (file.exists(x)) {
    para <- readLines(x, warn = FALSE)
    para <- trimws(para)
    para <- para[!para %in% c("", " ")]
    return(para)
  } else {
    return(NA)
  }
}

n_para <- function(x) {
  x <- paragraph_from_script(x)
  if (length(x) == 0) {
    return(0)
  }
  ifelse(all(is.na(x)), NA, length(x))
}

length0 <- function(x) {
  length(x) == 0
}

length0_to_NA <- function(x) {
  if (length0(x)) {
    x <- NA
  }
  x
}


#' Google Slides Helper Functions
#'
#' @param file markdown file for manuscript
#'
#' @return A scalar character vector
#' @export
#' @rdname gs_helpers
gs_id_from_slide <- function(file) {
  if (!file.exists(file)) {
    return(NA_character_)
  }
  x <- readLines(file, warn = FALSE)
  ind <- grep(x, pattern = "\\[(S|s)lides\\]")
  if (length(ind) > 0) {
    x <- x[ind]
  } else {
    x <- x[grep(x, pattern = ".*presentation/d/")]
  }
  if (!any(grepl("http", x))) {
    return(NA_character_)
  }
  x <- sub(".*\\(\\s*(http.*)\\s*\\).*", "\\1", x)
  x <- unlist(sapply(x, function(r) httr::parse_url(r)$path))
  x <- sub("/edit$", "", x)
  x <- sub("/export/.*", "", x)
  x <- basename(x)
  x <- stats::na.omit(x)
  x <- x[nchar(x) > 5]
  ux <- unique(x)
  if (length(ux) > 1) {
    warning(paste0(
      "Multiple sheets identified! Taking most frequent.",
      "  Please check ",
      file
    ))
    x <- sort(table(x), decreasing = TRUE)
    x <- names(x)[1]
    # x = x[1]
  } else {
    x <- ux
  }
  if (length(x) == 0 || grepl("\\(\\)", x)) {
    return(NA_character_)
  }
  if (nchar(x) < 10) {
    warning(paste0("ID extracted is ", x, ", seems short"))
  }
  return(x)
}

######################################
# this returns the actual links in the text
######################################
#' @export
#' @rdname gs_helpers
get_image_link_from_slide <- function(file) {
  x <- readLines(file, warn = FALSE)
  x <- grep(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", value = TRUE)
  x <- sub(x, pattern = "!\\[(.*)\\]\\(((resources/|)images.*)\\)", replacement = "\\1")
  # if (length(x) == 0) {
  #   return(NA)
  # }
  return(x)
}

######################################
# this returns the actual image filenames referenced
# we will check to see if all images referenced exist
######################################
#' @export
#' @rdname gs_helpers
get_image_from_slide <- function(file) {
  x <- readLines(file, warn = FALSE)
  x <- grep(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", value = TRUE)
  x <- sub(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", replacement = "\\1")
  # if (length(x) == 0) {
  #   return(NA)
  # }
  return(x)
}


list_one_file <- function(x, ending = "pdf") {
  pdfs <- list.files(
    pattern = paste0("[.]", ending),
    path = x,
    full.names = TRUE
  )
  if (length(pdfs) > 1) {
    warning(paste0(
      x, " had more than one ", ending, "! ",
      "Only grabbing first"
    ))
    pdfs <- pdfs[1]
  }
  pdfs <- length0_to_NA(pdfs)
  return(pdfs)
}




png_pattern <- function() {
  paste0(
    "^!\\[.+\\]\\((?!\\.png)\\)|",
    "^!\\[\\]\\((?!\\.png)\\)|",
    "^!\\[.+\\]\\((?!\\.png)\\)|",
    "!\\[.+\\]\\(.+[^.png]\\)|",
    "^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)"
  )
}

yt_pattern <- function() {
  paste0(
    "^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)|",
    "^!\\[.+\\]\\(https\\:\\/\\/youtu.+\\)"
  )
}

is.Token <- function(token) {
  inherits(token, "Token") ||
    (inherits(token, "request") &&
      inherits(token$auth_token, "Token"))
}


na_false <- function(test) {
  test[is.na(test)] <- FALSE
  test
}

na_true <- function(test) {
  test[is.na(test)] <- TRUE
  test
}


os_type <- function() {
  .Platform$OS.type
}

sys_type <- function() {
  if (os_type() == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (os_type() == "unix") {
    # "unix"
    "linux"
  } else {
    stop("Unknown OS")
  }
}


png_url <- function(id, page_id) {
  paste0(
    "https://docs.google.com/presentation/d/",
    id, "/export/png?id=", id,
    "&pageid=", page_id
  )
}

download_png_urls <- function(urls) {
  res <- vapply(urls, function(url) {
    tfile <- tempfile(fileext = ".png")
    out <- httr::GET(
      url, httr::write_disk(tfile),
      httr::content_type(".png")
    )
    httr::stop_for_status(out)
    ctype <- out$headers$`content-type`
    ctype <- strsplit(ctype, " ")[[1]]
    ctype <- sub(";$", "", ctype)
    if (any(ctype == "text/html") &&
      !any(grepl("png", ctype))) {
      stop("Output is not a PNG!")
    }
    tfile
  }, FUN.VALUE = character(1))
  return(res)
}


add_footer <- function(rmd_path, footer_text = NULL) {
  if (is.null(footer_text)) {
    stop("Need character string in footer_text argument to append to end of file.")
  }

  write(as.character(footer_text),
    file = rmd_path,
    append = TRUE
  )
}

#' Convert Bookdown footnotes to Leanpub-formatted footnotes
#'
#' @param content a character vector containing the lines of content from a file read in with readLines()
#'
#' @return a character vector containing the content given but with Leanpub formatted foonotes
#' @export
#' @rdname footnotes
#'
convert_footnotes <- function(content) {
  
  #### Find footnotes
  # For a vector of content read in, look for Bookdown-formatted footnotes and format them as Leanpub wants them
  start_footnote_indices <- grep("\\^\\[", content)

  # Don't bother if there are no footnotes
  if (length(start_footnote_indices) > 0) {
    
    # Find the line which the footnote ends at
    end_footnote_indices <- sapply(start_footnote_indices,
      find_end_of_footnote,
      content = content
    )
    
    ### Build footnotes for the end of the page
    # Number the footnotes: 
    footnote_number <- 1:length(start_footnote_indices)
    
    # Build the footnotenotation we will replace the `^[` with
    footnote_tag <- paste0("[^note ", footnote_number, "]")
    
    # Collapse multiline footnotes: 
    footnotes <- paste0(trimws(content[start_footnote_indices:end_footnote_indices]), collapse = " ")
    
    # Get rid of bookdown formatting in the footnotes 
    footnotes <- stringr::str_remove_all(footnotes, "\\^\\[|\\]$")
    
    # Add footnote tag at the beginning
    footnotes <- paste0(footnote_tag, ": ", footnotes)
    
    #### Remove footnotes from the middle of the page
    # Delete anything after a old footnote tag and put the new footnote tag
    content[start_footnote_indices] <- paste0(stringr::word(content[start_footnote_indices], sep = "\\^\\[", 1), footnote_tag)
  
    # Delete end lines
    content[end_footnote_indices] <- stringr::word(content[end_footnote_indices], sep = "\\]$", 2)

    # Delete middle lines completely 
    find_any_middle_lines <- setdiff(start_footnote_indices:end_footnote_indices, 
                                     c(start_footnote_indices, end_footnote_indices))
    
    content <- content[-find_any_middle_lines]
    
    #### Append footnotes to the end of the file
    content <- append(content, footnotes)
    }
  return(content)
}

# Given an index of the start of a footnote, find the end of it.
find_end_of_footnote <- function(start_footnote_index, content) {

  # See if the end of the footnote is in the same line
  end_bracket <- grepl("\\]$", content[start_footnote_index])

  # Keep looking in each next line until we find it.
  if (end_bracket == FALSE) {
    footnote_index <- start_footnote_index
    while (end_bracket == FALSE) {
      # Add one
      footnote_index <- footnote_index + 1

      # Look in next line
      end_bracket <- grepl("\\]$", content[footnote_index])

      if (footnote_index == length(content) && end_bracket == FALSE) {
        stop(paste("Searched end of file and could not find end of footnote:", content[start_footnote_index]))
      }
    }
    return(footnote_index)
  }
}
