
#' Convert youtube link
#'
#' @param utube_link a link to a youtube video that may or may not be "www.youtube.com/embed" or "www.youtube.com/watch?v="
#' format.
#'
#' @return Returns a youtube link in the "watch" format so it will render properly in Leanpub or Coursera-friendly files
#' @export
#'
convert_utube_link <- function(utube_link) {

  # If it has a youtube embed link, switch to the watch format link
  if (grepl("www.youtube.com/embed", utube_link)) {
    utube_link <- paste0(
      "https://www.youtube.com/watch?v=",
      strsplit(utube_link,
        split = "www.youtube.com/embed/"
      )[[1]][2]
    )
  }
  return(utube_link)
}


get_html_element <- function(x, element = "img") {
  x <- paste(x, collapse = "\n")
  doc <- xml2::read_html(x)
  nodes <- rvest::html_nodes(doc, xpath = paste0("//", element))
  nodes
}


get_figure_div <- function(x) {
  div <- get_html_element(x, element = 'div[@class="figure"]')
  types <- div %>%
    rvest::html_elements(xpath = ".//img|.//iframe") %>%
    rvest::html_name()
  img <- div %>%
    rvest::html_elements(xpath = ".//img|.//iframe") %>%
    rvest::html_attrs()
  # missing something - not a img or iframe
  stopifnot(length(img) == length(div))
  div_aligns <- div %>%
    rvest::html_attr(name = "style")
  div_aligns <- trimws(sub("text-align: ", "", div_aligns))
  # markua
  div_aligns[div_aligns == "center"] <- "middle"
  div_aligns[is.na(div_aligns)] <- "middle"
  captions <- div %>%
    rvest::html_elements(xpath = './p[@class="caption"]') %>%
    rvest::html_text()
  img <- mapply(function(x, y) {
    c(x, caption = y)
  }, img, captions, SIMPLIFY = FALSE)
  img <- mapply(function(x, y) {
    c(x, element_type = y)
  }, img, types, SIMPLIFY = FALSE)
  img <- mapply(function(x, y) {
    y[y %in% ""] <- NA_character_
    if (!all(is.na(y))) {
      x <- c(x, align = y)
    }
    x
  }, img, div_aligns, SIMPLIFY = FALSE)

  return(img)
}



get_html_attr <- function(x, element = "img", name = "src") {
  x <- get_html_element(x, element = element)
  rvest::html_attr(x, name)
}

get_iframe_attr <- function(x, name = "src") {
  x <- get_html_attr(x, element = "iframe", name = name)
}

get_img_attr <- function(x, name = "src") {
  x <- get_html_attr(x, element = "img", name = name)
}

get_iframe_src <- get_iframe_attr
get_img_src <- get_img_attr

get_iframe_alt <- function(x) {
  get_iframe_attr(x, name = "alt")
}

get_img_alt <- function(x) {
  get_img_attr(x, name = "alt")
}




find_figure_div <- function(x) {
  regex <- paste0('<div class="figure"')
  start <- which(grepl(regex, x = x) & !grepl("^<!--", trimws(x)))
  regex <- paste0("</div>")
  end <- which(grepl(regex, x = x) & !grepl("^<!--", trimws(x)))
  stopifnot(length(start) == length(end))
  div_index <- cbind(start = start, end = end)
  diff <- div_index[, 2] - div_index[, 1]
  if (any(diff < 0)) {
    stop("Something is wrong with the `div` tags")
  }
  if (any(diff > 10)) {
    warning(
      "Some divs may be off, could happen with long captions, but",
      ", just a warning"
    )
  }
  # indices = mapply(function(x, y) {
  #   seq(x, y)
  # }, start, end, SIMPLIFY = TRUE)
  # indices = c(indices)
  # indices
  div_index
}


find_iframe <- function(x) {
  regex <- paste0("<iframe")
  grepl(regex, x) & !grepl("^<!--", trimws(x))
}

find_img <- function(x) {
  regex <- paste0("<img")
  xx <- grepl(regex, x = x) & !grepl("^<!--", trimws(x))
}

find_caption <- function(x) {
  regex <- paste0('<p class="caption"')
  caption_start <- grep(regex, x = x)
  regex <- paste0("</p>")
  caption_end <- grep(regex, x = x)
  stopifnot(length(caption_start) == length(caption_end))
  indices <- mapply(function(x, y) {
    seq(x, y)
  }, caption_start, caption_end, SIMPLIFY = TRUE)
  indices <- c(indices)
  indices
}

get_caption <- function(x, element = "p") {
  x <- paste(x, collapse = "\n")
  doc <- xml2::read_html(x)
  nodes <- rvest::html_nodes(doc, xpath = paste0("//", element))
  rvest::html_text(nodes)
}


# don't need this as
# https://leanpub.com/markua/read#leanpub-auto-adding-a-link-around-an-image
# find_double = function(x) {
#   x = "[![](resources/images/unnamed-chunk-3-1.png)](http://www.youtube.com/embed/9bZkp7q19f0?rel=0)"
#   regex = "\\[!\\["
#   # image_tag <- "{alt: 'an image', width=80%}"
# }
na_empty <- function(x) {
  x[is.na(x)] <- ""
  x
}

empty_to_null <- function(x) {
  if (all(x == "")) {
    x <- NULL
  }
  x
}

split_style <- function(x) {
  xx <- strsplit(x, ";")[[1]]
  xx <- strsplit(xx, ":")
  xx <- lapply(xx, trimws)
  xx <- lapply(xx, gsub, pattern = "\\s+", replacement = " ")
  att <- sapply(xx, function(x) x[1])
  xx <- lapply(xx, function(x) x[2:length(x)])
  names(xx) <- att
  xx
}

get_margin <- function(x) {
  if (length(x) == 0) {
    x <- ""
  }
  out <- split_style(x)
  margin <- out$margin
  if (length(margin) > 0) {
    margin <- strsplit(margin, " ")[[1]]
  }
  if (
    (length(margin) == 0 || all(margin == "")) ||
      (length(margin) == 1 && margin == "auto")
  ) {
    margin <- rep("auto", 4)
  }
  margin <- c(margin, rep("0", length = 4 - length(margin)))
  names(margin) <- c("top", "right", "bottom", "left")
  margin
}

remove_div <- function(x) {
  regex <- paste0("^<(/|)div.*>\\s*$")
  gsub(pattern = regex, replacement = "", x)
}

margin_to_align <- function(x) {
  stopifnot(length(x) == 4)
  names(x) <- c("top", "right", "bottom", "left")
  right <- x["right"]
  left <- x["left"]
  if (right == "0" & left == "auto") {
    return("right")
  }
  if (left == "0" & right == "auto") {
    return("left")
  }
  if (left == right) {
    return("middle")
  }
  warning("don't know what the alignment should be, defaulting to middle")
  return("middle")
}


build_image <- function(src, ..., caption = NULL, embed = NULL,
                        fullbleed = FALSE,
                        remove_resources_start = TRUE, element = NULL) {
  if (remove_resources_start) {
    src <- gsub("^resources/", "", src)
  }

  myenv <- list(...,
    caption = caption,
    embed = embed,
    src = src
  )
  myenv <- as.environment(myenv)
  specs <- c(
    'alt: "{alt}",',
    'height: "{height}",',
    'width: "{width}",',
    'align: "{align}",',
    'type: "{type}",',
    'poster: "{poster}",',
    'embed: "{embed}"'
  )
  if (is.null(fullbleed) ||
    length(fullbleed) == 0 ||
    fullbleed == "" ||
    is.na(fullbleed)) {
    fullbleed <- FALSE
  }

  ## Set defaults for items that haven't been specified

  # Default for align is center
  if (is.null(myenv$align)) {
    myenv$align <- "center"
  }

  # Default for width is 100%
  if (is.null(myenv$width)) {
    myenv$width <- "100%"
  }

  # Put everything together
  specs <- sapply(specs, glue::glue, .envir = myenv)

  # Make sure it's coerced as a character
  specs <- unlist(sapply(specs, as.character))

  # Set as fullbleed if TRUE
  specs <- c(specs, if (fullbleed) "fullbleed: true")

  # Collapse it all together and add a new line
  specs <- paste(specs, collapse = " ")
  specs <- paste0("{", specs, "}\n")

  # If caption was set, use that for link
  # Default is to set this for a link
  words <- "Check out this link"

  # If a caption is set use that
  if (!is.null(myenv$caption)) {
    words <- myenv$caption

    # Otherwise if video use this wording
  } else if (!is.null(myenv$type)) {
    if (myenv$type == "video") words <- "Click on the lower right corner to expand the screen"

    # Otherwise if image use this wording
  } else if (!is.null(element)) {
    if (element == "img") words <- ""
  }

  # Default is to not use a !
  link <- paste0("[", words, "](", myenv$src, ")")

  # But if its an image or video, use use !
  if (!is.null(element)) {
    if (element == "img") {
      link <- paste0("![", words, "](", myenv$src, ")")
    }
  }
  if (!is.null(myenv$type)) {
    if (myenv$type == "video") {
      link <- paste0("![", words, "](", myenv$src, ")")
    }
  }

  # Tack on the link
  specs <- paste0(specs, link)

  return(specs)
}

replace_div_data <- function(x, fullbleed = FALSE, remove_resources_start = TRUE,
                             element = NULL) {
  div_index <- find_figure_div(x)
  if (NROW(div_index) == 0) {
    return(x)
  }
  div_indices <- mapply(function(x, y) {
    seq(x, y)
  }, div_index[, 1], div_index[, 2], SIMPLIFY = FALSE)
  # no nested divs
  stopifnot(!anyDuplicated(unlist(div_indices)))
  divs <- lapply(div_indices, function(ind) {
    x[ind]
  })
  images <- lapply(divs, get_figure_div)
  out_images <- sapply(images, function(ii) {
    attributes <- c(
      "src", "alt", "height",
      "width", "style", "caption", "title",
      "embed", "type", "poster"
    )
    if (length(ii) == 1) ii <- ii[[1]]
    args <- as.list(ii)
    for (iattr in attributes) {
      if (!iattr %in% names(args)) {
        args[iattr] <- ""
      }
    }
    args <- lapply(args, empty_to_null)
    args$remove_resources_start <- remove_resources_start
    args$element <- element
    do.call(build_image, args = args)
  })
  first_div_index <- sapply(div_indices, dplyr::first)
  x[first_div_index] <- out_images
  remove_div_indices <- c(unlist(sapply(div_indices, function(x) x[-1])))
  x <- x[-remove_div_indices]
  # just so \n is again right
  ttfile <- tempfile(fileext = ".txt")
  writeLines(x, ttfile)
  x <- readLines(ttfile)
  x
}

replace_image_data <- function(x, element = c("img", "iframe"), fullbleed = FALSE,
                               remove_resources_start = TRUE) {
  element <- match.arg(element)
  func <- switch(element,
    img = find_img,
    iframe = find_iframe
  )
  image_logical <- func(x)
  if (!any(image_logical)) {
    return(x)
  }
  image_index <- which(image_logical)
  image_df <- data.frame(
    start = c(0, image_index - 1),
    end = c(image_index, length(x) + 1)
  )
  image_df$number <- c(0, seq_along(image_index))
  image_df <- image_df[image_df$number > 0, ]
  images <- x[image_logical]

  # need to do some subsetting


  attributes <- c(
    "src", "alt", "height", "width", "style",
    "caption", "title", "fullbleed", "type", "poster"
  )
  # style="display: block; margin: auto;" is center
  image_attributes <- lapply(images, function(x) {
    out <- lapply(attributes, function(name) {
      na_empty(get_html_attr(x = x, name = name, element = element))
    })
    names(out) <- attributes

    # If it has a youtube embed link, switch to the watch format link
    if (grepl("www.youtube.com/embed", out$src)) {
      out$src <- convert_utube_link(out$src)
      # If it's youtube put this image in the tag
      out$type <- "video"
      out$poster <- "http://img.youtube.com/vi/VOCYL-FNbr0/mqdefault.jpg"
    }
    if (length(unlist(out) == 0)) {
      # when <p align = "center>
      msg <- paste0(
        "There may be an HTML issue in this text, when",
        " looking for ", element, " elements"
      )
      # message(msg)
      # warning(msg)
    }
    out$margin <- get_margin(out$style)
    if (is.null(out$caption) && !is.null(out$title)) {
      out$caption <- out$title
    }
    out$align <- margin_to_align(out$margin)
    out <- lapply(out, empty_to_null)
    out
  })


  out_images <- sapply(image_attributes, function(args) {
    args$remove_resources_start <- remove_resources_start
    args$element <- element
    do.call(build_image, args = args)
  })

  out_images <- c(unlist(out_images))
  stopifnot(length(out_images) == length(image_index))
  out_x <- x
  out_x[image_logical] <- out_images
  ttfile <- tempfile(fileext = ".txt")
  writeLines(out_x, ttfile)
  x <- readLines(ttfile)
  x
}




#' Replace HTML and other Tags in Leanpub Markdown
#'
#' @param path path to the markdown files that need replacement.
#' @param fullbleed should the image have the attribute `fullbleed: true`?
#' @param remove_resources_start remove the word `resources/` at the front
#' of any image path.
#' @param footer_text a bit of text that will be added to the
#' end of each file before the references section.
#' @param verbose print diagnostic messages
#'
#' @return A list of output files and diagnostics
#' @export
replace_html <- function(path = "manuscript",
                         remove_resources_start = TRUE,
                         footer_text = NULL,
                         fullbleed = FALSE,
                         verbose = TRUE) {
  md_files <- list.files(
    path = path, pattern = "[.]md$", ignore.case = TRUE,
    full.names = TRUE
  )

  md_files <- lapply(md_files, replace_single_html,
    fullbleed = fullbleed,
    verbose = verbose,
    footer_text = footer_text
  )
  return(md_files)
}

#' @param file individual markdown file
#' @export
#' @rdname replace_html
replace_single_html <- function(file,
                                footer_text = NULL,
                                remove_resources_start = TRUE,
                                fullbleed = FALSE, verbose = TRUE) {
  stopifnot(length(file) == 1 && file.exists(file))
  x <- readLines(file, warn = FALSE)
  if (verbose) {
    message("Replacing Div data")
  }
  x <- replace_div_data(x,
    fullbleed = fullbleed,
    remove_resources_start = remove_resources_start
  )

  if (verbose) {
    message("Replacing image data")
  }
  x <- replace_image_data(x,
    element = "img", fullbleed = fullbleed,
    remove_resources_start = remove_resources_start
  )

  if (verbose) {
    message("Replacing iframe data")
  }
  x <- replace_image_data(x,
    element = "iframe", fullbleed = fullbleed,
    remove_resources_start = remove_resources_start
  )
  if (verbose) {
    message("Converting footnotes")
  }
  x <- convert_footnotes(x)

  # need to actually do changes
  writeLines(x, con = file)

  # Add on footer if it was given
  if (!is.null(footer_text)) {
    add_footer(
      rmd_path = file,
      footer_text
    )
  }
  return(file)
}
