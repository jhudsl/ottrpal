#
# Carrie Wright and Candace Savonen 2021

split_embed <- function(to_replace_embed) {
  # Create a sentence with a link given a line with an include_url function
  #
  # Args:
  #   to_replace_embed: a character string from a line in an Rmd that looks like:
  #                     "knitr::include_url('https://www.youtube.com/embed/yiZQaE0q9BY')"
  #
  # Returns:
  #   a character string with a link to the input url

  # Grab the second set of text in quotes
  link_url <- stringr::word(to_replace_embed, sep = "\\\"|src=", start = 2)

  # Put link in the sentence
  link <- paste0("Check out this [link](", link_url, ").")

  return(link)
}

# to change from embed to watch for lines about youtube videos
fix_youtube <- function(utube) {
  # Create a Markua readable line with a link given an original line with a youtube link
  #
  # Args:
  #   to_replace_embed: a character string from a line in an Rmd that looks like:
  #                     "knitr::include_url('https://www.youtube.com/embed/yiZQaE0q9BY')"
  #
  # Returns:
  #   a character string with a watchable youtube video link via Markua/Leanpub
  utube <- stringr::str_extract(string = utube, pattern = "(?<=embed/).*(?=\\))")

  # Paste it together
  link <- paste0("![Video](", "https://www.youtube.com/watch?v=", utube, ")")

  return(link)
}


# code to modify the lines within the rmd about the embedded files - to instead create a link
split_links <- function(rmd_file) {
  # Update embedded links for a given Rmd to be leanpub/Markua friendly.
  #
  # Args:
  #   rmd_file: the path to an Rmd
  #
  # Returns:
  #   Rmd with updated embedded links with the name `leanpub_ready_` prefix
  # in the filename

  # we want the pipe
  `%>%` <- dplyr::`%>%`

  # Print message
  message(c("Converting ", rmd_file, "..."))

  # Read in file
  rmd_lines <- data.frame(
    original_line = readLines(rmd_file)
  ) %>%
    dplyr::mutate(
      ws_trimmed = trimws(original_line),

      # Find the lines needed to change
      lines2change = dplyr::case_when(

        # Mark commented lines -- keep these the same
        grepl("^<!--", ws_trimmed) ~ "comment",

        # Mark embedded lines that are include_url:
        grepl("include_url", original_line) & !grepl("youtube", ws_trimmed) ~ "knitr_embed",

        # Mark embedded lines that are html iframe src:
        grepl("iframe src", original_line) & !grepl("youtube", ws_trimmed) ~ "html_embed",

        # Mark Youtube lines
        grepl("http", ws_trimmed) & grepl("youtube", ws_trimmed) ~ "youtube",

        # The rest are good
        TRUE ~ "good"
      )
    )

  # Put links +2 any embed
  rmd_lines$lines2change[which(rmd_lines$lines2change == "knitr_embed") + 2] <- "after_embed"

  # Now do the changes!
  rmd_lines_updated <- rmd_lines %>%
    dplyr::mutate(
      updated_lines = dplyr::case_when(
        # If a comment or doesn't have the other stuff; keep as is
        lines2change == "good" | lines2change == "comment" | lines2change == "knitr_embed" ~ original_line,

        # If html embedded and NOT youtube, then run the split_embed function:
        lines2change == "html_embed" ~ split_embed(original_line),

        # If youtube but not the previous, then run
        lines2change == "youtube" ~ split_youtube(original_line),
      )
    )

  # Make a named vector
  updated_lines <- rmd_lines_updated$updated_lines
  names(updated_lines) <- rmd_lines_updated$lines2change

  # Bring the link from the a knitr embedded down two lines
  updated_lines[which(names(updated_lines) == "knitr_embed") + 2] <-
    split_embed(rmd_lines$original_line[which(rmd_lines$lines2change == "knitr_embed")])

  # Write new lines to file
  writeLines(as.vector(updated_lines),
    con = paste0(rmd_file)
  )
}
