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
    footnote_tag <- paste0("[^note", footnote_number, "]")
    
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
    content <- append(content, c("\n", footnotes))
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
