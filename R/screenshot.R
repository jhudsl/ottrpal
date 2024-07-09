#' A function to make screenshots from an OTTR bookdown website
#' @description This function creates screenshots of course chapters that are stored in a created output directory 
#' @param git_pat default is NULL; required argument; a Git secret
#' @param repo default is NULL; required argument; GitHub repository name, e.g., jhudsl/OTTR_Template
#' @param output_dir default is "resources/chapt_screen_images"; Output directory where the chapter's screen images should be stored. For OTTR courses, don't change this unless you've changed the downstream functions accordingly. 
#' @param base_url default is NULL; rendered bookdown URL where screenshots are taken from, if NULL, the function will use the repo_name and and git_pat to find the base_url
#' @import cow
#' @import dplyr
#' @importFrom webshot2 webshot
#' @importFrom magrittr %>%
#' @importFrom rprojroot find_root has_dir
#' @author Candace Savonen
make_screenshots <- function(git_pat = NULL, repo = NULL, output_dir = "resources/chapt_screen_images", base_url = NULL){
  
  # Find .git root directory
  root_dir <- find_root(has_dir(".git"))
  
  output_folder <- file.path(output_dir)
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  if (is.null(base_url)){
    base_url <- cow::get_pages_url(repo_name = repo, git_pat = git_pat) #what if these arguments are still NULL/not supplied?
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
  
}