#' A function to make screenshots from an OTTR bookdown website
#' @description This function creates screenshots of course chapters that are stored in a created output directory 
#'
#' @param git_pat required argument; a Git secret -- see https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens for more info
#' @param repo required argument; GitHub repository name, e.g., jhudsl/OTTR_Template
#' @param output_dir default is "resources/chapt_screen_images"; Output directory where the chapter's screen images should be stored. For OTTR courses, don't change this unless you've changed the downstream functions accordingly. 
#' @param base_url default is NULL; rendered bookdown URL where screenshots are taken from, if NULL, the function will use the repo_name and and git_pat to find the base_url
#'
#' @return the file path for file where chapter urls are saved
#'
#' @import dplyr
#' @importFrom webshot2 webshot
#' @importFrom magrittr %>%
#' @importFrom rprojroot find_root has_dir
#' @importFrom janitor make_clean_names
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
make_screenshots <- function(git_pat, repo, output_dir = "resources/chapt_screen_images", base_url = NULL){
  
  # Find .git root directory
  root_dir <- find_root(has_dir(".git"))
  
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
  
  # Get file names and make unique
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
    return(gsub(".png", "", file_name)) #remove .png so clean_names is adding any numbers before file extension
  }) %>% 
    make_clean_names() %>% #handle repeat chapter names
    paste0(".png") #add back .png
  
  #add cleaned file names as a column in the dataframe with URLs
  chapt_df <- chapt_df %>%
    dplyr::mutate(img_path = unlist(file_names))
  
  # Now take screenshots for each, referencing the dataframe for the URL and desired filename
  lapply(1:nrow(chapt_df), 
         function(x) webshot(chapt_df$url[x], 
                             file = chapt_df$img_path[x]))
  
  # Save file of chapter urls and file_names
  chapt_df %>% 
    readr::write_tsv(file.path(output_folder, "chapter_urls.tsv"))
  
  message(paste("Image Chapter key written to: ", file.path(output_folder, "chapter_urls.tsv")))
  
  return(file.path(output_folder, "chapter_urls.tsv"))
  
}