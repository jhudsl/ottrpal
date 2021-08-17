# script to fix embedded files for Leanpub
# originally used the raw rmd files before conversion  with bookdown_to_leanpub() - now part of the function at the beginning

library(here)
library(stringr)


split_embed_knitr<-function(to_replace_embed) {
  # grab the second set of text in quotes
  link_url <-strsplit(to_replace_embed, "\\\"")[[1]][2]
  y =  paste0("Check out this [link](", link_url,").")
  y}

split_embed_html<-function(to_replace_embed) {
  # grab contents after src=
  link_url <-strsplit(to_replace_embed, "src=")[[1]][2]
  # only keep the part after "\" and bfore the next element
  link_url <-strsplit(link_url, "\\\"")[[1]][2]
  y =  paste0("Check out this [link](", link_url,").")
  y}

# to find rmd files to modify
files<-list.files(here(), pattern = ".rmd|.Rmd|.RMd|.RMD")

#code to modify the lines within the rmd about the embedded files - to instead create a link
for(i in files){
  tx_i  <- readLines(here::here(i))
  # want lines that have include_url but not youtube
  to_replace_embed_knitr <-tx_i[grepl(pattern = "include_url", tx_i, fixed=TRUE)  &  !grepl("^<!--", trimws(tx_i)) & !grepl("youtube", trimws(tx_i))]
  to_replace_embed_html <-tx_i[grepl(pattern = "iframe src", tx_i, fixed=TRUE)  &  !grepl("^<!--", trimws(tx_i)) & !grepl("youtube", trimws(tx_i))]
  to_replace_embed_knitr_loc <-intersect(intersect(grep(pattern = "include_url", tx_i, fixed=TRUE), grep("^<!--", trimws(tx_i), invert = TRUE)) ,grep("youtube", trimws(tx_i), invert = TRUE))
  to_replace_embed_knitr_loc <- (to_replace_embed_knitr_loc +2) #move one line down outside chunk
  to_replace_embed_html_loc <-intersect(intersect(grep(pattern = "iframe src", tx_i, fixed=TRUE), grep("^<!--", trimws(tx_i), invert = TRUE)), grep("youtube", trimws(tx_i), invert = TRUE))
  # use split_emed_knitr function to replace the knitr version of embedded files
  tx_i[to_replace_embed_knitr_loc] <-unlist(lapply(to_replace_embed_knitr , split_embed_knitr))
  # use split_emed_html function to replace the html version of embedded files
  tx_i[to_replace_embed_html_loc] <-unlist(lapply(to_replace_embed_html , split_embed_html))
  writeLines(tx_i, con=here(i))
  #just in case lets remove the temp version of the file
  rm(tx_i)
}
