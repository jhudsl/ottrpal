# script to fix videos for Leanpub
# originally used the output of the bookdown_to_leanpub() - now part of the function at the end

library(here)
library(stringr)

# to change from embed to watch for lines about youtube videos
split_youtube<-function(utube) {
  alt <- str_extract(string = utube, pattern = "(?<=\\[).*(?=\\])")
  utube <-str_extract(string = utube, pattern = "(?<=embed/).*(?=\\))")
  y =  paste0( "![Video](", "https://www.youtube.com/watch?v=", utube, ")")
  y}

# to find md files to modify
files<-list.files(here("manuscript"), pattern = ".md")
# to only modify original - will thus overwrite any "leanpub_ready" markdown files which are only created with the following code
files <- files[!grepl(pattern = "leanpub_ready", files)]

#code to modify the lines within the md about the youtube videos - both attributes and the actual video call
for(i in files){
  tx_i  <- readLines(here::here("manuscript", i))
  # want lines that have youtube and https in them but not commented out
  to_replace_video <-tx_i[grepl(pattern = "youtube", tx_i, fixed=TRUE) & grepl(pattern = "https", tx_i, fixed=TRUE) &  !grepl("^<!--", trimws(tx_i))]
  # use split_youtube function to change the way the video is called
  tx_i[grepl(pattern = "youtube", tx_i) & grepl(pattern = "https", tx_i) &  !grepl("^<!--", trimws(tx_i))]<- unlist(lapply(to_replace_video, split_youtube))
  # will replace attribute line with this so that an image shows and the video works in markua
  r <-"{type: video, poster: 'http://img.youtube.com/vi/VOCYL-FNbr0/mqdefault.jpg',"
  # will replace attribute lines ... one above video line - so first select these lines
  to_replace_video_att <-(intersect(intersect(grep("youtube",tx_i) , grep(pattern = "https", tx_i)), grep("^<!--",tx_i,invert=TRUE))-1)
  tx_i[to_replace_video_att]<-
    str_replace(tx_i[to_replace_video_att], "\\{", r)
  #write new files with the replaced lines but call the files leanpub_ready
  writeLines(tx_i, con=here("manuscript", paste("leanpub_ready", i, sep = "_")))
  #just in case lets remove the temp version of the file
  rm(tx_i)
}

# code to change book.text to include only leanpub_ready md files
Book <-readLines(here("manuscript", "Book.txt"))
# will only change orininal md files... thus the leanpub_ready md files will get rewritten each time code is run
Book[!grepl(pattern = "leanpub_ready", Book)] <- paste("leanpub_ready", Book[!grepl(pattern = "leanpub_ready", Book)], sep = "_")
writeLines(Book, here("manuscript", "Book.txt"))
