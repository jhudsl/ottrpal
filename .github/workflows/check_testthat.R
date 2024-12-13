# Check up on the testthat results
# C. Savonen
# Nov 2023

library(magrittr)


# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".github"))

message(root_dir)

out_file <- list.files(pattern = ".Rout$|Rout.fail$", file.path(root_dir, "check"),
 recursive = TRUE, full.names = TRUE)

 message(out_file)

check_content <- readLines(out_file)
test_result <- grep("\\[ FAIL", check_content, value = TRUE)[1]
test_result <- unlist(strsplit(test_result, "\\||\\[|\\]"))

# Format the data into a dataframe
test_result_df <- data.frame(result = trimws(test_result)) %>%
  dplyr::filter(result != "") %>%
  tidyr::separate(result, sep = " ", into = c("test_name", "num")) %>%
  dplyr::mutate(num = as.numeric(num))

fail_num <- test_result_df %>%
  dplyr::filter(test_name %in% c("FAIL", "WARN")) %>%
  dplyr::summarize(total = sum(num))

fail_num <- as.character(fail_num$total)

# Spit the number out
writeLines(fail_num, con = stdout())
