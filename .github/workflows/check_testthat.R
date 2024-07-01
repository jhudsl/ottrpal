# Check up on the testthat results
# C. Savonen
# Nov 2023

library(magrittr)


# Find .git root directory
root_dir <- rprojroot::find_root(rprojroot::has_dir(".git"))

# Read in the testthat results
out_file <- list.files(pattern = "testthat.Rout$|Rout.fail$", file.path(root_dir, "check"),
 recursive = TRUE, full.names = TRUE)

# Extract testhat results
testthat_check_content <- readLines(out_file)
testthat_result <- grep("\\[ FAIL", testthat_check_content, value = TRUE)[1]
testthat_result <- unlist(strsplit(testthat_result, "\\||\\[|\\]"))

# Read in standard check results
check_content <- readLines(file.path(root_dir, "check", "ottrpal.Rcheck", "00check.log"))

# Extract standard check results
check_result <- grep("Status\\:", check_content, value = TRUE)
check_result <- unlist(strsplit(check_result, ","))
check_result <- stringr::str_remove(check_result, "Status:| ")

# Format the data into a dataframe
testthat_result_df <- data.frame(result = trimws(testthat_result)) %>%
  dplyr::filter(result != "") %>%
  tidyr::separate(result, sep = " ", into = c("test_name", "num")) %>%
  dplyr::mutate(num = as.numeric(num))

# Do the same for the check results
check_result_df <- data.frame(result = trimws(check_result)) %>%
  tidyr::separate(result, sep = " ", into = c("num", "test_name")) %>%
  dplyr::mutate(num = as.numeric(num)) %>%
  dplyr::select("test_name", "num")

# We only want warnings or errors or fails
fail_num <- dplyr::bind_rows(check_result_df, testthat_result_df) %>%
  dplyr::filter(test_name %in% c("FAIL", "WARN", "WARNINGs", "ERROR")) %>%
  dplyr::summarize(total = sum(num))

fail_num <- as.character(fail_num$total)

# Spit the number out
writeLines(fail_num, con = stdout())
