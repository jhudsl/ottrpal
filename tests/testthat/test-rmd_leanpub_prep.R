
test_that("Create Leanpub IFrames for Rmd", {

  ### Set up the OTTR repo
  dir <- download_ottr_template(dir = ".", type = "rmd")

  dir.exists(dir)

  bookdown::render_book(dir)

  ### Now run functions we will test
  base_url <- ottrpal::get_pages_url(repo_name = "jhudsl/OTTR_Template",
                                     git_pat = Sys.getenv("secrets.GH_PAT"))

  # TODO: Test that the URL can
  #testthat::expect_condition()

  chapt_df <- ottrpal::get_chapters(html_page = file.path("OTTR_Template-main", "docs", "index.html"))

  # We want to make screenshots from the course
  chapt_df_file <- make_screenshots(git_pat = Sys.getenv("secrets.GH_PAT"),
                                    repo = "jhudsl/OTTR_Template",
                                    path = "OTTR_Template-main")

  testthat::expect_equal(chapt_df_file, "resources/chapt_screen_images/chapter_urls.tsv")

  chapt_df <- readr::read_tsv("resources/chapt_screen_images/chapter_urls.tsv")

  # Expect column names should still be the
  expect_names(chapt_df, c("url", "chapt_title", "img_path"))

  ## TEST HERE:
  # 1. Does each chapter have screenshot?
  # 2. Is the file 'resources/chapt_screen_images/chapter_urls.tsv' made fresh?
  # 2. Does chapter_urls.tsv file made have columns with information that are labeled "url", "chapt_title" and "img_path"

  #set_up_leanpub(
  #  make_book_txt = TRUE,
  #  quiz_dir = NULL
  #)

  #website_to_embed_leanpub(
    #chapt_img_key = 'resources/chapt_screen_images/chapter_urls.tsv',
    #make_book_txt = TRUE,
    #quiz_dir = NULL)

  ## TEST HERE:
  # 1. Did each chapter get a md in the manuscript folder?
  # 2. Does each md link to the appropriate sceenshot?
  # 3. Did the screenshot file path that's in the md lead to the appropriate file path?

  unlink(dir, recursive = TRUE)
  file.remove(paste0(dir, ".zip"))
})
