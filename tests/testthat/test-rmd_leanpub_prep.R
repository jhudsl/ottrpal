
test_that("Get base URL", {

  ### Now run functions we will test
  base_url <- get_pages_url(repo_name = "jhudsl/OTTR_Template",
                            git_pat = Sys.getenv("secrets.GH_PAT"))

  # TODO: Test that the URL can
  testthat::expect_true(base_url == "https://jhudatascience.org/OTTR_Template/")
})

test_that("Get chapters", {
  ### Set up the OTTR repo
  dir <- setup_ottr_template(dir = ".", type = "rmd")

  chapt_df <- get_chapters(html_page = file.path("OTTR_Template-main", "docs", "index.html"))

  testthat::expect_named(chapt_df, c("url", "chapt_title"))

})

test_that("Make screenshots", {
  # We want to make screenshots from the course
  chapt_df_file <- make_screenshots(git_pat = Sys.getenv("secrets.GH_PAT"),
                                    repo = "jhudsl/OTTR_Template",
                                    path = "OTTR_Template-main")

  testthat::expect_equal(chapt_df_file, "resources/chapt_screen_images/chapter_urls.tsv")

  chapt_df <- readr::read_tsv("resources/chapt_screen_images/chapter_urls.tsv")

  # Expect column names should still be the
  expect_names(chapt_df, c("url", "chapt_title", "img_path"))
})

test_that("Set Up", {
  #set_up_leanpub(
  #  make_book_txt = TRUE,
  #  quiz_dir = NULL
  #)
})

  #website_to_embed_leanpub(
  #chapt_img_key = 'resources/chapt_screen_images/chapter_urls.tsv',
  #make_book_txt = TRUE,
  #quiz_dir = NULL)

  ## TEST HERE:
  # 1. Did each chapter get a md in the manuscript folder?
  # 2. Does each md link to the appropriate sceenshot?
  # 3. Did the screenshot file path that's in the md lead to the appropriate file path?

