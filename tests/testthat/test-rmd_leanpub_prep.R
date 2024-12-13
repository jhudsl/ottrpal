if (Sys.getenv("GH_PAT") != "") {

test_that("Get base URL", {
  # Authorize GitHub
  auth_from_secret("github",
                   token = Sys.getenv("GH_PAT"),
                   in_test = FALSE
  )
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

  chapt_df <- readr::read_tsv(file.path("OTTR_Template-main",
                                        "resources",
                                        "chapt_screen_images",
                                        "chapter_urls.tsv"))

  # Expect column names should still be the
  testthat::expect_names(chapt_df, c("url", "chapt_title", "img_path"))

  testthat::expect_number(nrow(chapt_df), 5)
})

test_that("Set Up Leanpub", {
  dir <- setup_ottr_template(dir = ".", type = "rmd")

  # We're going to delete this so we can test making it again
  unlink(file.path(dir, "manuscript"), recursive = TRUE)

  # Now run the iframe maker bit
  website_to_embed_leanpub(
    path = dir,
    chapt_img_key = file.path('resources', 'chapt_screen_images', 'chapter_urls.tsv'),
    make_book_txt = TRUE,
    quiz_dir = NULL,
    output_dir = "manuscript")

  testthat::expect_true(
    file.exists(file.path(dir,
                          "manuscript",
                          "resources",
                          "chapt_screen_images",
                          "introduction.png")))

  # Lets check what the file looks like
  intro <- readLines(file.path(dir, "manuscript", "1-Introduction.md"))

  # Make sure the png is pointed to
  testthat::expect_true(any(grepl("poster:resources/chapt_screen_images/introduction.png", intro)))

  # Make sure we link to the page
  testthat::expect_true(any(grepl("![](https://jhudatascience.org/OTTR_Template/introduction.html)", intro, fixed = TRUE)))

  clean_up()
})

}
