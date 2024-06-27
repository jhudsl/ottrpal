
test_that("Create Leanpub IFrames for Rmd", {

  dir <- download_ottr_template(dir = ".", type = "rmd")

  bookdown::render_book("OTTR_Template-main")

  # TODO: This should be functionalized and incorporated into the package
  # curl -o make_screenshots.R https://raw.githubusercontent.com/jhudsl/ottr-reports/main/scripts/make_screenshots.R
  # Rscript --vanilla make_screenshots.R
  #             --git_pat sys.getEnv("GH_PAT")
  #             --repo fhdsl/OTTR_Template
  #             --output_dir resources/chapt_screen_images)

  ## TEST HERE:
  # 1. Does each chapter have screenshot?
  # 2. Is the file 'resources/chapt_screen_images/chapter_urls.tsv' made fresh?
  # 2. Does chapter_urls.tsv file made have columns with information that are labeled "url", "chapt_title" and "img_path"

  ottrpal::bookdown_to_embed_leanpub(
    render = FALSE,
    chapt_img_key = 'resources/chapt_screen_images/chapter_urls.tsv',
    make_book_txt = TRUE,
    quiz_dir = NULL)

  ## TEST HERE:
  # 1. Did each chapter get a md in the manuscript folder?
  # 2. Does each md link to the appropriate sceenshot?
  # 3. Did the screenshot file path that's in the md lead to the appropriate file path?

  unlink(dir, recursive = TRUE)
})
