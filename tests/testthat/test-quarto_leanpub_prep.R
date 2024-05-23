test_that("Setting Up Quarto Repo", {
  dir <- download_ottr_template(dir = "inst/extdata", type = "quarto")
})

test_that("Tocless render for Quarto", {
  # render_without_toc()

  # TODO: This should be functionalized and incorporated into the package
  # curl -o make_screenshots.R https://raw.githubusercontent.com/jhudsl/ottr-reports/main/scripts/make_screenshots.R
  # chapt_urls=$(Rscript --vanilla make_screenshots.R \
  #             --git_pat ${{ secrets.GH_PAT }} \
  #             --repo $GITHUB_REPOSITORY \
  #             --output_dir resources/chapt_screen_images)
})


test_that("Create Leanpub IFrames for Quarto", {
  # ottrpal::bookdown_to_embed_leanpub(
  #    render = FALSE, \
  #    chapt_img_key = 'resources/chapt_screen_images/chapter_urls.tsv', \
  #    make_book_txt = as.logical('${{needs.yaml-check.outputs.make_book_txt}}'), \
  #    quiz_dir = NULL)

  # ottrpal::bookdown_to_embed_leanpub(
  #            render = FALSE, \
  #            chapt_img_key = 'resources/chapt_screen_images/chapter_urls.tsv', \
  #            make_book_txt = as.logical('${{needs.yaml-check.outputs.make_book_txt}}'))
})
