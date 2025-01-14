test_that("Test OTTRfy", {

  dir.create("rmd")
  writeLines("# A title", file.path("rmd", "01-intro.Rmd"))
  writeLines("---\ntitle: An index \n--- \n# Intro", file.path("rmd", "index.Rmd"))
  ottrfy(path = "rmd", type = "rmd", git_commit = FALSE)
  update_chapters("rmd")
  bookdown::render_book("rmd")
  #'
  #' ottrfy(type = "quarto")
  #'
  #' ottrfy(type = "rmd_web")
  #'
  #' ottrfy(type = "quarto_web")
  #'
  #'
  #'
})
