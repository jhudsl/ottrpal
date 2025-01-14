test_that("Test OTTRfy - Rmd", {

  # Make a repository with Rmd files
  dir.create("rmd")
  writeLines("# A title", file.path("rmd", "01-intro.Rmd"))
  writeLines("---\ntitle: An index \n--- \n# Intro", file.path("rmd", "index.Rmd"))

  # OTTR fy it
  ottrfy(path = "rmd", type = "rmd", git_commit = FALSE)

  # Update bookdown
  update_chapters("rmd")

  # Render it
  bookdown::render_book("rmd")

  testthat::expect_true(file.exists(file.path("rmd", "docs", "index.html")))
})

test_that("Test OTTRfy - Rmd", {

  # Make a repository with Rmd files
  dir.create("quarto")
  writeLines("# A title", file.path("quarto", "01-intro.qmd"))
  writeLines("# Intro", file.path("quarto", "index.qmd"))

  # OTTR fy it
  ottrfy(path = "quarto", type = "quarto", git_commit = FALSE, overwrite = TRUE)

  # Update bookdown
  update_chapters("quarto")

  # Render it
  quarto::quarto_render("quarto", as_job = FALSE)

  testthat::expect_true(file.exists(file.path("quarto", "docs", "index.html")))
})

test_that("Test OTTRfy - Rmd web", {

  # Make a repository with Rmd files
  dir.create("rmd_web")
  writeLines("# A page", file.path("rmd_web", "page1.Rmd"))
  writeLines("# Home Page", file.path("rmd_web", "index.Rmd"))

  # OTTR fy it
  ottrfy(path = "rmd_web", type = "rmd_web", git_commit = FALSE)

  writeLines(yaml::as.yaml(list(
    name =  "OTTR Template Website",
    output_dir = 'docs',
    navbar = list(
    left= list(href = "index.html")))
  ),
  "rmd_web/_site.yml")

  # Render it
  rmarkdown::render_site("rmd_web")

  testthat::expect_true(file.exists(file.path("rmd", "docs", "index.html")))
})

  #' ottrfy(type = "quarto")
  #'
  #' ottrfy(type = "rmd_web")
  #'
  #' ottrfy(type = "quarto_web")
  #'
  #'
  #'
})
