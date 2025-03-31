test_that("Test OTTRfy - Rmd", {
  testthat::skip_on_cran()
  # Make a repository with Rmd files
  dir.create("rmd")
  writeLines("# A title", file.path("rmd", "01-intro.Rmd"))
  writeLines("---\ntitle: An index \n--- \n# Intro", file.path("rmd", "index.Rmd"))

  # OTTR fy it
  ottrfy(path = "rmd", type = "rmd")

  writeLines(
    "book_filename: 'Course_Name'
rmd_files: ['index.Rmd', '01-intro.Rmd']",
    file.path("rmd", "_bookdown.yml"))

  # Render it
  bookdown::render_book("rmd", output_dir = "docs")

  testthat::expect_true(file.exists(file.path("rmd", "docs", "index.html")))

  clean_up()
})

test_that("Test OTTRfy - Quarto", {
  testthat::skip_on_cran()
  # Make a repository with Rmd files
  dir.create("quarto")
  writeLines("# A title", file.path("quarto", "intro.qmd"))
  writeLines("# Intro", file.path("quarto", "index.qmd"))

  # OTTR fy it
  ottrfy(path = "quarto", type = "quarto", overwrite = TRUE)

  writeLines(
    "project:
  type: book
  output-dir: docs

book:
  title: Quarto Based OTTR Course

  chapters:
    - index.qmd
    - intro.qmd",
    "quarto/_quarto.yml")

  # Render it
  quarto::quarto_render("quarto", as_job = FALSE)

  testthat::expect_true(file.exists(file.path("quarto", "docs", "index.html")))

  clean_up()
})

test_that("Test OTTRfy - Rmd web", {
  testthat::skip_on_cran()
  # Make a repository with Rmd files
  dir.create("rmd_web")
  writeLines("# A page", file.path("rmd_web", "page1.Rmd"))
  writeLines("# Home Page", file.path("rmd_web", "index.Rmd"))

  # OTTR fy it
  ottrfy(path = "rmd_web", type = "rmd_web")

  writeLines(
"name: OTTR Template Website
output_dir: 'docs'
navbar:
  title: OTTR Web
  left:
  - text: \"\"
    href: index.html
    icon: fa-home",
  "rmd_web/_site.yml")

  # Render it
  rmarkdown::render_site("rmd_web")

  testthat::expect_true(file.exists(file.path("rmd_web", "docs", "index.html")))

  clean_up()
})

test_that("Test OTTRfy - Quarto Web", {
  testthat::skip_on_cran()
  # Make a repository with Rmd files
  dir.create("quarto_web")
  writeLines("# A webpage", file.path("quarto_web", "index.qmd"))

  # OTTR fy it
  ottrfy(path = "quarto_web", type = "quarto_web", overwrite = TRUE)

  writeLines(yaml::as.yaml(list(
    name =  "OTTR Quarto Website",
    output_dir = 'docs',
    navbar = list(
      left= list(href = "index.html")))
  ),
  "quarto_web/_quarto.yml")

  # Render it
  quarto::quarto_render("quarto_web", as_job = FALSE)

  testthat::expect_true(file.exists(file.path("quarto_web", "index.html")))

  clean_up()
})
