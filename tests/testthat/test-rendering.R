test_that("Rmd Rendering", {
  dir <- download_ottr_template(dir = "inst/extdata", type = "rmd")

  bookdown::render_book("index.Rmd", output_format = "all")
})

test_that("Quarto Rendering", {
  dir <- download_ottr_template(dir = "inst/extdata", type = "quarto")

  quarto::quarto_render(dir)
  quarto::quarto_render(dir,
    metadata = list(sidebar = F, toc = F),
    quarto_args = c("--output-dir", "docs/no_toc/")
  )
})

test_that("Rmd Website Rendering", {
  dir <- download_ottr_template(dir = "inst/extdata", type = "rmd")

  rmarkdown::clean_site(dir, preview = FALSE)

  rmarkdown::render_site(dir)
})


test_that("Quarto Website Rendering", {
  dir <- download_ottr_template(dir = "inst/extdata", type = "quarto")

  quarto::quarto_render(dir)
})
