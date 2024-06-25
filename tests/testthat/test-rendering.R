test_that("Rmd Rendering", {
  dir <- download_ottr_template(dir = ".", type = "rmd")

  bookdown::render_book("OTTR_Template-main")
  unlink(dir)
})

test_that("Quarto Rendering", {
  dir <- download_ottr_template(dir = ".", type = "quarto")

  quarto::quarto_render(dir)
  quarto::quarto_render(dir,
                        metadata = list(sidebar = F, toc = F),
                        quarto_args = c("--output-dir", "docs/no_toc/")
  )
  unlink(dir)
})

test_that("Rmd Website Rendering", {
  dir <- download_ottr_template(dir = ".", type = "rmd_website")

  rmarkdown::clean_site(dir, preview = FALSE)

  rmarkdown::render_site(dir)
  
  unlink(dir)
})


test_that("Quarto Website Rendering", {
  dir <- download_ottr_template(dir = ".", type = "quarto_website")

  quarto::quarto_render(dir)
  
  unlink(dir)
})
