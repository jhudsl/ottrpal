test_that("Rmd Rendering", {
  dir <- download_ottr_template(dir = ".", type = "rmd")

  bookdown::render_book("OTTR_Template-main")

  unlink(dir)
})

test_that("Quarto Rendering", {
  dir <- download_ottr_template(dir = ".", type = "quarto")

  # Render it normal
  quarto::quarto_render("OTTR_Quarto-main", as_job = FALSE)

  # Render it a different way
  quarto::quarto_render("OTTR_Quarto-main",
                        metadata = list(sidebar = F, toc = F),
                        quarto_args = c("--output-dir", "docs/no_toc/"),
                        as_job = FALSE

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

  quarto::quarto_render(dir, as_job = FALSE)

  unlink(dir)
})
