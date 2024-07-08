test_that("Rmd Rendering", {
  rmd_dir <- download_ottr_template(dir = ".", type = "rmd")

  dir.exists(rmd_dir)

  bookdown::render_book(rmd_dir)

  unlink(rmd_dir, recursive = TRUE)
  file.remove(paste0(rmd_dir, ".zip"))
})

test_that("Quarto Rendering", {
  quarto_dir <- download_ottr_template(dir = ".", type = "quarto")

  dir.exists(quarto_dir)

  # Render it normal
  quarto::quarto_render(quarto_dir, as_job = FALSE)

  # Render it a different way
  quarto::quarto_render(quarto_dir,
                        metadata = list(sidebar = F, toc = F),
                        quarto_args = c("--output-dir", "docs/no_toc/"),
                        as_job = FALSE

  )
  unlink(quarto_dir, recursive = TRUE)
  file.remove(paste0(quarto_dir, ".zip"))
})

test_that("Rmd Website Rendering", {
  rmd_web_dir <- download_ottr_template(dir = ".", type = "rmd_website")

  dir.exists(rmd_web_dir)

  rmarkdown::clean_site(rmd_web_dir, preview = FALSE)

  rmarkdown::render_site(rmd_web_dir)

  unlink(rmd_web_dir, recursive = TRUE)
  file.remove(paste0(rmd_web_dir, ".zip"))
})


test_that("Quarto Website Rendering", {
  quarto_web_dir <- download_ottr_template(dir = ".", type = "quarto_website")

  dir.exists(quarto_web_dir)

  quarto::quarto_render(quarto_web_dir, as_job = FALSE)

  unlink(quarto_web_dir, recursive = TRUE)
  file.remove(paste0(quarto_web_dir, ".zip"))
})
