test_that("Extracting slide ID", {

  slide_url <- paste0(
    "https://docs.google.com/presentation/d/",
    "1Zdy7Vm13Yq5QXgcKGJ_nGqvsMpZgk-L5faw_FyXzQHA/edit#slide=id.g2e4ad2897da_0_82")

  slide_id <- get_slide_id(slide_url)

  testthat::expect_equal(slide_id, "1Zdy7Vm13Yq5QXgcKGJ_nGqvsMpZgk-L5faw_FyXzQHA")

  png_url <- gs_png_url(slide_url)

  download.file(png_url, destfile = "test_slide.png")

  testthat::expect_true(file.exists("test_slide.png"))

  gs_png_download(slide_url)

  testthat::expect_true(file.exists("./1Zdy7Vm13Yq5QXgcKGJ_nGqvsMpZgk-L5faw_FyXzQHA_g2e4ad2897da_0_82.png"))

  file.remove("./1Zdy7Vm13Yq5QXgcKGJ_nGqvsMpZgk-L5faw_FyXzQHA_g2e4ad2897da_0_82.png")
  file.remove("test_slide.png")

  obj <- include_slide(slide_url)

  testthat::expect_true(all(class(obj) %in% c("knit_image_paths", "knit_asis")))
})

test_that("Add alternative text", {
  dir <- setup_ottr_template(dir = ".", type = "rmd", render = FALSE)

  slide_df <- get_slide_urls(dir,
                             pattern = "include_slide(",
                             target = ".",
                             file_suffix = "md$",
                             recursive = FALSE)

  slide_decks <- sapply(slide_df$gs_slides, stringr::word, sep = "https://docs.google.com/presentation/d/|/edit#slide", start = 2)
  slide_deck_url <- paste0("https://docs.google.com/presentation/d/", unlist(slide_decks))
  get_object_id_notes(slide_deck_url)

})
