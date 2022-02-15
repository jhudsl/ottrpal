testthat::test_that("Download a Slide ", {
  output_dir = tempdir()
  outfile = ottr_aide::include_slide(
    paste0("https://docs.google.com/presentation/d/",
           "12DPZgPteQBwgal6kSPP58zhPhjZ7QSPZLe3NkA8M3eo/",
           "edit#slide=id.gc87451c247_0_17"),
    output_dir = output_dir,
    overwrite = FALSE)
  testthat::expect_true(file.exists(outfile))
  bn = basename(outfile)
  testthat::expect_true(
    bn == "12DPZgPteQBwgal6kSPP58zhPhjZ7QSPZLe3NkA8M3eo_gc87451c247_0_17.png"
  )
})
