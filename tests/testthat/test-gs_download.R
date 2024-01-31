testthat::test_that("Download a Slide ", {

  if (.Platform$OS.type != "windows") {
  output_dir = tempdir()
  outfile = ottrpal::include_slide(
    "https://docs.google.com/presentation/d/1-7UvgVq5tP1pasTEErUM3bJFH2fU_pilH6i6_81CCXU/edit#slide=id.p",
    output_dir = output_dir,
    overwrite = FALSE)
  testthat::expect_true(file.exists(outfile))
  bn = basename(outfile)
  testthat::expect_true(
    bn == "12DPZgPteQBwgal6kSPP58zhPhjZ7QSPZLe3NkA8M3eo_gc87451c247_0_17.png"
  )
  }
})
