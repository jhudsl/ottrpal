rmd_regex = "[.][R|r]md$"

get_bookdown_spec = function(path = ".") {
  file = bookdown_file(path = path)
  suppressWarnings({
    out = yaml::yaml.load_file(file)
  })
  out
}
bookdown_path = function(path = ".") {
  rprojroot::find_root(rprojroot::has_file("_bookdown.yml"), path = path)
}

bookdown_file =  function(path = ".") {
  root_dir = bookdown_path(path = path)
  file.path(root_dir, "_bookdown.yml")
}

bookdown_rmd_files = function(path = ".") {
  spec = get_bookdown_spec(path)
  spec$rmd_files
}

bookdown_destination = function(path = ".") {
  root_dir = bookdown_path(path = path)
  spec = get_bookdown_spec(path = path)
  output_dir = spec$output_dir
  if (is.null(output_dir)) {
    output_dir = "docs"
  }
  full_output_dir = file.path(root_dir, output_dir)
  full_output_dir = normalizePath(full_output_dir, winslash = "/")
  full_output_dir
}

# copy_md_files = function(path = ".") {
#   path = bookdown_path(path)
#   files = bookdown_rmd_files(path)
#   files = sub(rmd_regex, ".md", files)
#   files = file.path(bookdown_destination(path), files)
#
# }

copy_directory_contents = function(from, to) {
  x = list.files(path = from, full.names = TRUE, all.files = TRUE,
                 recursive = TRUE)
  file.copy(x, to, recursive = TRUE)
}

copy_resources = function(path = ".") {
  path = bookdown_path(path)
  res_image_dir = file.path(path, "resources/images")
  manuscript_image_dir = file.path(path, "manuscript/resources/images")
  dir.create(manuscript_image_dir, showWarnings = FALSE)
  copy_directory_contents(res_image_dir, manuscript_image_dir)
}


bookdown_to_leanpub = function(path = ".", verbose = TRUE) {
  path = bookdown_path(path)
  owd = getwd()
  setwd(path)
  on.exit({
    setwd(owd)
  })


  copy_resources(path)
  # FIXME Can also use bookdown_rmd_files
  rmd_files = list.files(pattern = rmd_regex)


  bib_files = list.files(pattern = "[.]bib$")
  if (length(bib_files) > 0) {
    pandoc_args = paste0("--bibliography=", normalizePath(bib_files))
  } else {
    pandoc_args = NULL
  }

  for (file in rmd_files) {

    output_file = sub(rmd_regex, ".md", file)
    rmarkdown::render(
      file,
      output_file = output_file,
      output_dir = "manuscript",
      intermediates_dir = "manuscript",
      output_format = rmarkdown::output_format(
        knitr = rmarkdown::knitr_options(
          opts_chunk = list(fig.path = "resources/images/")
        ),
        pandoc = rmarkdown::pandoc_options(
          to = "markdown_strict+autolink_bare_uris+tex_math_single_backslash",
          args = c(pandoc_args, "--citeproc")
        ))
    )
    infile = normalizePath(file.path("manuscript", output_file))
    rmarkdown::pandoc_convert(
      input = infile,
      output = infile,
      options = pandoc_args,
      to = "markdown_strict+autolink_bare_uris+tex_math_single_backslash",
      citeproc = TRUE,
      verbose = verbose)
  }
  return()
}


