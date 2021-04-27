
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

copy_resources = function(path = ".", output_dir = "manuscript") {
  path = bookdown_path(path)
  res_image_dir = file.path(path, "resources/images")
  manuscript_image_dir = normalizePath(file.path(output_dir, "resources/images"))
  dir.create(manuscript_image_dir, showWarnings = FALSE)
  copy_directory_contents(res_image_dir, manuscript_image_dir)
}



#' Convert Bookdown to Leanpub
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param render if `TRUE`, then [rmarkdown::render()] will be run on each Rmd.
#' @param verbose print diagnostic messages
#'
#' @return A list of output files and diagnostics
#' @export
#'
#' @examples
bookdown_to_leanpub = function(path = ".",
                               output_dir = "manuscript",
                               render = TRUE,
                               verbose = TRUE) {

  rmd_regex = "[.][R|r]md$"

  path = bookdown_path(path)
  owd = getwd()
  setwd(path)
  on.exit({
    setwd(owd)
  })


  copy_resources(path, output_dir = output_dir)
  # FIXME Can also use bookdown_rmd_files
  rmd_files = list.files(pattern = rmd_regex)


  bib_files = list.files(pattern = "[.]bib$")
  if (length(bib_files) > 0) {
    pandoc_args = paste0("--bibliography=", normalizePath(bib_files))
  } else {
    pandoc_args = NULL
  }

  output_files = NULL
  run_env = new.env()
  for (file in rmd_files) {

    output_file = sub(rmd_regex, ".md", file, ignore.case = TRUE)
    if (render) {
      rmarkdown::render(
        file,
        output_file = output_file,
        output_dir = output_dir,
        intermediates_dir = output_dir,
        envir = run_env,
        output_format = rmarkdown::output_format(
          knitr = rmarkdown::knitr_options(
            opts_chunk = list(fig.path = "resources/images/")
          ),
          pandoc = rmarkdown::pandoc_options(
            # to = "markdown_strict+autolink_bare_uris+tex_math_single_backslash",
            to = "markdown+autolink_bare_uris+tex_math_single_backslash",
            args = c(pandoc_args, "--citeproc")
          )
        )
      )
    }
    infile = normalizePath(file.path(output_dir, output_file))
    output_files = c(output_files, infile)
    rmarkdown::pandoc_convert(
      input = infile,
      output = infile,
      options = pandoc_args,
      to = "markdown_strict+autolink_bare_uris+tex_math_single_backslash",
      citeproc = TRUE,
      verbose = verbose)
  }
  L = list(output_files = output_files)
  return(L)
}


#' Convert Bookdown to Leanpub
#'
#' @param path path to the bookdown book, must have a `_bookdown.yml` file
#' @param output_dir output directory to put files.  It should likely be
#' relative to path
#' @param verbose print diagnostic messages
#'
#' @return A list of output files and diagnostics
#' @export
#'
#' @examples
bookdown_to_book_txt = function(
  path = ".",
  output_dir = "manuscript",
  verbose = TRUE) {
  path = bookdown_path(path)
  owd = getwd()
  setwd(path)
  on.exit({
    setwd(owd)
  })
  rmd_regex = "[.][R|r]md$"

  stop("Not done - need to fix quizzes")
  rmd_files = bookdown_rmd_files(path = path)
  md_files = sub(rmd_regex, ".md", rmd_files, ignore.case = TRUE)
  book_txt = file.path(output_dir, "Book.txt")
  if (file.exists(book_txt)) {
    x = readLines(book_txt)
    quiz = x[grepl("quiz.md", x)]
  }
  # need to fix about quiz
  writeLines(md_files, book_txt)
  return(book_txt)
}


