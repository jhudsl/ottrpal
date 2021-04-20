#' Convert Bookdown Book to Leanpub
#'
#' @param path path to the directory with bookdown
#' @param bookdown_dir Directory for output of bookdown run.  If
#' \code{NULL}, then  `output_dir` from `_bookdown.yml` will be extracted.
#' Otherwise, defaults to `docs`
#'
#' @return A character vector of output files
#' @export
#'
#' @examples
lp_convert_bookdown = function(path = ".", bookdown_dir = NULL) {
  files = list.files(pattern = "[.]R?md$", path = path, ignore.case = TRUE)
  bd = "_bookdown.yml"
  if (file.exists(bd)) {
    bd_yml = yaml::yaml.load_file(bd)
    rmd_files = bd_yml$rmd_files
    if (!is.null(rmd_files)) {
      files = intersect(rmd_files, files)
    }
    if (is.null(bookdown_dir)) {
      bookdown_dir = file.path(path, bd_yml$output_dir)
    }
  }
  if (is.null(bookdown_dir)) {
    bookdown_dir = file.path(path, "docs")
  }
  dir.create(bookdown_dir, showWarnings = FALSE)
  bibs = list.files(pattern = ".bib", path = path, full.names = TRUE)
  
  
  bookdown_files = sub("[.]R?md$", ".md", basename(files))
  bookdown_files = file.path(bookdown_dir, bookdown_files)
  if (!all(file.exists(bookdown_files))) {
    warning("Not all files exist in bookdown output dir")
  }
  manu_dir = file.path(path, "manuscript")
  dir.create(manu_dir, showWarnings = FALSE, recursive = TRUE)
  
  outfiles = file.path(manu_dir, basename(bookdown_files))
  file.copy(bookdown_files, outfiles, overwrite = TRUE)
  
  file.copy(bibs, manu_dir, overwrite = TRUE)
  
  outfiles = outfiles[ file.exists(outfiles) ]
  outfiles = normalizePath(outfiles, winslash = "/", mustWork = TRUE)
  # need to run this for the citations
  options = NULL
  if (length(bibs) > 0) {
    options = paste0("--bibliography=", basename(bibs))
  }
  lapply(outfiles, rmarkdown::pandoc_convert, 
         options = options,
         to = "markdown_strict", citeproc = TRUE,
         verbose = TRUE)
  outfiles
}