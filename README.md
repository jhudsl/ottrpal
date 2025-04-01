⚠️**All OTTR related code and templates are moving to https://github.com/orgs/ottrproject/ as of April 2025⚠️**

If you'd like to contribute to or use OTTR tools please go there! or visit [ottrproject.org](https://www.ottrproject.org/)

<!-- badges: start -->

[![R-CMD-check](https://github.com/jhudsl/ottrpal/workflows/R-CMD-check/badge.svg)](https://github.com/jhudsl/ottrpal/actions) [![CRAN status](https://www.r-pkg.org/badges/version/ottrpal)](https://CRAN.R-project.org/package=ottrpal) [![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ottrpal)](https://cran.r-project.org/package=ottrpal) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) <!-- [![GitHub release (latest by --> <!-- date)](https://img.shields.io/github/v/release/jhudsl/ottrpal?style=social)](https://github.com/jhudsl/ottrpal/releases/tag/v1.0.0) --> <!-- [![Codecov test --> <!-- coverage](https://codecov.io/gh/jhudsl/ottrpal/branch/main/graph/badge.svg)](https://codecov.io/gh/jhudsl/ottrpal?branch=main) -->

<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ottrpal package

`ottrpal` is a companion R package for OTTR courses (Open-source Tools for Training Resources).

Go to [ottrproject.org](https://www.ottrproject.org/) to get started! :tada:

- Perform URL, spell, and formatting checks for your Quarto and R markdown, and markdown files.
- Prep your courses for upload to Massive Open Online Courses (MOOCs): [Coursera](https://www.coursera.org/) and [Leanpub](https://leanpub.com/).
- `ottrfy()` your course to make it ready for all the OTTR functionality. 

## Installing ottrpal:

You can install `ottrpal` from GitHub with:
```
install.packages("ottrpal")
```

If you want the development version (not advised) you can install using the `remotes` package to install from GitHub.
```
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("jhudsl/ottrpal")
```
