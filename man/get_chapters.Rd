% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iframe_leanpub.R
\name{get_chapters}
\alias{get_chapters}
\title{Make Leanpub file that has embed webpage of a chapter}
\usage{
get_chapters(html_page = file.path("docs", "index.html"), base_url = ".")
}
\arguments{
\item{html_page}{The file path of the rendered index.html file. It can be a url}

\item{base_url}{The base url of where the chapters are published -- the url to provide to the iframe in Leanpub
e.g. https://jhudatascience.org/OTTR_Template/coursera}
}
\value{
A data.frame of the chapter urls and their titles that are to be ported to Leanpub.
This can be passed to
}
\description{
Make Leanpub file that has embed webpage of a chapter
}
