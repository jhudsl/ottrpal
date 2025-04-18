% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_slides.R
\name{get_slide_id}
\alias{get_slide_id}
\title{Get Slide ID from URL}
\usage{
get_slide_id(x)
}
\arguments{
\item{x}{URL of slide}
}
\value{
A character vector
}
\description{
Get Slide ID from URL
}
\examples{
x <- paste0(
  "https://docs.google.com/presentation/d/",
  "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8",
  "/edit#slide=id.g154aa4fae2_0_58"
)
get_slide_id(x)
}
