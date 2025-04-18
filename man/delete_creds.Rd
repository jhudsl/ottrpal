% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{delete_creds}
\alias{delete_creds}
\title{Delete cached ottrpal credentials}
\usage{
delete_creds(app_name = "all")
}
\arguments{
\item{app_name}{which app would you like to delete the creds for? Default is to delete the creds for all.}
}
\value{
Cached credentials are deleted and report is given back
}
\description{
This is a function to delete cached creds and creds in the current environment that were set by ottrpal
}
\examples{
\dontrun{

delete_creds("google")
}
}
