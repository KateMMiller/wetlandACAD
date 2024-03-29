% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NWCA16_download.R
\name{NWCA16_download}
\alias{NWCA16_download}
\title{NWCA16_download: Downloads data from 2016 EPA National Wetland Condition Assessment}
\usage{
NWCA16_download(path = NA, zip = TRUE)
}
\arguments{
\item{path}{Quoted path of folder to download files into. If not specified, will save to working directory.}

\item{zip}{Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.}
}
\value{
Writes downloaded files to disk.
}
\description{
This function downloads the 2016 EPA NWCA data directly from the National Aquatic Resource Surveys website and saves the dataset to disk.
}
\examples{
\dontrun{
# Run with defaults: path = getwd(), zip = T
NWCA16_download()

# Save files to specified data folder and not zipped
NWCA16_download(path = "./data", zip = FALSE)

}
}
