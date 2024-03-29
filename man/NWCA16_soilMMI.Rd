% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NWCA16_soilMMI.R
\name{NWCA16_soilMMI}
\alias{NWCA16_soilMMI}
\title{NWCA16_soilMMI: calculate Soil Multi-metric Index}
\usage{
NWCA16_soilMMI()
}
\value{
Data frame with soil MMI metrics and ratings
}
\description{
This function calculates the 4 metrics used to calculate the soil multi-metric index (MMI) developed by Miller et al. (2016) using the top horizon from the soil chemistry horizon data (not the standard core) to be consistent with previous analyses. This function is only implemented for ACAD sites.
}
\examples{
\dontrun{

# Import ACAD only data and export
NWCA16_import(path = "../data/NWCA16", zip_name = "NWCA2016_data.zip")
smmi <- NWCA16_soilMMI()

}

}
