% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NWCA16_waterMMI.R
\name{NWCA16_waterMMI}
\alias{NWCA16_waterMMI}
\title{NWCA16_waterMMI: calculate Water Chemistry Multi-metric Index}
\usage{
NWCA16_waterMMI()
}
\value{
Data frame with water MMI metrics and ratings
}
\description{
This function calculates the 3 metrics used to calculate the water chemistry multi-metric index (MMI) developed by Miller et al. (2016). This function is only implemented for ACAD sites.
}
\examples{
\dontrun{

# Import ACAD only data and export
NWCA16_import(path = "../data/NWCA16", zip_name = "NWCA2016_data.zip")
wmmi <- NWCA16_wegMMI()

}

}
