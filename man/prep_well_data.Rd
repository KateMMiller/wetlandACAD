% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_well_data.R
\name{prep_well_data}
\alias{prep_well_data}
\title{prep_well_data: Prepares sentinel well data for conversion to water level using data tables stored in the NETN RAM database
(for internal use).}
\usage{
prep_well_data(
  path = NULL,
  year = as.numeric(format(Sys.Date(), "\%Y")),
  rejected = FALSE,
  growing_season = TRUE,
  export = TRUE,
  quietly = FALSE
)
}
\arguments{
\item{path}{Quoted path of the folder where the exported Hobo tables are located.}

\item{year}{Numeric. The year you are preparing the data for. Function will only run 1 year at a time.}

\item{rejected}{\code{TRUE} or \code{FALSE}. If \code{TRUE} will include flagged rejected records.
If \code{FALSE} will replace the rejected flagged records with NA. Defaults to \code{FALSE}.}

\item{growing_season}{\code{TRUE} or \code{FALSE}. If \code{TRUE} will include only growing season
measurements, defined as May 15 to Oct. 1. If \code{FALSE}, will return all measurements.
If growing_season = \code{TRUE} and export = \code{TRUE}, the output filename will include "GS"
Defaults to \code{TRUE}.}

\item{export}{\code{TRUE} or \code{FALSE}. Export csv file to specified path. Defaults to \code{TRUE}.}

\item{quietly}{\code{TRUE} or \code{FALSE}. If \code{FALSE}, code will not print progress into console.
Defaults to \code{FALSE}}
}
\value{
Returns a wide data frame with timestamp, SITENAME_AbsPres.
}
\description{
This function pulls in the water level data from the NETN RAM database,
joins the location, well visit and water level data together into a wide
format of the data with the timestamp as the joining column. Function requires
all 8 sites and 2 barometric loggers to run, and only includes data between the
spring and fall well visit per year. \strong{Must have a the NETN RAM backend
database named as a DSN.} Function is primarily for internal use.
}
\examples{
\dontrun{
# Export growing season only data to a table
dir = c('C:/Water_level_data/growing_season_2019')
prep_well_data(path = dir, year = 2019, export = TRUE, growing_season = TRUE)

# Assign output from all 2018 data, including flagged records, to global environment
welld_2018 <- prep_well_data(year = 2018, rejected = TRUE, growing_season = FALSE,
                             export = FALSE)

# Run for 2019 growing season data without printing messages in the console, and save output to file
welld_2018 <- prep_well_data(year = 2019, growing_season = TRUE, export = TRUE,
                             quietly = TRUE)
}

}
