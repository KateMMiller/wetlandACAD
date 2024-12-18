% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importRAM.R
\name{importRAM}
\alias{importRAM}
\title{importRAM: Imports and compiles views for wetland RAM data package}
\usage{
importRAM(
  export_protected = FALSE,
  type = c("DSN"),
  odbc = "RAM_BE",
  filepath = NA,
  new_env = TRUE,
  export_data = FALSE,
  export_path = NA,
  zip = FALSE
)
}
\arguments{
\item{export_protected}{Logical. If TRUE, all records are exported. If FALSE (Default), only non-protected
species are exported.}

\item{type}{Select whether to use the default Data Source Named database (DSN) to import data, a
different database, or a data package with CSVs. If "DSN" is selected, must specify name in odbc argument.
If dbfile, csv, or zip are selected, must specify a filepath.
\describe{
\item{"DSN"}{Default. DSN database. If odbc argument is not specified, will default to "RAM_BE"}
\item{"dbfile"}{A different database than default DSN}
\item{"csv"}{Import csv views that have already been compiled as data package.}
\item{"zip"}{Import zip file containing csvs from a data package.}
}}

\item{odbc}{DSN of the database when using type = DSN. If not specified will default to "RAM_BE", which
is the back end of the MS Access RAM database.}

\item{filepath}{Quoted path of database back end dbfile, csvs or zip. If dbfile or zip are the type, must
include the name of the database or zip file (see examples).}

\item{new_env}{Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
views in VIEWS_RAM environment. If \code{FALSE}, stores views in global environment}

\item{export_data}{Logical. If TRUE, writes views to disk. If FALSE (Default), views are only
stored in specified R environment.}

\item{export_path}{Quoted path to export views to. If blank, exports to working directory.}

\item{zip}{Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.}
}
\value{
Compiles and assigns RAM views to specified environment
}
\description{
This function imports RAM-related tables in the wetland RAM backend and combines them
into flattened views for the data package. Each view is added to a VIEWS_RAM environment in your
workspace, or to your global environment based on whether new_env = TRUE or FALSE.
}
\examples{
\dontrun{

filepath <- "C:/NETN/R_Dev/data/wetland_data_package/"
ex_path <- filepath

# Import tables from database in specific folder and only export non-protected:
importRAM(type = 'dbfile', filepath = paste0(filepath, "NETN_RAM_20241030.accdb"), export_path = ex_path, export_data = T)

# Import ODBC named database into global env with protected species and export as zip file
importRAM(type = 'DSN', odbc = "RAM_BE", new_env = F, export_protected = F, export_data = T, zip = T, export_path = ex_path)

# Import csvs of data package without protected species
importRAM(type = 'csv', filepath = filepath, export_data = T, export_path = ex_path, zip = T)

# Import csvs of data package with protected species
importRAM(type = 'csv', filepath = filepath, export_data = T, export_path = ex_path, zip = T, export_protected = T)

# Import zip of data package containing csvs but don't export anything
importRAM(type = 'zip', filepath = paste0(filepath, "NETN_Wetland_RAM_Data_20241209_NPSonly.zip"))

# Import zip of data package containing csvs without protected species but don't export anything
importRAM(type = 'zip', filepath = paste0(filepath, "NETN_Wetland_RAM_Data_20241209_public.zip"))

}

}
