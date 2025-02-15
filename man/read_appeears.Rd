% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_appeears.R
\name{read_appeears}
\alias{read_appeears}
\title{Reads in AppEEARS MODIS data}
\usage{
read_appeears(file, name = "Nadir_Reflectance")
}
\arguments{
\item{file}{a file location of the file to process}

\item{name}{band name qualifier to filter out RS values}
}
\value{
a cleaned up data frame with bad values screened
out and marked as NA - no gap filling is performed
at this step
}
\description{
Reads in AppEEARS MODIS data and converts from the
wide to a long format, filtering based upon good
data quality using data QA/QC descriptions
}
\details{
Only products MOD09GA.061, MODOCGA.006, MOD11A1.006,
and MCD43A4.061 are supported if not matching these
the intermediate data is returned for manual screening.
}
