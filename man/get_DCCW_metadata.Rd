% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_DCCW_metadata.R
\name{get_DCCW_metadata}
\alias{get_DCCW_metadata}
\title{Get DCCW Metadata}
\usage{
get_DCCW_metadata(output_directory)
}
\arguments{
\item{output_directory}{Location on your local device where the metadata Excel workbook (.xlsx) should be saved.}
}
\value{
An Excel sheet saved on the local device.
}
\description{
Download an excel file containing metadata for all data sources extracted using the DCCW (Data Cubes for Canadian Wildfires) R package.
}
\examples{
\dontrun{
library(curl)

# download the Excel metadata sheet to the working directory:
get_DCCW_metadata(getwd())

}
}
