% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contemperaneous_VIIRS_GOES_summary.R
\name{contemperaneous_VIIRS_GOES_summary}
\alias{contemperaneous_VIIRS_GOES_summary}
\title{Summarize contemperaneous VIIRS & GOES hotspots}
\usage{
contemperaneous_VIIRS_GOES_summary(
  VIIRS_hotspots,
  GOES16_hotspots,
  GOES17_hotspots,
  FRP_FWI_summary_table,
  CFS_REF_ID = NULL,
  filename_prefix = NULL,
  output_directory = NULL
)
}
\arguments{
\item{VIIRS_hotspots}{sf points of VIIRS hotspots. The output of calling \code{VIIRS_hotspots_grab()}.}

\item{GOES16_hotspots}{sf points of GOES-16 hotspots. The output of calling \code{GOES_GEE_grab(satellite = 16)}.}

\item{GOES17_hotspots}{sf points of GOES-17 hotspots. The output of calling \code{GOES_GEE_grab(satellite = 17)}.}

\item{FRP_FWI_summary_table}{tibble; found within Fire Progression Reanalysis database with suffix 'FRP_FWI_summary_table_hourly.csv'.}

\item{CFS_REF_ID}{String. value to be placed in the CFS_REF_ID field, simply for identification purposes. If \code{NULL}, no CFS_REF_ID column will be added.}

\item{filename_prefix}{Optional. String that will be appended to the start of the exported CSV. Ignored if \code{output_directory = NULL}.}

\item{output_directory}{Optional. Output directory where a CSV should be saved.}
}
\value{
A tibble
}
\description{
Summarize hotspot count and FRP for contemperaneous VIIRS and GOES hotspots for a given fire.
}
\details{
This function creates a tibble summarizing GOES/VIIRS hotspot count and sum FRP for a given fire.

GOES-16 and GOES-17 hotspots are first filtered to remove observations with mask values of 15 or 35 (low probability fire).
Therefore, GOES hotspot counts only represent pixels with values of 10, 11, 12, 13, 14, 30, 31, 32, 33, and 34.

Each row in the resulting table represents a unique VIIRS overpass time. There is a preprocessing step where the
length of time between consecutive detections is evaluated. If VIIRS datetimes are found < 5 minutes apart from one another,
they are considered as a single overpass event (and the latter datetimes are replaced with the former).
VIIRS overpass times are then rounded to the nearest 10 minutes in order to be joined with the GOES 10-minute data.
}
