# ----------------------------------------------------

# Project title: Fire Progression Reanalysis
# Organization: NRCAN, Northern Forestry Centre, Edmonton AB
# Author: Xue Yan Chan, xueyan.chan@canada.ca
# Supervised by: Piyush Jain and Dan Thompson
# Last updated: 2021-04-08

# ----------------------------------------------------

#' Grab VIIRS hotspots
#'
#' Import and/or download the archived VIIRS S-NPP 375m hotspots from the FIRMS database within a user-defined date range and spatial extent.
#'
#' @param reference_poly An sp or sf polygon, defining the extent that hotspots are extracted for. The location of a polygon file is also accepted.
#' @param match_crs Logical. Should the extracted hotspots be projected to the CRS of the \code{reference_poly}? Defaults to \code{TRUE.} If \code{FALSE}, the hotspots are returned in WGS 84.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in extent, thereby increasing the extraction extent. Defaults to 0.
#'
#' @details
#' This function extracts the archived VIIRS S-NPP 375m hotspots from \url{https://firms.modaps.eosdis.nasa.gov/download/}.
#' The Country Yearly Summary CSVs are used for download, which means there is a considerable time lag until hotspots are available using this function (> 1 year lag).
#'
#' Currently, this function does not support download of NRT VIIRS hotspots.
#'
#' @return sf points
#' @export
#'
#' @references FIRMS: \code{https://firms.modaps.eosdis.nasa.gov/download/}
#'
#' @import sf
#' @import raster
#' @import lubridate
#' @import dplyr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' library(lubridate)
#' library(dplyr)
#' library(tibble)
#'
#' # Define a polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>%
#'      st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' viirs <- VIIRS_hotspots_grab(reference_poly = e, start_date = '2019-05-20', end_date = '2019-06-10')
#' plot(viirs[,'acq_date'])
#' }

VIIRS_hotspots_grab <- function(reference_poly,
                                match_crs = TRUE,
                                start_date,
                                end_date,
                                buff_width = 0){
  if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
  if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
  if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
  if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ message("Reference polygon must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if(!year(start_date) == year(end_date)) {stop('Year of start_date must match year of end_date. Please fix this before proceeding.')}

  year_of_fire <- year(start_date)
  # parse download URL
  # main data directory at https://firms.modaps.eosdis.nasa.gov/download/
  VIIRS_url <- paste0('https://firms.modaps.eosdis.nasa.gov/data/country/viirs-snpp/',
                      year_of_fire,
                      '/viirs-snpp_',
                      year_of_fire,
                      '_Canada.csv')

  VIIRS_temp <- tempfile(fileext = '.csv')
  utils::download.file(destfile = VIIRS_temp, url = VIIRS_url)
  VIIRS <- utils::read.csv(VIIRS_temp) %>% as_tibble() %>%
    st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>%
    st_transform(crs = 3978)

  orig_crs <- st_crs(reference_poly) # keep original crs
  reference_poly <- st_transform(reference_poly, crs = 3978)

  if(!buff_width == 0){
    reference_poly <- st_buffer(reference_poly, dist = buff_width)
  }
  VIIRS_ref <- VIIRS[reference_poly,] # select hotspots within ref poly buffer

  if(match_crs == FALSE){ # if match_crs = F, transform to native crs: WGS84
    VIIRS_ref <- st_transform(VIIRS_ref, crs = 4326)
  } else {
    VIIRS_ref <- st_transform(VIIRS_ref, crs = orig_crs)
  }

  # only keep hotspots from within specified dates
  date_interval <- interval(start = start_date, end = end_date)
  VIIRS_ref <- VIIRS_ref %>% filter(as.Date(acq_date) %within% date_interval)

  # make new UTC datetime (dttm) field
  UTC_acq_time <- formatC(as.numeric(VIIRS_ref$acq_time), width = 4, flag = '0')
  VIIRS_ref <- VIIRS_ref %>%
    add_column(UTC_dttm = paste0(VIIRS_ref$acq_date, ' ',UTC_acq_time),
               .after = 5)

  unlink(c(gsub(".csv","",VIIRS_temp),list.files(tempdir(),pattern = ".csv",full.names = T)),recursive = T)

  message(paste0('Returning VIIRS S-NPP 375m hotspots detected from ', start_date, ' to ', end_date))
  return(VIIRS_ref)
}

# ----------------------------------------------------

#' Grab MODIS hotspots
#'
#' Import and/or download archived MODIS Collection 6 hotspots from the FIRMS database within a user-defined date range and spatial extent.
#'
#' @param reference_poly An sp or sf polygon, defining the extent that hotspots are extracted for. The location of a polygon file is also accepted.
#' @param match_crs Logical. Should the extracted hotspots be projected to the CRS of the \code{reference_poly}? Defaults to \code{TRUE}. If \code{FALSE}, the hotspots are returned in WGS 84.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in extent, thereby increasing the extraction extent. Defaults to 0.
#'
#' @details
#' This function extracts the archived MODIS Collection 6 hotspots from \url{https://firms.modaps.eosdis.nasa.gov/download/}.
#' The Country Yearly Summary CSVs are used for download, which means there is a considerable time lag until hotspots are available using this function (> 1 year lag).
#'
#' Currently, this function does not support download of NRT MODIS Collection 6 hotspots.
#'
#' @return sf points
#' @export
#'
#' @references FIRMS: \url{https://firms.modaps.eosdis.nasa.gov/download/}
#'
#' @import sf
#' @import raster
#' @import lubridate
#' @import dplyr
#' @import tibble
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' library(lubridate)
#' library(dplyr)
#' library(tibble)
#'
#' # Define a polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>%
#'      st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' modis <- MODIS_hotspots_grab(reference_poly = e, start_date = '2019-05-20', end_date = '2019-06-10')
#' plot(modis[,'acq_date'])
#' }

MODIS_hotspots_grab <- function(reference_poly,
                                match_crs = TRUE,
                                start_date,
                                end_date,
                                buff_width = 0){
  if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
  if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
  if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
  if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ message("Reference polygon must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if(!year(start_date) == year(end_date)) {stop('Year of start_date must match year of end_date. Please fix this before proceeding.')}

  year_of_fire <- year(start_date)
  # parse download URL
  # main data directory at https://firms.modaps.eosdis.nasa.gov/download/
  MODIS_url <- paste0('https://firms.modaps.eosdis.nasa.gov/data/country/modis/',
                      year_of_fire,
                      '/modis_',
                      year_of_fire,
                      '_Canada.csv')

  MODIS_temp <- tempfile(fileext = '.csv')
  utils::download.file(destfile = MODIS_temp, url = MODIS_url)
  MODIS <- utils::read.csv(MODIS_temp) %>% as_tibble() %>%
    st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>%
    st_transform(crs = 3978) # match ref poly projection

  orig_crs <- st_crs(reference_poly) # keep original crs
  reference_poly <- st_transform(reference_poly, crs = 3978)

  if(!buff_width == 0){
    reference_poly <- st_buffer(reference_poly, dist = buff_width)
  }
  MODIS_ref <- MODIS[reference_poly,] # select hotspots within fire buffer

  if(match_crs == FALSE){ # if match_crs = F, transform to native crs: WGS84
    MODIS_ref <- st_transform(MODIS_ref, crs = 4326)
  } else {
    MODIS_ref <- st_transform(MODIS_ref, crs = orig_crs)
  }

  # only keep hotspots from within specified dates
  date_interval <- interval(start = start_date, end = end_date)
  MODIS_ref<- MODIS_ref %>% filter(as.Date(acq_date) %within% date_interval)

  # make new UTC datetime (dttm) field
  UTC_acq_time <- formatC(as.numeric(MODIS_ref$acq_time), width = 4, flag = '0')
  MODIS_ref <- MODIS_ref %>%
    add_column(UTC_dttm = paste0(MODIS_ref$acq_date, ' ',UTC_acq_time),
               .after = 5)

  unlink(c(gsub(".csv","",MODIS_temp),list.files(tempdir(),pattern = ".csv",full.names = T)),recursive = T)

  message(paste0('Returning MODIS Collection 6 hotspots detected from ', start_date, ' to ', end_date))
  return(MODIS_ref)
}

