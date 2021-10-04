#' Grab VIIRS hotspots
#'
#' Import and/or download the archived VIIRS S-NPP 375m hotspots from the FIRMS database within a user-defined date range and spatial extent.
#'
#' @param reference_poly An sp or sf polygon, defining the extent that hotspots are extracted for. The location of a polygon file is also accepted.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in \code{reference_poly}, thereby increasing the extraction extent. Defaults to 0.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param return_sf Logical. Should the points be returned as an sf object? If \code{FALSE}, returns a tibble with lat/lon information. Defaults to TRUE.
#' @param match_crs Logical. Should the extracted points be projected to the CRS of the reference \code{extent} object? Only applicable if \code{return_sf = TRUE}. Defaults to \code{FALSE}. If \code{FALSE}, sf points are returned in WGS84.
#'
#' @details
#' This function extracts the archived VIIRS S-NPP 375m hotspots from \url{https://firms.modaps.eosdis.nasa.gov/download/}.
#' The Country Yearly Summary CSVs are used for download, which means there is a considerable time lag until hotspots are available using this function (> 1 year lag).
#'
#' Currently, this function does not support download of NRT VIIRS hotspots.
#'
#' @return sf points object or a tibble
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
#' viirs <- VIIRS_hotspots_grab(reference_poly = e,
#'                              start_date = '2019-05-20',
#'                              end_date = '2019-06-10',
#'                              return_sf = TRUE,
#'                              match_crs = TRUE)
#' plot(viirs[,'acq_date'])
#' }

VIIRS_hotspots_grab <- function(reference_poly,
                                buff_width = 0,
                                start_date,
                                end_date,
                                return_sf = TRUE,
                                match_crs = FALSE){
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
  utils::download.file(destfile = VIIRS_temp, url = VIIRS_url, method = 'libcurl')
  VIIRS <- utils::read.csv(VIIRS_temp) %>% as_tibble() %>%
    st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>%
    st_transform(crs = 3978)

  orig_crs <- st_crs(reference_poly) # keep original crs
  reference_poly <- st_transform(reference_poly, crs = 3978)

  if(!buff_width == 0){
    reference_poly <- st_buffer(reference_poly, dist = buff_width)
  }

  VIIRS_ref <- VIIRS[reference_poly,] # select hotspots within ref poly buffer
  VIIRS_ref <- VIIRS_ref %>% dplyr::relocate(c('acq_date','acq_time'), .before = 1)
  VIIRS_ref <- VIIRS_ref %>% dplyr::relocate(c('satellite','instrument'), .after = 2)

  # should a sf object or tibble be returned?
  if(return_sf == TRUE){ # return an sf object
    if(match_crs == FALSE){ # if match_crs = F, transform to native crs: WGS84
      VIIRS_ref <- st_transform(VIIRS_ref, crs = 4326)
    } else {
      VIIRS_ref <- st_transform(VIIRS_ref, crs = orig_crs)
    }
  } else { # if return_sf == FALSE, return a tibble with latlon info
    VIIRS_ref <- VIIRS_ref %>% st_transform(crs = 4326)
    VIIRS_ref <- VIIRS_ref %>% tibble::add_column(VIIRS_ref %>% st_coordinates() %>% as_tibble() %>%
                                                  rename(lon = X, lat = Y),
                                                  .before = 1) %>%
      st_drop_geometry()
  }

  # only keep hotspots from within specified dates
  date_interval <- lubridate::interval(start = start_date, end = end_date)
  VIIRS_ref <- VIIRS_ref %>% filter(as.Date(acq_date) %within% date_interval)

  # make new UTC datetime (dttm) and epoch fields
  UTC_acq_time <- paste0(VIIRS_ref$acq_date, ' ',formatC(as.numeric(VIIRS_ref$acq_time), width = 4, flag = '0'))

  VIIRS_ref <- VIIRS_ref %>%
    add_column(UTC_dttm = UTC_acq_time,
               epoch = lubridate::ymd_hm(UTC_acq_time) %>% as.integer(),
               .after = 'acq_time')

  # remove the tempfile
  unlink(VIIRS_temp, recursive = TRUE)

  message(paste0('Returning VIIRS S-NPP 375m hotspots detected from ',
                 start_date, ' to ', end_date,
                 ifelse(return_sf == TRUE,' as an sf object.',' as a tibble.')))

  return(VIIRS_ref)
}

# ----------------------------------------------------

#' Grab MODIS hotspots
#'
#' Import and/or download archived MODIS Collection 6 hotspots from the FIRMS database within a user-defined date range and spatial extent.
#'
#' @param reference_poly An sp or sf polygon, defining the extent that hotspots are extracted for. The location of a polygon file is also accepted.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in \code{reference_poly}, thereby increasing the extraction extent. Defaults to 0.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param return_sf Logical. Should the points be returned as an sf object? If \code{FALSE}, returns a tibble with lat/lon information. Defaults to TRUE.
#' @param match_crs Logical. Should the extracted points be projected to the CRS of the reference \code{extent} object? Only applicable if \code{return_sf = TRUE}. Defaults to \code{FALSE}. If \code{FALSE}, sf points are returned in WGS84.
#'
#' @details
#' This function extracts the archived MODIS Collection 6 hotspots from \url{https://firms.modaps.eosdis.nasa.gov/download/}.
#' The Country Yearly Summary CSVs are used for download, which means there is a considerable time lag until hotspots are available using this function (> 1 year lag).
#'
#' Currently, this function does not support download of NRT MODIS Collection 6 hotspots.
#'
#' @return sf points or a tibble
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
#' modis <- MODIS_hotspots_grab(reference_poly = e,
#'                              start_date = '2019-05-20',
#'                              end_date = '2019-06-10',
#'                              return_sf = TRUE,
#'                              match_crs = TRUE)
#' plot(modis[,'acq_date'])
#' }

MODIS_hotspots_grab <- function(reference_poly,
                                buff_width = 0,
                                start_date,
                                end_date,
                                return_sf = TRUE,
                                match_crs = TRUE){
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
  utils::download.file(destfile = MODIS_temp, url = MODIS_url, method = 'libcurl')
  MODIS <- utils::read.csv(MODIS_temp) %>% as_tibble() %>%
    st_as_sf(coords = c('longitude','latitude'), crs = 4326) %>%
    st_transform(crs = 3978)

  orig_crs <- st_crs(reference_poly) # keep original crs
  reference_poly <- st_transform(reference_poly, crs = 3978)

  if(!buff_width == 0){
    reference_poly <- st_buffer(reference_poly, dist = buff_width)
  }
  MODIS_ref <- MODIS[reference_poly,] # select hotspots within fire buffer
  MODIS_ref <- MODIS_ref %>% dplyr::relocate(c('acq_date','acq_time'), .before = 1)
  MODIS_ref <- MODIS_ref %>% dplyr::relocate(c('satellite','instrument'), .after = 2)

  # should a sf object or tibble be returned?
  if(return_sf == TRUE){ # return an sf object
    if(match_crs == FALSE){ # if match_crs = F, transform to native crs: WGS84
      MODIS_ref <- st_transform(MODIS_ref, crs = 4326)
    } else {
      MODIS_ref <- st_transform(MODIS_ref, crs = orig_crs)
    }
  } else { # if return_sf == FALSE, return a tibble with latlon info
    MODIS_ref <- MODIS_ref %>% st_transform(crs = 4326)
    MODIS_ref <- MODIS_ref %>% tibble::add_column(MODIS_ref %>% st_coordinates() %>% as_tibble() %>%
                                                    rename(lon = X, lat = Y),
                                                  .before = 1) %>%
      st_drop_geometry()
  }

  # only keep hotspots from within specified dates
  date_interval <- lubridate::interval(start = start_date, end = end_date)
  MODIS_ref<- MODIS_ref %>% dplyr::filter(as.Date(acq_date) %within% date_interval)

  # make new UTC datetime (dttm) and epoch fields
  UTC_acq_time <- paste0(MODIS_ref$acq_date, ' ',formatC(as.numeric(MODIS_ref$acq_time), width = 4, flag = '0'))

  MODIS_ref <- MODIS_ref %>%
    add_column(UTC_dttm = UTC_acq_time,
               epoch = lubridate::ymd_hm(UTC_acq_time) %>% as.integer(),
               .after = 'acq_time')

  unlink(MODIS_temp, recursive = TRUE)

  message(paste0('Returning MODIS Collection 6 hotspots detected from ',
                 start_date, ' to ', end_date,
                 ifelse(return_sf == TRUE,' as an sf object.',' as a tibble.')))

  return(MODIS_ref)
}

