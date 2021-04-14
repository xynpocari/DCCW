#' Grab GOES-16 and GOES-17 FRP rasters from Google Earth Engine
#'
#' Extract GOES-16 or GOES-17 FDCF Series ABI Level 2 FRP rasters using Google Earth Engine from the R environment (via package rgee).
#'
#' @param satellite Specify 16 for GOES-16, or 17 for GOES-17.
#' @param extent An sp, sf, or Raster* object used to define the extraction extent.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in extent, thereby increasing the extraction extent. Defaults to 0.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param match_crs Logical. Should the extracted rasters be projected to the CRS of the reference extent object? Defaults to \code{FALSE}. If \code{FALSE}, the raster is returned in the native projection and resolution (GOES-R or GOES-S ABI fixed grid projection, res = 2000 m).
#' @param output_directory Optional. Output directory where a NetCDF will be exported. If \code{NULL}, the NetCDF will not be exported to the local device.
#' @param filename_prefix Optional. String that will be appended to the start of the exported raster file name.
#'
#' @details
#' This function extracts GOES-16 or GOES-17 10-minute rasters to a user-defined date interval and spatial extent.
#' A Google Earth Engine account is required to use this function. For more information, see \url{https://earthengine.google.com/}.
#' Google drive must also be accessible by Google Earth Engine for downloading purposes. Run \code{ee_initialize(drive = TRUE)} before running this function.
#'
#' A separate request is made for each unique month of data requested. This is to limit the amount of data transferred with any one request. All requests are then merged and returned as a single RasterBrick.
#' Data is updated in near real-time (~ 30 minute lag).
#'
#' For more information, see
#' \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description}
#' and
#' \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description}
#'
#' @return A RasterBrick
#' @export
#'
#' @references Google Earth Engine, GOES-16 FDCF: \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description}
#' @references Google Earth Engine, GOES-17 FDCF: \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description}
#' @references rgee: \url{https://github.com/r-spatial/rgee}
#'
#' @import raster
#' @import sf
#' @import lubridate
#' @import rgee
#' @import lutz
#' @import rgdal
#' @import sp
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#' library(sp)
#' library(lubridate)
#' library(rgee)
#' library(lutz)
#' library(rgdal)
#'
#' ee_Initialize(drive = TRUE)
#'
#' # create extent polygon
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' # import GOES-16 FRP rasters as RasterBrick in R environment
#' GOES16_FRP <- GOES_FRP_grab(satellite = 16, # either 16 for GOES-16, or 17 for GOES-17
#'                             extent = e,
#'                             buff_width = 2000, # meters
#'                             start_date = '2019-05-20',
#'                             end_date = '2019-05-21',
#'                             match_crs = TRUE,
#'                             output_directory = NULL)
#'
#'# plot 10-minute rasters
#'plot(GOES16_FRP[[1:4]])
#'
#'# to visualize better, sum all layers together
#'GOES16_FRP_sum <- stackApply(GOES16_FRP, indices = rep(1,nlayers(GOES16_FRP)), sum)
#'plot(GOES16_FRP_sum)
#' }
GOES_FRP_grab <- function(satellite,
                          extent,
                          buff_width = 0,
                          start_date,
                          end_date,
                          match_crs = FALSE,
                          output_directory = NULL,
                          filename_prefix = NULL){

  if( grepl("sf", class(extent)[1]) ){ extent <- st_bbox(extent) %>% st_as_sfc()}
  if( grepl("Spatial", class(extent)[1]) ){ extent <- st_as_sf(extent) %>% st_bbox() %>% st_as_sfc() }
  if( grepl("Raster", class(extent)[1]) ){ extent <- st_bbox(extent) %>% st_as_sfc() }
  if( !grepl("sf|Spatial", class(extent)[1])){ stop('extent must be an sf, sp, or Raster* object')}

  if(!buff_width==0){
    extent <- st_buffer(extent, buff_width)
  }

  # transform to WGS 84
  reference_poly_wgs84 <- st_transform(extent, crs = 4326) %>% st_geometry()

  # convert buffer to ee object
  reference_ee <- sf_as_ee(reference_poly_wgs84)

  # define dates of interest
  # recall GEE uses UTC, so an extra day is added to the end to ensure you get the entire desired range.
  start_date <- as.Date(start_date)
  end_date <- (as.Date(end_date)+1)

  # make an ee to local request per month (to ensure gee is not overloaded, which will return an error)
  # parse intervals by month
  dates_seq <- seq.Date(from = start_date, to = end_date, by = 'day')
  months_vec <- month(dates_seq) %>% unique()

  calls_ls <- vector(mode = 'list', length = length(months_vec))
  for(i in 1:length(months_vec)){
    calls_ls[[i]] <- interval(start = min(dates_seq[which(month(dates_seq)==months_vec[i])]),
                              end = max(dates_seq[which(month(dates_seq)==months_vec[i])]))
  }



  # create tmpfiles to write to
  tmpfile_vec <- vector(mode = 'list',length = length(calls_ls))
  for(i in seq_along(calls_ls)){ tmpfile_vec[[i]] <- tempfile(fileext = '.tif')}

  # and store datetime information from the calls
  datetime_ls <- vector(mode = 'list', length = length(months_vec))

  for(i in 1:length(calls_ls)){
    call_int <- calls_ls[[i]]
    ee_start_date <- ee$Date(int_start(call_int) %>% date() %>% as.character())
    ee_end_date <- ee$Date(int_end(call_int) %>% date() %>% as.character())

    if(satellite == 16){ # get GOES16
      # extract GOES 16 data, collection name is:'NOAA/GOES/16/FDCF'
      # see https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description
      goes16_data <- ee$ImageCollection('NOAA/GOES/16/FDCF')$filterDate(ee_start_date, ee_end_date)$filterBounds(reference_ee)

      # select the band of interest
      FRP16_data <- goes16_data$select('Power') # 'Power' is the band for FRP

      # convert to bands
      FRP_data_tobands <- FRP16_data$toBands()

      # grab datetime info
      datetimes <- FRP16_data$aggregate_array('system:index')
      datetime_ls[[i]] <- datetimes$getInfo() # convert to a vector
    }
    if(satellite == 17){ # get GOES17
      # extract GOES 17 data, collection name is:'NOAA/GOES/17/FDCF'
      # see https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description
      goes17_data <- ee$ImageCollection('NOAA/GOES/17/FDCF')$filterDate(ee_start_date, ee_end_date)$filterBounds(reference_ee)

      # select the band of interest
      FRP17_data <- goes17_data$select('Power') # 'Power' is the band for FRP

      # convert to bands
      FRP_data_tobands <- FRP17_data$toBands()

      # grab datetime info
      datetimes <- FRP17_data$aggregate_array('system:index')
      datetime_ls[[i]] <- datetimes$getInfo() # convert to a vector

    }

    suppressWarnings(ee_as_raster(image = FRP_data_tobands,
                                  region = reference_ee,
                                  scale = 2000, # resolution in meters
                                  via = 'drive',
                                  dsn = tmpfile_vec[[i]]))
    # pause for 1.5 seconds between calls
    Sys.sleep(1.5)
  }

  # the rasterbrick is read in flipped
  # flipping the raster takes a long time, opt to read in tmpfile with rgdal and flip the SpatialGridDF instead
  ee_GOES_spGrid <- suppressWarnings(lapply(tmpfile_vec, readGDAL))
  ee_GOES_spGrid <- lapply(ee_GOES_spGrid, sp::flipVertical) # flip in y direction
  ee_GOES_spGrid_combined <- do.call(cbind, ee_GOES_spGrid) # combine all spgrids into 1
  ee_GOES_new <- raster::brick(ee_GOES_spGrid_combined)

  # flipping as SpatialGridDF shifts the raster. shift up 1 cell to match extent of previous ee_GOES
  ee_GOES_new <- raster::shift(ee_GOES_new, dy = 2000) # 2000 m is 1 cell up

  # define the projection. this is taken from:
  # https://gis.stackexchange.com/questions/372129/reprojecting-goes-16-data-in-r
  # goes 17 seems to use the same parameters, except the central meridian is -137

  if(satellite == 16){
    crs(ee_GOES_new) <- suppressWarnings(CRS("+proj=geos +lon_0=-75 +h=35786023 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs +sweep=x"))
  }
  if(satellite == 17){
    crs(ee_GOES_new) <- suppressWarnings(CRS("+proj=geos +lon_0=-137 +h=35786023 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs +sweep=x"))
  }

  if(match_crs == FALSE){
    message('Returning GOES ',satellite,' FRP RasterBrick')
  } else {
    ee_GOES_new <- projectRaster(ee_GOES_new, crs = crs(extent), method = 'bilinear')
    message('Returning GOES ',satellite,' FRP RasterBrick')
  }

  # add in datetime information
  UTC_dttms <- datetime_ls %>% unlist() %>% substring(1,11) %>%
    parse_date_time(orders = '%Y%j%H%M') # parse as UTC ddtm

  ee_GOES_new <- setZ(ee_GOES_new, z = as.integer(UTC_dttms), name = 'time (seconds since 1970-01-01 00:00:00)')

  # parse local dttms
  timezone <- tz_lookup(suppressWarnings(st_centroid(extent)), method = 'accurate')# get local timezone
  Local_dttms <- UTC_dttms %>% with_tz(tzone = timezone)
  Local_dttm_names <- paste0(substr(as.character(Local_dttms), 1,16), '.',base::format(Local_dttms[1], format = '%Z'))
  names(ee_GOES_new) <- Local_dttm_names # rename layers

  if(!is.null(output_directory)){

    if(!is.null(filename_prefix)){
      filename <- file.path(output_directory, paste0(filename_prefix,'_GOES',satellite,'_FRP_',gsub('-','',start_date),'_',gsub('-','',end_date),'.nc'))
    } else {
      filename <- file.path(output_directory, paste0('GOES',satellite,'_FRP_',gsub('-','',start_date),'_',gsub('-','',end_date),'.nc'))
    }

    writeRaster(x = ee_GOES_new,
                filename = filename,
                varname = 'Power',
                varunit = 'MW',
                longname = 'ABI L2+ Fire-Hot Spot Characterization: Fire Radiative Power',
                zname = 'Time',
                zunit = 'seconds since 1970-01-01 00:00:00',
                overwrite = T)
    message(paste0('Saving GOES NetCDF locally as ', file.path(output_directory, paste0('GOES',satellite,'_FRP_',gsub('-','',start_date),'_',gsub('-','',(end_date-1)),'.nc'))))
  }
  # remove the tempfile created
  unlink(unlist(tmpfile_vec), recursive = T)

  return(ee_GOES_new)
}


