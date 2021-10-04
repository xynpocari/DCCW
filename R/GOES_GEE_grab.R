#' Grab GOES point data from Google Earth Engine
#'
#' Extract GOES-16 or GOES-17 FDCF Series ABI Level 2 data as points using Google Earth Engine from the R environment (via package rgee).
#'
#' @param satellite Specify 16 for GOES-16, or 17 for GOES-17.
#' @param maskvals Vector of maskvals to retain in extraction. Default is \code{c(10,11,12,13,14,15,30,31,32,33,34,35)}. See details.
#' @param extent An sp, sf, or Raster* object used to define the extraction extent.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in \code{extent}, thereby increasing the extraction extent. Defaults to 0.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param return_sf Logical. Should the points be returned as an sf object? If \code{FALSE}, returns a tibble with lat/lon information. Defaults to TRUE.
#' @param match_crs Logical. Should the extracted points be projected to the CRS of the reference \code{extent} object? Only applicable if \code{return_sf = TRUE}. Defaults to \code{FALSE}. If \code{FALSE}, sf points are returned in WGS84.
#'
#' @details
#' This function extracts GOES-16 or GOES-17 FDCF Series ABI Level 2 rasters as points to a user-defined date interval and spatial extent.
#' A Google Earth Engine account is required to use this function. For more information, see \url{https://earthengine.google.com/}.
#' Google drive must also be accessible by Google Earth Engine for downloading purposes. Run \code{ee_initialize(drive = TRUE)} before running this function.
#'
#' A single data transfer call to GEE is made if less than 30 days of data are requested. If > 30 days are requested, multiple calls to GEE are made.
#' Data is updated in near real-time (~ 30 minute lag).
#'
#' \code{maskvals} is a vector that denotes which GOES raster mask values to retain. For more information on mask values, see page 530 of the GOES Users' Guide: \url{https://www.goes-r.gov/products/docs/PUG-L2+-vol5.pdf}.
#' To retain ALL mask values, use \code{maskvals = 'all'}
#'
#' For more information, see
#' \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description}
#' and
#' \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description}
#'
#' @return A tibble or sf points object.
#' @export
#'
#' @references Google Earth Engine, GOES-16 FDCF: \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description}
#' @references Google Earth Engine, GOES-17 FDCF: \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description}
#' @references rgee: \url{https://github.com/r-spatial/rgee}
#'
#' @import sf
#' @import lubridate
#' @import rgee
#' @import dplyr
#' @import tibble
#' @import data.table
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(lubridate)
#' library(rgee)
#' library(tibble)
#' library(data.table)
#' library(dplyr)
#' library(tmap)
#'
#' ee_Initialize(drive = TRUE)
#'
#' # create extent polygon
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' # import GOES-16 FDCF product into the R environment as sf points, in WGS84:
#' goes_points <- GOES_GEE_grab(satellite = 16, # '16' or '17'
#'                              maskvals = c(10,11,12,13,14,15,30,31,32,33,34,35), #maskvals to keep
#'                              extent = e, # sf, sp or Raster* object to grab extent from
#'                              buff_width = 8000, # will buffer the extent of the extent above
#'                              start_date = '2019-05-19',
#'                              end_date = '2019-05-22',
#'                              return_sf = TRUE, # if TRUE, return sf
#'                              match_crs = FALSE) #if return_sf = TRUE, match crs of extent?
#'
#' goes_points # returns 2,614 features
#'
#' # visualize
#' tm_shape(e) + tm_borders() +
#'   tm_shape(goes_points) + tm_dots() +
#'   tm_layout(frame = F)
#'
#' # import GOES-16 FDCF product into the R environment as a tibble:
#' goes_tibble <- GOES_GEE_grab(satellite = 16,
#'                              maskvals = c(10,11,12,13,14,15,30,31,32,33,34,35),
#'                              extent = e,
#'                              buff_width = 8000,
#'                              start_date = '2019-05-19',
#'                              end_date = '2019-05-22',
#'                              return_sf = FALSE)
#'
#' goes_tibble # returns 2,614 features. latlon info given.
#'
#' # keep only highest confidence mask values:
#' goes_tibble_conf <- GOES_GEE_grab(satellite = 16,
#'                              maskvals = c(10,11,30,31),
#'                              extent = e,
#'                              buff_width = 8000,
#'                              start_date = '2019-05-19',
#'                              end_date = '2019-05-22',
#'                              return_sf = FALSE)
#'
#' goes_tibble_conf # returns 743 features (subset only to highest confidence). latlon info given.
#' }
GOES_GEE_grab <- function(satellite,
                          maskvals = c(10,11,12,13,14,15,30,31,32,33,34,35),
                          extent,
                          buff_width = 0,
                          start_date,
                          end_date,
                          return_sf = TRUE,
                          match_crs = FALSE){

  # check whether satellite input is valid
  satellite <- as.character(satellite)
  if(!grepl('16|17', satellite)){ stop('satellite must either be "16" or "17".')}

  # parse name of gee catalogue collection
  collection_name <- paste0('NOAA/GOES/',satellite,'/FDCF')

  # define dates of interest
  # add a day to end date, because gee does not count the final day
  start_date <- as.Date(start_date)
  end_date <- (as.Date(end_date) + 1)

  # check if data is available for the requested dates
  if(satellite == '16' & start_date < as.Date('2017-05-24')){
    stop('GOES 16 FRP data are not available for the requested dates. GOES 16 data are available only from 2017-05-24 onwards.')
  }
  if(satellite == '17' & start_date < as.Date('2018-08-27')){
    stop('GOES 17 FRP data are not available for the requested dates. GOES 17 data are available only from 2018-08-27 onwards.')
  }

  # coerce extent into useful form
  if( grepl("sf", class(extent)[1]) ){ extent <- extent %>% st_transform(crs = 4326) %>% st_bbox() %>% st_as_sfc() %>% st_sf()  }
  if( grepl("Spatial", class(extent)[1]) ){ extent <- extent %>% st_as_sf() %>% st_transform(crs = 4326)%>% st_bbox() %>% st_as_sfc() %>% st_sf() }
  if( grepl("Raster", class(extent)[1]) ){ extent <- st_bbox(extent) %>% st_as_sfc() %>% st_sf() }
  if( !grepl("sf|Spatial", class(extent)[1])){ stop('extent must be an sf, sp, or Raster* object') }

  # grab original crs in case data must be reprojected later
  orig_crs <- st_crs(extent)

  # extend the extent if buff_width is defined as something other than 0
  if(!buff_width==0){
    extent <- extent %>% st_transform(crs = 3978) %>% st_buffer(dist = buff_width)
  }

  # transform to WGS84
  extent_WGS84 <- st_transform(extent, crs = 4326)

  # make ee geometry from extent
  bounds <- st_bbox(extent_WGS84)
  ee_bbox <- ee$Geometry$Rectangle(bounds[1],bounds[2],bounds[3],bounds[4])

  # coerce maskvals vector to an ee list
  if(maskvals[1] == 'all'){ # if 'all' is chosen, retain all mask values
    maskvals <- c(0,10,11,12,13,14,15,30,31,32,33,34,35,40,50,60,100,120,
                  121,123,124,125,126,127,150,151,152,153,170,180,182,185,
                  186,187,188,200,201,205,210,215,220,225,230,240,245)
    ee_maskvals <- ee$List(maskvals)
  } else {
    ee_maskvals <- ee$List(maskvals)
  }

  # define earth engine mapping functions
  # function that replaces NA values with -999 (will replace with NAs after) and add timestamp band
  addTime <- function(image){
    return(image$unmask(-999)$addBands(image$metadata('system:time_start')$rename('time_start')))
  }

  # function that converts raster to points using ee sample
  getSamples <- function(image){
    return(image$sample(region = ee_bbox, geometries = TRUE, dropNulls = FALSE))
  }

  # function that adds lon/lat bands to a feature
  addCoords <- function(feature){
    return (feature$set(list(lat = feature$geometry()$coordinates()$get(1),
                             lon = feature$geometry()$coordinates()$get(0))))
  }

  # check how many days of data are requested
  # if > 30 days are being requested, make multiple calls by month
  num_days <- lubridate::time_length(lubridate::interval(start = start_date, end = end_date), unit = 'day')

  if(num_days <= 30){ # if less than 30 days, only make 1 call to earth engine
    # create tempfile to write to
    temp <- tempfile(fileext = '.csv')

    # coerce dates to ee dates
    ee_start_date <- ee$Date(as.character(start_date))
    ee_end_date <- ee$Date(as.character(end_date))

    # grab image collection
    collection <- ee$ImageCollection(collection_name)$filterDate(ee_start_date, ee_end_date)$filterBounds(ee_bbox)

    # turn NAs to -999 and add timestamp band
    collection_unmask <- collection$map(addTime)

    # convert raster to points
    collection_points <- collection_unmask$map(getSamples)

    # flatten collection of collections
    flat <- collection_points$flatten()

    # filter the feature collection by mask value
    flat_subset <- flat$filter(ee$Filter$inList('Mask',ee_maskvals))

    # add in lat/lon info as properties to the feature collection
    flat_subset_coords <- flat_subset$map(addCoords)

    # create earth engine task description
    taskname <- paste0('GOES_GEE_grab_task_',base::format(Sys.time(), '%Y-%m-%d-%H:%M:%S'))

    # move results to google drive
    ee_table <- rgee::ee_table_to_drive(collection = flat_subset_coords,
                                  description = taskname,
                                  fileFormat = 'CSV')

    message('STARTING EARTH ENGINE CALL 1 OF 1')
    ee_table$start() # start the task
    message('Moving Earth Engine results to Google Drive (call 1 of 1)')
    ee_monitoring(ee_table) # print progress to console

    # move google drive table to local
    message('Moving Google Drive table to local directory')

    ee_drive_to_local(task = ee_table,
                      dsn = temp,
                      quiet = TRUE)

    message('DONE EARTH ENGINE CALL 1 OF 1\n')

    # check to see if the csv is empty. if not, import the csv as tibble
    if(file.size(temp) >2){ # consider empty if file is < 2 bytes
      goes_tibble <- data.table::fread(temp) %>% tibble::as_tibble()
    } else { # else if the table is empty (aka < 2 bytes):
      stop('No GOES observations exist given your input criteria. Consider expanding which mask values are included,
           or double check whether a fire should exist with the given spatial and temporal range you provided.')
    }

  } else { # if more than 30 days is being requested, make one call per month
    # parse intervals by month
    dates_seq <- seq.Date(from = start_date, to = end_date, by = 'day')
    months_vec <- month(dates_seq) %>% unique()

    calls_ls <- vector(mode = 'list', length = length(months_vec))

    for(i in 1:length(months_vec)){
      if(i == 1){
        calls_ls[[i]] <- lubridate::interval(start = min(dates_seq[which(month(dates_seq)==months_vec[i])]),
                                  end = max(dates_seq[which(month(dates_seq)==months_vec[i])]))
      } else {
        calls_ls[[i]] <- lubridate::interval(start = (min(dates_seq[which(month(dates_seq)==months_vec[i])]) - 1),
                                  end = max(dates_seq[which(month(dates_seq)==months_vec[i])]))
      }
    }

    # create tmpfiles to write to
    temp_vec <- vector(mode = 'list',length = length(calls_ls))
    for(i in seq_along(calls_ls)){ temp_vec[[i]] <- tempfile(fileext = '.csv')}

    for(i in 1:length(calls_ls)){
      call_int <- calls_ls[[i]]

      # error will occur if start date = end date
      #(this could happen if you want to return the last day of the month)
      # if start date = end date, subtract one day from start date.
      # (possible duplicates will be evaluated and removed later)
      if(lubridate::int_start(call_int) == lubridate::int_end(call_int)){
        lubridate::int_end(call_int) <- lubridate::int_end(call_int) + 86400
      }

      # parse ee dates for the call
      ee_start_date <- ee$Date(int_start(call_int) %>% date() %>% as.character())
      ee_end_date <- ee$Date(int_end(call_int) %>% date() %>% as.character())

      # grab image collection
      collection <- ee$ImageCollection(collection_name)$filterDate(ee_start_date, ee_end_date)$filterBounds(ee_bbox)

      # turn NAs to -999 and add timestamp band
      collection_unmask <- collection$map(addTime)

      # convert raster to points
      collection_points <- collection_unmask$map(getSamples)

      # flatten collection of collections
      flat <- collection_points$flatten()

      # filter the feature collection by mask value
      flat_subset <- flat$filter(ee$Filter$inList('Mask',ee_maskvals))

      # add in lat/lon info as properties to the feature collection
      flat_subset_coords <- flat_subset$map(addCoords)

      # create earth engine task description
      taskname <- paste0('GOES_GEE_grab_task_',base::format(Sys.time(), '%Y-%m-%d-%H:%M:%S'))

      # move results to google drive
      ee_table <- rgee::ee_table_to_drive(collection = flat_subset_coords,
                                          description = taskname,
                                          fileFormat = 'CSV')

      message(paste0('STARTING EARTH ENGINE CALL ',i,' OF ',length(calls_ls)))
      ee_table$start() # start the task
      message(paste0('Moving Earth Engine results to Google Drive (call ',i,' of ',length(calls_ls),')'))
      ee_monitoring(ee_table) # print progress to console

      # move google drive table to local
      message(paste0('Moving Google Drive table to local directory (call ',i,' of ',length(calls_ls),')'))

      ee_drive_to_local(task = ee_table,
                        dsn = temp_vec[[i]],
                        quiet = TRUE)
      message(paste0('DONE EARTH ENGINE CALL ',i,' OF ',length(calls_ls),'\n'))
      # pause for 1.5 seconds between calls
      Sys.sleep(1.5)
    }

    # import the saved csvs
    goes_tibble <- vector(mode = 'list', length = length(calls_ls))
    for(i in 1:length(goes_tibble)){

      # check to see if the csv is empty or not. if empty, put in NA
      if(file.size(temp_vec[[i]]) > 2){
        goes_tibble[[i]] <- data.table::fread(temp_vec[[i]]) %>% as_tibble()
      } else {
        goes_tibble[[i]] <- NA # NA if the csv is empty
      }
    }

    # bind all elements of list together
    goes_tibble <- do.call(rbind, goes_tibble)

    # if the binded csvs returns an object of class matrix, then they were all empty (all NA)
    if(class(goes_tibble)[1] == 'matrix'){
      stop('No GOES observations exist given your input criteria. Consider expanding which mask values are included,
           or double check whether a fire should exist with the given spatial and temporal range you provided.')
    }

    # if the goes_tibble is not a matrix, then there were indeed observations. continue processing.
    # remove rows where all entries are NA (these would occur when some but not all csvs are empty)
    goes_tibble <- goes_tibble %>% filter_all(all_vars(!is.na(.)))

  }

  # further process the goes data
  message('Processing GOES data...')

  # ensure there are no duplicates (could occur if start date = end date for multi-month call)
  goes_tibble <- dplyr::distinct(goes_tibble)

  # replace all -999 values with NA
  goes_tibble <- goes_tibble %>% dplyr::mutate(across(everything(), ~replace(., . ==  -999 , NA)))

  # apply scaling factor + offset for variables Area and Temp
  goes_tibble$Area <- (goes_tibble$Area*60.98) + 4000
  goes_tibble$Temp <- (goes_tibble$Temp*0.05493667) + 400

  # reformat datetime info
  goes_tibble <- goes_tibble %>%
    tibble::add_column(epoch = as.integer((goes_tibble$time_start)/1000),
                       UTC_dttm = as_datetime(goes_tibble$time_start/1000),
                       .before = 1) %>%
    dplyr::select(-c(3,'time_start')) # remove system start time field

  # relocate coordinate information
  goes_tibble <- goes_tibble %>% dplyr::relocate(c('lon','lat'), .before = 1) %>%
    dplyr::select(-'.geo') # remove geometry columns

  # add in satellite and product information
  goes_tibble <- goes_tibble %>% tibble::add_column(satellite = ifelse(satellite == '16','GOES-16','GOES-17'),
                                            product = 'ABI-L2-FDCF',
                                            .after = 4)

  # remove tempfiles
  if(num_days <= 30){ # this means that object temp exists
    unlink(temp, recursive = TRUE)
  } else { # this means that object temp_vec exists
    for(i in 1:length(temp_vec)){
      unlink(temp_vec[[i]], recursive = TRUE)
    }

  }

  if(return_sf == TRUE){
    # create sf points object from tibble
    goes_sf <- goes_tibble %>% st_as_sf(coords = c('lon','lat'), crs = 4326)

    if(match_crs == TRUE){
      goes_sf <- goes_sf %>% st_transform(goes_sf, crs = orig_crs)
    }

    message(paste0('Returning GOES-',satellite,' hotspots from ',
                   start_date,' to ',end_date-1, ' with ', nrow(goes_sf),' features as an sf object.'))
    return(goes_sf)

  } else { # if return_sf == FALSE, return a tibble with latlon info
    message(paste0('Returning GOES-',satellite,' hotspots from ',
                   start_date,' to ',end_date-1, ' with ', nrow(goes_tibble),' features as a tibble.'))
    return(goes_tibble)
  }

}

