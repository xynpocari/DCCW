#' Grab GOES-16 and GOES-17 FRP tables from Google Earth Engine
#'
#' Extract GOES-16 or GOES-17 FDCF Series ABI Level 2 FRP tables using Google Earth Engine from the R environment (via package rgee).
#'
#' @param reference_poly A sf or sp polygon that defines the spatial extent that FRP values are summed within.
#' @param buff_width Width (in meters) to buffer the \code{reference_poly}. Defaults to 0.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param interval Time interval to sum FRP values over. Choices are \code{"Daily"}, \code{"Hourly"}, or \code{"10min"}. Defaults to \code{"Hourly"}. See details for definiton of \code{"Daily"} aggregation.
#'
#' @details This function extracts GOES-16 and GOES-17 FRP values within a user-defined date interval and spatial polygon.
#' The returned table values represent the sum of FRP values within the specified time interval in the spatial polygon provided.
#'
#' If \code{interval = "Daily"}, the interval is defined as a 24 hr period starting at 9AM local time and ending at 8:59AM on the subsequent day.
#'
#' A Google Earth Engine account is required to use this function. For more information, see \url{https://earthengine.google.com/}.
#'
#' Data is updated in near real-time (~ 30 minute lag). For more information, see
#' \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description} and
#' \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description}
#'
#' @return A tibble
#' @export
#'
#' @references Google Earth Engine, GOES-16 FDCF: \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description}
#' @references Google Earth Engine, GOES-17 FDCF: \url{https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description}
#' @references rgee: \url{https://github.com/r-spatial/rgee}
#'
#' @import rgee
#' @import raster
#' @import sf
#' @import lubridate
#' @import lutz
#' @import tidyr
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#' library(lubridate)
#' library(rgee)
#' library(lutz)
#' library(tidyr)
#'
#' ee_Initialize()
#'
#' # Create extent polygon
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' GOES_tbl <- GOES_FRP_table_grab(reference_poly = e,
#'                                buff_width = 0,
#'                                start_date = '2019-05-20',
#'                                end_date = '2019-05-25',
#'                                interval = 'Hourly')
#'
#'GOES_tbl
#' }

GOES_FRP_table_grab <- function(reference_poly,
                          buff_width = 0,
                          start_date,
                          end_date,
                          interval = 'Hourly'){

  # define dates of interest
  # this script will work pretty quickly for ~ 4-5 months of data
  # recall GEE uses UTC, so extracted data wont exactly match the local dates you choose below
  # suggested: use a 1 day buffer to account for time zone discrepancies
  start_date <- as.Date(start_date) %>% as.character() # ee$Date requires character input
  end_date <- as.Date(end_date) %>% as.character()

  # check to see if dates defined coincide with GOES data availability
  if(as.Date(start_date) < as.Date('2017-05-24')){
    stop('GOES 16 and GOES 17 data are not available for selected dates. GOES 16 and GOES 17 data are available from 2017-05-24 and 2018-08-27 onwards, respectively.')
  }
  if(as.Date(start_date) > as.Date('2017-05-24') & as.Date(start_date) > as.Date('2018-08-27')){
    message('GOES 16 and GOES 17 data are available for selected dates.')
    grab_17 <- TRUE
  }
  if(as.Date(start_date) > as.Date('2017-05-24') & as.Date(start_date) < as.Date('2018-08-27')){
    message('Only GOES 16 data are available for selected dates and will be returned. GOES 17 data are only available from 2018-08-27 onwards.')
    grab_17 <- FALSE
  }

  start_date <- ee$Date(start_date) # example: '2019-05-17'
  end_date <- ee$Date(end_date)

  reference_poly <- st_as_sf(reference_poly)

  if(buff_width > 0){
    reference_poly <- st_transform(reference_poly, crs = 3978) %>%
      st_buffer(dist = buff_width)
  }

  # get local timezone of reference poly centroid
  timezone <- tz_lookup(suppressWarnings(st_centroid(reference_poly)), method = 'accurate')
  timezone <- timezone[1]

  # transform to WGS 84
  reference_poly <- st_transform(reference_poly, crs = 4326) %>% st_geometry()

  # convert buffer to ee object
  reference_ee <- sf_as_ee(reference_poly)

  # begin grabbing data from GEE
  message('Retrieving GOES 16 FRP data from Earth Engine')

  # extract GOES 16 data, collection name is:'NOAA/GOES/16/FDCF'
  # see https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description
  goes16_data <- ee$ImageCollection('NOAA/GOES/16/FDCF')$filterDate(start_date, end_date)$filterBounds(reference_ee)

  # select the band of interest
  FRP16_data <- goes16_data$select('Power') # 'Power' is the band for FRP

  # extract FRP over the image collection
  # first convert to bands
  FRP16_data_tobands <- FRP16_data$toBands()

  # the following returns a dictionary of values
  FRP16_extract <- FRP16_data_tobands$reduceRegion( # reduce region is like raster::extract()
    reducer = ee$Reducer$sum(), # use a sum to aggregate data
    geometry = reference_ee, # the region to summarize by
    scale = 2000 # resolution of GOES. this must be set to get accurate results.
  )

  FRP16_vector <- FRP16_extract$toArray()$getInfo() %>% unlist() # coerce to a vector
  FRP16_t <- tibble(FRP = FRP16_vector) # create a tibble of the sum FRP

  # extract the datetimes, which is listed as 'system:index'
  datetimes16 <- FRP16_data$aggregate_array('system:index') # this is the time_start (not time_end)
  ee_datetime16 <- datetimes16$getInfo() # convert to a vector

  # append ee datetimes to FRP tibbles,
  # and add in parsed dttm in UTC and MDT
  FRP16_t$UTC_dttm <- ee_datetime16 %>% substring(1, 11) %>% # substring to the minute
    parse_date_time(orders = '%Y%j%H%M') # parse as UTC ddtm
  Local_dttm <-  FRP16_t$UTC_dttm %>% with_tz(tzone = timezone)
  Local_dttm_name <- paste0(base::format(Local_dttm[1], format = '%Z'),'_dttm')
  FRP16_t[Local_dttm_name] <- Local_dttm

  ##############
  if(grab_17 == TRUE){ # only do this if GOES 17 is available for selected dates
    message('Retrieving GOES 17 FRP data from Earth Engine')
    # extract GOES 17 data, collection name is:'NOAA/GOES/17/FDCF'
    # see https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description
    goes17_data <- ee$ImageCollection('NOAA/GOES/17/FDCF')$filterDate(start_date, end_date)$filterBounds(reference_ee)

    # select the band of interest
    FRP17_data <- goes17_data$select('Power') # 'Power' is the band for FRP

    # extract FRP over the image collection
    # first convert to bands
    FRP17_data_tobands <- FRP17_data$toBands()

    # the following returns a dictionary of values
    FRP17_extract <- FRP17_data_tobands$reduceRegion( # reduce region is like raster::extract()
      reducer = ee$Reducer$sum(), # use a sum to aggregate data
      geometry = reference_ee, # the region to summarize by
      scale = 2000 # resolution of GOES. this must be set to get accurate results.
    )

    FRP17_vector <- FRP17_extract$toArray()$getInfo() %>% unlist() # coerce to a vector
    FRP17_t <- tibble(FRP = FRP17_vector) # create a tibble of the sum FRP

    # extract the datetimes, which is listed as 'system:index'
    datetimes17 <- FRP17_data$aggregate_array('system:index')
    ee_datetime17 <- datetimes17$getInfo() # convert to a vector

    # append ee datetimes to FRP tibbles,
    # and add in parsed dttm in UTC and MDT
    FRP17_t$UTC_dttm <- ee_datetime17 %>% substring(1, 11) %>% # substring to the minute
      parse_date_time(orders = '%Y%j%H%M') # parse as UTC ddtm
    Local_dttm <-  FRP17_t$UTC_dttm %>% with_tz(tzone = timezone)
    Local_dttm_name <- paste0(base::format(Local_dttm[1], format = '%Z'),'_dttm')
    FRP17_t[Local_dttm_name] <- Local_dttm

    # combine the FRP tibbles from GOES 16 and 17
    FRP_10_min <- full_join(FRP16_t, FRP17_t,  by = c('UTC_dttm',Local_dttm_name)) %>%
      rename(GOES16_SumFRP = FRP.x, GOES17_SumFRP = FRP.y)
    FRP_10_min <- FRP_10_min[,c(3,2,1,4)] # reorder columns
  } else { # if grab_17 == FALSE
    FRP_10_min <- FRP16_t[,c(3,2,1)] %>%
      rename(GOES16_SumFRP = FRP)
  }

  # now dealing with time aggregation
  if(interval == '10min'){ # original form
    message(paste0('Returning 10-minute table of GOES FRP data' ))
    return(FRP_10_min)
  }

  if(interval !='10min'){
    # summarize FRP to be hourly

    FRP_hourly <- FRP_10_min %>%
      group_by(date(pull(FRP_10_min[Local_dttm_name])), hour(pull(FRP_10_min[Local_dttm_name]))) %>%
      summarise_if(is.numeric, sum) %>% # summarize numeric cols as a sum
      rename(Date = 1, # rename cols appropriately
             Hour = 2)

    FRP_hourly <- FRP_hourly %>% # add datetime to table
      add_column(UTC_dttm = paste(FRP_hourly$Date,FRP_hourly$Hour) %>% ymd_h() %>% force_tz(timezone)%>% with_tz('UTC'),
                 !!Local_dttm_name := paste(FRP_hourly$Date,FRP_hourly$Hour) %>% ymd_h() %>% force_tz(timezone),
                 .before = 1) %>%
      ungroup() %>%
      dplyr::select(-c(Date, Hour))

    # fill missing datetimes
    FRP_hourly <- FRP_hourly %>%
      complete(nesting(!!Local_dttm_name := seq.POSIXt(min(pull(FRP_hourly[Local_dttm_name])), max(pull(FRP_hourly[Local_dttm_name])), by = 3600),
                       UTC_dttm = seq.POSIXt(min(UTC_dttm), max(UTC_dttm), by = 3600)))

    if(interval == 'Hourly'){
      message(paste0('Returning hourly table of GOES FRP data'))
      return(FRP_hourly)
    }
    if(interval == 'Daily'){
      # get the first and last 9am times covered in the data
      times9am <- FRP_hourly %>% pull(!!Local_dttm_name)
      start_int <- times9am[which(hour(times9am)==9)] %>% min()
      end_int <- times9am[which(hour(times9am)==9)] %>% max()

      # parse intervals
      goes_int <- lubridate::interval(start = start_int, end = end_int)
      goes_int_split <- split_24hrs(goes_int)

      int_end(goes_int_split) <- int_end(goes_int_split)-1 # this -1 ensures FRP wont be double counted in next steps

      # create tibble
      daily_goes <- tibble(Interval_Local = goes_int_split,
                           GOES16_SumFRP = as.numeric(NA),
                           GOES17_SumFRP = as.numeric(NA))

      for(i in 1:nrow(daily_goes)){
        int_hotspots <- FRP_hourly %>% filter(!!rlang::sym(Local_dttm_name) %within% daily_goes$Interval_Local[i])
        daily_goes$GOES16_SumFRP[i] <- int_hotspots$GOES16_SumFRP %>% sum(na.rm = TRUE)

        if(grab_17 == TRUE){
          daily_goes$GOES17_SumFRP[i] <- int_hotspots$GOES17_SumFRP %>% sum(na.rm = TRUE)
        }
      }

      # if there was no GOES 17, remove the column now
      if(grab_17 == FALSE){
        daily_goes <- daily_goes[,c(1:2)]
      }

      # change back the interval ends and add in UTC intervals
      daily_goes$Interval_Local <- split_24hrs(goes_int)
      local_int_name <- paste0(base::format(Local_dttm[1], format = '%Z'), '_Interval')
      daily_goes <- daily_goes %>%
        add_column(UTC_Interval = lubridate::interval(start = with_tz(int_start(daily_goes$Interval_Local), tzone = 'UTC'), end = with_tz(int_end(daily_goes$Interval_Local), tzone = 'UTC')), .before = 2) %>%
        rename(!!local_int_name := Interval_Local) # rename local int

      message(paste0('Returning daily table of GOES FRP data' ))
      return(daily_goes)
    }
  }

}
