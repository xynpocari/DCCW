#' Grab ERA5-Land hourly tables from Google Earth Engine
#'
#' Extract ERA5-Land hourly tables using Google Earth Engine to a user-defined date range and spatial extent (via package rgee).
#'
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param extent An sp, sf, or Raster* object used to define the extraction extent.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in extent, thereby increasing the extraction extent. Defaults to 0.
#' @param output_directory Optional. Output directory where a CSV will be exported. If \code{NULL}, the CSV will not be exported to the local device.
#' @param filename_prefix Optional. String that will be appended to the start of the exported CSV.
#'
#' @details
#' This function extracts ERA5-Land hourly variables (in table format) to a user-defined date range and spatial extent using the rgee package.
#' In contrast, \code{ERA5Land_GEE_grab()} will extract the hourly rasters.
#' The benefit of using this \code{ERA5Land_table_GEE_grab()} is improved speed.
#'
#' This function only extracts a selected subset of weather metrics, including dewpoint_temperature_2m, temperature_2m, snow_cover, total_precipitation_hourly,
#' surface_solar_radiation_downwards_hourly, u_component_of_wind_10m, v_component_of_wind_10m, and surface_pressure.
#' Relative humidity and windspeed are automatically calculated from these inputs.
#' The values in the resulting table represent the MEDIAN value over the extraction extent per hourly timestep.
#'
#' A Google Earth Engine account is required to use this function. For more information, see \url{https://earthengine.google.com/}.
#' Google drive must also be accessible by Google Earth Engine for downloading purposes. Run \code{ee_initialize(drive = TRUE)} before running this function.
#'
#' @return A tibble
#' @export
#'
#' @references Google Earth Engine, ERA5-Land hourly: \url{https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY#description}
#' @references rgee: \url{https://github.com/r-spatial/rgee}
#'
#' @import rgee
#' @import sf
#' @import lubridate
#' @import lutz
#' @import humidity

ERA5Land_table_GEE_grab <- function(start_date,
                                    end_date,
                                    extent,
                                    buff_width = 0,
                                    output_directory = NULL,
                                    filename_prefix = NULL){

  # define the extent
  if(!grepl("Raster|Spatial|sf", class(extent)[1])){
    stop('extent must be an sp, sf, or Raster* object')
  } else {
    extent_crs <- crs(extent) # grab crs for possible reprojection below

    if(grepl('Raster', class(extent)[1])){
      extent <- st_bbox(extent) %>% st_as_sfc()
      extent <- st_sf(geometry = extent)
    }
    extent <- st_as_sf(extent)
    extent <- st_transform(extent, crs = 3978) %>% # buffer can only be applied on PCS, not GCS
      st_buffer(dist = buff_width) %>% st_transform(crs = 4326) %>% st_bbox() %>% st_as_sfc()

  }

  reference_ee <- sf_as_ee(extent) # convert extent to ee object

  # define dates of interest
  # recall GEE uses UTC, so extracted data wont exactly match the local dates you choose below
  # suggested: use a 1 day buffer to account for time zone discrepancies
  start_date <- as.Date(start_date)
  end_date <- (as.Date(end_date) + 1)

  ee_start_date <- ee$Date(start_date %>% as.character()) # ee$Date requires character input
  ee_end_date <- ee$Date(end_date %>% as.character())

  # extract ERA5Land data, collection name is:'"ECMWF/ERA5_LAND/HOURLY"
  # see https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY#description
  ERA5Land_data <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$filterDate(ee_start_date, ee_end_date)$filterBounds(reference_ee)

  ERA5Land_data_subset <- ERA5Land_data$select('dewpoint_temperature_2m','temperature_2m','snow_cover','total_precipitation_hourly',
                                               'surface_solar_radiation_downwards_hourly','u_component_of_wind_10m','v_component_of_wind_10m',
                                               'surface_pressure')

  # define earth engine mapping functions
  # function that adds timestamp band
  addTime <- function(image){
    return(image$addBands(image$metadata('system:time_start')$rename('time_start')))
  }

  # function that converts raster to points using ee sample
  getSamples <- function(image){
    return(image$sample(region = reference_ee, geometries = FALSE))
  }

  ERA5Land_time <- ERA5Land_data_subset$map(addTime)
  ERA5Land_time_samples <- ERA5Land_time$map(getSamples)

  # flatten
  ERA5Land_points <- ERA5Land_time_samples$flatten()

  # create earth engine task description
  taskname <- paste0('ERA5Land_GEE_grab_task_',base::format(Sys.time(), '%Y-%m-%d-%H:%M:%S'))

  # move results to google drive
  ee_table <- rgee::ee_table_to_drive(collection = ERA5Land_points,
                                      description = taskname,
                                      fileFormat = 'CSV')

  ee_table$start() # start the task

  message('Moving Earth Engine results to Google Drive.')
  ee_monitoring(ee_table)

  # move google drive table to local
  message('Moving Google Drive table to local directory.')

  # save to local tempfile
  ERA5Land_temp <- tempfile(fileext = '.csv')
  ee_drive_to_local(task = ee_table,
                    dsn = ERA5Land_temp,
                    quiet = TRUE)

  # import to R
  ERA5land_table <- read.csv(ERA5Land_temp) %>% as_tibble()

  # process the table output
  # adding datetimes
  ERA5land_vars <- ERA5land_table %>% dplyr::select(-1) %>% # remove time index column
    dplyr::mutate(epoch = as.integer(time_start/1000),.before = 1) %>%
    dplyr::select(-c(time_start,.geo))

  names(ERA5land_vars)

  # summarise by epoch
  ERA5land_vars <- ERA5land_vars %>% group_by(epoch) %>%
    summarise(dewpoint_temperature_2m = median(dewpoint_temperature_2m),
              snow_cover = median(snow_cover),
              surface_pressure = median(surface_pressure),
              surface_solar_radiation_downwards_hourly = median(surface_solar_radiation_downwards_hourly),
              temperature_2m = median(temperature_2m),
              total_precipitation_hourly = median(total_precipitation_hourly),
              u_component_of_wind_10m = median(u_component_of_wind_10m),
              v_component_of_wind_10m = median(v_component_of_wind_10m)) %>%
    mutate(UTC_dttm = as_datetime(epoch),.after = epoch)

  # add in other variables

  # If both t2m and d2m layers are present, calculate relative humidity.
  # Uses humidity::SVP.ClaCla()
  if('dewpoint_temperature_2m' %in% names(ERA5land_vars) & 'temperature_2m' %in% names(ERA5land_vars)){
    message('Both 2m dewpoint temperature and 2m temperature variables detected. Automatically generating relative humidity from these inputs.')
    e <-  SVP.ClaCla(ERA5land_vars$dewpoint_temperature_2m)
    Es <- SVP.ClaCla(ERA5land_vars$temperature_2m)
    psi <- e/Es * 100

    ERA5land_vars$relative_humidity_2m <- psi
  }

  # this seems to be an issue --> there are negative values of total_precipitation_hourly?
  # also seems to be negatives for ssrd hourly
  # for now, change negative values to 0s. but this requires more investigation.
  if('total_precipitation_hourly' %in% names(ERA5land_vars)){
    tp_hourly_tmp <- ERA5land_vars$total_precipitation_hourly
    tp_hourly_tmp[tp_hourly_tmp <0] <- 0
    tp_hourly_tmp <- tp_hourly_tmp*1000 # convert from m to mm
    ERA5land_vars$total_precipitation_hourly <- tp_hourly_tmp
  }

  if('surface_solar_radiation_downwards_hourly'%in% names(ERA5land_vars)){
    ssrd_hourly_tmp <- ERA5land_vars$surface_solar_radiation_downwards_hourly
    ssrd_hourly_tmp[ssrd_hourly_tmp <0] <- 0
    ERA5land_vars$surface_solar_radiation_downwards_hourly <- ssrd_hourly_tmp
  }

  # if both u and v components of wind are present, calculate windspeed raster
  if('u_component_of_wind_10m' %in% names(ERA5land_vars) & 'v_component_of_wind_10m' %in% names(ERA5land_vars)){
    message('Both u component of wind 10m and v component of wind 10m detected. Automatically generating a windspeed RasterBrick from these inputs.')

    u_sqrd <- ERA5land_vars$u_component_of_wind_10m^2
    v_sqrd <- ERA5land_vars$v_component_of_wind_10m^2

    windspeed <- sqrt(u_sqrd + v_sqrd)
    windspeed <- windspeed*3.6 # convert to km/h

    ERA5land_vars$windspeed_10m <- windspeed
  }

  # if total precip hourly is an attribute, calculate 24 hr precip
  if('total_precipitation_hourly' %in% names(ERA5land_vars)){
    n <- 24 # denotes to use 24 rows
    ERA5land_vars$total_precipitation_24hr <- c(rep_len(NA, n - 1), base::rowSums(embed(ERA5land_vars$total_precipitation_hourly,n)))

  }

  # coerce dttm to character
  ERA5land_vars$UTC_dttm <- ERA5land_vars$UTC_dttm %>% as.character()

  # if output directory is given, export this table

  if(!is.null(output_directory)){

    if(!is.null(filename_prefix)){
      outname <- file.path(output_directory, paste0(filename_prefix,'_ERA5Land_Hourly_Weather_metrics.csv'))
    } else {
      outname <- file.path(output_directory, 'ERA5Land_Hourly_Weather_metrics.csv')
    }

    write.csv(ERA5land_vars, file = outname, row.names = FALSE)


  }

  return(ERA5land_vars)

}
