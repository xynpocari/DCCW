# ----------------------------------------------------

# Project title: Fire Progression Reanalysis
# Organization: NRCAN, Northern Forestry Centre, Edmonton AB
# Author: Xue Yan Chan, xueyan.chan@canada.ca
# Supervised by: Piyush Jain and Dan Thompson
# Last updated: 2021-04-08

# ----------------------------------------------------

#' ERA5-Land hourly Variable List
#'
#' See a list of ERA5-Land hourly variables that can be pulled from Google Earth Engine.
#'
#' @details Run this function to see a list of ERA5 Land hourly variables that can be pulled using the function \code{ERA5Land_GEE_grab()}.
#' A vector of the items under the BandName column can be used for the parameter variable in \code{ERA5Land_GEE_grab()}.
#' Units for each variable are also included.
#'
#' For more information, see \url{https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY#description}.
#'
#' @return data.frame of ERA5 Land variables
#' @export
#'
#' @references Google Earth Engine, ERA5-Land hourly: \url{https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY#description}
#'
#' @examples
#' \dontrun{
#' ERA5Land_VariableList()
#' }

ERA5Land_VariableList <- function(){
  data.frame(BandName = c('dewpoint_temperature_2m',
                            'temperature_2m',
                            'skin_temperature',
                            'soil_temperature_level_1',
                            'soil_temperature_level_2',
                            'soil_temperature_level_3',
                            'soil_temperature_level_4',
                            'lake_bottom_temperature',
                            'lake_ice_depth',
                            'lake_ice_temperature',
                            'lake_mix_layer_depth',
                            'lake_mix_layer_temperature',
                            'lake_shape_factor',
                            'lake_total_layer_temperature',
                            'snow_albedo',
                            'snow_cover',
                            'snow_density',
                            'snow_depth',
                            'snow_depth_water_equivalent',
                            'snowfall',
                            'snowmelt',
                            'temperature_of_snow_layer',
                            'skin_reservoir_content',
                            'volumetric_soil_water_layer_1',
                            'volumetric_soil_water_layer_2',
                            'volumetric_soil_water_layer_3',
                            'volumetric_soil_water_layer_4',
                            'forecast_albedo',
                            'surface_latent_heat_flux',
                            'surface_net_solar_radiation',
                            'surface_net_thermal_radiation',
                            'surface_sensible_heat_flux',
                            'surface_solar_radiation_downwards',
                            'surface_thermal_radiation_downwards',
                            'evaporation_from_bare_soil',
                            'evaporation_from_open_water_surfaces_excluding_oceans',
                            'evaporation_from_the_top_of_canopy',
                            'evaporation_from_vegetation_transpiration',
                            'potential_evaporation',
                            'runoff',
                            'snow_evaporation',
                            'sub_surface_runoff',
                            'surface_runoff',
                            'total_evaporation',
                            'u_component_of_wind_10m',
                            'v_component_of_wind_10m',
                            'surface_pressure',
                            'total_precipitation',
                            'leaf_area_index_high_vegetation',
                            'leaf_area_index_low_vegetation',
                            'snowfall_hourly',
                            'snowmelt_hourly',
                            'surface_latent_heat_flux_hourly',
                            'surface_net_solar_radiation_hourly',
                            'surface_net_thermal_radiation_hourly',
                            'surface_sensible_heat_flux_hourly',
                            'surface_solar_radiation_downwards_hourly',
                            'surface_thermal_radiation_downwards_hourly',
                            'evaporation_from_bare_soil_hourly',
                            'evaporation_from_open_water_surfaces_excluding_oceans_hourly',
                            'evaporation_from_the_top_of_canopy_hourly',
                            'evaporation_from_vegetation_transpiration_hourly',
                            'potential_evaporation_hourly',
                            'runoff_hourly',
                            'snow_evaporation_hourly',
                            'sub_surface_runoff_hourly',
                            'surface_runoff_hourly',
                            'total_evaporation_hourly',
                            'total_precipitation_hourly'),
               Units = c('K','K','K','K','K','K','K','K','m','K','m','K','Dimensionless','K','(0-1)',
                        '%','kg/m^3','m','m of water equivalent','m of water equivalent','m of water equivalent',
                        'K','m of water equivalent','m3/m3','m3/m3','m3/m3','m3/m3','(0-1)','J/m2','J/m2',
                        'J/m2','J/m2','J/m2','J/m2','m of water equivalent','m of water equivalent','m of water equivalent',
                        'm of water equivalent','m','m','m of water equivalent','m','m','m of water equivalent','m/s',
                        'm/s','Pa','m','m^2/m^2','m^2/m^2','m of water equivalent','m of water equivalent','J/m2',
                        'J/m2','J/m2','J/m2','J/m2','J/m2','m of water equivalent','m of water equivalent','m of water equivalent',
                        'm of water equivalent','m','m','m of water equivalent','m','m','m of water equivalent','m'))
}

# ----------------------------------------------------

#' Grab ERA5-Land hourly rasters from Google Earth Engine
#'
#' Extract ERA5-Land hourly rasters using Google Earth Engine to a user-defined date range and spatial extent (via package rgee).
#'
#' @param variable A string or vector of ERA5 Land hourly raster band names. E.g. \code{c('dewpoint_temperature_2m','temperature_2m')}. Run \code{ERA5Land_VariableList()} to see all accepted options.
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. End date of data extraction, using UTC. E.g."2018-05-07".
#' @param extent An sp, sf, or Raster* object used to define the extraction extent.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in extent, thereby increasing the extraction extent. Defaults to 0.
#' @param match_crs Logical. Should the extracted rasters be projected to the CRS of the reference extent object? Defaults to \code{FALSE.} If \code{FALSE}, the raster is returned in the native projection and resolution (WGS 84, 0.1 arc degrees).
#' @param output_directory Optional. Output directory where NetCDFs will be exported. If \code{NULL}, the NetCDFs will not be exported to the local device.
#' @param filename_prefix Optional. String that will be appended to the start of the exported raster file name. Root names of files are based on variable names.
#'
#' @details
#' This function extracts ERA5-Land hourly variables to a user-defined date range and spatial extent using the rgee package.
#'
#' A Google Earth Engine account is required to use this function. For more information, see \url{https://earthengine.google.com/}.
#' Google drive must also be accessible by Google Earth Engine for downloading purposes. Run \code{ee_initialize(drive = TRUE)} before running this function.
#'
#' @return A named list of RasterBricks
#' @export
#'
#' @references Google Earth Engine, ERA5-Land hourly: \url{https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY#description}
#' @references rgee: \url{https://github.com/r-spatial/rgee}
#'
#' @import rgee
#' @import raster
#' @import sf
#' @import lubridate
#' @import lutz
#' @import humidity
#'
#' @examples
#' \dontrun{
#'
#' library(rgee)
#' library(raster)
#' library(sf)
#' library(lubridate)
#' library(lutz)
#' library(humidity)
#'
#' ee_Initialize(drive = TRUE) # initialize rgee
#'
#' # define an extent polygon
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>%
#'      st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' # use ERA5Land_VariableList() to see band options
#' ERA5Land_VariableList()
#'
#' temperature_brick <- ERA5Land_GEE_grab(variable = 'temperature_2m',
#'                                        start_date = '2019-05-20',
#'                                        end_date = '2019-05-25',
#'                                        extent = e, # sp or sf or Raster*
#'                                        buff_width = 0, # meters
#'                                        match_crs = TRUE,
#'                                        output_directory = NULL,
#'                                        filename_prefix = NULL)
#'
#' plot(temperature_brick[[1]][[1:4]])
#' }

ERA5Land_GEE_grab <- function(variable = NULL,
                              start_date,
                              end_date,
                              extent,
                              buff_width = 0,
                              match_crs = FALSE,
                              output_directory = NULL,
                              filename_prefix = NULL){

  if(is.null(variable)){
    stop("Please specify variables to pull. Use ERA5Land_VariableList() to see valid variable names.")
  }

  # make sure all specified variables are allowed
  if(!all(variable %in% ERA5Land_VariableList()[,1])){
    stop('One or more input variable names are invalid. Use ERA5Land_VariableList() to see valid variable names.')
  }

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

  # create tempfiles, one for each band of interest, then request ERA5Land
  tmpfile_vec <- vector(length = length(variable))
  for(i in seq_along(tmpfile_vec)){ tmpfile_vec[i] <- tempfile(fileext = '.tif')}

  # get ERA5Land data
  for(i in 1:length(variable)){
    # select band of interest
    ERA5Land_tmp <- ERA5Land_data$select(variable[i])
    ERA5Land_tmp_tobands <- ERA5Land_tmp$toBands() # convert to bands

    # request ERA5Land data and download to tempfile
    # here, scale is not specified. will default to native resolution (0.1 arc degrees)
    suppressWarnings(ee_as_raster(image = ERA5Land_tmp_tobands,
                                  region = reference_ee,
                                  via = 'drive',
                                  dsn = tmpfile_vec[i]))
    # pause for 1.5 seconds between calls
    Sys.sleep(1.5)
  }

  # read in ERA5Land rasterbricks
  ERA5Land_List <- lapply(tmpfile_vec, raster::brick)

  names(ERA5Land_List) <- variable

  # If both t2m and d2m layers are present, calculate relative humidity.
  # Uses humidity::SVP.ClaCla()
  if('dewpoint_temperature_2m' %in% names(ERA5Land_List) & 'temperature_2m' %in% names(ERA5Land_List)){
    message('Both 2m dewpoint temperature and 2m temperature variables detected. Automatically generating a relative humidity RasterBrick from these inputs.')
    e <- calc(ERA5Land_List$dewpoint_temperature_2m, fun = SVP.ClaCla)
    Es <- calc(ERA5Land_List$temperature_2m, fun = SVP.ClaCla)
    psi <- e/Es * 100
    names(psi) <- names(ERA5Land_List$dewpoint_temperature_2m)
    ERA5Land_List$relative_humidity_2m <- psi
  }

  # this seems to be an issue --> there are negative values of total_precipitation_hourly?
  # also seems to be negatives for ssrd hourly
  # for now, change negative values to 0s. but this requires more investigation.
  if('total_precipitation_hourly' %in% names(ERA5Land_List)){
    tp_hourly_tmp <- ERA5Land_List$total_precipitation_hourly
    tp_hourly_tmp[tp_hourly_tmp <0] <- 0
    ERA5Land_List$total_precipitation_hourly <- tp_hourly_tmp
  }

  if('surface_solar_radiation_downwards_hourly'%in% names(ERA5Land_List)){
    ssrd_hourly_tmp <- ERA5Land_List$surface_solar_radiation_downwards_hourly
    ssrd_hourly_tmp[ssrd_hourly_tmp <0] <- 0
    ERA5Land_List$surface_solar_radiation_downwards_hourly <- ssrd_hourly_tmp
  }

  # if match_crs is TRUE, match the extent objects crs
  if(match_crs == TRUE){
    # get original extremes before reprojecting
    layer_min_list <- lapply(ERA5Land_List, cellStats, stat = 'min')
    layer_max_list <- lapply(ERA5Land_List, cellStats, stat = 'max')

    ERA5Land_List <- lapply(ERA5Land_List, projectRaster, crs = extent_crs, method = 'bilinear')

    message('Projecting RasterBricks to target crs...')
    # ensure that new values exist only within the range of pre-existing values using raster::clamp()
    for(i in seq_along(ERA5Land_List)){
      lower <- layer_min_list[[i]] %>% replace(is.na(.), 0) # replace NAs or else raster::clamp() throws an error.
      upper <- layer_max_list[[i]] %>% replace(is.na(.), 0) # does not affect desired output.

      ERA5Land_List[[i]] <- raster::clamp(ERA5Land_List[[i]],
                                          lower = lower,
                                          upper = upper,
                                          useValues = T)
    }
  }

  # add z variables (epoch) for all layers
  datetimes <- ERA5Land_data$aggregate_array('system:index')
  datetimes <- datetimes$getInfo() # convert to a vector

  zvals <-  datetimes %>% gsub('T','',.) %>% parse_date_time(orders = 'y.m.d.H') %>% as.integer()
  ERA5Land_List <- lapply(ERA5Land_List, setZ, z = zvals, 'time (seconds since 1970-01-01)')

  # name layers with the local time
  # parse local dttms
  rast_centroid <- st_bbox(ERA5Land_List[[1]]) %>% st_as_sfc() %>% st_transform(crs = 3978) %>% st_centroid()
  timezone <- tz_lookup(rast_centroid, method = 'accurate')# get local timezone
  Local_dttms <- as_datetime(zvals) %>% with_tz(tzone = timezone)
  Local_dttm_names <- paste0(substr(as.character(Local_dttms), 1,16), '.',base::format(Local_dttms[1], format = '%Z'))

  ERA5Land_List <- lapply(ERA5Land_List, stats::setNames, Local_dttm_names) # rename all bricks in list

  # if an output directory is given, output ncdfs
  if(!is.null(output_directory)){
    for(i in 1:length(ERA5Land_List)){
      varlist <- ERA5Land_VariableList() %>% as_tibble()
      varname <- names(ERA5Land_List[i])
      if(varname == 'relative_humidity_2m'){
        varunit <- '%'
      } else {
        varunit <- varlist %>% filter(BandName == varname) %>% pull(Units)
      }

      if(!is.null(filename_prefix)){
        # if a filename prefix is provided:
        filename <- file.path(output_directory,paste0(filename_prefix,'_ERA5Land','_',varname,'_', gsub('-','',start_date),'_',gsub('-','',(end_date-1)),'.nc'))
      } else {
        filename <- file.path(output_directory,paste0('ERA5Land','_',varname,'_', gsub('-','',start_date),'_',gsub('-','',(end_date-1)),'.nc'))
      }

      writeRaster(x = ERA5Land_List[[i]],
                  filename = filename,
                  varname = varname,
                  varunit = varunit,
                  longname = varname,
                  zname = 'Time', zunit = 'seconds since 1970-01-01 00:00:00',
                  overwrite = T)

    }
    message(paste0('NetCDFs saved locally in ',output_directory))
  }
  ERA5Land_List_final <- ERA5Land_List
  message('Returning ERA5Land RasterBricks.')
  #unlink(tmpfile_vec, recursive = TRUE)
  return(ERA5Land_List_final)
}
