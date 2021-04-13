# ----------------------------------------------------

# Project title: Fire Progression Reanalysis
# Organization: NRCAN, Northern Forestry Centre, Edmonton AB
# Author: Xue Yan Chan, xueyan.chan@canada.ca
# Supervised by: Piyush Jain and Dan Thompson
# Last updated: 2021-04-08

# ----------------------------------------------------

#' Summarize daily fire variables
#'
#' Summarize daily fire variables, such as hotspot counts, FRP, and FWI metrics on a daily timeframe based on estimated area burned.
#'
#' @param interim_stack Output of calling \code{IDW_interpolate_hotspots(interval = 'Daily', cumulative = F)}. A RasterBrick or RasterStack containing non-cumulative estimates of daily area burned, where 1 = burned during the daily interval and 0 = not burned during the daily interval.
#' @param NBAC sf polygon of the final NBAC fire perimeter. Likely the output of calling \code{simplify_NBAC()}.
#' @param VIIRS_hotspots sf points of VIIRS hotspots. The output of calling \code{VIIRS_hotspots_grab()}.
#' @param MODIS_hotspots sf points of MODIS hotspots. The output of calling \code{MODIS_hotspots_grab()}.
#' @param GOES_table GOES FRP table. The output of calling \code{GOES_FRP_table(interval = 'Hourly')}.
#' @param FWI_list List of FWI RasterBricks. The output of calling \code{FWI_ERA5_grab()}.
#' @param variable_stack Named RasterBrick or RasterStack of topography and landcover rasters. See details.
#' @param output_directory Optional. Output directory where a CSV should be saved.
#' @param filename_prefix Optional. String that will be appended to the start of the exported CSV. Ignored if \code{output_directory = NULL}.
#'
#' @details
#' In brief, this function summarizes hotspot and FWI data on a daily timeframe based on estimated area of burn (as determined by IDW interpolation of VIIRS hotspots).
#' Note that "daily" summaries refer to a 24 hr period from 9AM on a given day to 8:59AM on the subsequent day (a "day" does not begin at midnight).
#'
#' This function requires very specific inputs, which are derived from running other functions created for the Data Cubes for Canadian Wildfires project.
#' Only specific names of items in the variable_stack are recognized, including "Ecozone", "FBP_fuels_2014", "Landcover", "Elevation", "Aspect", "Slope", "Canopy_closure", "Stand_height", "Broadleaf_percentage", "Needleleaf_percentage", "Branch_biomass", "Foliage_biomass".
#' Using the provided functions to extract static rasters will ensure layers are named correctly.
#'
#' To see an example of this function in use, please refer to the example extraction workflow script.
#'
#' @return A tibble
#' @export
#'
#' @import sf
#' @import raster
#' @import tibble
#' @import dplyr
#' @import lubridate
#' @import lutz
#' @import circular

summarize_daily_fire_vars <- function(interim_stack,
                                NBAC = NULL,
                                VIIRS_hotspots = NULL,
                                MODIS_hotspots = NULL,
                                GOES_table = NULL,
                                FWI_list = NULL,
                                variable_stack = NULL,
                                output_directory = NULL,
                                filename_prefix = NULL){

  # start creating the table based off the interim stack epochs
  # get timezone based on raster location
  rast_centroid <- interim_stack %>% st_bbox() %>% st_as_sfc() %>% st_centroid()
  timezone <- tz_lookup(rast_centroid, method = 'accurate')

  interim_dttm <- getZ(interim_stack) %>% as_datetime() %>% with_tz(tzone = timezone)
  summary_ints <- lubridate::interval(start = (interim_dttm-86400), end = interim_dttm)

  # create the summary table
  summary_tbl <- tibble(Interval = summary_ints,
                        Interval_Start = int_start(summary_ints),
                        Interval_End = int_end(summary_ints),
                        Cumulative_hectares = as.numeric(NA),
                        Newburned_hectares = as.numeric(NA))

  # check to see if the IDW interim rasterstack is cumulative or not.
  area_per_lyr <- summary_tbl %>% dplyr::select(Interval) %>%
    add_column(Area = as.numeric(NA), Diff = as.numeric(NA))

  for(i in 1:nlayers(interim_stack)){
    interim_rast <- interim_stack[[i]]
    rast_res <- raster::res(interim_rast)[1]*raster::res(interim_rast)[2]
    rast_count <- interim_stack[[i]][interim_stack[[i]] == 1] %>% length()
    rast_hectares <- rast_count*rast_res %>%
      set_units(m^2) %>% set_units(ha) %>% as.numeric()
    area_per_lyr$Area[i] <- rast_hectares
  }
  area_per_lyr$Diff <- c(area_per_lyr$Area[1], diff(area_per_lyr$Area))

  # if negatives exist in the Diff column, assume the rasterstack is non-cumulative.
  # if only positives exist, assume the rasterstack is cumulative.
  if(TRUE %in%  (area_per_lyr$Diff < 0)){
    summary_tbl$Newburned_hectares <- area_per_lyr$Area
    summary_tbl$Cumulative_hectares <- cumsum(summary_tbl$Newburned_hectares)
  } else {
    summary_tbl$Cumulative_hectares <- area_per_lyr$Area
    summary_tbl$Newburned_hectares <- c(summary_tbl$Cumulative_hectares[1], diff(summary_tbl$Cumulative_hectares))
  }

  # add in nbac fire info if available
  # hm not decided about this one yet. is it useful?
  if(!is.null(NBAC)){
    nbac_varnames <- c("CFS_REF", "NAME", "YEAR", "NFIREID", "AGENCY", "SDATE", "EDATE", "NBAC_HA")
    nbac_vars <- NBAC %>% dplyr::select(any_of(nbac_varnames)) %>% st_drop_geometry()

    summary_tbl <- summary_tbl %>% mutate(nbac_vars, .after = Interval_End)
  }

  # if VIIRS hotspots are given, summarize them
  if(!is.null(VIIRS_hotspots)){
    summary_tbl <- summary_tbl %>% add_column(VIIRS_HotspotCount = as.integer(NA), # create new col for hotspot count
                                              VIIRS_SumFRP = as.numeric(NA)) # create new col for sum FRP)

    for(i in 1:nrow(summary_tbl)){
      int_hotspots <- VIIRS_hotspots %>% filter(ymd_hm(UTC_dttm) %within% summary_tbl$Interval[i])
      summary_tbl$VIIRS_HotspotCount[i] <- nrow(int_hotspots) # count number of hotspots
      summary_tbl$VIIRS_SumFRP[i] <- int_hotspots$frp %>% sum() # sum frp of hotspots
      rm(int_hotspots)
    }
  }

  if(!is.null(MODIS_hotspots)){
    # if MODIS hotspots are given, summarize them
    summary_tbl <- summary_tbl %>% add_column(MODIS_HotspotCount = as.integer(NA), # create new col for hotspot count
                                              MODIS_SumFRP = as.numeric(NA)) # create new col for sum FRP)

    for(i in 1:nrow(summary_tbl)){
      int_hotspots <- MODIS_hotspots %>% filter(ymd_hm(UTC_dttm) %within% summary_tbl$Interval[i])
      summary_tbl$MODIS_HotspotCount[i] <- nrow(int_hotspots) # count number of hotspots
      summary_tbl$MODIS_SumFRP[i] <- int_hotspots$frp %>% sum() # sum frp of hotspots
      rm(int_hotspots)
    }
  }

  if(!is.null(GOES_table)){
    # if GOES FRP table is given, summarize it
    summary_tbl <- summary_tbl %>% add_column(GOES16_SumFRP = as.numeric(NA),
                                              GOES17_SumFRP = as.numeric(NA))

    for(i in 1:nrow(summary_tbl)){
      int_frp <- GOES_table %>% filter(pull(GOES_table[2]) %within% interval(start = summary_tbl$Interval_Start[i], end = summary_tbl$Interval_End[i]-1))

      summary_tbl$GOES16_SumFRP[i] <- int_frp$GOES16_SumFRP %>% sum(na.rm = TRUE)
      summary_tbl$GOES17_SumFRP[i] <- int_frp$GOES17_SumFRP %>% sum(na.rm = TRUE)
    }
  }

  if(!is.null(FWI_list)){
    # get names of items in FWI list
    FWI_vars <- names(FWI_list)

    # create a new column for each item in the stack
    summary_tbl[FWI_vars] <- as.numeric(NA)

    # now summarize each layer based on the interim raster

    # make a function to get mode
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    # extract for each brick for each day.
    # currently this is a nested loop.
    # WIP idea to make this more efficient: maybe polygonize and create mask for all days first, then run a single loop

    for(i in seq_along(FWI_vars)){
      var_brick <- FWI_list[[i]]

      for(j in 1:nlayers(interim_stack)){
        interim_rast <- interim_stack[[j]]

        # get the corresponding brick layer based on date
        juliandate <- getZ(interim_rast) %>% as_datetime(tz = timezone) %>% yday()
        brick_lyr <- var_brick[[which(var_brick@z %>% unlist() %>% as.vector() == juliandate)]]

        # get median of cells that intersect with burning areas
        # this uses the center of raster cells of the interim rast only (to determine intersection)
        interim_points <- rasterToPoints(interim_rast, fun=function(x){x==1}, spatial = TRUE) # points
        brick_extract <- raster::extract(brick_lyr, interim_points)

        if(is.null(brick_extract)){
          summary_tbl[[FWI_vars[i]]][j] <- NA
        } else {
          summary_tbl[[FWI_vars[i]]][j] <- stats::median(brick_extract, na.rm = T) # assign value to summary tibble
        }

      }
    }
  }

  if(!is.null(variable_stack)){
    # if variable stack is given, summarize it
    # get names of items in rasterstack variables
    varnames <- names(variable_stack)

    # create a new column for each item in the stack
    summary_tbl[varnames] <- NA

    # now summarize each layer based on the interim raster
    # for now, lets just make it so that I assume I know the names of the variables already.

    # make a function to get mode
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    # get mode FBP fuel type for each interim, based on the interim rasterstack
    if('FBP_fuels_2014' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        fbp_fuel_mode <- variable_stack$FBP_fuels_2014[interim_rast == 1] %>% getmode()

        # get human readable fuel type
        # this lookup table applies ONLY to the 2014 FBP fuels grid (diff numbers in 2019 grid)
        fbpfuel_lookup <- tibble(Value = c(101,102,103,104,105,106,107,108,108,109,110,111,112,113,114,115,116,117, 118,119,120,121,122),
               FBP_fuel = c("C-1", "C-2" ,"C-3", "C-4" ,"C-5" ,"C-6" ,"C-7" ,"D-1", "D-2" ,"M-1","M-2", "M-3", "M-4" ,"S-1", "S-2", "S-3", "O-1a", "O-1b","Water","Non-fuel","Wetland","Urban","Vegetated non-fuel"))

        if(is.null(fbp_fuel_mode)){
          summary_tbl$FBP_fuels_2014[i] <- NA
        } else {
          summary_tbl$FBP_fuels_2014[i] <- fbpfuel_lookup[fbp_fuel_mode %>% match(fbpfuel_lookup$Value),] %>% pull(FBP_fuel)
        }

      }
    }


    # get mode ecozone for each interim
    if('Ecozone' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        ecozone_mode <- variable_stack$Ecozone[interim_rast == 1] %>% getmode()

        # get human readable ecozone
        ecozone_lookup <- tibble(Value = 1:15, Ecozone = c('Artic Cordillera', 'Northern Arctic', 'Southern Arctic','Taiga Plains',
                                                           'Taiga Shield', 'Boreal Shield','Atlantic Maritime','Mixedwood Plains',
                                                           'Boreal Plains','Prairies','Taiga Cordillera','Boreal Cordillera','Pacific Maritime',
                                                           'Montane Cordillera','Hudson Plains'))
        if(is.null(ecozone_mode)){
          summary_tbl$Ecozone[i] <- NA
        } else {
          summary_tbl$Ecozone[i] <- ecozone_lookup[ecozone_mode %>% match(ecozone_lookup$Value),] %>% pull(Ecozone)
        }

      }
    }

    # get mode landcover for each interim
    if('Landcover' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        landcover_mode <- variable_stack$Landcover[interim_rast == 1] %>% getmode()

        landcover_lookup <- tibble(Value = c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19),
                                   Landcover = c('Temperate or sub-polar needleleaf forest','Sub-polar taiga needleleaf forest',
                                                 'Temperate or sub-polar broadleaf deciduous forest','Mixed forest',
                                                 'Temperate or sub-polar shrubland','Temperate or sub-polar grassland',
                                                 'Sub-polar or polar shrubland-lichen-moss','Sub-polar or polar grassland-lichen-moss',
                                                 'Sub-polar or polar barren-lichen-moss','Wetland','Cropland','Barren lands','Urban',
                                                 'Water','Snow and Ice'))
        if(is.null(landcover_mode)){
          summary_tbl$Landcover[i] <- NA
        } else {
          summary_tbl$Landcover[i] <- landcover_lookup[landcover_mode %>% match(landcover_lookup$Value),] %>% pull(Landcover)

        }
      }
    }

    # get median elevation for each interim
    if('Elevation' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        elevation_median <- variable_stack$Elevation[interim_rast == 1] %>% stats::median(na.rm = TRUE)

        if(is.null(elevation_median)){
          summary_tbl$Elevation[i] <- NA
        } else {
          summary_tbl$Elevation[i] <- elevation_median
        }

      }
    }

    # get slope
    if('Slope' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        slope_median <- variable_stack$Slope[interim_rast == 1] %>% stats::median(na.rm = TRUE)

        if(is.null(slope_median)){
          summary_tbl$Slope[i] <- NA
        } else {
          summary_tbl$Slope[i] <- slope_median
        }

      }
    }
    # get mean aspect (uses circular stats)
    if('Aspect' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]

        asp_vals <- variable_stack$Aspect[interim_rast == 1]

        if(is.null(asp_vals)){
          summary_tbl$Aspect[i] <- NA
        } else {
          mean_asp <- circular(asp_vals, units = 'degrees', rotation = 'clock') %>%
            mean.circular() # get mean circular angle
          summary_tbl$Aspect[i] <- ifelse(mean_asp>=0, mean_asp, mean_asp + 360) # if negative, add 360
        }

      }
    }

    # get median canopy closure
    if('Canopy_closure' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        canopy_median <- variable_stack$Canopy_closure[interim_rast == 1] %>% stats::median(na.rm = TRUE)

        if(is.null(canopy_median)){
          summary_tbl$Canopy_closure[i] <- NA
        } else {
          summary_tbl$Canopy_closure[i] <- canopy_median
        }

      }
    }

    # get median stand height
    if('Stand_height' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        stand_median <- variable_stack$Stand_height[interim_rast == 1] %>% stats::median(na.rm = TRUE)

        if(is.null(stand_median)){
          summary_tbl$Stand_height[i] <- NA
        } else {
          summary_tbl$Stand_height[i] <- stand_median
        }

      }
    }

    # get median broadleaf percentage
    if('Broadleaf_percentage' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        broadleaf_median <- variable_stack$Broadleaf_percentage[interim_rast == 1] %>% stats::median(na.rm = TRUE)

        if(is.null(broadleaf_median)){
          summary_tbl$Broadleaf_percentage[i] <- NA
        } else {
          summary_tbl$Broadleaf_percentage[i] <- broadleaf_median
        }

      }
    }

    # get median needleleaf percentage
    if('Needleleaf_percentage' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        needle_median <- variable_stack$Needleleaf_percentage[interim_rast == 1] %>% stats::median(na.rm = TRUE)

        if(is.null(needle_median)){
          summary_tbl$Needleleaf_percentage[i] <- NA
        } else {
          summary_tbl$Needleleaf_percentage[i] <- needle_median
        }

      }
    }

    # get sum of branch biomass
    if('Branch_biomass' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        branch_vals <- variable_stack$Branch_biomass[interim_rast == 1]

        if(is.null(branch_vals)){
          summary_tbl$Branch_biomass[i] <- NA
        } else {
          summary_tbl$Branch_biomass[i] <- branch_vals %>% sum(na.rm = TRUE)
        }

      }
    }

    # get sum of foliage biomass
    if('Foliage_biomass' %in% varnames){
      for(i in 1:nrow(summary_tbl)){
        interim_rast <- interim_stack[[i]]
        foliage_vals <- variable_stack$Foliage_biomass[interim_rast == 1]

        if(is.null(foliage_vals)){
          summary_tbl$Foliage_biomass[i] <- NA
        } else {
          summary_tbl$Foliage_biomass[i] <- foliage_vals %>% sum(na.rm = TRUE)
        }
      }
    }
  }

  if(!is.null(output_directory)){
    if(!is.null(filename_prefix)){
      filename <- file.path(output_directory,paste0(filename_prefix, '_Summary_table_24hr.csv'))
    } else {
      filename <- file.path(output_directory,'Summary_table_24hr.csv')
    }

    utils::write.csv(summary_tbl, file = filename, row.names = F)
    message(paste('24hr Summary table saved to',output_directory))
  }

  return(summary_tbl)
}

# ----------------------------------------------------

#' Summarize hourly fire variables
#'
#' Summarize hourly fire variables, such as hotspot counts, FRP, and weather on an hourly timeframe based on estimated area burned.
#'
#' @param interim_stack Output of calling \code{IDW_interpolate_hotspots(interval = 'Daily', cumulative = F)}. A RasterBrick or RasterStack containing non-cumulative estimates of daily area burned, where 1 = burned during the daily interval and 0 = not burned during the daily interval.
#' @param NBAC sf polygon of the final NBAC fire perimeter. Likely the output of calling \code{simplify_NBAC()}.
#' @param VIIRS_hotspots sf points of VIIRS hotspots. The output of calling \code{VIIRS_hotspots_grab()}.
#' @param MODIS_hotspots sf points of MODIS hotspots. The output of calling \code{MODIS_hotspots_grab()}.
#' @param GOES_table GOES FRP table. The output of calling \code{GOES_FRP_table(interval = 'Hourly')}.
#' @param ERA5land_weather_list List of ERA5 Land hourly RasterBricks. The output of calling \code{ERA5Land_GEE_grab()}.
#' @param output_directory Optional. Output directory where a CSV should be saved.
#' @param filename_prefix Optional. String that will be appended to the start of the exported CSV. Ignored if \code{output_directory = NULL}.
#'
#' @details
#' This function requires very specific inputs, which are derived from running other functions created for the Data Cubes for Canadian Wildfires project.
#' In brief, this function summarizes hotspot and weather data on an hourly timeframe based on estimated area of burn (as determined by IDW interpolation of VIIRS hotspots).
#' Since there is no hourly estimate of area burned, all estimated burn pixels for the entire day that the hourly interval falls within are used for summarizing.
#'
#' To see an example of this function in use, please refer to the example extraction workflow script.
#'
#' @return A tibble
#' @export
#'
#' @import sf
#' @import raster
#' @import tibble
#' @import dplyr
#' @import lubridate
#' @import lutz

summarize_hourly_fire_vars <- function(interim_stack,
                                       NBAC = NULL,
                                       VIIRS_hotspots = NULL,
                                       MODIS_hotspots = NULL,
                                       GOES_table = NULL,
                                       ERA5land_weather_list = NULL,
                                       output_directory = NULL,
                                       filename_prefix = NULL){

  # start creating the table based off the interim stack epochs
  # get timezone based on raster location
  rast_centroid <- interim_stack %>% st_bbox() %>% st_as_sfc() %>% st_centroid()
  timezone <- tz_lookup(rast_centroid, method = 'accurate')

  full_interval <- interval(start = (getZ(interim_stack) %>% as_datetime() %>% with_tz(tzone = timezone) %>% min()),
                            end = getZ(interim_stack) %>% as_datetime() %>% with_tz(tzone = timezone) %>% max())
  summary_ints <- split_1hr(full_interval)

  # create the summary table
  summary_tbl <- tibble(Interval = summary_ints,
                        Interval_Start = int_start(summary_ints),
                        Interval_End = int_end(summary_ints))

  # add in nbac fire info if available
  # hm not decided about this one yet. is it useful?
  if(!is.null(NBAC)){
    nbac_varnames <- c("CFS_REF", "NAME", "YEAR", "NFIREID", "AGENCY", "SDATE", "EDATE", "NBAC_HA")
    nbac_vars <- NBAC %>% dplyr::select(any_of(nbac_varnames)) %>% st_drop_geometry()

    summary_tbl <- summary_tbl %>% mutate(nbac_vars, .after = Interval_End)
  }

  # if VIIRS hotspots are given, summarize them
  if(!is.null(VIIRS_hotspots)){
    summary_tbl <- summary_tbl %>% add_column(VIIRS_HotspotCount = as.integer(NA), # create new col for hotspot count
                                              VIIRS_SumFRP = as.numeric(NA)) # create new col for sum FRP)

    for(i in 1:nrow(summary_tbl)){
      int_hotspots <- VIIRS_hotspots %>% filter(ymd_hm(UTC_dttm) %within% summary_tbl$Interval[i])
      summary_tbl$VIIRS_HotspotCount[i] <- nrow(int_hotspots) # count number of hotspots
      summary_tbl$VIIRS_SumFRP[i] <- int_hotspots$frp %>% sum() # sum frp of hotspots
      rm(int_hotspots)
    }
  }

  if(!is.null(MODIS_hotspots)){
    # if MODIS hotspots are given, summarize them
    summary_tbl <- summary_tbl %>% add_column(MODIS_HotspotCount = as.integer(NA), # create new col for hotspot count
                                              MODIS_SumFRP = as.numeric(NA)) # create new col for sum FRP)

    for(i in 1:nrow(summary_tbl)){
      int_hotspots <- MODIS_hotspots %>% filter(ymd_hm(UTC_dttm) %within% summary_tbl$Interval[i])
      summary_tbl$MODIS_HotspotCount[i] <- nrow(int_hotspots) # count number of hotspots
      summary_tbl$MODIS_SumFRP[i] <- int_hotspots$frp %>% sum() # sum frp of hotspots
      rm(int_hotspots)
    }
  }

  if(!is.null(GOES_table)){
    # if GOES FRP table is given, summarize it
    summary_tbl <- summary_tbl %>% add_column(GOES16_SumFRP = as.numeric(NA),
                                              GOES17_SumFRP = as.numeric(NA))

    for(i in 1:nrow(summary_tbl)){
      int_frp <- GOES_table %>% filter(pull(GOES_table[2]) %within% interval(start = summary_tbl$Interval_Start[i], end = summary_tbl$Interval_End[i]-1))

      summary_tbl$GOES16_SumFRP[i] <- int_frp$GOES16_SumFRP %>% sum(na.rm = TRUE)
      summary_tbl$GOES17_SumFRP[i] <- int_frp$GOES17_SumFRP %>% sum(na.rm = TRUE)
    }
  }

  if(!is.null(ERA5land_weather_list)){ # if ERA5land weather list is given, summarize it
    # get names of items in the weather list
    ERA5vars <- names(ERA5land_weather_list)

    # create a new column for each item in the stack
    summary_tbl[ERA5vars] <- as.numeric(NA)

    # now summarize each layer based on the interim raster
    # this will be the median weather value of cells that intersect with areas burned on the same day
    # weather values correspond with the start interval time
    # currently this is a nested loop.
    # WIP idea to make this more efficient: maybe polygonize and create mask for all days first
    for(i in seq_along(ERA5land_weather_list)){
      var_brick <- ERA5land_weather_list[[i]]

      for(j in 1:nrow(summary_tbl)){
        # get the corresponding layer based on time
        dttm <- summary_tbl$Interval_Start[j] %>% as.numeric()
        brick_lyr <- var_brick[[which(var_brick@z %>% unlist() %>% as.vector() == dttm)]]

        # get correct interim rast
        # first create an interval for each item in the interim stack
        interim_dttm <- getZ(interim_stack) %>% as_datetime() %>% with_tz(tzone = timezone)
        interim_ints <- lubridate::interval(start = (interim_dttm-86400+1), end = interim_dttm)

        # evaluate which interim rast interval the brick_lyr dttm falls into
        interim_rast <- interim_stack[[which(as_datetime(getZ(brick_lyr), tz = timezone) %within% interim_ints)]]

        # get median of cells that intersect with burning areas
        # this uses the center of raster cells of the interim rast only (to determine intersection)
        interim_points <- rasterToPoints(interim_rast, fun=function(x){x==1}, spatial = TRUE) # points
        brick_extract <- raster::extract(brick_lyr, interim_points)

        if(is.null(brick_extract)){
          summary_tbl[[ERA5vars[i]]][j] <- NA
        } else {
          summary_tbl[[ERA5vars[i]]][j] <- stats::median(brick_extract, na.rm = TRUE) # assign value to summary tibble
        }

      }

    }


  }
  # if output directory provided, export table
  if(!is.null(output_directory)){
    if(!is.null(filename_prefix)){
      filename <- file.path(output_directory,paste0(filename_prefix, '_Summary_table_hourly.csv'))
    } else {
      filename <- file.path(output_directory,'Summary_table_hourly.csv')
    }

    utils::write.csv(summary_tbl, file = filename, row.names = F)
    message(paste('Hourly summary table saved to',output_directory))
  }
  return(summary_tbl)
}
