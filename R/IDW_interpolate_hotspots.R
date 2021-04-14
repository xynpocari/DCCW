#' Hotspot IDW interpolation
#'
#' Estimate daily area of burn using IDW interpolation of hotspots epoch.
#'
#' @param hotspots sf points representing VIIRS or MODIS hotspots. The output of calling \code{VIIRS_hotspots_grab()} or \code{MODIS_hotspots_grab}. VIIRS is recommended due to higher spatial resolution.
#' @param fire_perimeter sf polygon representing the final fire perimeter. The interpolated hotspots are masked to this polygon. Likely the output of \code{simplify_NBAC()}.
#' @param reference_grid Reference grid to interpolate hotspot values onto. E.g. the output from calling \code{fbp_fuel_grab()}.
#' @param interval What interval of time should each resulting layer represent? Choices are \code{'Daily'} or \code{'Hourly'}. At this time, only \code{'Daily'} is recommended, due to low confidence of hourly estimates.
#' @param format Format of returned object. Choices are \code{'Raster'}, which returns a RasterBrick, or \code{'Polygon'}, which returns a simple feature. Defaults to \code{'Raster'}.
#' @param cumulative Logical. Should layers represent cumulative area burned? Defaults to \code{TRUE}.
#' @param output_directory Optional. Output directory where a NetCDF will be saved. Currently export of polygon format not supported.
#' @param filename_prefix Optional. String that will be appended to the start of the exported raster file name. Ignored if \code{output_directory = NULL}.
#'
#' @details
#' This function uses IDW interpolation on the epoch field of hotspot points in order to estimate daily area burned.
#'
#' If the output format is \code{'Raster'}, then the resulting RasterBrick will consist of layers that have an associated z value representing the end of the daily burn interval.
#' The raster layers will consist of 1s (burned pixels) and 0s (unburned pixels). The cumulative argument defines whether the rasters depict cumulative area burned.
#'
#' Note that 'Daily' refers to a 24 hr period from 9AM on a given day to 8:59AM on the subsequent day (a "day" does not start at midnight).
#'
#' This function requires very specific inputs, which are derived from running other functions created for the Fire Progression Reanalysis project.
#'
#' @note
#' This function will throw errors the first time it has run, but the task has actually succeeded.
#'
#' @return RasterBrick if \code{format = 'Raster'} or an sf polygon is \code{format = 'Polygon'}
#' @export
#'
#' @import raster
#' @import sf
#' @import lubridate
#' @import lutz
#' @import units
#' @import igraph
#' @import dplyr
#' @import tibble
#' @import gstat
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' library(lubridate)
#' library(dplyr)
#' library(tibble)
#' library(lutz)
#' library(gstat)
#' library(units)
#'
#' # download 2019 NBAC to tempfile
#' nbac_temp <- tempfile(fileext = '.zip')
#' download.file(destfile = nbac_temp,
#'               url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_2019_r9_20200703.zip')
#'
#' nbac_files <- unzip(nbac_temp, list = T)
#' nbac <- unzip(zipfile = nbac_temp, files = nbac_files$Name[1:11], exdir = gsub('.zip','',nbac_temp))
#' nbac <- st_read(dsn = nbac[7]) # import nbac to R as sf polygons
#'
#' # search for the 2019 McMillan wildfire, CFS ID SWF-049-2019
#' fire <- searchNBAC_by_alias(nbac_file = nbac, alias = 'SWF-049-2019')
#'
#' # simplify NBAC to make it easier to work with
#' fire <- simplify_NBAC(fire)
#' plot(fire[,1]) # view the McMillan fire
#'
#' # grab VIIRS hotspots within the fire perimeter + 2 km buffer
#' viirs <- VIIRS_hotspots_grab(reference_poly = fire,
#'                              start_date = '2019-05-18',
#'                              end_date = '2019-06-10',
#'                              buff_width = 2000)
#' plot(viirs[,'acq_date']) # view hotspots
#'
#' # grab fbp fuels grid based on fire perimeter + 2 km buffer
#' fbp_fuel <- fbp_fuel_grab(reference_poly = fire,
#'                           match_crs = TRUE,
#'                           buff_width = 2000,
#'                           fbp_grid = NULL)
#' plot(fbp_fuel) # view fuels grid, which will be our reference grid
#'
#' # interpolate hotspots to fuels grid
#' # NOTE: the first time this is run, errors may occur, but the task actually was successful.
#' IDW <- IDW_interpolate_hotspots(hotspots = viirs,
#'                                fire_perimeter = fire,
#'                                reference_grid = fbp_fuel,
#'                                interval = 'Daily',
#'                                format = 'Raster',
#'                                cumulative = FALSE,
#'                                output_directory = NULL,
#'                                filename_prefix = NULL)
#'
#'plot(IDW[[1:4]]) # non-cumulative estimated daily area of burn
#' }
IDW_interpolate_hotspots <- function(hotspots,
                                     fire_perimeter,
                                     reference_grid,
                                     interval = 'Daily',
                                     format = 'Raster',
                                     cumulative = TRUE,
                                     output_directory = NULL,
                                     filename_prefix = NULL){

  # determine the local timezone based on fire centroid
  fire_centroid <- fire_perimeter %>% st_geometry() %>% st_centroid()
  timezone <- tz_lookup(fire_centroid, method = "accurate")

  # convert hotspot UTC datetimes to epoch
  hotspots$UTC_dttm <- hotspots$UTC_dttm %>% ymd_hm()
  hotspots$Epoch <- hotspots$UTC_dttm %>% as.integer()

  reference_grid <- reference_grid %>% clearValues() %>% as('SpatialGrid')

  # interpolate to reference grid
  IDW_grid <- gstat::idw(formula = Epoch ~ 1,
                         locations = as(hotspots, 'Spatial'), # sf must be coerced to sp
                         reference_grid,
                         nmax = 5,
                         nmin = 5)# only consider 5 nearest points, done by Parks 2014.

  IDW_raster <- as(IDW_grid, 'RasterLayer')

  # add in local dttm information to hotspots
  Local_dttm <- as_datetime(hotspots$UTC_dttm, tz = timezone)
  Local_dttm_name <- paste0(base::format(Local_dttm[1], format = '%Z'),'_dttm')
  hotspots <- hotspots %>% add_column(!!Local_dttm_name := Local_dttm,
                                      .after = "UTC_dttm")

  # get first and last local dttm for hotspots
  start_int <- hotspots %>% st_drop_geometry() %>% pull(!!Local_dttm_name) %>% min()
  end_int <- hotspots %>% st_drop_geometry() %>% pull(!!Local_dttm_name) %>% max()

  # get the previous 9am time and following 9am time. then parse 24hr intervals
  if(interval == 'Daily'){
    if(hour(start_int) >= 9){
      start_int <- round_date(start_int, unit = 'hour')
      hour(start_int) <- 9
    } else {
      start_int <- round_date(start_int, unit = 'hour')
      hour(start_int) <- 9
      start_int <- start_int - 86400
    }

    # if(hour(end_int) >= 9){
    #   end_int <- round_date(end_int, unit = 'hour')
    #   hour(end_int) <- 9
    #   end_int <- end_int + 86400
    # } else {
      end_int <- round_date(end_int, unit = 'hour')
      hour(end_int) <- 9
    # }
    # parse intervals
    fire_int <- lubridate::interval(start = start_int, end = end_int)
    fire_int_split <- split_24hrs(fire_int)

  }

  # if hourly, simply round time up or down to nearest hour. then parse hourly intervals
  if(interval == 'Hourly'){
    start_int <- start_int %>% floor_date(unit = 'hour')
    end_int <- end_int %>% ceiling_date(unit = 'hour')
    # parse intervals
    fire_int <- lubridate::interval(start = start_int, end = end_int)
    fire_int_split <- split_1hr(fire_int)
  }

  # create reclassification matrix
  from <- int_start(fire_int_split)
  to <- int_end(fire_int_split)
  becomes <- int_end(fire_int_split)
  rcl <- matrix(c(from,to,becomes), ncol = 3)

  # reclassify
  IDW_reclass <- reclassify(IDW_raster, rcl = rcl)

  # create stack
  IDW_stack <- stack(replicate(nrow(rcl), IDW_reclass))

  # reclassify into binary layers for each hour
  for(i in 1:nrow(rcl)){
    upper <- rcl[i,3]
    IDW_temp <- raster::clamp(IDW_stack[[i]], upper = upper, useValues = FALSE)
    IDW_temp[IDW_temp > 0] <- 1
    IDW_temp[is.na(IDW_temp[])] <- 0

    IDW_stack[[i]] <- IDW_temp
    rm(upper, IDW_temp)
  }

  # mask using the fire perimeter
  # create a mask that keeps all cells that intersect with grid (as opposed to only cells where centroid is covered)
  mask <- raster::rasterize(fire_perimeter, IDW_reclass, getCover = TRUE)
  mask[mask == 0] <- NA
  IDW_stack <- mask(IDW_stack, mask = mask, updatevalue = 0)

  # drop clumps of less than 16 cells (16 cells is 100 ha)
  for(i in 1:nlayers(IDW_stack)){
    rast_tmp <- IDW_stack[[i]]
    rast_tmp <- clump(rast_tmp, directions = 8)
    f <- freq(rast_tmp) %>% as.data.frame()
    excludeID <- f$value[which(f$count < 16)]

    rast_tmp[rast_tmp %in% excludeID] <- NA
    rast_tmp[!is.na(rast_tmp)] <- 1
    rast_tmp[is.na(rast_tmp)] <- 0

    # reassign to IDW_stack
    IDW_stack[[i]] <- rast_tmp
    rm(rast_tmp)
  }

  # set z variable (time)
  IDW_stack <- setZ(IDW_stack, rcl[,3], name = 'time (seconds since 1970-01-01 00:00:00)')

  # set layer names
  names(IDW_stack) <- as_datetime(rcl[,3], tz = timezone)

  if(cumulative == TRUE){
    # remove any empty layers
    empty_lyrs <- vector(length = nlayers(IDW_stack))

    for(i in 1:nlayers(IDW_stack)){
      uniquelength <- values(IDW_stack[[i]]) %>% unique() %>% length()
      empty_lyrs[[i]] <- ifelse(uniquelength ==1, TRUE, FALSE) # evaluate if layer is empty
    }
    if(empty_lyrs[1] == TRUE){IDW_stack <- IDW_stack[[-1]]}

    # if(length((unique(empty_lyrs)))>1){
    #   IDW_stack <- IDW_stack[[-which(empty_lyrs==TRUE)]] # remove empty layers from stack
    # }


  } else{ # subtract each raster using the previous raster
    IDW_stack_noncumulative <- IDW_stack
    for(i in 1:nlayers(IDW_stack)){
      if(i == 1){
        IDW_stack_noncumulative[[i]] <- IDW_stack[[i]]
      } else {
        IDW_lyr <- IDW_stack[[i]]
        IDW_lyr_tmp <- IDW_lyr - IDW_stack[[i-1]]
        IDW_stack_noncumulative[[i]] <- IDW_lyr_tmp
      }
    }
    names(IDW_stack_noncumulative) <- as_datetime(rcl[,3], tz = timezone)
    IDW_stack <- IDW_stack_noncumulative

    # remove the first layer if it is empty
    empty_lyrs <- vector(length = nlayers(IDW_stack))

    for(i in 1:nlayers(IDW_stack)){
      uniquelength <- values(IDW_stack[[i]]) %>% unique() %>% length()
      empty_lyrs[[i]] <- ifelse(uniquelength ==1, TRUE, FALSE) # evaluate if layer is empty
    }

    if(empty_lyrs[1] == TRUE){IDW_stack <- IDW_stack[[-1]]}

    # if(length((unique(empty_lyrs)))>1){
    #   IDW_stack <- IDW_stack[[-which(empty_lyrs==TRUE)]] # remove empty layers from stack
    # }
  }

  if(format == 'Raster'){
    if(!is.null(output_directory)){
      varname <- paste0('Estimated_',ifelse(cumulative == TRUE, 'cumulative','noncumulative'), '_area_burned')
      start_date <- as_datetime(rcl[1,3], tz = timezone) %>% date()
      end_date <- as_datetime(rcl[nrow(rcl),3], tz = timezone) %>% date()

      if(!is.null(filename_prefix)){
        filename <- file.path(output_directory, paste0(filename_prefix,'_IDW_interpolated_area_burned_',gsub('-','',start_date),'_',gsub('-','',end_date),'.nc'))
      } else {
        filename <- file.path(output_directory, paste0('IDW_interpolated_area_burned_',gsub('-','',start_date),'_',gsub('-','',end_date),'.nc'))
      }

      writeRaster(x = IDW_stack,
                  filename = filename,
                  varname = 'Estimated_area_burned',
                  longname = 'Binary_area_burned_estimated_from_IDW_interpolation_of_hotspots',
                  zname = 'Time',
                  zunit = 'seconds since 1970-01-01 00:00:00',
                  overwrite = T)
      message(paste0('Saving NetCDF locally as ',
                     file.path(output_directory, paste0('IDW_interpolated_area_burned_',gsub('-','',start_date),'_',gsub('-','',end_date),'.nc'))))
    }

    return(IDW_stack)
    }
  if(format == 'Polygon'){
    # polygonize the raster layers created, to keep everything consistent.
    IDW_poly <- vector(mode = 'list', length = nlayers(IDW_stack))
    for(i in 1:nlayers(IDW_stack)){
      lyr_tmp <- IDW_stack[[i]]
      lyr_tmp[lyr_tmp == 0] <- NA
      poly <- as(lyr_tmp, 'SpatialPolygonsDataFrame') %>% st_as_sf() %>%
        group_by(1) %>% summarise() %>% add_column(Epoch = getZ(lyr_tmp), .before = 1) %>%
        select(-2)
      IDW_poly[[i]] <- poly
    }

    # bind the resulting list to a single sf object
    IDW_poly <- do.call(rbind, IDW_poly)

    # add in datetime and hectares
    UTC_dttm <- as_datetime(IDW_poly$Epoch, tz = 'UTC')
    Local_dttm <- as_datetime(IDW_poly$Epoch, tz = timezone)

    IDW_poly <- IDW_poly %>%
      add_column(UTC_dttm = UTC_dttm,
                 !!Local_dttm_name := Local_dttm,
                 Hectares = st_area(IDW_poly) %>% set_units(ha) %>% as.numeric(),
                 .after = 1)

    if(!is.null(output_directory)){
      message('Direct export of polygon format to local file is not supported at this time.')
    }

    return(IDW_poly)
  }

}

# ------------------------------------------------------------

#' Convert IDW brick to layer
#'
#' Turn IDW interpolated estimated area of burn RasterBricks into a single RasterLayer.
#'
#' @param IDW_brick RasterBrick, output from calling \code{IDW_interpolate_hotspots(format = 'Raster')}.
#'
#' @details
#' This function turns the multilayer RasterBrick from \code{IDW_interpolate_hotspots()} into a single RasterLayer.
#' Whereas the raster output from \code{IDW_interpolate_hotspots()} stores epoch information as the Z variable and represents area burned using 0s and 1s,
#' the output of \code{IDW_StackToLayer()} creates a single RasterLayer where the value of each cell is the estimated epoch of when the cell burned.
#'
#' @return RasterLayer
#' @export
#'
#' @import raster
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' library(lubridate)
#' library(dplyr)
#' library(tibble)
#' library(lutz)
#' library(gstat)
#' library(units)
#'
#' # download 2019 NBAC to tempfile
#' nbac_temp <- tempfile(fileext = '.zip')
#' download.file(destfile = nbac_temp,
#'               url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_2019_r9_20200703.zip')
#'
#' nbac_files <- unzip(nbac_temp, list = T)
#' nbac <- unzip(zipfile = nbac_temp, files = nbac_files$Name[1:11], exdir = gsub('.zip','',nbac_temp))
#' nbac <- st_read(dsn = nbac[7]) # import nbac to R as sf polygons
#'
#' # search for the 2019 McMillan wildfire, CFS ID SWF-049-2019
#' fire <- searchNBAC_by_alias(nbac_file = nbac, alias = 'SWF-049-2019')
#'
#' # simplify NBAC to make it easier to work with
#' fire <- simplify_NBAC(fire)
#' plot(fire[,1]) # view the McMillan fire
#'
#' # grab VIIRS hotspots within the fire perimeter + 2 km buffer
#' viirs <- VIIRS_hotspots_grab(reference_poly = fire,
#'                              start_date = '2019-05-18',
#'                              end_date = '2019-06-10',
#'                              buff_width = 2000)
#' plot(viirs[,'acq_date']) # view hotspots
#'
#' # grab fbp fuels grid based on fire perimeter + 2 km buffer
#' fbp_fuel <- fbp_fuel_grab(reference_poly = fire,
#'                           match_crs = TRUE,
#'                           buff_width = 2000,
#'                           fbp_grid = NULL)
#' plot(fbp_fuel) # view fuels grid, which will be our reference grid
#'
#' # interpolate hotspots to fuels grid
#' # NOTE: the first time this is run, errors may occur, but the task actually was successful.
#' IDW <- IDW_interpolate_hotspots(hotspots = viirs,
#'                                fire_perimeter = fire,
#'                                reference_grid = fbp_fuel,
#'                                interval = 'Daily',
#'                                format = 'Raster',
#'                                cumulative = FALSE,
#'                                output_directory = NULL,
#'                                filename_prefix = NULL)
#'
#' plot(IDW[[1:4]]) # non-cumulative estimated daily area of burn
#'
#' # now convert the RasterBrick to a single RasterLayer,
#' # where pixel values represented estimated time of burn.
#' IDW_single_layer <- IDW_BrickToLayer(IDW_brick =  IDW)
#'
#' # pixel values represent estimated area burned (in UNIX epoch, seconds since 1970-01-01 00:00:00)
#' plot(IDW_single_layer)
#'
#' }
IDW_BrickToLayer <- function(IDW_brick){ # output from IDW_interpolate_hotspots(format = 'Raster')
  IDW_brick_tmp <- IDW_brick

  for(i in 1:nlayers(IDW_brick)){
    layer_tmp <- IDW_brick[[i]]
    layer_tmp[layer_tmp == 1] <- getZ(layer_tmp)
    layer_tmp[layer_tmp == 0] <- NA
    IDW_brick_tmp[[i]] <- layer_tmp
  }

  IDW_brick_tmp <- stackApply(IDW_brick_tmp, indices = 1, fun = min)
  names(IDW_brick_tmp) <- 'IDW_Interpolation'

  return(IDW_brick_tmp)
}

