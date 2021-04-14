#' Grab FWI metrics
#'
#' Extract daily rasters for Fire Weather Indices calculated using ERA5 Reanalysis data to a user-defined date range and spatial extent.
#'
#' @param variable A string or vector of FWI metrics. E.g. \code{c('FFMC','DMC','DC','ISI','BUI','FWI')}.
#' @param overwinter_DC Logical. Should FWI metrics using overwintered DC be extracted? Default is \code{TRUE}. If \code{FALSE}, FWI metrics using default DC startup is extracted (see details).
#' @param start_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01".
#' @param end_date Date object or character. Start date of data extraction, using UTC. E.g."2018-05-01". Must have the same year as start_date.
#' @param extent An sp or sf object used to define the extraction extent.
#' @param buff_width Numeric. Width (in meters) to buffer the object defined in extent, thereby increasing the extraction extent. Defaults to 0.
#' @param match_crs Logical. Should the extracted rasters be projected to the CRS of the reference extent object? Defaults to \code{FALSE}. If \code{FALSE}, the raster is returned in the native projection and resolution (WGS 84, 0.25 arc degrees).
#' @param input_directory Directory to where FWI NetCDFs are saved on the local device. If \code{NULL}, FWI NetCDFs will be downloaded to tempfiles. See details for recommendations.
#' @param output_directory Optional. Output directory where NetCDFs will be exported. If \code{NULL}, the NetCDFs will not be exported to the local device.
#' @param filename_prefix Optional. String that will be appended to the start of the exported raster file name.
#'
#' @details
#' This function extracts daily FWI rasters (from \url{https://zenodo.org/record/3626193}) to a user-defined date range and spatial extent.
#' Currently, this function can only temporally subset to dates within the same year.
#'
#' It is recommended to download NetCDFs for the variables and years of interest from \url{https://zenodo.org/record/3626193}, then define the \code{input_directory} to reduce download times.
#' If the \code{input_directory} is defined, this function will search for the proper NetCDF using both year and variable name within the \code{input_directory.} The user should not change the default name of files downloaded from the Zenodo page.
#'
#' @return List of named RasterBricks
#' @export
#'
#' @references Global Fire Weather Indices: \url{https://zenodo.org/record/3626193}
#'
#' @import raster
#' @import sf
#' @import lubridate
#' @import dplyr
#' @import ncdf4
#' @import curl
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#' library(lubridate)
#' library(dplyr)
#' library(ncdf4)
#' library(curl)
#'
#' # Create extent polygon
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>%
#'      st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' FWI <- FWI_ERA5_grab(variable = 'FWI',# string or vector of variables, e.g. c('FWI','FFMC')
#'                     overwinter_DC = TRUE,
#'                     start_date = '2018-05-20',
#'                     end_date='2018-05-30', # must be same year as start date
#'                     extent = e, # sp or sf object with extent and crs info
#'                     buff_width = 0,
#'                     match_crs = TRUE, # match crs of input extent object?
#'                     input_directory = NULL, # folder where ncdfs are saved
#'                     output_directory = NULL)
#'
#'plot(FWI[[1]][[1:4]])
#' }

FWI_ERA5_grab <- function(variable = NULL,
                     overwinter_DC = TRUE,
                     start_date,
                     end_date,
                     extent,
                     buff_width = 0,
                     match_crs = FALSE,
                     input_directory = NULL,
                     output_directory = NULL,
                     filename_prefix = NULL){

  if(!year(as.Date(start_date)) == year(as.Date(end_date))){stop('start_date must be from the same year as end_date.')}
  pull_year <- as.Date(start_date) %>% year() %>% as.character()

  # make sure requested variables exist
  if(is.null(variable)){
    stop("variable argument empty. Valid variable inputs include: 'FFMC','DMC','DC','ISI','BUI','FWI','DSR', or a vector including a combination of these.")
  }
  allowed_vars <- c('FFMC','DMC','DC','ISI','BUI','FWI','DSR')
  if(all(variable %in% allowed_vars) == FALSE){
    notallowed <- variable[which(variable %in% allowed_vars == FALSE)]
    stop("Invalid variable input used. Valid variable inputs include: 'FFMC','DMC','DC','ISI','BUI','FWI','DSR'.")
  }

  if(!grepl('sf|character|SpatialPolygons|SpatialPolygonsDataFrame', class(extent)[1])){
    stop("extent must an object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.")
  }
  if(is.na(crs(extent))){
    stop('extent object must have a valid crs.')
  }

  if(is.null(input_directory)){ # if no user input, download from online
    # FWI metrics from https://zenodo.org/record/3626193
    # only available from 1979-2018 (inclusive).

    if(!as.numeric(pull_year) %in% 1979:2018){
      stop('FWI metrics are only available online from 1979-2018 (inclusive; see https://zenodo.org/record/3626193). Specify an input_directory to use NetCDFs stored on your local device.')
    }

    if(overwinter_DC == TRUE){
      download_vars <- paste0(variable, '_owDC')
    } else {
      download_vars <- paste0(variable, '_dDC')
    }

    URL_lookup <- tibble(var = c("FFMC_dDC","DMC_dDC","DC_dDC","ISI_dDC","BUI_dDC","FWI_dDC","DSR_dDC","FFMC_owDC","DMC_owDC","DC_owDC","ISI_owDC","BUI_owDC","FWI_owDC","DSR_owDC"),
                         partialURL = c('https://zenodo.org/record/3540950/files/no_overwintering_fine_fuel_moisture_code_',
                                        'https://zenodo.org/record/3540954/files/no_overwintering_duff_moisture_code_',
                                        'https://zenodo.org/record/3540959/files/no_overwintering_drought_code_',
                                        'https://zenodo.org/record/3540946/files/no_overwintering_initial_spread_index_',
                                        'https://zenodo.org/record/3540942/files/no_overwintering_build_up_index_',
                                        'https://zenodo.org/record/3540938/files/no_overwintering_fire_weather_index_',
                                        'https://zenodo.org/record/3540962/files/no_overwintering_daily_severity_rating_',
                                        'https://zenodo.org/record/3540922/files/fine_fuel_moisture_code_',
                                        'https://zenodo.org/record/3540924/files/duff_moisture_code_',
                                        'https://zenodo.org/record/3540926/files/drought_code_',
                                        'https://zenodo.org/record/3540920/files/initial_spread_index_',
                                        'https://zenodo.org/record/3540918/files/build_up_index_',
                                        'https://zenodo.org/record/3539654/files/fire_weather_index_',
                                        'https://zenodo.org/record/3540928/files/daily_severity_rating_'))

    partialURLs <- URL_lookup %>% filter(var %in% download_vars) %>% pull(partialURL)
    URLs <- paste0(partialURLs,pull_year,'.nc?download=1')

    # Download ncs
    tempfile_list <- vector(length = length(URLs))
    for(url in seq_along(URLs)){
      message(paste0('Downloading file ',url,' of ', length(URLs), ' (',variable[url],')...'))

      tempfile_list[url] <- tempfile(fileext = '.nc')
      curl_download(url = URLs[url], destfile = tempfile_list[url])

      message(paste0('Downloaded file ',url,' of ', length(URLs), ' (',variable[url],')'))
    }

    # import nc as RasterBrick
    rasts <- vector(mode = 'list', length=length(tempfile_list))
    for(nc in seq_along(rasts)){
      rasts[[nc]] <- brick(tempfile_list[nc])
    }
  }

  if(!is.null(input_directory)){ # user supplied input directory containing the FWI nc files

    message('Reading in files from user defined input directory...')

    # search for the files within the folder
    filename_lookup <- tibble(var = c('FFMC','DMC','DC','ISI','BUI','FWI','DSR'),
                              partialFilename = c('fine_fuel_moisture_code_','duff_moisture_code_','drought_code_',
                              'initial_spread_index_','build_up_index_','fire_weather_index_','daily_severity_rating_'))

    partialFilenames <- filename_lookup %>% filter(var %in% variable)
    partialFilenames <- partialFilenames[match(variable, partialFilenames$var),] %>%
      pull(partialFilename) # reorder filenames to match user input

    Filenames <- paste0(partialFilenames,pull_year, '.nc')
    nc_paths <- file.path(input_directory, Filenames)

    if(all(file.exists(nc_paths))==FALSE){
      missingvars <- variable[which(file.exists(nc_paths)==FALSE)]
      missingfile <- Filenames[which(file.exists(nc_paths)==FALSE)]
      stop(paste0(missingvars, ' file could not be located in specified input directory. File should be named ',missingfile, collapse = '\n'))
    } else {
      message('Files for all specified variables located within input directory.')
    }

    # import nc as RasterBrick
    rasts <- vector(mode = 'list', length=length(nc_paths))
    for(nc in seq_along(rasts)){
      rasts[[nc]] <- brick(nc_paths[nc])
    }
  }

  # parse to and from julian dates
  from_julian <- yday(start_date)
  to_julian <- yday(end_date)

  # get spatial extent (including buffer, if provided)
  if(!buff_width == 0){
    pull_extent <- st_buffer(extent, dist = buff_width)
    pull_extent <- st_transform(pull_extent, crs = 4326)
  } else {
    pull_extent <- st_transform(extent, crs = 4326)
  }

  # temporal subset to dates and area of interest for each brick
  message('Subsetting RasterBrick(s)...')
  rasts_subset <- vector(mode = 'list', length=length(rasts))

  for(rast in seq_along(rasts)){
    rast_tmp <- rasts[[rast]]
    rast_tmp <- raster::subset(rast_tmp, from_julian:to_julian) # temporal subset
    rast_tmp <- raster::rotate(rast_tmp) # rotate from 0-360 to 0-180
    rast_tmp <- raster::crop(rast_tmp, pull_extent)

    # set layer names
    layer_names <- paste0(as.numeric(pull_year), from_julian:to_julian) %>% as.Date(format = '%Y%j')
    names(rast_tmp) <- layer_names

    # project to extent crs if match_crs is TRUE
    if(match_crs == TRUE){
      # get upper and lower limits, for clamping later
      lower <- raster::cellStats(rast_tmp, min) %>% min()
      upper <- raster::cellStats(rast_tmp, max) %>% max()

      rast_tmp <- projectRaster(from = rast_tmp, crs = crs(extent))

      rast_tmp <- raster::clamp(rast_tmp, lower = lower, upper = upper)
    }
    # setZ. this has to be performed after projecting, bc projectRaster drops z info.
    rast_tmp <- setZ(rast_tmp, from_julian:to_julian, name = 'Time (Day)')

    rasts_subset[[rast]] <- rast_tmp
    message(paste0('Processed variable ',variable[rast],'...'))
  }

  # name the list of RasterBricks
  names(rasts_subset) <- variable

  if(is.null(input_directory)){
    unlink(tempfile_list, recursive = T) # remove tempfiles, if they were created
  }

  if(!is.null(output_directory)){

    filename_lookup <- tibble(var = c('FFMC','DMC','DC','ISI','BUI','FWI','DSR'),
                              partialFilename = c('fine_fuel_moisture_code_','duff_moisture_code_','drought_code_',
                                                  'initial_spread_index_','build_up_index_','fire_weather_index_','daily_severity_rating_'))

    for(brick in seq_along(variable)){
      varname <- variable[brick]
      partialFilename <- filename_lookup %>% filter(var %in% varname) %>% pull(partialFilename)

      if(!is.null(filename_prefix)){
        outname <- paste0(filename_prefix,'_FireIndices_' ,partialFilename, gsub('-','',start_date),'_',gsub('-','',end_date),'.nc')
      } else {
        outname <- paste0('FireIndices_' ,partialFilename, gsub('-','',start_date),'_',gsub('-','',end_date),'.nc')
      }

      simpleCap <- function(x) { # function to capitalize first letter of each word in string
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
      }

      longname <- gsub('_',' ',partialFilename) %>% simpleCap()

      writeRaster(x = rasts_subset[[brick]],
                  filename = file.path(output_directory, outname),
                  varname = varname,
                  longname = longname,
                  zname = 'Time',
                  zunit = 'Day',
                  overwrite = T)
    }
    message(paste0('FWI metrics NetCDFs saved locally in ',output_directory))
  }

  message(paste0(c('Returning subsetted RasterBrick(s) for variable(s)',variable), collapse=" "))
  return(rasts_subset)
}
