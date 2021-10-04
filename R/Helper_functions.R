#' Split intervals into 1-hour intervals
#'
#' Split lubridate interval objects into 1-hour intervals.
#'
#' @param time_range A lubridate interval object.
#'
#' @return A vector of lubridate intervals.
#' @export
#'
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' library(lubridate)
#' int <- interval(start = '2019-05-10', end = '2019-05-11')
#' int_split_1hr <- split_1hr(int)
#' int_split_1hr
#' }
split_1hr <- function(time_range){ # function that splits into hourly intervals
  start <- int_start(time_range)
  end <- int_end(time_range)
  arr <-seq(start, end, 3600) # seconds in 1 hour
  arr <- arr[-c(1,length(arr))]
  return(interval(c(start, arr), c(arr, end)))
}

# --------------------------------------------------------

#' Split intervals into 24-hour intervals
#'
#' Split lubridate interval objects into 24-hour intervals.
#'
#' @param time_range A lubridate interval object.
#'
#' @return A vector of lubridate intervals.
#' @export
#'
#' @import lubridate
#'
#' @examples
#' \dontrun{
#' library(lubridate)
#' int <- interval(start = '2019-05-10', end = '2019-05-20')
#' int_split_24hr <- split_24hrs(int)
#' int_split_24hr
#' }
split_24hrs <- function(time_range){ # function that splits into daily (24 hr) intervals
  start <- int_start(time_range)
  end <- int_end(time_range)
  arr <-seq(start, end, 86400) # seconds in 24 hours
  arr <- arr[-c(1,length(arr))]
  return(interval(c(start, arr), c(arr, end)))
}

# --------------------------------------------------------

#' Convert slope degrees to percent
#'
#' Convert slope degrees to percent slope.
#' @param degrees Vector of slope values in degrees. For example, the output of calling \code{raster::terrain(opt = 'slope', unit = 'degrees')}.
#'
#' @return Vector of slope values in percent.
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # create reference raster
#' r <- raster(xmn = -1198869, xmx = -1131697, ymn = 901665.8, ymx = 1004528, res = 250, crs = 3978)
#' elev <- elev_grab(reference_grid = r, output_directory = NULL)
#' plot(elev)
#'
#' # get slope
#' slope_degrees <- raster::terrain(elev, opt = 'slope', unit = 'degrees')
#' slope_percent <- degree_to_percent(slope_degrees) # now slope is represented as percent
#' }
degree_to_percent <- function(degrees){
  percentage <- (tan(degrees*pi/180))*100
  return(percentage)
}

# --------------------------------------------------------

#' Search NBAC by alias
#'
#' Searches the NBAC comments field using a user-defined alias string to return matching polygons.
#'
#' @param nbac_file Location of an NBAC file or NBAC polygon object. Download NBAC from \url{https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac}.
#' @param alias A string that can contain the fire alias or CFS REF ID. This string is searched for matches within the NBAC Comments field.
#'
#' @return sf polygon
#' @export
#'
#' @references NBAC: \url{https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac}
#'
#' @import sf
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # download 2019 NBAC to tempfile
#' nbac_temp <- tempfile(fileext = '.zip')
#' download.file(destfile = nbac_temp,
#'               url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_2019_r9_20200703.zip')
#'
#' nbac_files <- unzip(nbac_temp, list = T)
#' nbac <- unzip(zipfile = nbac_temp, files = nbac_files$Name[1:11], exdir = gsub('.zip','',nbac_temp))
#' nbac <- st_read(dsn = nbac[7])
#'
#' # search for the 2019 McMillan wildfire, CFS ID SWF-049-2019
#' fire <- searchNBAC_by_alias(nbac_file = nbac, alias = 'SWF-049-2019')
#'
#' plot(fire[,1]) # view the McMillan fire
#' }

searchNBAC_by_alias <- function(nbac_file,
                                alias){

  if( grepl("sf", class(nbac_file)[1]) ){ nbac_file <- nbac_file }
  if( grepl("Spatial", class(nbac_file)[1]) ){ nbac_file <- st_as_sf(nbac_file) }
  if( grepl("character", class(nbac_file)[1]) ){ nbac_file <- st_read(nbac_file) }

  alias <- as.character(alias)

  contains_string <- nbac_file %>% dplyr::filter(grepl(alias,COMMENTS))

  if(nrow(contains_string) == 0){
    message('No matches found within the COMMENTS field for this alias.')
  } else {
    message(paste0('Found ',nrow(contains_string),' matches within the COMMENTS field for this alias.'))
    return(contains_string)
  }

}

# --------------------------------------------------------

#' Simplify NBAC
#'
#' Simplify NBAC perimeters by dissolving the constituent polygons can calling \code{rmapshaper::ms_simplify()}
#'
#' @param original_NBAC sf polygon of an NBAC fire perimeter. Probably the output from \code{searchNBAC_by_alias()}
#'
#' @details This function simplifies NBAC perimeters to make them faster to work with for the Data Cubes for Canadian Wildfires project.
#'
#' Some NBAC field attributes are retained. Included retained attributes are YEAR, NFIREID, AGENCY, SDATE, EDATE.
#' Some attributes are added, including SEPOCH (Start EPOCH), EEPOCH (End EPOCH), NBAC_HA (final total size in ha).
#'
#' If an alias is available in the comments, this is also extracted to a new field called NAME (e.g. 'SWF-049-2019' has the NAME 'McMillan Complex Fire).
#' The \code{original_NBAC} is also simplified using default settings of \code{rmapshaper::ms_simplify()}, then the resulting polygons are dissolved together.
#'
#' @return sf polygon
#' @export
#'
#' @references NBAC: \url{https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac}
#'
#' @import rmapshaper
#' @import sf
#' @import stringr
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(stringr)
#' library(rmapshaper)
#' library(dplyr)
#'
#' # download 2019 NBAC to tempfile
#' nbac_temp <- tempfile(fileext = '.zip')
#' download.file(destfile = nbac_temp,
#'               url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_2019_r9_20200703.zip')
#'
#' nbac_files <- unzip(nbac_temp, list = T)
#' nbac <- unzip(zipfile = nbac_temp, files = nbac_files$Name[1:11], exdir = gsub('.zip','',nbac_temp))
#' nbac <- st_read(dsn = nbac[7])
#'
#' # search for the 2019 McMillan wildfire, CFS ID SWF-049-2019
#' fire <- searchNBAC_by_alias(nbac_file = nbac, alias = 'SWF-049-2019')
#'
#' plot(fire[,1]) # view the McMillan fire
#'
#' # simplify the geometry and dissolve into one polygon
#' NBAC_simplify <- simplify_NBAC(fire)
#' NBAC_simplify # simplified polygon, easier to work with
#' plot(NBAC_simplify[,1]) # notice simplified polygon looks pretty much the same as original
#' }

simplify_NBAC <- function(original_NBAC){

  # grab incident info of interest from original NBAC
  incident_info <- original_NBAC[1,] %>% st_drop_geometry() %>% # drop geom as we are creating a table
    dplyr::select(YEAR, NFIREID, AGENCY, SDATE, EDATE) %>%
    as_tibble()
  # add epoch information to table
  incident_info$SEPOCH <- ymd_hm(paste0(incident_info$SDATE, '0000')) %>% # parse date as datetime
    force_tz(tzone = "Canada/Mountain") %>% as.integer() # convert to seconds since Jan 1 1970
  incident_info$EEPOCH <- ymd_hm(paste0(incident_info$EDATE, '0000')) %>%
    force_tz(tzone = "Canada/Mountain") %>% as.integer()

  # add final size info to table
  incident_info$NBAC_HA <- sum(original_NBAC$POLY_HA)

  # extract the CNFDB name and alias for fire, if this information exists
  if(original_NBAC$COMMENTS[1] != 'Null'){
   # CFS_REF_ID <- stringr::word(original_NBAC[1,]$COMMENTS, 1, sep = fixed('[')) # CNFDB name
    common_name <- str_match(original_NBAC[1,]$COMMENTS, 'alias:\\s*(.*?)\\s*,')[,2] # alias
    incident_info <- incident_info %>% add_column(
                                                 #CFS_REF_ID = CFS_REF_ID, # append CNFDB name
                                                  NAME = common_name, # append alias
                                                  .before = 1)
  }

  # simplify fire polygon to make further processing quicker
  nbac_tmp <- ms_simplify(original_NBAC)
  nbac_tmp <- nbac_tmp %>% st_buffer(dist = 0) %>% st_union() %>% st_sf()

  nbac_tmp <- nbac_tmp %>% add_column(incident_info, .before = 1) # append incident info
  return(nbac_tmp)
}

# --------------------------------------------------------

#' Round numbers up/down to any interval
#'
#' Round numbers up/down to user defined interval.
#' @param x Numeric. Value to be rounded up or down to specified interval.
#' @param y Numeric. Interval used to round \code{x}. Positive \code{y} values denote rounding up, whereas negative values denote rounding down.
#'
#' @details For example, using \code{y = 5} will round \code{x} up to the nearest multiple of 5, whereas using \code{y = -5} will round \code{x} down to the nearest multiple of 5.
#'
#' @return Numeric; rounded value of \code{x}.
#' @export
#'
#' @examples
#' \dontrun{
#' round_choose(27.8, 2) # round UP to the nearest 2
#' round_choose(27.8, -2) # round DOWN to the nearest 2
#' round_choose(38497, 10) # round UP to the nearest 10
#' round_choose(38497, -10) # round DOWN to the nearest 10

#' }
round_choose <- function(x,y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
}

# --------------------------------------------------------

#' Get CFS REF ID
#'
#' Get CFS REF ID of NBAC fire perimeters.
#'
#' @param NBAC An sp or sf NBAC polygon of one fire event.
#'
#' @details
#' In order to extract the CFS REF ID of NBAC fire perimeters, this function downloads the CNFDB fire point data to a tempfile,
#' then subsets the CNFDB points temporally and spatially to parameters defined within the input \code{NBAC}.
#' The CFS REF ID of the CNFDB point with the largest area is then returned.
#'
#' Users should be cautious when using output from this function; it is simply a guess at what the CFS REF ID for a certain NBAC fire should be.
#' Other sources should be checked to validate the CFS REF ID output from this function.
#'
#'
#' @return sf points
#' @export
#'
#' @import sf
#' @import dplyr
#' @import tibble
get_CFS_ID <- function(NBAC){ # nbac of 1 fire event with orig field attributes

  # check NBAC class
  if( grepl('sf', class(NBAC)[1]) ){ NBAC <- NBAC }
  if( grepl("character", class(NBAC)[1]) ){ NBAC <- st_read(NBAC) }
  if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(NBAC)[1]) ){ NBAC <- st_as_sf(NBAC) }
  if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(NBAC)[1]) ){ message("NBAC must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }

  # download CNFDB textfile to tempfile
  NFDB_temp <- tempfile(fileext = '.zip')

  utils::download.file(destfile = NFDB_temp,
                       url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/current_version/NFDB_point_txt.zip')
  NFDB_files <- utils::unzip(zipfile = NFDB_temp, list = T) # get file names
  NFDB <- utils::unzip(zipfile = NFDB_temp, files = NFDB_files$Name[5], exdir = gsub('.zip','',NFDB_temp))
  NFDB <- read.csv(NFDB) %>% as_tibble()

  # subset by year
  NFDB_subset <- NFDB %>% dplyr::filter(YEAR == NBAC$YEAR[1])

  # coerce to sf points
  NFDB_sf <- NFDB_subset %>%
    st_as_sf(coords = c('LONGITUDE','LATITUDE'), crs = 4326) %>%
    st_transform(crs = st_crs(NBAC))

  # subset to within the fire perimeter
  NFDB_fire <- NFDB_sf[NBAC,]

  # take the biggest point (by area) and pull CFS REF ID
  #NFDB_ID <- dplyr::slice_max(NFDB_fire, SIZE_HA) #%>% dplyr::pull(CFS_REF_ID)

  # remove the tempfiles
  unlink(list.files(path = tempdir(),
                    pattern = gsub('.zip','',basename(NFDB_temp)), full.names = T), recursive = TRUE)

  return(NFDB_fire)

}

# --------------------------------------------------------
#' Import Canada timezones
#'
#' Import Canada timezones into R
#'
#' @param location A spatial object of class sf or sp. If provided, the intersecting timezones will be returned. If this parameter is not provided, timezone polygons for all of Canada are returned.
#' @param crs Calls \code{st_transform()} to project the timezone polygons into a desired coordinate system. Valid inputs to the \code{crs} argument of \code{st_transform()} are allowed. Defaults to WGS84 (EPSG 4326).
#'
#'
#' @details
#' Downloads Canada timezones to a temporary file and imports the timezone polygons into the R environment.
#' Timezones are taken from \url{https://github.com/evansiroky/timezone-boundary-builder}
#'
#' Additional attributes beyond the ones provided from \url{https://github.com/evansiroky/timezone-boundary-builder}
#' are included in order to facilitate compability with the R packages lutz and lubridate.
#' Often, local standard times (LST) for hotspots and weather observations are needed for wildfire applications. Both lutz and lubridate
#' only recognize official Olson names (see \code{OlsonNames())}) as timezones (e.g. "America/Edmonton"). These timezones force daylight savings,
#' even when standard times are desired.
#'
#' Accordingly, the following attributes are available from the sf object imported using \code{canada_timezones()}:
#'
#' \code{tzid}: The timezone ID as an Olson name. For example, "America/Edmonton".
#' These names were pulled from \url{https://github.com/evansiroky/timezone-boundary-builder}
#'
#' \code{LST_offset}: The local standard time (LST) UTC offset that corresponds with the tzid.
#' UTC offsets are positive east of UTC and negative west of UTC.
#'
#' \code{LDT_offset}: The local daylight time (LDT) UTC offset that corresponds with the tzid.
#' If the timezone does not experience daylight savings, then this field is the same as the \code{LST_offset}.
#' UTC offsets are positive east of UTC and negative west of UTC.
#'
#' \code{LST_tzid}: An Olson name timezone ID that corresponds with the LST_offset.
#' For example, an LST_offset of "-07:00" would correspond to the timezone "Etc/GMT+7".
#' The purpose of this is to allow for compatiability with the R packages lutz and lubridate, which only recognizes official Olson name timezones.
#' Accordingly, offsets that do not have a corresponding Olson name are NA. This is the case for America/St_Johns (LST_offset = -03:30).
#'
#' \code{LDT_tzid}: An Olson name timezone ID that corresponds with the LDT_offset.
#' For example, an LDT_offset of "-07:00" would correspond to the timezone "Etc/GMT+7".
#' The purpose of this is to allow for compatiability with the R packages lutz and lubridate, which only recognizes official Olson name timezones.
#' Accordingly, offsets that do not have a corresponding Olson name are NA. This is the case for America/St_Johns (LDT_offset = -02:30).
#'
#' If an area experiences daylight savings, then the user can instead use \code{LST_tzid} in the lubridate package to "force"
#' the equivalent local standard time.
#'
#' @return sf polygons object
#' @export
#'
#' @import sf
#' @import curl

get_canada_timezones <- function(location = NULL, crs = 4326){
  output_file <- tempfile(fileext = '.zip')
  curl_download(url = 'https://drive.google.com/uc?id=1jeEI9PDWTyfMVqU4xj4Wt3Dek0_GB4I8&export=download',
                destfile = output_file )
  # unzip
  canada_zones <- utils::unzip(zipfile = output_file, exdir = gsub('.zip','',output_file))

  # import to R as sf object
  canada_zones_sf <- sf::st_read(canada_zones[6], quiet = TRUE)

  # if given a location (spatial object), intersect and return parts that intersect
  if(!is.null(location)){
    canada_zones_sf <- suppressMessages(canada_zones_sf[st_transform(location, crs = 4326),])
  }

  # if given a crs, reproject
  canada_zones_sf <- st_transform(canada_zones_sf, crs = crs)

  # remove tempfiles
  unlink(c(gsub(".zip","",output_file),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

  message('Returning Canada timezones as an sf object. Timezones last updated 2021-08-03.')
  return(canada_zones_sf)
}

