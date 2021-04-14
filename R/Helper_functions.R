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
    CFS_REF_ID <- stringr::word(original_NBAC[1,]$COMMENTS, 1, sep = fixed('[')) # CNFDB name
    common_name <- str_match(original_NBAC[1,]$COMMENTS, 'alias:\\s*(.*?)\\s*,')[,2] # alias
    incident_info <- incident_info %>% add_column(CFS_REF = CFS_REF_ID, # append CNFDB name
                                                  NAME = common_name, # append alias
                                                  .before = 1)
  }

  # simplify fire polygon to make further processing quicker
  nbac_tmp <- ms_simplify(original_NBAC) %>% st_union() %>% st_sf()
  nbac_tmp <- nbac_tmp %>% add_column(incident_info, .before = 1) # append incident info
  return(nbac_tmp)
}
