#' Grab 2014 FBP fuels grid
#'
#' Extracts a spatial subset of the 2014 FBP fuels map to a user defined extent.
#'
#' @param reference_grid A reference raster used to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, Canada_Lambert_Conformal_Conic.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param fbp_grid A FBP fuels raster object or the location of the raster on your local device. If \code{NULL}, the FBP fuels 2014 raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details
#' The purpose of this function is to extract a spatial subset of the 2014 FBP fuels map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the reference_grid.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, Canada_Lambert_Conformal_Conic.
#'
#' It is recommended to define the \code{fbp_grid} if this function is being called multiple times, in order to reduce download time.
#' See metadata to interpret raster values at \url{https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/current/}
#'
#' @return A RasterLayer with the name 'FBP_fuels_2014'.
#' @export
#'
#' @references FBP Fuels 2014b: \url{https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/current/}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # Define a polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#' fbp_fuel <- fbp_fuel_grab(reference_poly = e, match_crs = TRUE, buff_width = 0, fbp_grid = NULL)
#' plot(fbp_fuel)
#' }

fbp_fuel_grab <- function(reference_grid,
                          reference_poly,
                          match_crs = TRUE,
                          buff_width = 0,
                          fbp_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
   }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }

  if(is.null(fbp_grid)){
    fbp_temp <- tempfile(fileext = '.zip')
    utils::download.file(destfile = fbp_temp,
                  url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/current/National_FBP_Fueltypes_version2014b.zip')
    fbp_files <- utils::unzip(zipfile = fbp_temp, list = T)
    fbp <- utils::unzip(zipfile = fbp_temp, files = fbp_files$Name[8], exdir = gsub('.zip','',fbp_temp))
    fbp <- raster(fbp)
  } else {
    if( grepl("RasterLayer", class(fbp_grid)) ){ fbp <- fbp_grid }
    if( grepl("character", class(fbp_grid)) ){ fbp <- raster(fbp_grid) }
    if( !grepl("RasterLayer|character", class(fbp_grid)) ){ message("fbp_grid must be the directory of the raster or a raster object.") }
  }

  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(fbp)) %>% extent()
    fbp_poly <- raster::crop(fbp, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      fbp_poly <- projectRaster(from = fbp_poly, crs = crs(reference_poly), method = 'ngb', res = res(fbp)[1])
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(fbp)) %>% extent()
    fbp_poly <- raster::crop(fbp, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      fbp_poly <- projectRaster(from = fbp_poly, to = reference_grid, method = 'ngb')
    }
  }
  names(fbp_poly) <- 'FBP_fuels_2014'

  if(is.null(fbp_grid)){
    unlink(c(gsub(".zip","",fbp_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)
  }

  return(fbp_poly)
}

# ------------------------------------------------

#' Grab ecozones
#'
#' Extracts and rasterizes a subset of the Terrestrial Ecozones of Canada to a user-defined reference grid.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#'
#' @details Extracts and rasterizes Terrestrial Ecozones of Canada to align with the input reference_grid. The original ecozones data are in polygon format, and are downloaded to a tempfile using this function.
#'
#' See original metadata to interpret raster values at \url{https://open.canada.ca/data/en/dataset/7ad7ea01-eb23-4824-bccc-66adb7c5bdf8}
#'
#' @return RasterLayer with the name 'Ecozone'.
#' @export
#'
#' @references Terrestrial Ecozones of Canada: \url{https://open.canada.ca/data/en/dataset/7ad7ea01-eb23-4824-bccc-66adb7c5bdf8}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # create reference raster
#' r <- raster(xmn = -1198869, xmx = -1131697, ymn = 901665.8, ymx = 1004528, res = 250, crs = 3978)
#' ecozones <- ecozone_grab(reference_grid = r)
#' plot(ecozones)
#' }

ecozone_grab <- function(reference_grid){
  if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  # download and extract ecozones of canada
  ecozone_temp <- tempfile(fileext = '.zip')
  utils::download.file(destfile = ecozone_temp, url = 'http://www.agr.gc.ca/atlas/data_donnees/bio/aafcEcostratification/gml/ECOZONE_V2_2_GML.zip')

  ecozone_files <- utils::unzip(zipfile = ecozone_temp, list = T) # get file names
  ecozones <- utils::unzip(zipfile = ecozone_temp, files = ecozone_files$Name[2], exdir = gsub('.zip','',ecozone_temp))
  ecozones <- st_read(dsn = ecozones)

  # reproject the ecozones
  ecozones <- st_transform(ecozones, crs = crs(reference_grid))

  # generate a crop extent based off FBP_fire to clip ecozones to fire region
  e <- st_bbox(reference_grid) %>% st_as_sfc()

  # clip ecozone to extent
  ecozone_e <- st_intersection(ecozones, e)

  # rasterize the output to ref grid
  ecozone_rast <- rasterize(as_Spatial(ecozone_e), reference_grid, field = 'ECOZONE_ID')
  names(ecozone_rast) <- 'Ecozone'

  unlink(c(gsub(".zip","",ecozone_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

  return(ecozone_rast)
}

# ------------------------------------------------

#' Grab canopy closure
#'
#' Extract a spatial subset of the canopy closure map from the 2011 Canada's Forest Attributes to a user defined extent.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, SR-ORG:8787.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param canopy_grid  A canopy closure raster object or the location of the raster on your local device. If \code{NULL}, the canopy cover raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details The purpose of this function is to extract a spatial subset of the Canada Forest Attributes canopy closure map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the \code{reference_grid.}
#' If \code{reference_grid} is used to define the extractiion extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, SR-ORG:8787.
#'
#' It is recommended to define the \code{canopy_grid} if this function is being called multiple times, in order to reduce download time.
#' Values in the returned raster represent percentages.
#'
#' @return RasterLayer with the name 'Canopy_closure'.
#' @export
#'
#' @references 2011 Canada's Forest Attributes: \url{https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # define polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#' r <- canopy_closure_grab(reference_poly = e, match_crs = TRUE, buff_width = 0, canopy_grid = NULL)
#' plot(r)
#' }

canopy_closure_grab <- function(reference_grid,
                                reference_poly,
                                match_crs = TRUE,
                                buff_width = 0,
                                canopy_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
  }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }

  if(is.null(canopy_grid)){ # grid not defined by user
    canclosure_temp <- tempfile(fileext = '.tif') # canopy closure
    utils::download.file(destfile = canclosure_temp, mode = 'wb', url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/NFI_MODIS250m_2011_kNN_Structure_Stand_CrownClosure_v1.tif')
    canclosure <- raster(canclosure_temp)
  } else { # grid defined by user
    if( grepl("RasterLayer", class(canopy_grid)) ){ canclosure <- canopy_grid }
    if( grepl("character", class(canopy_grid)) ){ canclosure <- raster(canopy_grid) }
    if( !grepl("RasterLayer|character", class(canopy_grid)) ){ message("canopy_grid must be the directory of the raster or a raster object.") }
  }

  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(canclosure)) %>% extent()
    canclosure_ref <- raster::crop(canclosure, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(canclosure_ref, stat = 'min')
      grid_max <- cellStats(canclosure_ref, stat = 'max')
      canclosure_ref <- projectRaster(from = canclosure_ref, crs = crs(reference_poly), method = 'bilinear', res = res(canclosure)[1])
      canclosure_ref <- raster::clamp(canclosure_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(canclosure)) %>% extent()
    canclosure_ref <- raster::crop(canclosure, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(canclosure_ref, stat = 'min')
      grid_max <- cellStats(canclosure_ref, stat = 'max')
      canclosure_ref <- projectRaster(from = canclosure_ref, to = reference_grid, method = 'bilinear')
      canclosure_ref <- raster::clamp(canclosure_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  names(canclosure_ref) <- 'Canopy_closure' # rename the raster layer

  if(is.null(canopy_grid)){
    unlink(c(gsub(".tif","",canclosure_temp),list.files(tempdir(),pattern = ".tif",full.names = T)),recursive = T)
  }
  return(canclosure_ref)
}

#' Grab stand height
#'
#' Extract a spatial subset of the stand height map from the 2011 Canada's Forest Attributes to a user defined extent.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, SR-ORG:8787.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param standheight_grid  A stand height raster object or the location of the raster on your local device. If \code{NULL}, the stand height raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details The purpose of this function is to extract a spatial subset of the Canada Forest Attributes stand height map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the \code{reference_grid.}
#' If \code{reference_grid} is used to define the extractiion extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, SR-ORG:8787.
#'
#' It is recommended to define the \code{standheight_grid} if this function is being called multiple times, in order to reduce download time.
#' Values in the returned raster represent meters.
#'
#' @return RasterLayer with the name 'Stand_height'.
#' @export
#'
#' @references 2011 Canada's Forest Attributes: \url{https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # define polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' r <- stand_height_grab(reference_poly = e,
#'                        match_crs = TRUE,
#'                        buff_width = 0,
#'                        standheight_grid = NULL)
#' plot(r)
#' }

stand_height_grab <- function(reference_grid,
                              reference_poly,
                              match_crs = TRUE,
                              buff_width = 0,
                              standheight_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
  }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }

  if(is.null(standheight_grid)){
    standheight_temp <- tempfile(fileext = '.tif') # stand height
    utils::download.file(destfile = standheight_temp, mode = 'wb', url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/NFI_MODIS250m_2011_kNN_Structure_Stand_Height_v1.tif')
    standheight <- raster(standheight_temp)
  } else {
    if( grepl("RasterLayer", class(standheight_grid)) ){ standheight <- standheight_grid }
    if( grepl("character", class(standheight_grid)) ){ standheight <- raster(standheight_grid) }
    if( !grepl("RasterLayer|character", class(standheight_grid)) ){ message("standheight_grid must be the directory of the raster or a raster object.") }
  }

  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(standheight)) %>% extent()
    standheight_ref <- raster::crop(standheight, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(standheight_ref, stat = 'min')
      grid_max <- cellStats(standheight_ref, stat = 'max')
      standheight_ref <- projectRaster(from = standheight_ref, crs = crs(reference_poly), method = 'bilinear', res = res(standheight)[1])
      standheight_ref <- raster::clamp(standheight_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(standheight)) %>% extent()
    standheight_ref <- raster::crop(standheight, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(standheight_ref, stat = 'min')
      grid_max <- cellStats(standheight_ref, stat = 'max')
      standheight_ref <- projectRaster(from = standheight_ref, to = reference_grid, method = 'bilinear')
      standheight_ref <- raster::clamp(standheight_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }

  names(standheight_ref) <- 'Stand_height' # rename the raster layer

  if(is.null(standheight_grid)){
    unlink(c(gsub(".tif","",standheight_temp),list.files(tempdir(),pattern = ".tif",full.names = T)),recursive = T)
  }

  return(standheight_ref)
}


#' Grab broadleaf percentage
#'
#' Extract a spatial subset of the broadleaf percentage map from the 2011 Canada's Forest Attributes to a user defined extent.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, SR-ORG:8787.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param broadleaf_grid  A broadleaf_grid raster object or the location of the raster on your local device. If \code{NULL}, the broadleaf percentage raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details
#' The purpose of this function is to extract a spatial subset of the Canada Forest Attributes broadleaf percentage map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the \code{reference_grid.}
#' If \code{reference_grid} is used to define the extractiion extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, SR-ORG:8787.
#'
#' It is recommended to define the \code{broadleaf_grid} if this function is being called multiple times, in order to reduce download time.
#' Values in the returned raster represent percentages.
#'
#' @return RasterLayer with the name 'Broadleaf_percentage'.
#' @export
#'
#' @references 2011 Canada's Forest Attributes: \url{https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # define polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#' r <- broadleaf_grab(reference_poly = e, match_crs = TRUE, buff_width = 0, broadleaf_grid = NULL)
#' plot(r)
#' }

broadleaf_grab <- function(reference_grid,
                           reference_poly,
                           match_crs = TRUE,
                           buff_width = 0,
                           broadleaf_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
  }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }


  if(is.null(broadleaf_grid)){
    broadleaf_temp <- tempfile(fileext = '.tif') # broadlead percentage
    utils::download.file(destfile = broadleaf_temp, mode = 'wb', url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/NFI_MODIS250m_2011_kNN_SpeciesGroups_Broadleaf_Spp_v1.tif')
    broadleaf <- raster(broadleaf_temp)
  } else {
    if( grepl("RasterLayer", class(broadleaf_grid)) ){ broadleaf <- broadleaf_grid }
    if( grepl("character", class(broadleaf_grid)) ){ broadleaf <- raster(broadleaf_grid) }
    if( !grepl("RasterLayer|character", class(broadleaf_grid)) ){ message("broadleaf_grid must be the directory of the raster or a raster object.") }
  }


  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(broadleaf)) %>% extent()
    broadleaf_ref <- raster::crop(broadleaf, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(broadleaf_ref, stat = 'min')
      grid_max <- cellStats(broadleaf_ref, stat = 'max')
      broadleaf_ref <- projectRaster(from = broadleaf_ref, crs = crs(reference_poly), method = 'bilinear', res = res(broadleaf)[1])
      broadleaf_ref <- raster::clamp(broadleaf_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(broadleaf)) %>% extent()
    broadleaf_ref <- raster::crop(broadleaf, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(broadleaf_ref, stat = 'min')
      grid_max <- cellStats(broadleaf_ref, stat = 'max')
      broadleaf_ref <- projectRaster(from = broadleaf_ref, to = reference_grid, method = 'bilinear')
      broadleaf_ref <- raster::clamp(broadleaf_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  names(broadleaf_ref) <- 'Broadleaf_percentage' # rename the raster layer

  if(is.null(broadleaf_grid)){
    unlink(c(gsub(".tif","",broadleaf_temp),list.files(tempdir(),pattern = ".tif",full.names = T)),recursive = T)
  }

  return(broadleaf_ref)
}

#' Grab needleleaf percentage
#'
#' Extract a spatial subset of the needleleaf percentage map from the 2011 Canada's Forest Attributes to a user defined extent.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, SR-ORG:8787.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param needleleaf_grid  A needleleaf percentage raster object or the location of the raster on your local device. If \code{NULL}, the needleleaf percentage raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details
#' The purpose of this function is to extract a spatial subset of the Canada Forest Attributes needleleaf percentage map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the \code{reference_grid.}
#' If \code{reference_grid} is used to define the extractiion extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, SR-ORG:8787.
#'
#' It is recommended to define the \code{needleleaf_grid} if this function is being called multiple times, in order to reduce download time.
#' Values in the returned raster represent percentages.
#'
#' @return RasterLayer with the name 'Needleleaf_percentage'.
#' @export
#'
#' @references 2011 Canada's Forest Attributes: \url{https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # define polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#' r <- needleleaf_grab(reference_poly = e, match_crs = TRUE, buff_width = 0, needleleaf_grid = NULL)
#' plot(r)
#' }
needleleaf_grab <- function(reference_grid,
                            reference_poly,
                            match_crs = TRUE,
                            buff_width = 0,
                            needleleaf_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
  }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }

  if(is.null(needleleaf_grid)){
    needleleaf_temp <- tempfile(fileext = '.tif') # needleleaf percentage
    utils::download.file(destfile = needleleaf_temp, mode = 'wb', url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/NFI_MODIS250m_2011_kNN_SpeciesGroups_Needleleaf_Spp_v1.tif')
    needleleaf <- raster(needleleaf_temp)
  } else {
    if( grepl("RasterLayer", class(needleleaf_grid)) ){ needleleaf <- needleleaf_grid }
    if( grepl("character", class(needleleaf_grid)) ){ needleleaf <- raster(needleleaf_grid) }
    if( !grepl("RasterLayer|character", class(needleleaf_grid)) ){ message("needleleaf_grid must be the directory of the raster or a raster object.") }
  }

  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(needleleaf)) %>% extent()
    needleleaf_ref <- raster::crop(needleleaf, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(needleleaf_ref, stat = 'min')
      grid_max <- cellStats(needleleaf_ref, stat = 'max')
      needleleaf_ref <- projectRaster(from = needleleaf_ref, crs = crs(reference_poly), method = 'bilinear', res = res(needleleaf)[1])
      needleleaf_ref <- raster::clamp(needleleaf_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(needleleaf)) %>% extent()
    needleleaf_ref <- raster::crop(needleleaf, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(needleleaf_ref, stat = 'min')
      grid_max <- cellStats(needleleaf_ref, stat = 'max')
      needleleaf_ref <- projectRaster(from = needleleaf_ref, to = reference_grid, method = 'bilinear')
      needleleaf_ref <- raster::clamp(needleleaf_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  names(needleleaf_ref) <- 'Needleleaf_percentage' # rename the raster layer

  if(is.null(needleleaf_grid)){
    unlink(c(gsub(".tif","",needleleaf_temp),list.files(tempdir(),pattern = ".tif",full.names = T)),recursive = T)
  }
  return(needleleaf_ref)
}

#' Grab branch biomass
#'
#' Extract a spatial subset of the branch biomass map from the 2011 Canada's Forest Attributes to a user defined extent.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, SR-ORG:8787.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param branch_grid  A branch biomass raster object or the location of the raster on your local device. If \code{NULL}, the branch biomass raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details
#' The purpose of this function is to extract a spatial subset of the Canada Forest Attributes branch biomass map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the \code{reference_grid.}
#' If \code{reference_grid} is used to define the extractiion extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, SR-ORG:8787.
#'
#' It is recommended to define the \code{branch_grid} if this function is being called multiple times, in order to reduce download time.
#' Values in the returned raster represent t/ha.
#'
#' @return RasterLayer with the name 'Branch_biomass'.
#' @export
#'
#' @references 2011 Canada's Forest Attributes: \url{https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # define polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#' r <- branch_biomass_grab(reference_poly = e, match_crs = TRUE, buff_width = 0, branch_grid = NULL)
#' plot(r)
#' }
branch_biomass_grab <- function(reference_grid,
                                reference_poly,
                                match_crs = TRUE,
                                buff_width = 0,
                                branch_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
  }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }

  if(is.null(branch_grid)){
    branch_temp <- tempfile(fileext = '.tif') # branch biomass
    utils::download.file(destfile = branch_temp, mode = 'wb', url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/NFI_MODIS250m_2011_kNN_Structure_Biomass_Branch_v1.tif')
    branch <- raster(branch_temp)
  } else {
    if( grepl("RasterLayer", class(branch_grid)) ){ branch <- branch_grid }
    if( grepl("character", class(branch_grid)) ){ branch <- raster(branch_grid) }
    if( !grepl("RasterLayer|character", class(branch_grid)) ){ message("branch_grid must be the directory of the raster or a raster object.") }
  }

  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(branch)) %>% extent()
    branch_ref <- raster::crop(branch, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(branch_ref, stat = 'min')
      grid_max <- cellStats(branch_ref, stat = 'max')
      branch_ref <- projectRaster(from = branch_ref, crs = crs(reference_poly), method = 'bilinear', res = res(branch)[1])
      branch_ref <- raster::clamp(branch_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(branch)) %>% extent()
    branch_ref <- raster::crop(branch, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(branch_ref, stat = 'min')
      grid_max <- cellStats(branch_ref, stat = 'max')
      branch_ref <- projectRaster(from = branch_ref, to = reference_grid, method = 'bilinear')
      branch_ref <- raster::clamp(branch_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  names(branch_ref) <- 'Branch_biomass' # rename the raster layer

  if(is.null(branch_grid)){
    unlink(c(gsub(".tif","",branch_temp),list.files(tempdir(),pattern = ".tif",full.names = T)),recursive = T)
  }
  return(branch_ref)
}


#' Grab foliage biomass
#'
#' Extract a spatial subset of the foliage biomass map from the 2011 Canada's Forest Attributes to a user defined extent.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object. See details.
#' @param reference_poly A reference polygon that provides a projection and extent to extract forest attributes for. It can be an sf or sp polygon object, or the location of a polygon on your local device. See details.
#' @param match_crs Logical. Should the extracted forest attribute grid be projected to the CRS of the reference object? Defaults to TRUE. If FALSE, the raster is returned in the native projection, SR-ORG:8787.
#' @param buff_width Numeric. Width of buffer (in meters) that should be applied to the \code{reference_poly} before extraction. Defaults to 0. Ignored if \code{reference_poly} is missing.
#' @param foliage_grid  A foliage biomass raster object or the location of the raster on your local device. If \code{NULL}, the foliage biomass raster will be downloaded to a tempfile. See details for recommendations.
#'
#' @details
#' The purpose of this function is to extract a spatial subset of the Canada Forest Attributes foliage biomass map.
#'
#' Only one of \code{reference_grid} or \code{reference_poly} must be defined to set the extraction extent. If both are defined, an error will occur.
#' If \code{reference_grid} is used to define the extraction extent and \code{match_CRS = TRUE}, then the returned raster will align with the \code{reference_grid.}
#' If \code{reference_grid} is used to define the extractiion extent and \code{match_CRS = FALSE}, then the returned raster will be in the native grid and projection, SR-ORG:8787.
#'
#' It is recommended to define the \code{foliage_grid} if this function is being called multiple times, in order to reduce download time.
#' Values in the returned raster represent t/ha.
#'
#' @return RasterLayer with the name 'Foliage_biomass'.
#' @export
#'
#' @references 2011 Canada's Forest Attributes: \url{https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # define polygon extent
#' e <- extent(-1198869,-1131697,901665.8,1004528) %>% st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(e) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#' r <- foliage_biomass_grab(reference_poly = e, match_crs = TRUE, buff_width = 0, foliage_grid = NULL)
#' plot(r)
#' }

# foliage biomass
foliage_biomass_grab <- function(reference_grid,
                                 reference_poly,
                                 match_crs = TRUE,
                                 buff_width = 0,
                                 foliage_grid = NULL){

  if(missing(reference_grid) & missing(reference_poly)) {stop('One of reference_grid or reference_poly must be provided to provide extent of extraction.')}
  if(!missing(reference_grid) & !missing(reference_poly)) {stop('Only one of reference_grid or reference_poly must be provided to define the extent of extraction.')}

  if(!missing(reference_poly)){
    if( grepl('sf', class(reference_poly)[1]) ){ reference_poly <- reference_poly }
    if( grepl("character", class(reference_poly)[1]) ){ reference_poly <- st_read(reference_poly) }
    if( grepl("SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ reference_poly <- st_as_sf(reference_poly) }
    if( !grepl("sf|character|SpatialPolygons|SpatialPolygonsDataFrame", class(reference_poly)[1]) ){ stop("reference_poly must be the directory of a polygon or object of class sf, SpatialPolygons, or SpatialPolygonsDataFrame.") }
  }

  if(!missing(reference_grid)){
    if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
    if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
    if( !grepl("RasterLayer|character", class(reference_grid)) ){ stop("reference_grid must be the directory of the raster or a raster object.") }
  }

  if(is.null(foliage_grid)){
    foliage_temp <- tempfile(fileext = '.tif') # foliage biomass
    utils::download.file(destfile = foliage_temp, mode = 'wb', url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2011-attributes_attributs-2011/NFI_MODIS250m_2011_kNN_Structure_Biomass_Foliage_v1.tif')
    foliage <- raster(foliage_temp)
  } else {
    if( grepl("RasterLayer", class(foliage_grid)) ){ foliage <- foliage_grid }
    if( grepl("character", class(foliage_grid)) ){ foliage <- raster(foliage_grid) }
    if( !grepl("RasterLayer|character", class(foliage_grid)) ){ message("foliage_grid must be the directory of the raster or a raster object.") }
  }

  if(!missing(reference_poly)){
    crop_extent <- st_buffer(reference_poly, buff_width) %>% st_transform(crs = crs(foliage)) %>% extent()
    foliage_ref <- raster::crop(foliage, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(foliage_ref, stat = 'min')
      grid_max <- cellStats(foliage_ref, stat = 'max')
      foliage_ref <- projectRaster(from = foliage_ref, crs = crs(reference_poly), method = 'bilinear', res = res(foliage)[1])
      foliage_ref <- raster::clamp(foliage_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  if(!missing(reference_grid)){
    crop_extent <- st_bbox(reference_grid) %>% st_as_sfc()
    crop_extent <- st_sf(geometry = crop_extent) %>% st_transform(crs = crs(foliage)) %>% extent()
    foliage_ref <- raster::crop(foliage, crop_extent, snap = 'out')
    if(match_crs == TRUE){
      # grab extreme max and min values
      grid_min <- cellStats(foliage_ref, stat = 'min')
      grid_max <- cellStats(foliage_ref, stat = 'max')
      foliage_ref <- projectRaster(from = foliage_ref, to = reference_grid, method = 'bilinear')
      foliage_ref <- raster::clamp(foliage_ref, lower = grid_min, upper = grid_max, useValues = T)
    }
  }
  names(foliage_ref) <- 'Foliage_biomass' # rename the raster layer

  if(is.null(foliage_grid)){
    unlink(c(gsub(".tif","",foliage_temp),list.files(tempdir(),pattern = ".tif",full.names = T)),recursive = T)
  }

  return(foliage_ref)
}

# ------------------------------------------------

#' Landcover grab
#'
#' Extract a spatial subset of the 2015 Land Cover of Canada to a user-defined reference grid.
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#' @param landcover_grid A landcover raster object or the location of a landcover raster on your local device. If NULL, the 2015 Land Cover of Canada will be downloaded to a tempfile (not recommended, see Details).
#'
#' @details
#' The purpose of this function is to extract a spatial subset of the 2015 Land Cover of Canada that aligns with your input reference grid.
#' It is highly recommended to download the 2015 Land Cover of Canada to your local device and use the landcover_grid argument to access it, especially if this function is being run multiple times.
#' The 2015 Land Cover grid is 1.9 GB and time consuming to download through this function.
#'
#' To interpret values in the returned raster, refer to \url{https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6}.
#'
#' @return RasterLayer with the name 'Landcover'.
#' @export
#'
#' @references 2015 Land Cover of Canada: \url{https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6}
#'
#' @import raster
#' @import sf
#'
#' @examples
#' \dontrun{
#' library(raster)
#' library(sf)
#'
#' # create reference raster
#' r <- raster(xmn = -1198869, xmx = -1131697, ymn = 901665.8, ymx = 1004528, res = 250, crs = 3978)
#' lc <- landcover2015_grab(reference_grid = r)
#' plot(lc)
#' }

landcover2015_grab <- function(reference_grid,
                               landcover_grid = NULL){
  if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  if(is.null(landcover_grid)) { # no landcover grid defined by user

    # download and extract landcover 2015 to tempfile
    # this is a large 1.9 GB file. recommended for users to download to local machien then define filepath.
    landcover_temp <- tempfile(fileext = '.zip')
    utils::download.file(destfile = landcover_temp, url = 'http://ftp.maps.canada.ca/pub/nrcan_rncan/Land-cover_Couverture-du-sol/canada-landcover_canada-couverture-du-sol/CanadaLandcover2015.zip')
    lc_files <- utils::unzip(zipfile = landcover_temp, list = T)
    landcover <- utils::unzip(zipfile = landcover_temp, files = lc_files$Name[1], exdir = gsub('.zip','',landcover_temp))
    landcover <- raster(landcover)

    e <- st_bbox(reference_grid) %>% st_as_sfc() # turn rast bbox to sfc
    e_proj <- st_sf(geometry = e) %>% st_transform(crs = crs(landcover))

    landcover_ref <- raster::crop(landcover, e_proj) %>% # crop
      projectRaster(to = reference_grid, method = 'ngb') # project and align back to ref grid parameters

    names(landcover_ref) <- 'Landcover'

    unlink(c(gsub(".zip","",landcover_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

    return(landcover_ref)

  } else { # landcover grid is defined by user
    # landcover grid options
    if( grepl("RasterLayer", class(landcover_grid)) ){ landcover <- landcover_grid }
    if( grepl("character", class(landcover_grid)) ){ landcover <- raster(landcover_grid) }
    if( !grepl("RasterLayer|character", class(landcover_grid)) ){ message("Landcover Grid must be the directory of the raster or a raster object.") }

    e <- st_bbox(reference_grid) %>% st_as_sfc() # turn rast bbox to sfc
    e_proj <- st_sf(geometry = e) %>% st_transform(crs = crs(landcover))

    landcover_ref <- raster::crop(landcover, e_proj) %>% # crop
      projectRaster(to = reference_grid, method = 'ngb') # project and align back to ref grid parameters

    names(landcover_ref) <- 'Landcover'

    return(landcover_ref)

  }

}

# ------------------------------------------------

#' Grab elevation
#'
#' A user-defined reference grid is used to define the NTS grids necessary to pull elevation from the CDEM layers provided by the Government of Canada.
#' @note Requires an internet connection
#'
#' @param reference_grid A reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#' @param output_directory Output directory where your elevation grid will be exported. If NULL, the raster will not be exported to the local device.
#' @param filename_prefix String that will be appended to the start of the exported raster file name.
#'
#' @details
#' The purpose of this function is to generate a common and rapid elevation layer that is sampled and masked to the reference grid.
#' The pre-packaged GeoTiff datasets used to generate the output grid are based on the NTS grids at the 1:250 000 scale.
#'
#' @return RasterLayer with the name 'Elevation'.
#' @export
#'
#' @references Canadian Digital Elevation Model: \url{https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333}
#'
#' @import raster
#' @import sf
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
#' }

elev_grab <- function(reference_grid,
                      output_directory = NULL,
                      filename_prefix = NULL){
  if( grepl("RasterLayer", class(reference_grid)) ){ reference_grid <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ reference_grid <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  e <- st_bbox(reference_grid) %>% st_as_sfc()

  ## Download and extract the NTS grid specified by user (will be removed after use), uses the 250k grid as that is what CDEM is based on
  nts_temp <- tempfile(fileext = '.zip')
  utils::download.file(destfile = nts_temp,url = "http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/index/nts_snrc.zip")
  nts_files <- utils::unzip(nts_temp,list = T)
  utils::unzip(zipfile = nts_temp,files = nts_files$Name,exdir = gsub(".zip","",nts_temp))
  nts_grid <- st_read(dsn = gsub(".zip","",nts_temp),layer = "nts_snrc_250k")

  ## Determine the NTS grids the data exists across
  layers <- st_intersection(nts_grid, st_transform(e,crs = st_crs(nts_grid)))$NTS_SNRC

  ## Extract the CDEM tiles needed to generate the grid.
  elevation <- lapply(layers,function(i){
    loc <- tempfile(fileext = ".zip")
    loc <- gsub("\\\\","/",loc)
    utils::download.file(url = paste0("http://ftp.geogratis.gc.ca/pub/nrcan_rncan/elevation/cdem_mnec/",
                               substr(i,1,3),
                               "/cdem_dem_",
                               i,
                               "_tif.zip"),
                  destfile = loc)
    files <- utils::unzip(loc,list = T)

    raster(paste0("/vsizip/",loc,"/",files$Name[grep(".tif$",files$Name)]))

  }
  )

  elev_layers <- lapply(elevation,function(x) projectRaster(from = x, to = reference_grid))

  if(length(elev_layers) > 1){
    elev_mosaic <- do.call(merge,elev_layers)
  } else{
    elev_mosaic <- elev_layers[[1]]
  }
  names(elev_mosaic) <- 'Elevation'

  if(!is.null(output_directory)){
    if(!is.null(filename_prefix)){
      filename <- paste0(filename_prefix, '_Elevation.tif')
    } else {
      filename <- 'Elevation.tif'
    }
    writeRaster(elev_mosaic, file.path(output_directory, filename), format = 'GTiff', overwrite = T)
  }

  unlink(c(gsub(".zip","",nts_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

  return(elev_mosaic)
}

