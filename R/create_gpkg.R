# ----------------------------------------------------

# Project title: Fire Progression Reanalysis
# Organization: NRCAN, Northern Forestry Centre, Edmonton AB
# Author: Xue Yan Chan, xueyan.chan@canada.ca
# Supervised by: Piyush Jain and Dan Thompson
# Last updated: 2021-04-08

# ----------------------------------------------------

#' Create GeoPackage
#'
#' Export a combination of vector, raster, and table data to a GeoPackage file.
#'
#' @param vector_list A named list of sf objects. Each item in the list will be one layer in the GeoPackage, and will inherit the list item name. Consider using \code{tibble::lst()} to create the named list.
#' @param raster_layers A Raster* object. If \code{raster_layers} is a RasterBrick or RasterStack, each layer will be one layer in the GeoPackage, and will inherit the layer name.
#' @param table_list A named list of tibbles. Each tibble will inherit the list item name. Consider using \code{tibble::lst()} to created the named list.
#' @param file_name Output filename. E.g. 'test.gpkg'.
#' @param output_directory Location where GeoPackage will be written.
#'
#' @details
#' This function takes a combination of vector, raster, and table inputs and exports them into a single GeoPackage file.
#' Multiband rasters are not supported; each layer of \code{raster_layers} will be written out as a separate layer in the GeoPackage.
#'
#' It is important to name list items or raster layers as you wish for them to appear in the output GeoPackage.
#' Currently, since only one Raster* object is accepted as input to \code{raster_layers}, exporting multiple raster extents/resolutions is not supported
#' (because by default, all layers in a RasterStack or Brick have the same resolution and extent).
#'
#' The resulting GeoPackage file is convenient to open up in desktop GIS software, such as QGIS.
#'
#' @return A GeoPackage file
#' @export
#'
#' @import RSQLite
#' @import stars
#' @import raster
#' @import sf
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' library(RSQLite)
#' library(stars)
#' library(raster)
#' library(sf)
#' library(dplyr)
#'
#' # create polygon
#' poly1 <- extent(-1198869,-1131697,901665.8,1004528) %>%
#'          st_bbox() %>% st_as_sfc() %>% st_sf()
#' st_crs(poly1) <- 3978 # assign CRS as EPSG 3978, Canada Atlas Lambert
#'
#' # generate random points
#' random_points <- st_sample(poly1, size = 20)
#' # generate buffers
#' point_buffers <- st_buffer(random_points, dist = 5000)
#'
#' # create rasters
#' raster1 <- raster(xmn = -1198869, xmx = -1131697, ymn = 901665.8, ymx = 1004528,
#'                   res = 250, crs = 3978)
#' values(raster1) <- 1:ncell(raster1) # assign raster values
#' raster2 <- raster1
#' values(raster2) <- runif(ncell(raster2))
#' rast_stack <- stack(raster1, raster2)
#'
#' # assign names. This is what will show up in exported GeoPackage layers
#' names(rast_stack) <- c('rast1', 'rast2')
#'
#' # create tibbles
#' tibble1 <- as_tibble(iris) # no significance, just an example.
#' tibble2 <- as_tibble(iris3)
#'
#' # export to GeoPackage
#' create_gpkg(vector_list = tibble::lst(poly1, random_points, point_buffers),
#'             raster_layers = rast_stack,
#'             table_list = tibble::lst(tibble1, tibble2),
#'             file_name = 'test.gpkg',
#'             output_directory = getwd())
#' }

create_gpkg <- function(vector_list = NULL,
                        raster_layers = NULL,
                        table_list = NULL,
                        file_name,
                        output_directory){

  file_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file_name))
  outname <- file.path(output_directory, paste0(file_name, '.gpkg'))

  # check to make sure tables are not written out into gpkg without other spatial layers
  if(!is.null(table_list) & is.null(vector_list) & is.null(raster_layers)){
    stop('Tables cannot be written to GeoPackage without the existence of other spatial data. Define one of vector_list or raster_layers to continue.')
  }

  # first case: both vector and rasters are present
  if(!is.null(vector_list) & !is.null(raster_layers)){

    # check if vector_list is named list
    if(!grepl('list', class(vector_list)[1]) | is.null(names(vector_list))){
      stop('vector_list must be a named list of sf objects. Try wrapping your input using tibble::lst() before proceeding.')
    }

    # check if raster_layers Raster*
    if(!grepl('Raster', class(raster_layers))){
      stop('raster_layers must be a named RasterLayer, RasterStack or RasterBrick.')
    }

    # write out vector layers
    for(item in 1:length(vector_list)){
      if(item == 1){
        st_write(vector_list[[item]], dsn = outname, names(vector_list[item]), overwrite = TRUE)
      } else {
        st_write(vector_list[[item]], dsn = outname, names(vector_list[item]), append = TRUE)
      }
    }

    # append raster layers
    for(item in 1:nlayers(raster_layers)){
      var_lyr <- raster_layers[[item]]
      options <- c(paste0('RASTER_TABLE=',names(raster_layers[[item]])),"APPEND_SUBDATASET=YES")

      var_lyr %>% st_as_stars() %>%
        write_stars(dsn = outname, driver = 'GPKG', options = options)
    }
  }

  # second case: vector is present, raster is not.
  if(!is.null(vector_list) & is.null(raster_layers)){
    # write out vector layers
    for(item in 1:length(vector_list)){
      if(item == 1){
        st_write(vector_list[[item]], dsn = outname, names(vector_list[item]), overwrite = TRUE)
      } else {
        st_write(vector_list[[item]], dsn = outname, names(vector_list[item]), append = TRUE)
      }
    }
  }

  # third case: raster is present, vector is not
  if(is.null(vector_list) & !is.null(raster_layers)){
    for(item in 1:nlayers(raster_layers)){
      if(item == 1){
        raster_layers[[item]] %>% st_as_stars() %>%
          write_stars(dsn = outname, driver = 'GPKG')
      } else {
        var_lyr <- raster_layers[[item]]
        options <- c(paste0('RASTER_TABLE=',names(raster_layers[[item]])),"APPEND_SUBDATASET=YES")

        var_lyr %>% st_as_stars() %>%
          write_stars(dsn = outname, driver = 'GPKG', options = options)
      }
    }
  }

  if(!is.null(table_list)){
    # write out tables
    for(item in 1:length(table_list)){
      # connect to existing gpkg
      con = dbConnect(SQLite(),dbname=outname)
      dbListTables(con)

      # add in table
      dbWriteTable(con, name = dbQuoteIdentifier(con, names(table_list)[item]), value = table_list[[item]])
      dbDisconnect(con) # disconnect
    }
  }

  message(paste0('GeoPackage exported to ', outname))

}

