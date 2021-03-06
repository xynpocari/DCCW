% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_GOES_grid.R
\name{get_GOES_grid}
\alias{get_GOES_grid}
\title{Get GOES grid}
\usage{
get_GOES_grid(
  satellite,
  GOES_hotspots,
  return_full_tile = FALSE,
  match_crs = TRUE
)
}
\arguments{
\item{satellite}{Either '16' for GOES-16 (GOES-E) or '17' for GOES-17 (GOES-W).}

\item{GOES_hotspots}{sf points object representing the centroid of GOES pixels, likely the result of running \code{GOES_GEE_grab()}. See details.}

\item{return_full_tile}{Logical. Should the full tile of the GOES grid be returned? Defaults to \code{FALSE}. When \code{FALSE}, only grid cells which intersect with \code{GOES_hotspots} are returned. See details.}

\item{match_crs}{Logical. Should the returned polygon grid be projected in the same crs as \code{GOES_hotspots}? Defaults to \code{TRUE}.}
}
\value{
sf polygons object
}
\description{
Import GOES-16 and GOES-17 grids (defined by the geostationary
goes_imager_projection) as vector polygons into R.
}
\details{
Polygons represent GOES pixels, which become highly oblique when transformed out of their native projection (goes_imager_projection).
This shapefile can be used in conjunction with the GOES hotspots pulled using \code{GOES_GEE_grab()} for a variety of purposes.
Transforming the original GOES rasters is not suitable, because the raster attributes typically describe sub-pixel characteristics,
which would become inaccurate/meaningless when transformed out of the native projection.

This function can be run using any sf object where an st_bbox can be extracted as input to \code{GOES_hotspots}.
However, when \code{return_full_tile = FALSE}, only grid cells which intersect with the sf object in \code{GOES_hotspots} are retained.
Users can use \code{return_full_tile = TRUE} to retain the entire tile of the GOES grid (which is quite large).

The tiling system used to split up the GOES grid was arbitrarily created as 120 tiles over the GOES extent.
PNG files that show the tiles overlain on Canada as well as a metadata file can be found
\href{https://drive.google.com/drive/folders/1DtAs6fIJXym9HxjB-9TP-ubtDa-XWJSs?usp=sharing}{here for GOES-16} and
\href{https://drive.google.com/drive/folders/145yeMCgq1_szrbbTFk6NtQlnQOc29lsI?usp=sharing}{here for GOES-17}.

The returned sf polygons object also includes the following attributes:

\code{c_lon}: Centroid longitude. This field represents the longitude of the given polygons centroid in WGS84.

\code{c_lat}: Centroid latitude. This field represents the latitude of the given polygons centroid in WGS84.

\code{Tile_ID}: Tile_ID ranges from 0-119 and corresponds to the tiles present in the PNG files found at the links listed above.
}
\examples{
\dontrun{

library(sf)
library(dplyr)
library(curl)
library(tmap)
tmap_mode('view')

# create sf points object representing centroid of GOES pixels.
# Here, GOES-17 pixel centroids are used.
# small sample included here, but would probably extract these using GOES_GEE_grab()
centroids <- data.frame(lon = c(-120.0590 ,-120.0916, -120.0440, -120.0766,
                                -120.0264, -120.0113, -120.0289, -119.9938 ,
                                -119.9787 ,-119.9962, -119.9460),
                        lat = c(50.30403 ,50.30260, 50.34064, 50.33921,50.30546,
                                50.34207, 50.37730, 50.30689, 50.34351, 50.37874,
                                50.34495))

centroids <- st_as_sf(centroids, coords = c('lon','lat'), crs = 4326) # coerce to sf

# view
tm_shape(centroids) + tm_dots() +
  tm_basemap("Esri.WorldTopoMap")

# get GOES-17 grid that coincide with centroids only by setting return_full_tile = FALSE
GOES_grid_subset <- get_GOES_grid(satellite = '17',
                                  GOES_hotspots = centroids,
                                  return_full_tile = FALSE,
                                  match_crs = TRUE)

GOES_grid_subset # 11 features are returned, one for each centroid we created

# view
tm_shape(centroids) + tm_dots() +
  tm_shape(GOES_grid_subset) + tm_borders() +
  tm_basemap("Esri.WorldTopoMap")

# get GOES-17 grid for the entire tile that contains our points by setting return_full_tile = TRUE
GOES_grid_full <- get_GOES_grid(satellite = '17',
                                GOES_hotspots = centroids,
                                return_full_tile = TRUE,
                                match_crs = TRUE)

GOES_grid_full # 16382 features

# view
tm_shape(GOES_grid_full) + tm_borders() +
  tm_shape(centroids) + tm_dots() +
  tm_basemap("Esri.WorldTopoMap")
}
}
