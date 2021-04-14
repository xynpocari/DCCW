
# Data Cubes for Canadian Wildfires (DCCW)

<!-- badges: start -->
<!-- badges: end -->

DCCW consists of functions to compile and summarize fire detection and environmental data from a variety of online sources in order to contextualize Canadian wildfires.

## Installation

You can install DCCW from [GitHub](https://github.com/) with:

``` r
library(remotes)
install_github('xynpocari/DCCW')
```

NOTE: This package requires that users have a Google Earth Engine Account and access to the R package rgee.
If you already have rgee, you can skip these installation steps.

You can install rgee with: 

``` r
install.packages("rgee")
rgee::ee_install() # Recommended for users with no Python experience
```

See details on rgee installation here: [rgee](https://github.com/r-spatial/rgee)

## Example: compile wildfire data cubes

In this demonstration, we will compile a data package for SWF-049-2019, otherwise known as the McMillan Complex Fire.
The McMillan Complex Fire was ignited in May 2019 northeast of Slave Lake, AB,
and grew to about 273,000 ha. 

### 1. Set up

Let's begin our data extraction by loading a number of required packages: 

``` r
# Load packages
library(DCCW)
library(sf)
library(lubridate)
library(raster)
library(rgdal)
library(rgee)
library(units)
library(lutz)
library(gstat)
library(humidity)
library(tmap)
library(tidyverse)
library(curl)
library(circular)
library(RSQLite)
library(stars)
library(rmapshaper)

# Connect to Google Earth Engine via rgee
ee_Initialize(drive = TRUE)

```
Great, now we're all set up. For this example, we're going to download the McMillan Complex Fire Perimeter from the [CWFIS Datamart](https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac) to a tempfile.

``` r
# Download 2019 NBAC to tempfile:
nbac_temp <- tempfile(fileext = '.zip')
download.file(destfile = nbac_temp,
              url = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_2019_r9_20200703.zip')

nbac_files <- unzip(nbac_temp, list = T)
nbac <- unzip(zipfile = nbac_temp, files = nbac_files$Name[1:11], exdir = gsub('.zip','',nbac_temp))
nbac <- st_read(dsn = nbac[7])

```
To help us find the correct NBAC perimeter, we can use `searchNBAC_by_alias()`, which searches the "COMMENTS" field for matching strings. In this case, we could search for either "McMillan" or "SWF-049-2019".

``` r
# Search for the 2019 McMillan wildfire using alias or CFS ID (in this case, SWF-049-2019)
McMillan <- searchNBAC_by_alias(nbac_file = nbac, alias = 'SWF-049-2019')

```
This returns four matching polygons. Notice all features belong to the same fire, but each polygon is a different "BURNCLAS".
To make this polygon easier to work with, let's call `simplify_NBAC()` to simplify the geometry and dissolve the polygons into one feature. 

``` r
McMillan # notice there are 4 polygons, one for each BURNCLAS
McMillan_simple <- simplify_NBAC(McMillan)
McMillan_simple # Simplified polygon, easier to work with

```
Let's plot the simplified perimeter using the tmap package: 

``` r
# Plot to view the fire perimeter:
tm_shape(McMillan_simple) + tm_polygons() +
  tm_layout(frame = FALSE)

```
We are almost finished setting up. We will now define the spatial and temporal extent of variables to be pulled.

``` r
# SPATIAL EXTENT: 
# buff_width: our fire poly will be buffered, and topographical rasts will be pulled
# to the spatial extent of the buffered poly.
buff_width <- 2000 # 2 km

# weather_buff_width: it makes sense to extract weather variables to
# a greater spatial extent surrounding the fire. Here we choose 40 km
weather_buff_width <- 40000 # 40 km

# TEMPORAL EXTENT: 
# These dates were chosen based on prior knowledge, but you
# might need to play around with reported/estimated start and end dates for your fire.
startdate <- '2019-05-17'
enddate <- '2019-06-22'

```
Finally, let's define an output directory to save exported data, and a filename prefix that will be appended to the beginning of all exported files. 

``` r
# Define the location of an output directory on your local device:
output_directory <- 'C:\\Users\\YOUR_NAME\\Documents\\DCCW'
filename_prefix <- 'SWF0492019' # using CFS REF ID for clarity here

```
### 2. Data extraction: fire detection vector data

We'll start our data extraction phase by pulling VIIRS hotspots S-NPP using `VIIRS_hotspots_grab()`, and pulling MODIS Collection 6 hotspots using `MODIS_hotspots_grab()`. This function downloads archives hotspots from the [FIRMS](https://firms.modaps.eosdis.nasa.gov/download/) database. 

The hotspots will be pulled within the boundary of the buffered `reference_poly`, and only hotspots detected between the user-defined `start_date` and `end_date` will be returned. 

`match_crs = TRUE` ensures that the returned sf object is in the same CRS as our input. If `match_crs = FALSE`, the returned sf object will be in WGS84. 

``` r
# Pull VIIRS S-NPP hotspots from the FIRMS database
viirs <- VIIRS_hotspots_grab(reference_poly = McMillan_simple,
                             match_crs = TRUE,
                             start_date = startdate,
                             end_date = enddate,
                             buff_width = buff_width)

# Let's see the hotspots attribute table
head(viirs) # notice the CRS is the same as our input, NAD83 Canada Atlas Lambert                             

# Pull MODIS Collection 6 hotspots, by calling MODIS_hotspots_grab()
modis <- MODIS_hotspots_grab(reference_poly = McMillan_simple,
                             match_crs = TRUE,
                             start_date = startdate,
                             end_date = enddate,
                             buff_width = buff_width)

# View attribute table:
head(modis)

```
Let's plot the hotspots over our fire polygon. Notice the density difference, and recall that the VIIRS resolution is 375 m, whereas MODIS is 1 km. 

``` r
# Plot VIIRS
tm_shape(McMillan_simple) + tm_polygons() +
  tm_shape(viirs) + tm_dots(col = 'darkred') +
  tm_layout(frame = F, title = 'VIIRS S-NPP \nHotspots')

# Plot MODIS
tm_shape(McMillan_simple) + tm_polygons() +
  tm_shape(modis) + tm_dots(col = 'darkred') +
  tm_layout(frame = F, title = 'MODIS\n Col. 6 Hotspots')


```
### 3. Data extraction: fire detection raster data

We will now pull [GOES-16](https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description) and [GOES-17](https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description) FRP rasters using Google Earth Engine. `GOES_FRP_grab()` will extract the FRP (fire radiative power) rasters to our defined spatial extent and temporal range, and return a RasterBrick.

One request to GEE is made for each unique month requested, in order to reduce large data transfers. 

``` r
# If the output directory is defined, a NetCDF will be saved.
GOES16_rast <- GOES_FRP_grab(satellite = 16, # either 16 for GOES16, or 17 for GOES17
                             extent = McMillan_simple,
                             buff_width = buff_width, # recall: 2 km
                             start_date = startdate, # recall: "2019-05-17"
                             end_date = enddate,# recall: "2019-06-22"
                             match_crs = TRUE, # if FALSE, return RasterBrick in native crs
                             output_directory = output_directory, # save the output somewhere!
                             filename_prefix = filename_prefix)

# View the RasterBrick object
GOES16_rast # Dont let the NAs scare you! These are accurate.

# Let's visualize a subset of the GOES 16 data
FRP_subset <- GOES16_rast %>% raster::subset(c("X2019.05.26.21.20.MDT","X2019.05.26.21.30.MDT",
                                               "X2019.05.26.21.40.MDT","X2019.05.26.21.50.MDT", 
                                               "X2019.05.26.22.00.MDT", "X2019.05.26.22.10.MDT",
                                               "X2019.05.26.22.20.MDT", "X2019.05.26.22.30.MDT",                                                              "X2019.05.26.22.40.MDT"))
tm_shape(McMillan_simple) + tm_borders() +
  tm_shape(FRP_subset) + tm_raster(title = 'GOES 16 FRP') +
  tm_facets(ncol = 3)

```
Notice that GOES produces a new raster every 10 minutes,
but the resolution at northern latitudes is very coarse.

``` r
raster::res(GOES16_rast) # Pixels are ~ 3 km x 6 km

```
To save time, we won't pull the GOES-17 FRP rasters, but you would do it the same as above:
``` r
# (uncomment if you want to try pulling GOES-17 FRP rasters)

# GOES17_rast <- GOES_FRP_grab(satellite = 17, # either 16 for GOES16, or 17 for GOES17
#                              extent = McMillan_simple,
#                              buff_width = buff_width,
#                              start_date = startdate,
#                              end_date = enddate,
#                              match_crs = TRUE,
#                              output_directory = output_directory,
#                              filename_prefix = filename_prefix)

```

If we only want an aspatial table that summarizes [GOES-16](https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_16_FDCF#description) and [GOES-17](https://developers.google.com/earth-engine/datasets/catalog/NOAA_GOES_17_FDCF#description) FRP, we can run `GOES_FRP_table_grab()`.

What's the benefit of using this function? Pulling the tabular summary is considerably quicker than pulling spatial data.
This function provides the sum of GOES FRP within the user-defined polygon (and optional buffer).
You can also choose what temporal scale to aggregate data over.

``` r
# Pull hourly GOES 16/17 aspatial FRP summary
GOES_FRP_table <- GOES_FRP_table_grab(reference_poly = McMillan_simple,
                                      start_date = startdate,
                                      end_date = enddate,
                                      interval = 'Hourly', # or 'Daily' or '10min'
                                      buff_width = buff_width)

GOES_FRP_table # Notice there are 864 rows. Now let's aggregate over a 10 minute scale.

GOES_FRP_table_10min <- GOES_FRP_table_grab(reference_poly = McMillan_simple,
                                            start_date = startdate,
                                            end_date = enddate,
                                            interval = '10min', # or 'Daily' or '10min'
                                            buff_width = buff_width)

GOES_FRP_table_10min # Now we have 5,188 rows due to higher temporal res.

```
### 4. Data extraction: environmental rasters in the immediate vicinity

We will now move onto extracting environmental rasters in the immediate vicinity (within a 2 km buffer) of the McMillan Complex Fire. Variables that might be relevant only in the immediate vicinity of a fire include topography (elevation, slope, and aspect), as well as ecozone or landcover data.

A lot of the DCCW functions require a reference grid, so here we'll use `fbp_fuel_grab()` to extract the [FBP fuels grid]( https://cwfis.cfs.nrcan.gc.ca/downloads/fuels/current/) as a reference raster.

``` r
# You could define the fbp_grid if you have the .tif stored on your
# local device, but here we will download it to a tempfile.
fbp_fuel <- fbp_fuel_grab(reference_poly = McMillan_simple,
                          match_crs = TRUE,
                          buff_width = buff_width,
                          fbp_grid = NULL)

# Let's plot the fuels grid (Note: standard fuel colours not being used here)
tm_shape(fbp_fuel) + tm_raster(palette = 'cat', n = 8) +
  tm_shape(McMillan_simple) + tm_polygons() +
  tm_layout(legend.bg.color = 'white', frame = F)

```

We will now extract rasters from the [2011 Canada's Forest Attributes maps](https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990), using the FBP fuels grid as a reference raster to align with. Please note, you could define the grid for each function (as a file saved on your local device),
which would save considerable download time.


For the purposes of this demo, we will not define the grids and instead download to tempfile.

``` r
# Extract forest attributes:
canopy_closure <- canopy_closure_grab(reference_grid = fbp_fuel, canopy_grid = NULL) # % canopy closure
stand_height <- stand_height_grab(reference_grid = fbp_fuel, standheight_grid = NULL) # stand height in m
broadleaf_pct <- broadleaf_grab(reference_grid = fbp_fuel, broadleaf_grid = NULL) # % broadleaf
needleleaf_pct <- needleleaf_grab(reference_grid = fbp_fuel, needleleaf_grid = NULL) # % needleleaf
branch_biomass <- branch_biomass_grab(reference_grid = fbp_fuel, branch_grid = NULL) # branch biomass in t/ha
foliage_biomass <- foliage_biomass_grab(reference_grid = fbp_fuel, foliage_grid = NULL) # foliage biomass in t/ha

```
We will now extract ecozone and topography variables using `ecozone_grab()` and `elev_grab()`.

Please note: the function `landcover2015_grab()` exists, but the tempfile it downloads from the [2015 Land Cover of Canada](https://open.canada.ca/data/en/dataset/4e615eae-b90c-420b-adee-2ca35896caf6) is 1.9 GB (huge!), so it is recommended to save the landcover raster locally, then define the `landcover_grid` in the function call. Due to this, we will not extract landcover in this demonstration. 

``` r
ecozone <- ecozone_grab(reference_grid = fbp_fuel) # ecozone
ecozone # notice values are only 9, because the McMillan fire occured within 1 ecozone.

# Grab elevation (from CDEM, based on tiles derived at a 1:250 000 scale)
elevation <- elev_grab(reference_grid = fbp_fuel)

# Visualize elevation:
tm_shape(elevation) + tm_raster(palette = "inferno", n = 25, contrast = c(0.2, 0.95), legend.show = F) +
  tm_shape(McMillan_simple) + tm_polygons() +
  tm_layout(frame = F, title = 'Elevation', title.color = 'white')

# Derive both aspect and slope from elevation
aspect <- terrain(elevation, opt = 'aspect', unit = 'degrees') %>% setNames('Aspect')
slope <- terrain(elevation, opt = 'slope', unit = 'degrees') %>% degree_to_percent() %>% setNames('Slope')

# Notice above that we set the layer names. This will help us in later steps,
# when we summarize fire variables and also export these rasters.

```
### 5. Data extraction: environmental rasters in the broader vicinity

Next up, we will extract environmental rasters in the broader vicinity (within a 40 km buffer) of the McMillan Complex Fire. Variables that might be desires at a greater extent include FWI and weather metrics. 

The function `FWI_ERA5_grab()` extracts [Global Fire Weather Indices](https://zenodo.org/record/3626193) rasters that were derived using ERA5 data (at 0.25 deg resolution). 

Because the data are only available online up to 2018, we will pull two variables here for 2018 for demonstration purposes only. These rasters are NOT associated with our McMillan fire. To save on download times, you can also define the location of a folder in which the FWI NetCDFs (from [https://zenodo.org/record/3626193](https://zenodo.org/record/3626193))  are saved. If defined, this function will search the folder for the correct files based on the original filenames.

``` r
FWI_metrics <- FWI_ERA5_grab(variable = c('FWI','ISI'),# vector of variables
                             overwinter_DC = TRUE, # look at above link; default and overwintered DC available
                             start_date = '2018-06-08', # random date for demo
                             end_date= '2018-06-30', # random date for demo
                             extent = McMillan_simple, # sp or sf object with extent and crs info
                             buff_width = weather_buff_width, # 40 km
                             match_crs = TRUE, # match crs of input extent object?
                             input_directory = NULL, # folder with ncdfs. will download from online if NULL
                             output_directory = NULL, # not saving locally since NetCDFS dont relate to McMillan
                             filename_prefix = NULL)

# This returns a list of named RasterBricks
FWI_metrics

# Let's visualize for good measure:
# FWI
tm_shape(FWI_metrics$FWI[[10:13]]) + tm_raster(title = 'FWI') +
  tm_shape(McMillan_simple) + tm_polygons() +
  tm_layout(panel.labels = names(FWI_metrics$FWI[[1:9]]))

# ISI
tm_shape(FWI_metrics$ISI[[10:13]]) + tm_raster(title = 'ISI') +
  tm_shape(McMillan_simple) + tm_polygons() +
  tm_layout(panel.labels = names(FWI_metrics$ISI[[1:9]]))

```

Finally, let's grab hourly reanalysis weather rasters ([ERA5-Land hourly](https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_HOURLY)) using GEE. We do this by using the function `ERA5Land_GEE_grab()`.
This requires a vector of user-defined variable names to pull. To see accepted variable names, we can call `ERA5Land_VariableList()`.

``` r
ERA5Land_VariableList() # view BandName choices

# For this demo, lets grab four datasets:
# dewpoint_temperature_2m, temperature_2m, snow_cover, and total_precipitation_hourly.

# make a vector of the desired BandNames from ERA5Land_VariableList()
pull_vars <- c('dewpoint_temperature_2m','temperature_2m','snow_cover','total_precipitation_hourly')

# Pull rasters from Earth Engine
# The time this function takes to run depends on the amount of variables, temporal range you define,
# and also by the amount of traffic Google is experiencing.
# At the time of this demo creation, each dataset took about ~ 30-60 sec to transfer.
era5land <- ERA5Land_GEE_grab(variable = pull_vars,
                              start_date = startdate,
                              end_date = enddate,
                              extent = McMillan_simple,
                              buff_width = weather_buff_width,
                              match_crs = TRUE, # 40 km
                              output_directory = output_directory, # save output as NetCDFs
                              filename_prefix = filename_prefix)


```
Notice that since we pulled both dewpoint temperature and temperature, a relative humidity RasterBrick was automatically generated for us.

Let's take a look at what we pulled:

``` r
era5land # a named list of RasterBricks; one for each variable (and generated RH as a bonus!)

# Plot, and notice these are hourly rasters at 0.1 deg resolution:
tm_shape(era5land$temperature_2m[[65:70]]) + tm_raster(title = 'Temperature 2m (K)', n=10) +
  tm_shape(McMillan_simple) + tm_polygons() +
  tm_layout(panel.labels = names(era5land$temperature_2m[[65:70]]))

```
That concludes the available functions for extracting fire detection and environmental data to contextualize wildfires! In the next section, we will derive estimated daily area burned. Doing so will allow us to summarize these raster variables into a tabular format later on.

### 6. Estimate daily area burned using IDW interpolation of hotspots

DCCW uses IDW interpolation to estimate daily area burned. The environmental rasters are then summarized using these daily estimates of area burned. 

Let's begin by interpolating the EPOCH field of the VIIRS hotspots to the FBP fuels grid, using `IDW_interpolate_hotspots()`. This will generate a RasterBrick where each layer represents a time slice (time is given as the z-value of each layer in UNIX epoch). 

``` r
# Interpolate the VIIRS hotspots to the FBP fuels grid
daily_IDW <- IDW_interpolate_hotspots(hotspots = viirs,
                                      fire_perimeter = McMillan_simple, # raster gets masked by this polygon
                                      reference_grid = fbp_fuel,
                                      interval = 'Daily', # not recommended to use 'Hourly'
                                      format = 'Raster', # or 'Polygon'
                                      cumulative = FALSE, # should layers depict cumulative area burned?
                                      output_directory = output_directory, # save the output!
                                      filename_prefix = filename_prefix)

# Let's take a look at the output RasterBrick:
daily_IDW # notice there are 33 layers, each layer corresponding to a day

# Plot a small subset of the rasters: 
tm_shape(daily_IDW[[1:9]]) + # recall these are non-cumulative
  tm_raster(title = 'Burn status', n = 2, labels = c('Unburned (0)','Burned (1)')) +
  tm_layout(panel.labels = names(daily_IDW[[1:9]]))
tm_shape(daily_IDW[[10:18]]) +
  tm_raster(title = 'Burn status', n = 2, labels = c('Unburned (0)','Burned (1)')) +
  tm_layout(panel.labels = names(daily_IDW[[10:18]]))  

```
Notice that we set `cumulative = FALSE`. As a result, in each time slice, pixel values of 1 indicate the pixel was estimated to have burned within the 24 hr prior to the assigned datetime, and values of 0 indicate the pixel was not estimated to have burned within 24 hr prior to the assigned datetime. If we had set `cumulative = TRUE`, then for any time slice, pixel values of 1 would have indicated pixels that burned any time prior to the layers assigned datetime (z-value).

We might want to visualize estimated time of burn as one raster, where pixel values represent estimated UNIX epoch that the pixel burned. To do this, we can use `IDW_BrickToLayer()`: 

``` r
# convert the RasterBrick to a single RasterLayer: 
IDW_SingleLayer <- IDW_BrickToLayer(IDW_brick =  daily_IDW)

# Let's plot to see the results:
tm_shape(IDW_SingleLayer) +
  tm_raster( title = 'Est. date of burn', palette = 'seq', n = 10) +
  tm_layout(frame = F, legend.width = 0.56)

# This is good, but tmaps default legend settings make the dates hard to interpret.
# Let's plot again and format the UNIX epoch values as calendar dates:

tm_shape(IDW_SingleLayer) +
  tm_raster( title = 'Est. date of burn', palette = 'seq', n = 10,
             legend.format = function(x){x %>% 
             as_datetime(tz = 'America/Edmonton') %>% date() %>% as.character()}) +
  tm_layout(frame = F, legend.width = 0.56)
```
Wonderful, now we have a reasonable estimation on how the fire progressed over the course of 33 days. In the next section we will derive tabular summaries based on the IDW interpolated area burned.

### 7. Summarize spatial data

There are two functions provided in DCCW that help to summarize spatial variables. First, `summarize_daily_fire_vars()` takes in hotspots, FWI metrics, and also a variable_stack (which will be comprised of our topographical variables, like elevation). Secondly, `summarize_hourly_fire_vars()` takes in hotspots and hourly weather to be summarized.

In both cases, data is summarized for the areas estimated to burn on any given day (using the IDW RasterBrick generated using `IDW_interpolate_hotspots()`). Let's summarize daily fire variables first, which will require us to make our `variable_stack`, comprising the immediate-vicinity rasters we pulled earlier. 

``` r
# Create stack of immediate-vicinity rasters 
# Since they were all pulled using the same reference grid, it's simple to stack them
var_stack <- stack(ecozone, fbp_fuel, elevation, aspect, slope, 
                   canopy_closure, stand_height, broadleaf_pct,
                   needleleaf_pct, branch_biomass, foliage_biomass)

var_stack # notice the layers in the stack have meaningful names.                   

# In the below function, interim_stack is the only required input, the rest are optional.
daily_summary <- summarize_daily_fire_vars(interim_stack = daily_IDW,# from IDW_interpolate_hotspots(cumulative = F)
                                           NBAC = McMillan_simple,
                                           VIIRS_hotspots = viirs, # sf from VIIRS_hotspots_grab()
                                           MODIS_hotspots = modis, # sf from MODIS_hotspots_grab()
                                           GOES_table = GOES_FRP_table, # table from GOES_FRP_table()
                                           FWI_list = NULL,# list from FWI_ERA5_grab(). Blank bc 2019 was not available
                                           variable_stack = var_stack,# stack of topography rasters, must be named
                                           output_directory = output_directory,
                                           filename_prefix = filename_prefix)

# And of course, let's take a look at the output:
View(hourly_summary) # Looking good!

```
Notice that the first column is denoted "Interval", which tells you the precise time period over which data is being aggregated over (to interpret the exact values in other attributes, read the final section which pulls a metadata file).

Now we will summarize the hourly fire variables. Once again, only `daily_IDW` is a required input. The rest are optional based on what you extracted. Since the IDW estimated area of burn is daily, variables are summarized using all the
pixels estimated to have burned on the day that encapsulates the hourly interval of interest.

``` r
# Create an hourly summary
hourly_summary <- summarize_hourly_fire_vars(interim_stack = daily_IDW, # from IDW_interpolate_hotspots(cumulative = F)
                                             NBAC = McMillan_simple,
                                             VIIRS_hotspots = viirs, # sf from VIIRS_hotspots_grab()
                                             MODIS_hotspots = modis, # sf from MODIS_hotspots_grab()
                                             GOES_table = GOES_FRP_table, # table from GOES_FRP_table()
                                             ERA5land_weather_list = era5land, # list from ERA5Land_GEE_grab()
                                             output_directory = output_directory,
                                             filename_prefix = filename_prefix)

# And of course, let's take a look at the output:
View(hourly_summary) # the intervals are hourly 

```

Finally, in the next section we will export our extracted data into a GeoPackage, and retrieve a metadata file to help us interpret our data. 

### 8. Export GeoPackage and get metadata

The function `create_gpkg()` allows users to export a combination of vector, raster, and tabular data into a GeoPackage. GeoPackages are a great way to share data, and are easily opened in desktop GIS software, such as QGIS. Unfortunately, GeoPackages do not support multiband rasters, so our temporal RasterBricks have all been exported in previous steps as NetCDFs within our output folder.

When using `create_gpkg()`, it's crucial to make sure you used named lists for vector and table data, and to use named rasters for the raster_layers argument. You could use `tibble::lst()` to create a list which inherits object names, or create a list and rename items appropriately. We will do demo a combination of these methods

``` r
# Vector data using renaming method:
vector_list <- list(McMillan, viirs, modis) %>%
  setNames(c('McMillan_NBAC','VIIRS_Hotspots','MODIS_Hotspots')) # set names!!

# Tables using tibble::lst() to inherit object names:
table_list = tibble::lst(daily_summary, hourly_summary)

# Adding the IDW_SingleLayer to static rasters we wish to include:
raster_layers <- stack(var_stack, IDW_SingleLayer)

# Run create_gpkg()
create_gpkg(vector_list = vector_list,# named list of sf objects. each item in list will be 1 layer in the gpkg
            raster_layers = raster_layers,  # a named RasterLayer, Brick, or Stack. Each layer will be 1 layer in the gpkg
            table_list = table_list, # named list of tables
            file_name = filename_prefix, # geopackage name. Could be any string, but using CFS REF ID here.
            output_directory = output_directory)

```
You can open your GeoPackage in a desktop GIS like QGIS, but let's also take a sneak peak here:

``` r
# View GeoPackage contents
gpkg_file <- file.path(output_directory,paste0(filename_prefix,'.gpkg'))

gdalUtils::gdalinfo(gpkg_file) %>%
  cat(sep = "\n")

```
All our expected outputs are now within the GeoPackage! 

Finally, let's download the DCCW data package metadata by calling `get_DCCW_metadata()`.
This function will download a readme.xlsx Excel sheet with helpful information and links to data sources,
so that the data and summary tables can be interpreted appropriately.

``` r
# Download DCCW metadata to your output directory
get_DCCW_metadata(output_directory = output_directory)

```
Remember to check out your output_directory to see everything we've exported throughout this demonstration.

### 9. Conclusion

That ends this demonstration, where we compiled spatial data from a variety of online sources to help contextualize the McMillan Complex Wildfire. Stay tuned! Future steps of this package include the addition of functions which
automatically generate daily hotspot and estimated area of burn maps.
