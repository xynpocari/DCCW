#' Look-up Canada Landcover 2015 values and RGB colours.
#'
#' See Canada Landcover 2015 raster values, associated landcover descriptions, and standard RGB colour codes.
#'
#' @return A data.frame with Canada Landcover 2015 look-up information.
#' @export
#'
#' @examples
#' \dontrun{
#' can_landcover2015_lookup()
#' }
can_landcover2015_lookup <- function(){
  data.frame('Value' = c(1,2,5,6,8,10,11,12,13,14,15,16,17,18,19),
             'Landcover' = c("Temperate or sub-polar needleleaf forest","Sub-polar taiga needleleaf forest",
                             "Temperate or sub-polar broadleaf deciduous forest","Mixed forest","Temperate or sub-polar shrubland",
                             "Temperate or sub-polar grassland","Sub-polar or polar shrubland-lichen-moss",
                             "Sub-polar or polar grassland-lichen-moss","Sub-polar or polar barren-lichen-moss","Wetland",
                             "Cropland","Barren lands","Urban","Water","Snow and Ice"),
             'ShortName' = c("Temperate/sub-polar needleleaf","Sub-polar taiga needleleaf",
                             "Temperate/sub-polar broadleaf","Mixed forest","Temperate/sub-polar shrubland",
                             "Temperate/sub-polar grassland","Sub-polar/polar shrubland-lichen-moss",
                             "Sub-polar/polar grassland-lichen-moss","Sub-polar/polar barren-lichen-moss","Wetland",
                             "Cropland","Barren lands","Urban","Water","Snow/ice"),
             'R' = c(0,147,20,91,178,224,155,186,63,107,229,168,219,76,255),
             'G' = c(61,155,140,117,137,206,117,211,137,163,173,170,33,112,249),
             'B' = c(0,112,61,43,51,137,84,142,114,137,102,173,38,163,255),
             'Alpha'=rep('255',15),
             'HexValue'=c("#003D00","#939B70","#148C3D","#5B752B","#B28933","#E0CE89","#9B7554","#BAD38E",
                          "#3F8972","#6BA389","#E5AD66","#A8AAAD","#DB2126","#4C70A3","#FFF9FF"))
}




