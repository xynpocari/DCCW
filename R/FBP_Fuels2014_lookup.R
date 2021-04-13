# ----------------------------------------------------

# Project title: Fire Progression Reanalysis
# Organization: NRCAN, Northern Forestry Centre, Edmonton AB
# Author: Xue Yan Chan, xueyan.chan@canada.ca
# Supervised by: Piyush Jain and Dan Thompson
# Last updated: 2021-04-08

# ----------------------------------------------------

#' Look-up FBP Fuels 2014 Values and RGB colours.
#'
#' See FBP Fuels 2014 raster values, associated fuel types, and standard RGB colour codes.
#'
#' @return A data.frame with FBP Fuels 2014 look-up information.
#' @export
#'
#' @examples
#' \dontrun{
#' FBP_Fuels2014_lookup()
#' }
FBP_Fuels2014_lookup <- function(){
  data.frame('Value' = c(101,	102,	103,	104,	105,	106,	107,	108,	108,	109,	110,	111,	112,	113,	114,	115,	116,	117,	119,	118),
             'FBPFuel' = c("C-1",	"C-2",	"C-3",	"C-4",	"C-5",	"C-6",	"C-7",	"D-1",	"D-2",	"M-1",	"M-2",
                           "M-3",	"M-4",	"S-1",	"S-2",	"S-3",	"O-1a",	"O-1b",	"Non-fuel",	"Water"),
             'Description' = c("C-1 Spruce-Lichen Woodland",	"C-2 Boreal Spruce",	"C-3 Mature Jack or Lodgepole Pine",
                               "C-4 Immature Jack or Lodgepole Pine",	"C-5 Red and White Pine",	"C-6 Conifer Plantation",
                               "C-7 Ponderosa Pine / Douglas Fir",	"D-1/D-2 Aspen",	"D-1/D-2 Aspen",	"M-1 Boreal Mixedwood - Leafless",
                               "M-2 Boreal Mixedwood - Green",	"M-3 Dead Balsam Fir / Mixedwood - Leafless",
                               "M-4 Dead Balsam Fir / Mixedwood - Green",	"S-1 Jack or Lodgepole Pine Slash",	"S-2 White Spruce / Balsam Slash",
                               "S-3 Coastal Cedar / Hemlock / Douglas-Fir Slash",	"O-1 (Matted/Standing Grass)",	"O-1 (Matted/Standing Grass)",	"Non-fuel",	"Water"),
             'R' = c(209,	34,	131,	112,	223,	172,	112,	196,	137,	255,	255,	99,	170,	251,	247,	174,	255,	230,	130,	115),
             'G' = c(255,	102,	199,	168,	184,	102,	12,	189,	112,	211,	170,	0,	0,	190,	104,	1,	255,	230,	130,	223),
             'B' = c(115,	51,	149,	0,	230,	237,	242,	151,	68,	127,	0,	0,	0,	185,	161,	126,	190,	0,	130,	255))
}




