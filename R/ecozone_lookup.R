#' Look-up Canada Ecozone values and RGB colours.
#'
#' See Canada Ecozone raster values, associated names, and RGB colour codes.
#'
#' @return A data.frame with Canada Ecozone look-up information.
#' @export
#'
#' @examples
#' \dontrun{
#' ecozone_lookup()
#' }
ecozone_lookup <- function(){
  data.frame('Value' = 1:15,
             'Ecozone' = c('Artic Cordillera', 'Northern Arctic', 'Southern Arctic','Taiga Plains',
                           'Taiga Shield', 'Boreal Shield','Atlantic Maritime','Mixedwood Plains',
                           'Boreal Plains','Prairies','Taiga Cordillera','Boreal Cordillera','Pacific Maritime',
                           'Montane Cordillera','Hudson Plains'),
             'R' = c(81,69,254,150,171,205,233,130,55,157,46,173,5,181,128),
             'G' = c(41,113,250,150,111,171,217,130,197,46,67,197,115,215,2),
             'B' = c(76,158,200,151,41,103,77,71,143,121,135,45,77,159,2),
             'Alpha'=rep('255',15),
             'HexValue'=c("#51294C","#45719E","#FEFAC8","#969697","#AB6F29","#CDAB67","#E9D94D",
                          "#828247","#37C58F","#9D2E79","#2E4387","#ADC52D","#05734D","#B5D79F","#800202"
))
}



