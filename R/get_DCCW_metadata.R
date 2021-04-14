#' Get DCCW Metadata
#'
#' Download an excel file containing metadata for all data sources extracted using the DCCW (Data Cubes for Canadian Wildfires) R package.
#'
#' @param output_directory Location on your local device where the metadata Excel workbook (.xlsx) should be saved.
#'
#' @return An Excel sheet saved on the local device.
#' @export
#'
#' @import curl
#'
#' @examples
#' \dontrun{
#' library(curl)
#'
#' # download the Excel metadata sheet to the working directory:
#' get_DCCW_metadata(getwd())
#'
#' }
get_DCCW_metadata <- function(output_directory){
  output_file <- file.path(output_directory, 'readme.xlsx')
  curl_download(url = 'https://drive.google.com/uc?id=1Pl8uNmz9-021dCHnp055qh0JQhtCbbzO&export=download',
                destfile = output_file )
  message(paste0('DCCW metadata file saved locally as ',output_file))
}
