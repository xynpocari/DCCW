
#' Summarize contemperaneous VIIRS & GOES hotspots
#'
#' Summarize hotspot count and FRP for contemperaneous VIIRS and GOES hotspots for a given fire.
#'
#' @param VIIRS_hotspots sf points of VIIRS hotspots. The output of calling \code{VIIRS_hotspots_grab()}.
#' @param GOES16_hotspots sf points of GOES-16 hotspots. The output of calling \code{GOES_GEE_grab(satellite = 16)}.
#' @param GOES17_hotspots sf points of GOES-17 hotspots. The output of calling \code{GOES_GEE_grab(satellite = 17)}.
#' @param FRP_FWI_summary_table tibble; found within Fire Progression Reanalysis database with suffix 'FRP_FWI_summary_table_hourly.csv'.
#' @param CFS_REF_ID String. value to be placed in the CFS_REF_ID field, simply for identification purposes. If \code{NULL}, no CFS_REF_ID column will be added.
#' @param output_directory Optional. Output directory where a CSV should be saved.
#' @param filename_prefix Optional. String that will be appended to the start of the exported CSV. Ignored if \code{output_directory = NULL}.
#'
#' @details
#' This function creates a tibble summarizing GOES/VIIRS hotspot count and sum FRP for a given fire.
#'
#' GOES-16 and GOES-17 hotspots are first filtered to remove observations with mask values of 15 or 35 (low probability fire).
#' Therefore, GOES hotspot counts only represent pixels with values of 10, 11, 12, 13, 14, 30, 31, 32, 33, and 34.
#'
#' Each row in the resulting table represents a unique VIIRS overpass time. There is a preprocessing step where the
#' length of time between consecutive detections is evaluated. If VIIRS datetimes are found < 5 minutes apart from one another,
#' they are considered as a single overpass event (and the latter datetimes are replaced with the former).
#' VIIRS overpass times are then rounded to the nearest 10 minutes in order to be joined with the GOES 10-minute data.
#'
#' @return A tibble
#' @export
#'
#' @import sf
#' @import tibble
#' @import dplyr
#' @import lubridate

contemperaneous_VIIRS_GOES_summary <- function(VIIRS_hotspots,
                                       GOES16_hotspots,
                                       GOES17_hotspots,
                                       FRP_FWI_summary_table,
                                       CFS_REF_ID = NULL,
                                       filename_prefix = NULL,
                                       output_directory = NULL){


  # summarize contemperaneous viirs and goes
  # get timezone info
  can_timezones <- get_canada_timezones(location = VIIRS_hotspots[1,]) %>% st_drop_geometry()

  LST_tz <- can_timezones$LST_tzid[1]
  LST_tz_abbr <- paste0(can_timezones$LST_abbr[1],'_dttm')

  viirs <- VIIRS_hotspots %>% mutate(UTC_dttm = as_datetime(epoch))

  # arrange by epoch
  viirs <- viirs %>% arrange(epoch)

  # look for consecutive unique datetimes that are less than 5 min apart (300 seconds)
  viirs_dttms <- viirs %>% pull(UTC_dttm) %>% unique()
  dttm_length <- int_length(int_diff(viirs_dttms))

  x <- which(dttm_length < 300)

  # if dttms are less than 5 min apart, have the latter dttm take on the value of the immediate previous dttm
  if(length(x) > 0){

    for(i in 1:length(x)){
      dttm <- x[i]
      viirs$epoch[viirs$epoch == as.numeric(viirs_dttms[dttm+1])] <- as.numeric(viirs_dttms[dttm])

    }
  }

  # make LST field
  viirs <- viirs %>% mutate(LST_dttm = epoch %>% as_datetime() %>% with_tz(tzone = LST_tz),.before = UTC_dttm)

  # lets make this table
  viirs_goes_summary <- viirs %>% st_drop_geometry() %>%
    group_by(epoch, LST_dttm) %>% summarise(VIIRS_count = n(),VIIRS_sumFRP = sum(frp, na.rm = T)) %>%
    mutate(LST_dttm_round = LST_dttm %>% round_date('10 minutes'), .before = 1)

  goes16_subset <- GOES16_hotspots %>% filter(!Mask %in% c(15,35))

  # get round dates for GOES
  goes16_subset <- goes16_subset %>% mutate(LST_dttm_round = epoch %>% as_datetime() %>% with_tz(tzone = LST_tz) %>% round_date('10 minutes'),
                                            .before = 1)

  GOES16_summary <- goes16_subset %>% st_drop_geometry() %>%
    group_by(LST_dttm_round) %>% summarise(GOES16_count = n(),GOES16_sumFRP = sum(Power, na.rm = T))

  goes17_subset <- GOES17_hotspots %>% filter(!Mask %in% c(15,35))
  goes17_subset <- goes17_subset %>% mutate(LST_dttm_round = epoch %>% as_datetime() %>% with_tz(tzone = LST_tz) %>% round_date('10 minutes'),
                                            .before = 1)

  GOES17_summary <- goes17_subset %>% st_drop_geometry() %>%
    group_by(LST_dttm_round) %>% summarise(GOES17_count = n(),GOES17_sumFRP = sum(Power, na.rm = T))

  # join to viirs table
  viirs_goes_summary <- left_join(viirs_goes_summary, GOES16_summary) %>% left_join(GOES17_summary)

  # turn NAs to 0s
  viirs_goes_summary[is.na(viirs_goes_summary)] <- 0

  # append hourly FWI metrics
  # to do this, the dates in viirs goes summary have to be rounded to nearest hour
  viirs_goes_summary <- viirs_goes_summary %>% mutate(LST_dttm_round = round_date(LST_dttm_round, '1 hour'))
  FWI_table <- FRP_FWI_summary_table %>%
    mutate(LST_dttm_round = epoch %>% as_datetime() %>% with_tz(tzone = LST_tz), .before = 1) %>%
    dplyr::select(LST_dttm_round,FFMC,DMC,DC,ISI,BUI,FWI,DSR,hFFMC,hDMC,hDC,hISI,hBUI,hFWI,hDSR)

  # join
  viirs_goes_summary <- left_join(viirs_goes_summary, FWI_table, by = 'LST_dttm_round')

  viirs_goes_summary <- viirs_goes_summary %>% dplyr::select(-LST_dttm_round) %>%
    mutate(UTC_dttm = epoch %>% as_datetime() %>% as.character(), .before = 2) %>%
    mutate(LST_dttm = as.character(LST_dttm)) %>%
    rename(!!sym(LST_tz_abbr) := LST_dttm)

  # if CFS REF ID is given, add it to the table as an identifier
  if(!is.null(CFS_REF_ID)){
    viirs_goes_summary <- viirs_goes_summary %>% mutate(CFS_REF_ID = CFS_REF_ID, .before = VIIRS_count)
  }

  if(!is.null(output_directory)){

    if(!is.null(filename_prefix)){
      outname = file.path(output_directory, paste0(filename_prefix, '_Contemperaneous_VIIRS_GOES_summary.csv'))
    } else {
      outname = file.path(output_directory, 'Contemperaneous_VIIRS_GOES_summary.csv')
    }

    write.csv(viirs_goes_summary,file = outname, row.names = F)
  }

  return(viirs_goes_summary)

}
