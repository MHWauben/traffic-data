# GOOGLE MOBILITY DATA
# Country-wide and some regions

### FALLBACK IF GOOGLE TAKES THEIR DIRECT LINK DOWN: MATT
# # Get the date from the last downloaded file, and create dates up to today
# dates_since_last <- seq(max(as.Date(stringr::str_extract(last_file('google_mob', '.csv'), 
#                                                          '[0-9]{4}-[0-9]{2}-[0-9]{2}')))+1, 
#                         Sys.Date(), by = 'day')
# for(date in dates_since_last){
#   tryCatch(download.file(paste0('https://raw.githubusercontent.com/mattkerlogue/google-covid-mobility-scrape/master/data/',
#                                 date, '_alldata_long.csv'),
#                          dest = paste0('google_mob/mattk_', date, '.csv'),
#                          mode = 'wb'), 
#            error=function(e) {
#              print('Not available')
#            })
# }
# data_files <- rownames(file.info(list.files('google_mob/', full.names = T)))
# google_m <- purrr::map_dfr(data_files, readr::read_csv)
# google_sub <- google_m %>%
#   dplyr::filter(country_name %in% countries)

# Google now has a stable link to refer to (!!)
google_m <- readr::read_csv('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv')
google_sub <- google_m %>%
  dplyr::filter(country_region %in% countries) %>%
  tidyr::pivot_longer(cols = contains('baseline'),
                      names_to = 'entity',
                      values_to = 'value') %>%
  dplyr::arrange(date) %>%
  dplyr::group_by(country_region, sub_region_1, sub_region_2, entity) %>%
  dplyr::mutate(value_roll = zoo::rollmean(value, 7, fill = NA, align = 'right'),
                diff = value_roll - value) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(entity = gsub('_percent_change_from_baseline', '', entity),
                entity = gsub('_', ' ', entity))
