# CITYMAPPER MOBILITY DATA
# A few cities only, but good detail

cmi_file <- paste0('cmi/cmi_', format(Sys.Date(), '%Y%m%d'), '.csv')
if(!file.exists(cmi_file)){
  # This may fail if today's data isn't up yet, hence tryCatch
  tryCatch(download.file(paste0('https://cdn.citymapper.com/data/cmi/Citymapper_Mobility_Index_', 
                                format(Sys.Date(), '%Y%m%d'), '.csv'),
                         dest = cmi_file,
                         mode = 'wb'), 
           error=function(e) {
             print('Error')
             })
}
cities <- read.csv('https://raw.githubusercontent.com/girijesh18/dataset/master/City_and_province_list.csv') %>%
  rbind(., data.frame(City = c("Copenhagen","Montréal","New York City","Rhine-Ruhr","São Paulo","Washington DC"),
                      Country = c('Denmark', 'Canada', 'United States of America', 'Germany', 'Brazil', 'United States of America')))


cmi <- readr::read_csv(last_file('cmi/', '.csv'), skip = 3) %>%
  tidyr::pivot_longer(cols = -Date,
                      names_to = 'City',
                      values_to = 'CMI') %>%
  dplyr::left_join(cities, by = 'City')

cmi_sub <- dplyr::filter(cmi,  Country %in% countries & !is.na(CMI))
