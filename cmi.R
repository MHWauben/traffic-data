# CITYMAPPER MOBILITY DATA
# A few cities only, but good detail
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)

countries <- c('United Kingdom', 'United States of America', 'France', 'Germany', 'Italy', 'Netherlands')

# Function: get last file from a folder
last_file <- function(folder, filetype){
  last_file <- file.info(list.files(folder, full.names = T))
  file_name <- rownames(last_file)[which.max(last_file$mtime) & grepl(filetype, rownames(last_file))]
  return(file_name)
}

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

ggplot(cmi_sub, aes(x = Date, y = CMI, colour = City))+
  geom_line()+
  facet_wrap(~Country)+
  theme_minimal()+
  theme(legend.position = 'none')+
  labs(title = 'Citymapper Mobility Index for selected countries',
       subtitle = 'Selected cities within each country. For the UK: London, Birmingham, Manchester',
       x = '',
       y = 'CMI',
       caption = 'Source: Citymapper.com/CMI')

