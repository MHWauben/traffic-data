# Pulling data together
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(zoo)
library(ggplot2)

countries <- c('United Kingdom', 'United States', 'United States of America', 
               'France', 'Germany', 'Italy', 'Netherlands')

# Function: get last file from a folder
last_file <- function(folder, filetype){
  last_file <- file.info(list.files(folder, full.names = T))
  file_name <- rownames(last_file)[which.max(last_file$mtime) & grepl(filetype, rownames(last_file))]
  return(file_name)
}

source('cmi.R')
source('google_mob.R')
source('regional.R')

##### VISUALISATIONS ####
# Citymapper Mobility Index: international
ggplot(cmi_sub, aes(x = Date, y = CMI, colour = City))+
  geom_line()+
  facet_wrap(~Country)+
  theme_minimal()+
  theme(legend.position = 'none')+
  labs(title = 'Citymapper Mobility Index for selected countries',
       subtitle = 'Selected cities within each country. For the UK: London, Birmingham, Manchester',
       x = '',
       y = 'CityMapper Index (number of trips planned)',
       caption = 'Source: Citymapper.com/CMI')+
  ggsave('City Mapper Index - intl.png')

# Google Mobility Index
dplyr::filter(google_sub, is.na(sub_region_1)) %>%
  ggplot()+
  geom_ribbon(aes(x = date, ymin = value, ymax = value_roll, fill = entity), alpha = 0.2)+
  geom_line(aes(x = date, y = value_roll, group = entity, colour = entity))+
  facet_wrap(~country_region)+
  labs(title = 'Google Places Mobility Index',
       subtitle = 'Country aggregate; 7-day rolling average',
       x = '',
       y = '% Change in Mobility',
       colour = 'Area type',
       fill = 'Area type')+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  ggsave('Google Mobility Index - intl.png')

# Local Authority outliers
google_uk %>%
  dplyr::filter(outlier & !is.na(CTRY19NM) & entity != 'residential') %>%
  ggplot(aes(x = date, y = value_roll))+
  geom_line(data = agg_uk, group = '', colour = 'black', size = 1.5, alpha = 0.3)+
  geom_ribbon(data = agg_uk, aes(ymin = value_roll - sd_roll, ymax = value_roll + sd_roll))+
  geom_line(aes(group = sub_region_1, colour = sub_region_1))+
  facet_wrap(entity~., nrow = 1)+
  labs(title = 'Google Mobility Index: outlier Local Authorities',
       subtitle = '7-day rolling average; whole-country average in grey',
       x = '',
       y = '% Change in Mobility',
       colour = 'Local Authority')+
  theme_minimal()+
  ggsave('Local Authority outliers - GMI.png')
