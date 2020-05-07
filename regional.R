### UK regions?
# Pulling data together
# Lookup: Open Geography Portal 
# https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-to-county-to-region-to-country-december-2019-lookup-in-united-kingdom
uk_lookup <- readr::read_csv('https://opendata.arcgis.com/datasets/cdcc46d656e84e3d997e4ab2cd77881a_0.csv')

google_uk <- google_sub %>%
  dplyr::filter(country_region == 'United Kingdom') %>%
  dplyr::left_join(uk_lookup, by = c('sub_region_1' = 'LAD19NM')) %>%
  dplyr::group_by(CTRY19NM, entity, date) %>%
  dplyr::mutate(perc = dplyr::ntile(value, 100)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(sub_region_1) %>%
  dplyr::mutate(avg_perc = mean(perc, na.rm = T),
                outlier = ifelse(entity == 'residential',
                                 FALSE,
                                 avg_perc > 60))

plots <- list()
for(country in c('England', 'Wales', 'Scotland', 'Northern Ireland')){
  plots[[country]] <- google_uk %>%
    dplyr::filter(CTRY19NM == country) %>%
    ggplot(aes(x = date, y = value_roll, group = sub_region_1, colour = outlier))+
    geom_line(alpha = 0.5)+
    facet_wrap(entity~.)+
    labs(title = paste0('Google Mobility Index per Local Authority: ', country),
         subtitle = '7-day rolling average',
         x = '',
         y = '% Change in Mobility')+
    theme_minimal()+
    ggsave(paste0('Local Authorities GMI - ', country, '.png'))
}

agg_uk <- google_uk %>%
  dplyr::group_by(date, entity) %>%
  dplyr::summarise(value_roll = mean(value_roll, na.rm = T),
                   sd_roll = sd(value_roll, na.rm = T)) %>%
  dplyr::filter(entity != 'residential')



