
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tigris)
library(haven)
library(maptools)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(ggmap)
library(sf)
library(tidycensus)
library(maptools)
library(mapedit)
library(lubridate)
library(zoo)


setwd('/Users/yaz/Documents/covid19/covid-19-data')

counties <- read_csv('us-counties.csv')

wisco_health <- readxl::excel_sheets('wisconsin_health_data.xlsx')

View(wisco_health)

wisco_health_sub <- readxl::read_excel('wisconsin_health_data.xlsx',
                                       'Outcomes & Factors SubRankings',
                                        skip = 1)

counties %>% filter(state=='Wisconsin') %>%
  pivot_wider(names_from = c('date'),
              values_from = c('cases','deaths')) %>%
  View()

wisc_counties <- counties %>%
  filter(state=='Wisconsin')


cases_gt50 <- wisc_counties %>% filter(county %in%
                                         c(wisc_counties %>%
                                             group_by(county) %>%
                                             summarize(n_sum = sum(cases)) %>%
                                             filter(n_sum >50) %>%
                                             .$county))

get_xgt50 <- function(x){
  y <- x%>% filter(county %in%
                             c(x %>%
                                 group_by(county) %>%
                                 summarize(n_sum = sum(cases)) %>%
                                 filter(n_sum >50) %>%
                                 .$county))
  return(y)
}

growth <- function(x)x/(lag(x)-1)

calculate_growth_rate <- function(x){
  y <- cases_gt50 %>%
    mutate(date = date) %>%
    mutate(cumsum = cumsum(cases)) %>%
    complete( fill = list(cases = 0)) %>%
    mutate(cum_rolling = rollapplyr(cases, width = 10, FUN = sum, partial = TRUE)) %>%
    drop_na(cumsum)
  return(y)
}

growth_rate_plot<- function(x) {
  x %>%
    group_by(county) %>%
    mutate_each(growth, cases, deaths)  %>% 
    ggplot() +
    geom_line(aes(x=date, y = cases, color = county))+
    facet_wrap(~county) +
    labs(y='cases growth rate')
}

cases_gt50 %>% left_join(wisco_health_sub,by=c('fips'='FIPS'))





growth_rate_plot(cases_gt50_cums)

cases_gt50_cums 



louisiana_counties <- counties %>%
  filter(state=='Louisiana')

la_gt_50 <- get_xgt50(louisiana_counties)

la_gt_50 <- calculate_growth_rate(la_gt_50)

growth_rate_plot(la_gt_50)  
