
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
  y <- x %>%
    mutate(date = date) %>%
    mutate(cumsum = cumsum(cases)) %>%
    complete( fill = list(cases = 0)) %>%
    mutate(cum_rolling = rollapplyr(cases, width = 10, FUN = sum, partial = TRUE)) %>%
    drop_na(cumsum)
  return(y)
}

day_by_day_plot <- function(x, facet_on = FALSE){
  plot <- x %>%
    group_by(county) %>%
    ggplot() +
    geom_line(aes(x=date, y = cases, color = county))
  if(facet_on){
    plot+facet_wrap(~county)}
  else{plot}
}

growth_rate_plot<- function(x, facet_on = FALSE) {
  plot <- x %>%
    group_by(county) %>%
    mutate_each(growth, cases, deaths)  %>% 
    ggplot() +
    geom_line(aes(x=date, y = cases, color = county))+
    labs(y='cases growth rate')
  if(facet_on){
    plot+facet_wrap(~county)}
  else{plot}
}

cases_gt50 %>% left_join(wisco_health_sub,by=c('fips'='FIPS'))

wisc_gt_50 <- get_xgt50(wisc_counties)

growth_rate_plot(wisc_gt_50 )


growth_rate_plot(cases_gt50_cums %>% filter(county == 'Milwaukee'))

cases_gt50_cums 



louisiana_counties <- counties %>%
  filter(state=='Louisiana')

la_gt_50 <- get_gt50(louisiana_counties, facet_on =TRUE)

la_gt_50 <- calculate_growth_rate(la_gt_50, facet_on = TRUE)

day_by_day_plot(la_gt_50)

growth_rate_plot(la_gt_50) 

day_by_day_plot(counties %>% filter(county %in% c('Milwaukee','Orleans','New York City')))

day_by_day_plot(la_gt_50 %>% filter(county=='Orleans'))


day_by_day_plot(wisc_counties %>% filter(county=='Milwaukee'))

