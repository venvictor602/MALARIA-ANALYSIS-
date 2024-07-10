# MALARIA ANALYSIS

install.packages("tidyverse")
library(tidyverse)

malaria_inc <-  read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_inc.csv")

View(malaria_inc)

#renameing the columns 

malaria_inc_processed <- malaria_inc %>% 
  setNames(c("country", "code", "year", "incidence")) %>% 
  mutate(incidence = incidence/1000)

malaria_inc_processed %>% 
  filter(country %in% sample(unique(country), 6)) %>% 
  ggplot(aes(year, incidence, color = country))+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())


#looking at 2015 levels and the change from 2000 to 2015

malaria_spread <- malaria_inc_processed %>% 
  mutate(year = paste0("Y", year)) %>% 
  spread(year, incidence)


malaria_spread %>% 
  filter(country  != "Turkey",
         !is.na(code)) %>% 
  mutate(current = Y2015,
         change = Y2015- Y2000) %>% 
  ggplot(aes(current, change))+
  geom_point()+
  geom_text(aes(label = code),  vjust = 1, hjust = 1)


#plotting with maps

world <- map_data("world") %>% 
  filter(region != "Antarctica")


library(maps)

install.packages("map")

maps::iso3166 %>% 
  select(a2, mapname)


malaria_inc_processed %>% 
  filter(incidence <1) %>% 
  inner_join(maps::iso3166 %>% 
               select(a3, mapname), by  = c(code = "a3")) %>% 
  inner_join(world, by = c(mapname = "region")) %>% 
  ggplot(aes(long, lat, group = group, fill = incidence))+
  geom_polygon()+
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0.20, labels = scales::percent_format())+
  coord_map()+
  facet_wrap(~year)+
  theme_void()+
  labs(title = "Malaria incidence over time around the world")


#malaria deaths overtime 

malaria_death <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_deaths.csv")
    
malaria_deaths_processed <- malaria_death %>% 
  setNames(c('country', 'code', 'year', 'deaths'))

malaria_deaths_processed %>% 
  filter(country %in% sample(unique(country),6)) %>% 
  ggplot(aes(year, deaths, color = country))+
  geom_line()+
  labs(y = "deaths  per 100,000")+
  scale_y_continuous(labels = scales::percent_format())

install.packages("fuzzyjoin")
library(fuzzyjoin)
library(stringr)


malaria_country_data <- malaria_deaths_processed %>% 
  inner_join(maps::iso3166 %>% 
               select(a3, mapname), by = c(code = "a3")) %>% 
  mutate(mapname = str_remove(mapname, "\\(.*"))

malaria_map_data <- map_data("world") %>% 
  filter(region != "Antarctica") %>% 
  tbl_df() %>% 
  inner_join(malaria_country_data, by  = c(region  = "mapname"))

malaria_map_data %>% 
  ggplot(aes(long, lat, group = group, fill = deaths))+
  geom_polygon()+
  scale_fill_gradient2(low ="blue", high = "red", midpoint = 100)+
  theme_void()+
  labs(title = "Malaria deaths over time around the world",
       fill = "deaths per 100,000")

install.packages("countrycode")
library(gganimate)
library(countrycode)  

countrycode()

animation <- malaria_map_data %>% 
  mutate(continent = countrycode(code, "iso3c", "continent")) %>% 
filter(continent == "Africa") %>% 
  ggplot(aes(long, lat, group = group, fill = deaths))+
  geom_polygon()+
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 100)+
  theme_void()+
  transition_manual(year)+
  labs(title = "Malaria deaths over time in Africa ({current_frame})",
       fill = "deaths per 100,000")
  

animate(animation, nframes = 300, fps = 10, renderer=gifski_renderer("test.gif"))
