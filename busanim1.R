#busroute anim

library(tidyverse)
library(sf)
library(rgdal)
library(gganimate)
library(tidycensus)
library(tigris)


#data
septaroutes <- readOGR("C:/Users/Charlie/Documents/GIS/Transit/transitshp", layer = "busroutes2021", verbose = T) %>% st_as_sf()

city <- get_decennial(geography = "county",
                        year = 2020, 
                        variables = "H1_001N", 
                        geometry = T,
                        state = "PA", 
                        county = "Philadelphia", 
                        output = "wide")%>%
  st_as_sf()%>%
  st_transform(crs=4326)


citybus <- septaroutes%>% 
  st_crop(y = c(ymax = 40.06, ymin = 39.86, xmin =-75.27, xmax = -75.05))



tinybus <- septaroutes[city, , op= st_within]%>% 
  st_crop(y = c(ymax = 40.06, ymin = 39.86, xmin =-75.27, xmax = -75.05))

tinybus$LineName2 <- paste(tinybus$LineAbbr, tinybus$LineName, sep = " - ")

tinybus <-tinybus[1:49,]



#mapping

ggplot()+
  geom_sf(data = septaroutes)+
  geom_sf(data = citybus, color = 'blue')

mapTheme <- theme(plot.title =element_text(size=20, color = 'red', vjust = -5, hjust = .05, family = 'sans'),
                  plot.subtitle = element_text(size=17, color = 'red', vjust = -130, hjust = .9,  family = 'sans'),
                  plot.caption = element_text(size = 6),
                  panel.background=element_rect(fill = 'black'),
                  panel.border=element_blank(),
                  plot.background = element_rect(fill= 'black'),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "none",
                  plot.margin = margin(.1, .2, .1, .1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))



ggplot()+
  geom_sf(data = citybus, alpha = .25, color = 'grey')+
  geom_sf(data = citybus[4,], aes(group = LineName), color = 'red')+
  mapTheme+
  labs(title = 'SEPTA Bus Routes Fall 2021',
       subtitle = 'Columbus-Dock to 50th-Woodland')

p <- ggplot()+
  geom_sf(data = tinybus, aes(group = LineName2), color = 'red')+
  geom_sf(data = citybus, alpha = .25, color = 'grey')+
  mapTheme+
  labs(title = 'SEPTA Bus Routes',
       subtitle = '{closest_state}')+
  transition_states(LineName2, state_length = 1)+
  exit_fade()

animate(p, duration = 45,)

g <- ggplot()+
  geom_sf(data = citybus, alpha = .25, color = 'grey')+
  geom_sf(data = tinybus, aes(group = LineName2), color = 'red')+
  mapTheme+
  labs(title = 'SEPTA Fall 2021 Bus Routes',
       subtitle = '{closest_state}')+
  transition_states(LineName2)+
  exit_fade()


anim_save('citybusses.gif', g, path = "C:/Users/Charlie/Documents/GIS/Transit",  duration = 70, fps = 10, bg = 'black')
    