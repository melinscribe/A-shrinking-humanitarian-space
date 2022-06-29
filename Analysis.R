

### MWA ----

# The MWA requires rJava (x64 Version, https://www.oracle.com/java/technologies/downloads/#jdk18-mac)
# and Quartz (www.xquartz.org)

mw_data_sub <- df
## carry out matched wake analysis; past treatment control control; weighted
mw_data_sf_tcm <- mwa::matchedwake(mw_data_sub, t_window = c(10, 60, 10), spat_window = c(3, 15, 3),
                                   treatment = c('event_type',
                                                 'qip'),
                                   control = c('event_type',
                                               'iati'),
                                   dependent = c('event_type',
                                                 'Violence against civilians'),
                                   matchColumns = c('nl', 'road_dist', 'unbase', 'water_dist', 'pop_dens'),
                                   TCM = T, weighted = T, alpha2 = .05)


mw_data_sf_tcm3 <- mwa::matchedwake(mw_data_sub, t_window = c(10, 60, 10), spat_window = c(3, 15, 3),
                                    treatment = c('event_type',
                                                  'qip'),
                                    control = c('event_type',
                                                'iati'),
                                    dependent = c('event_type',
                                                  'Battles'),
                                    matchColumns = c('nl', 'unbase','road_dist', 'water_dist', 'pop_dens'),
                                    TCM = T, weighted = T, alpha2 = .05)


## main results plot
mwa:::plot.matchedwake(mw_data_sf_tcm)
mwa:::plot.matchedwake(mw_data_sf_tcm3)




## main results table
library(xtable)
sum_mw_data_sf_tcm <- mwa:::summary.matchedwake(mw_data_sf_tcm)
sum_mw_data_sf_tcm <- mwa:::summary.matchedwake(mw_data_sf_tcm3)





#Create subsets for map
cs_sub <- subset(cshapes, admin1name%in%c("Gao", "Kidal", "Mopti", "Ségou", "Tombouctou"))


mw_data_plot <- st_as_sf(mw_data, coords = c('lon', 'lat'),
                       crs = 4326, agr = 'identity')


mw_data_plot <- st_crop(mw_data_plot, sl)

mli <-summarize(sl)

sl_sub2 <- subset(sl, admin3Name%in%c("Bamako","Gao", "Kidal", "Mopti", "Ségou", "Tombouctou"))

north <- sl_sub %>%
  group_by(admin1Name) %>%
  summarize()

north2 <- sl_sub2 %>%
  group_by(admin1Name) %>%
  summarize()
library("ggrepel")


cities.sf <- st_as_sf(cities, coords = c('lon', 'lat'), remove = FALSE, 
                         crs = 4326, agr = 'identity')


library(magrittr) #for the pipe
north2.sf <- north2.sf %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2])
st_coordinates(north2.sf)




mw_data_plot.sf$event_type[(mw_data_plot.sf$event_type)=="qip"] <- "QIPs"
mw_data_plot.sf$event_type[(mw_data_plot.sf$event_type)=="iati"] <- "Aid Projects"

capital2 <- subset(cities, Name=="Bamako")
capital2 <- st_as_sf(capital2, coords = c('lon', 'lat'), remove = FALSE, 
                      crs = 4326, agr = 'identity')



viridis(20, option = "B")
show_col(viridis_pal(option = "B")(20))
pdf('Figures/event_map.pdf', width = 10, height = 10)
,"#08051EFF")
mw_data_plot.sf2 <- subset(mw_data_plot.sf, event_type!="Battles")

ggplot(mw_data_plot.sf2, aes(x=lat, y=lon, group=event_type)) +
  geom_polygon(data=cs_sub, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white") +
 geom_point( aes(shape=event_type, color=event_type), size=2, alpha = .4)+
  scale_shape_manual(values=c(17, 17, 20))+
  scale_color_manual(values=c("#FCA007FF" ,"#190C3EFF", "#C43C4EFF")) +
 # scale_fill_viridis(discrete=TRUE) +
  geom_sf(data = mli, inherit.aes = F, fill = NA, color = 'gray30') +
  geom_sf(data = north, inherit.aes = F, fill = NA, color = 'gray30') +
  geom_sf(data = roads, inherit.aes = F, color = 'gray60', linetype = 'dotted') +
  geom_sf(data = capital2, inherit.aes = F, fill = NA, shape = 18, size = 3,
          stroke = 2, color = 'black') +
  coord_sf(crs = st_crs(mw_data_sf), datum = NA) +
  theme_bw()  +
  labs(x=NULL, y=NULL, caption = "Data Source: ACLED, MINUSMA, IATI") +
  theme(legend.position = 'bottom', legend.title = element_blank(), 
        legend.text = element_text(size=10)) +
  guides(fill = guide_legend(nrow = 1, title.position = 'left'),
         color = guide_legend(nrow = 1, title.position = 'left')) +
  geom_label_repel(data=cities.sf, inherit.aes = F, aes(x = lon, y = lat, label = Name),
                   fontface = "bold", 
                   nudge_x = c(-2, 1, 1, 1, -4, 2.5, -1), nudge_y = c(-1.2,-2, 2.8, -1.5, 1, -1, 2.5)
  ) 

  



#Check again your descriptives with ln
## create renamed dataframes for ggplot graphics
mw_data_sub_gg <- mw_data_sub %>%
  filter(event_type %in% c('QIPs', 'Aid Projects')) %>%
  dplyr::select('ln(Population Density)' = pop_dens,
                'Nightlights' = nl,
                'ln(Distance next UN Base)' = unbase,
                'ln(Road Distance)' = road_dist,
'ln(Water Distance)' = water_dist)

## descriptive statistics histograms
pdf('Figures/desc_hist.pdf', width = 8, height = 6)
ggplot(reshape2::melt(mw_data_sub_gg), aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ variable, scales = 'free') +
  theme_bw()
dev.off()

## correlation plots
#pdf('Figures/RUF_cor.pdf', width = 6, height = 6)
corrplot::corrplot(cor(mw_data_sub_gg)) 
                       #%>% 
                         #dplyr::select(-year)
                         
dev.off()
