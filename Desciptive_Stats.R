### Figure 1: PKO vs. Aid Workers ----


pko  <- read.csv("Data/PKO/odp_noticas.csv", header=TRUE, stringsAsFactors=FALSE)
awsd  <- read.csv("Data/PKO/security_incidents_2021-06-21.csv", header=TRUE, stringsAsFactors=FALSE)

pko$incidents <- 1
awsd$incidents <- 1

pko$date <- as.POSIXct(as.character(pko$Incident_Date), format="%Y-%m-%d")
dates <- as.POSIXct(as.character(pko$Incident_Date), format="%d-%m-%Y")
pko$year <- year(pko$date) 
pko <- subset(pko, year > 2007)

pko.mis <- pko %>%
  group_by(Mission_Acronym, year) %>% 
  summarise_at(.vars = vars("incidents"),
               .funs=c(incidents="sum"))

pko.mis <- subset(pko.mis, Mission_Acronym %in% c("MINUSMA", "UNAMID", "MONUSCO", 
                                                  "MINUSCA", "MINUSTAH"))

awsd.y <- awsd %>%
  group_by(Country, Year) %>% 
  summarise_at(.vars = vars("incidents"),
               .funs=c(incidents="sum"))
awsd.y <- subset(awsd.y, Country %in% c("Afghanistan ", "South Sudan", "Syrian Arab Republic", 
                                        "Somalia", "Sudan", "DR Congo"))

# Mali separate line for aid worker incidents 
awsd.m <- subset(awsd, Country == "Mali")
awsd.m <- awsd.m %>%
  group_by(Year, Country) %>% 
  summarise_at(.vars = vars("incidents"),
               .funs=c(incidents="sum"))

awsd.m <- subset(awsd.m, Year < 2021)
awsd.m <- subset(awsd.m, Year > 2009)

awsd.c <- awsd %>%
  group_by(Country) %>% 
  summarise_at(.vars = vars("incidents"),
               .funs=c(incidents="sum"))

pko.ma <- subset(pko.mis, Mission_Acronym %in% c("MINUSMA"))


pko.ma2 <- pko.ma %>%
  summarise_at(.vars = vars("incidents"),
               .funs=c(incidents="sum"))


awsd.y <- subset(awsd.y, Year > 2009)
awsd.y <- subset(awsd.y, Year < 2021)
pko.mis <- subset(pko.mis, year < 2021)
awsd.m <- subset(awsd.m, Year > 2009)
pko.mis <- subset(pko.mis, year > 2009)


library(ggthemes)

# Plot all aid workers incidents
p <- ggplot(awsd.y, aes(x = Year, y = incidents, group=Country, linetype=Country)) +
  geom_line() +
  geom_line(aes(x = Year, y = incidents), data=awsd.m, size = 1, color = 'red', linetype=1) + 
  labs(x = NULL,
       y = NULL,
       title = "Aid Worker Incidents" ) + 
  scale_x_continuous(breaks=seq(2007, 2020, 1))+ 
  scale_y_continuous(limits = c(0, 100)) +
  theme_hc()
p 



# Plot all PKO incidents
p1 <- ggplot(pko.mis, aes(x = year, y = incidents, group=Mission_Acronym, linetype=Mission_Acronym)) +
  geom_line() +
  labs(x = NULL,
       y = NULL,
       title = "PKO Incidents"  ) + 
  scale_x_continuous(breaks=seq(2010, 2020, 1))+ 
  theme_hc()

p1 

# Wrap both graphs
library(ggpubr)
ggarrange(p, p1, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)





### Table 2: Conditional Mean Table ----

### Matching Variables
# Stats for discriptive
mw_data_des <- data
mw_data_des_sf <- st_as_sf(mw_data_des, coords = c('lon', 'lat'),
                           crs = 4326, agr = 'identity')

# Calculate new to show table without log
mw_data_des$road_dist <- apply(st_distance(mw_data_des_sf, roads), 1, min)
mw_data_des$water_dist <- apply(st_distance(mw_data_des_sf, water), 1, min)
mw_data_des$pop_dens <- raster::extract(pop_dens,mw_data_des_sf)
mw_data_des$unbase <- apply(st_distance(mw_data_des_sf, unbase), 1, min)



mean(mw_data_des$road_dist)
tab2 <- mw_data_des %>% 
  group_by(event_type) %>% 
  summarise(water_dist_mean = mean(water_dist, na.rm = TRUE),
            road_dist_mean = mean(road_dist, na.rm = TRUE),
            unbase_mean = mean(unbase, na.rm = TRUE),
            pop_dens_mean = mean(pop_dens, na.rm = TRUE),
            nl_mean = mean(nl, na.rm = TRUE))

xtable(tab2)

print(xtable(sum_mw_data_sf_tcm, align = rep('l', ncol(sum_mw_data_sf_tcm) + 1),
             caption = 'Effect of QIPs on violence against civilians. Only space-time windows with statistically significant (p $\\leq .05$) effects are presented.',
             label = 'tab:main'),
      file = 'Tables/sum_mwa_out.tex',
      include.rownames = F, sanitize.text.function = identity)


### Figure 5: Distributions  ----
range(mw_data_sub$timestamp)

mw_data_sub$event_type[(mw_data_sub$event_type)=="qip"] <- "QIPs"
mw_data_sub$event_type[(mw_data_sub$event_type)=="iati"] <- "Aid Projects"

## histrogram of events
ggplot(mw_data_sub, aes(x = timestamp)) +
  geom_histogram(bins=30) +
  facet_wrap(~ event_type) +
  labs(x = '', y = 'Count') +
  theme_bw()




