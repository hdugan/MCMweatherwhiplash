library(tidyverse)
library(lubridate)
library(MetBrewer)
library(EDIutils)


###### load from EDI ###### 
# High frequency measurements from Lake Bonney
res <- read_data_entity_names(packageId = "knb-lter-mcm.7003.20")
raw <- read_data_entity(packageId = "knb-lter-mcm.7003.20", entityId = res$entityId[1])
# Parse with .csv reader
bon.air <- readr::read_csv(file = raw)

# Package ID: knb-lter-mcm.7010.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: High frequency measurements from Lake Fryxell Meteorological Station (FRLM), McMurdo Dry Valleys, Antarctica (1993-2022, ongoing).
res <- read_data_entity_names(packageId = "knb-lter-mcm.7010.17")
raw <- read_data_entity(packageId = "knb-lter-mcm.7010.17", entityId = res$entityId[1])
frx.air <- readr::read_csv(file = raw)

# High frequency measurements from Lake Vanda Meteorological Station (VAAM), McMurdo Dry Valleys, Antarctica (1994-2022, ongoing)
res <- read_data_entity_names(packageId = "knb-lter-mcm.7015.18")
raw <- read_data_entity(packageId = "knb-lter-mcm.7015.18", entityId = res$entityId[1])
van.air <- readr::read_csv(file = raw)

#############
# Get temperature data
fryxell.temp <- frx.air |> 
  mutate(dateTime = mdy_hm(date_time)) |> 
  filter(year(dateTime) >= 1994) |> 
  mutate(month = month(dateTime), year = year(dateTime)) |> 
  filter(!is.na(airt3m))

#############
# Values above zero 
fr1 = fryxell.temp |> 
  filter(month %in% c(3)) |>
  filter(airt3m >= 0) |> 
  mutate(samyear = `year<-`(dateTime, 2020))

fr2 = fryxell.temp |> 
  filter(month %in% c(3)) |> 
  filter(year %in% fr1$year) |> 
  mutate(samyear = `year<-`(dateTime, 2020))

ggplot(fr1) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_path(data = fr2, aes(x = samyear, y = airt3m, color = as.factor(year)), linewidth = 0.2) +
  geom_path(aes(x = samyear, y = airt3m, color = as.factor(year))) +
  geom_point(aes(x = samyear, y = airt3m, fill = as.factor(year)), shape = 21, stroke = 0.2, size = 1) +
  scale_fill_manual(values=met.brewer("Juarez", 5)) +
  scale_color_manual(values=met.brewer("Juarez", 5)) +
  ylab('Air Temp (°C)') +
  labs(title = 'Lake Fryxell, 1994-2022') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank())

ggsave('figures/Figure_Above0_Mar.png', width = 6, height = 3, dpi = 500)


#############
# Values above zero 
fr1 = fryxell.temp |> 
  filter(month %in% c(2)) |>
  filter(airt3m >= 4) |> 
  mutate(samyear = `year<-`(dateTime, 2020))

fr1.5 = fryxell.temp |> 
  filter(month %in% c(2)) |>
  filter(year %in% fr1$year) |> 
  filter(airt3m >= 0) |> 
  mutate(samyear = `year<-`(dateTime, 2020))
  

fr2 = fryxell.temp |> 
  filter(month %in% c(2)) |> 
  filter(year %in% fr1$year) |> 
  mutate(samyear = `year<-`(dateTime, 2020))

ggplot(fr1.5) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_path(data = fr2, aes(x = samyear, y = airt3m, color = as.factor(year)), linewidth = 0.2) +
  # geom_path(aes(x = samyear, y = airt3m, color = as.factor(year))) +
  geom_point(aes(x = samyear, y = airt3m, fill = as.factor(year)), shape = 21, stroke = 0.2, size = 1) +
  scale_fill_manual(values=met.brewer("Juarez", 5)[c(1,2,3,5)]) +
  scale_color_manual(values=met.brewer("Juarez", 5)[c(1,2,3,5)]) +
  ylab('Air Temp (°C)') +
  labs(title = 'Lake Fryxell, 1994-2022') +
  theme_bw(base_size = 10) +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank())

ggsave('figures/Figure_Above4_Feb.png', width = 6, height = 3, dpi = 500)
