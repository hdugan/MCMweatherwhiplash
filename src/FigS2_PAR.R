library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

################## GET EDI URLS ####################
# Package ID: knb-lter-mcm.7010.16 Cataloging System:https://pasta.edirepository.org.
# Data set title: High frequency measurements from Lake Fryxell Meteorological Station (FRLM), McMurdo Dry Valleys, Antarctica (1993-2021, ongoing).
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/16/1b7b71ad5c135e42783e6a83f8a14b27" 
infile2 <- tempfile()
download.file(inUrl2,infile2,method="curl")

dt2 <- read_csv(infile2) |> 
  mutate(dateTime = mdy_hm(date_time))

head(dt2)

############ Average PAR ############
dt3 = dt2 |> 
  filter(year(dateTime) >= 1994) |> 
  mutate(year = year(dateTime), yday = yday(dateTime)) |> 
  filter(yday != 366) |> 
  group_by(yday) |> 
  summarise(par.mean = mean(par, na.rm = T), par.min = min(par, na.rm = T), par.max = max(par, na.rm = T), 
            dateTime = as.Date(first(dateTime)))

p1 = ggplot(dt3) +
  geom_ribbon(aes(x = dateTime, ymin = par.min, ymax = par.max), fill = 'gold3') +
  geom_path(aes(x = dateTime, y = par.mean)) +
  geom_vline(aes(xintercept = as.Date('1994-03-18')), linetype = 2) +
  ylab("PAR"~(µmol~s^-1~m^-2)) +
  scale_x_date(date_labels = '%b') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

############ Hours a day ############
dt4 = dt2 |> 
  filter(year(dateTime) >= 1994) |> 
  mutate(year = year(dateTime), yday = yday(dateTime), hour = hour(dateTime)) |> 
  filter(yday != 366) |> 
  group_by(year, yday, hour) |> 
  summarise(par.mean = mean(par, na.rm = T), dateTime = first(dateTime))

dt5 = dt4 |> mutate(up = if_else(par.mean > 0, TRUE, FALSE)) |> 
  group_by(year, yday) |> 
  mutate(up = sum(up, na.rm = T), tot = n()) |> 
  ungroup() |> 
  filter(tot == 24)

sun = dt5 |> group_by(yday) |> 
  summarise(up = median(up))

p2 = ggplot(sun) +
  geom_col(aes(x = as.Date(yday, origin = as.Date('2020-01-01')), y = up), fill = 'gold3') +
  geom_vline(aes(xintercept = as.Date('2020-03-18')), linetype = 2) +
  ylab('Hours of the day with PAR') +
  scale_x_date(date_labels = '%b') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

######### Join Figures ######### 
p1 + p2 + plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag = element_text(size  = 8))

ggsave('figures/FigS2_LF_PAR.png', width = 6, height = 2.5, dpi = 500)
 
