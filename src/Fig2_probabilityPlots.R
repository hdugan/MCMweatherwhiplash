library(tidyverse)
library(lubridate)
library(patchwork)

################## GET EDI URLS ####################
# Package ID: knb-lter-mcm.7010.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: High frequency measurements from Lake Fryxell Meteorological Station (FRLM), McMurdo Dry Valleys, Antarctica (1993-2022, ongoing).
urlFrx.temp  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/17/1aafff5c9473045c30275cb2c839904d"   #fryxell
urlFrx.wind  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/17/b16d273c65f847ade1e9bad75845327b" 

# Package ID: knb-lter-mcm.7003.20 Cataloging System:https://pasta.edirepository.org.
# Data set title: High frequency measurements from Lake Bonney Meteorological Station (BOYM), McMurdo Dry Valleys, Antarctica (1993-2022, ongoing).
urlBon.temp  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7003/20/b2c8ba65775695b115b3afbbabf45211"  #bonney
urlBon.wind  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7003/20/d7abf1a2a9976be5e83f6b1768555ca7" 

################## AIR TEMP PROBABILITY ####################
makeTempplot <- function(url, title = NULL) {
  infile1 <- tempfile()
  download.file(url,infile1,method="curl")
  
  temp1 <-read_csv(infile1) |> 
    mutate(dateTime = mdy_hm(date_time)) |> 
    filter(year(dateTime) >= 1994)
  
  temp2 = temp1 |> 
    group_by(dateTime = as.Date(dateTime)) |> 
    summarise(temp = max(airt3m)) |> 
    mutate(month = month(dateTime), 
           year = year(dateTime), yday = yday(dateTime)) |> 
    mutate(useDate = `year<-`(dateTime, 2020)) 
  
  temp3 = temp2 |> group_by(yday) |> 
    summarise(p_0deg = pnorm(0, mean= mean(temp, na.rm = T), sd = sd(temp, na.rm = T), lower.tail = FALSE),
              p_4deg = pnorm(4, mean= mean(temp, na.rm = T), sd = sd(temp, na.rm = T), lower.tail = FALSE),
              p_7deg = pnorm(7, mean= mean(temp, na.rm = T), sd = sd(temp, na.rm = T), lower.tail = FALSE)) |> 
    pivot_longer(cols = 2:4, names_to = 'Temp')
  
  p3 = ggplot(temp3) +
    geom_ribbon(aes(x = as.Date(yday, origin = as.Date('2020-01-01')), ymin = 0, ymax = value, fill = Temp)) +
    geom_vline(aes(xintercept = as.Date('2020-03-18')), linetype = 2) +
    scale_fill_manual(values = c('#132E32', '#176087', '#53A2BE')) +
    ylab('Prob. daily max temp >\ngiven degree (°C)') +
    scale_x_date(date_labels = '%b') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(), 
          legend.position = 'bottom', 
          legend.key.size = unit(0.2, 'cm'))
  
  return(p3)
}


################## WIND SPEED PROBABILITY ####################
makeWindplot <- function(url, title = NULL) {
  infile6 <- tempfile()
  download.file(url,infile1,method="curl")
  
  wind1 <-read_csv(infile1) |> 
    mutate(dateTime = mdy_hm(date_time)) |> 
    filter(year(dateTime) >= 1994)
  
  wind2 = wind1 |> 
    group_by(dateTime = as.Date(dateTime)) |> 
    summarise(wspd = log(max(wspd))) |> 
    mutate(month = month(dateTime), 
           year = year(dateTime), yday = yday(dateTime)) |> 
    mutate(useDate = `year<-`(dateTime, 2020)) 
  
  wind3 = wind2 |> group_by(yday) |> 
    summarise(p_02 = pnorm(log(2), mean= mean(wspd, na.rm = T), sd = sd(wspd, na.rm = T), lower.tail = FALSE),
              p_05 = pnorm(log(5), mean= mean(wspd, na.rm = T), sd = sd(wspd, na.rm = T), lower.tail = FALSE),
              p_10 = pnorm(log(10), mean= mean(wspd, na.rm = T), sd = sd(wspd, na.rm = T), lower.tail = FALSE)) |> 
    pivot_longer(cols = 2:4, names_to = 'WindSpeed')
  
  p4 = ggplot(wind3) +
    geom_ribbon(aes(x = as.Date(yday, origin = as.Date('2020-01-01')), ymin = 0, ymax = value, fill = WindSpeed)) +
    geom_vline(aes(xintercept = as.Date('2020-03-18')), linetype = 2) +
    # scale_fill_manual(values = c('black', 'lightblue4', 'gold')) +
    scale_fill_manual(values = c('#420C14', '#732C2C', '#ABA361')) +
    ylab('Prob. daily max wind speed >\ngiven speed (m/s)') +
    scale_x_date(date_labels = '%b') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(), 
          legend.position = 'bottom', 
          legend.key.size = unit(0.2, 'cm'))
  
  return(p4)
}

################## Make and Join Figures ####################

p1 = makeTempplot(url = urlBon.temp)
p2 = makeTempplot(url = urlBon.temp)
p3 = makeWindplot(url = urlBon.wind)
p4 = makeWindplot(url = urlFrx.wind)

(p1 + p2 + plot_layout(guides = 'collect')) / (p3 + p4 + plot_layout(guides = 'collect')) + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag = element_text(size  = 8), legend.position = 'bottom', 
        legend.margin=margin(0,0,0,0))
  

ggsave('figures/Figure2.png', width = 6, height = 5)
 
