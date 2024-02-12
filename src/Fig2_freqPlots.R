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
    mutate(useDate = `year<-`(dateTime, 2020)) |> 
    filter(yday != 366)
  
  # ggplot(temp2 |> filter(month %in% c(3:5))) +
  #   geom_density(aes(x = temp, color = month, group = month))
  
  output = tibble(yday = 1:365, rangelow = NA, rangehigh = NA, 
                  p_0deg = NA, p_4deg = NA, p_7deg = NA)
  for (i in 1:365) {
    print(i)
    low = i - 7
    high = i + 7
    
    if(low >= 0 & high <= 365) {
      usetemp = temp2 |> filter(yday >= low & yday <= high)
    } else if(low < 0) {
      low = low + 365
      usetemp = temp2 |> filter(yday >= low | yday <= high)
    } else if(high > 365) {
      high = high - 365
      usetemp = temp2 |> filter(yday >= low | yday <= high)
    }
    
    usetemp = usetemp |> 
      arrange(yday) |> 
      filter(!is.na(temp))
    
    output[i,]$rangelow = low
    output[i,]$rangehigh = high
    output[i,]$p_0deg = nrow(filter(usetemp, temp >= 0))/nrow(usetemp)
    output[i,]$p_4deg = nrow(filter(usetemp, temp >= 4))/nrow(usetemp)
    output[i,]$p_7deg = nrow(filter(usetemp, temp >= 7))/nrow(usetemp)
  }
  
  # Calculate probability 
  temp3 = output |> 
    pivot_longer(cols = 4:6, names_to = 'Temp')
  
  p3 = ggplot(temp3) +
    geom_ribbon(aes(x = as.Date(yday, origin = as.Date('2020-01-01')), ymin = 0, ymax = value, fill = Temp)) +
    geom_vline(aes(xintercept = as.Date('2020-03-18')), linetype = 2) +
    scale_fill_manual(values = c('#132E32', '#176087', '#53A2BE'), 
                                 labels = c('0°C','4°C','7°C')) +
    ylab('Frequency days daily max\ntemp > given degree (°C)') +
    ylim(0,0.85) +
    scale_x_date(date_labels = '%b') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(), 
          legend.position = 'bottom', 
          legend.key.size = unit(0.2, 'cm'));
  
  return(p3)
}


################## WIND SPEED PROBABILITY ####################
makeWindplot <- function(url, title = NULL) {
  infile6 <- tempfile()
  download.file(url,infile6,method="curl")
  
  wind1 <-read_csv(infile6) |> 
    mutate(dateTime = mdy_hm(date_time)) |> 
    filter(year(dateTime) >= 1994)
  
  wind2 = wind1 |> 
    group_by(dateTime = as.Date(dateTime)) |> 
    summarise(wspd = max(wspd)) |> 
    mutate(month = month(dateTime), 
           year = year(dateTime), yday = yday(dateTime)) |> 
    mutate(useDate = `year<-`(dateTime, 2020)) 
  
  output = tibble(yday = 1:365, rangelow = NA, rangehigh = NA, 
                  p_02 = NA, p_05 = NA, p_10 = NA)
  for (i in 1:365) {
    print(i)
    low = i - 7
    high = i + 7
    
    if(low >= 0 & high <= 365) {
      usewind = wind2 |> filter(yday >= low & yday <= high)
    } else if(low < 0) {
      low = low + 365
      usewind = wind2 |> filter(yday >= low | yday <= high)
    } else if(high > 365) {
      high = high - 365
      usewind = wind2 |> filter(yday >= low | yday <= high)
    }
    
    output[i,]$rangelow = low
    output[i,]$rangehigh = high
    output[i,]$p_02 = nrow(filter(usewind, wspd >= 2))/nrow(usewind)
    output[i,]$p_05 = nrow(filter(usewind, wspd >= 5))/nrow(usewind)
    output[i,]$p_10 = nrow(filter(usewind, wspd >= 10))/nrow(usewind)
  }
  
  # Calculate probability 
  wind3 = output |> 
    pivot_longer(cols = 4:6, names_to = 'WindSpeed')
  
  p4 = ggplot(wind3) +
    geom_ribbon(aes(x = as.Date(yday, origin = as.Date('2020-01-01')), ymin = 0, ymax = value, fill = WindSpeed)) +
    geom_vline(aes(xintercept = as.Date('2020-03-18')), linetype = 2) +
    # scale_fill_manual(values = c('black', 'lightblue4', 'gold')) +
    scale_fill_manual(values = c('#420C14', '#732C2C', '#ABA361'),
                      labels = c('2 m/s','5 m/s','10 m/s')) +
    ylab('Frequency days daily max wind\nspeed > given speed (m/s)') +
    scale_x_date(date_labels = '%b') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(), 
          legend.position = 'bottom', 
          legend.key.size = unit(0.2, 'cm'))
  
  return(p4)
}

################## Make and Join Figures ####################

p1 = makeTempplot(url = urlFrx.temp)
p2 = makeTempplot(url = urlBon.temp)
p3 = makeWindplot(url = urlFrx.wind)
p4 = makeWindplot(url = urlBon.wind)

(p1 + p2 + plot_layout(guides = 'collect')) / (p3 + p4 + plot_layout(guides = 'collect')) + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag = element_text(size  = 8), legend.position = 'bottom', 
        legend.margin=margin(0,0,0,0))


ggsave('figures/Figure2_freq.png', width = 6, height = 5, dpi = 500)

