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
  
  # Test normality histogram
  temp2 |> 
    ggplot() +
    geom_histogram(aes(x = temp)) +
    facet_wrap(~month)
  
  # Perform a Shapiro-Wilk Test by goup
  SWresults = temp2 %>%
    group_by(month) %>%
    summarise(statistic = shapiro.test(temp)$statistic,
              p.value = shapiro.test(temp)$p.value)
  
  print(SWresults)
  
  test = temp2 |> filter(month == 3) |> 
    mutate(temp = temp + 100) |> 
    mutate(x = temp)
  
  library(gamlss)
  fit <- fitDist(test$temp, k = 2, type = "realAll", trace = FALSE, try.gamlss = TRUE)
  fit <- fitDist(temp2$temp, k = 2, type = "realAll", trace = FALSE, try.gamlss = TRUE)
  fit$fits
  summary(fit)
  
  # Skew Normal Type 2 distribution
  SN2()
  sn2.fit = gamlssML(temp2$temp, family=SN2)
  sn2.fit$mu
  
  getFit <- function(x) {
    fit <- fitDist(x$temp, k = 2, type = "realAll", trace = FALSE, try.gamlss = TRUE)
    # gamlss(temp~pb(yday), family = "SN2", data = x, method = mixed())
    return(fit)
  }
  
  # getFit <- function(x) {
  #   usedata = usetemp |> 
  #     mutate(x = row_number(), y = temp)
  #   # gamlssML(x, family=SN2)
  #   gamlss(y, family = SN2, data = usedata)
  # }
  output = tibble(yday = 1:365, rangelow = NA, rangehigh = NA, 
                      fitfamily = NA,
                      p_0deg = NA, p_4deg = NA, p_7deg = NA)
  for (i in 1:365) {
    print(i)
    low = i - 14
    high = i + 14
    
    if(low >= 0 & high <= 365) {
      usetemp = temp2 |> filter(yday > low & yday < high)
    } else if(low < 0) {
      low = low + 365
      usetemp = temp2 |> filter(yday > low | yday < high)
    } else if(high > 365) {
      high = high - 365
      usetemp = temp2 |> filter(yday > low | yday < high)
    }
    
    usetemp = usetemp |> 
      arrange(yday) |> 
      filter(!is.na(temp))
    
    nrow(filter(usetemp, temp >= 0))
    
    
    useFit = getFit(usetemp)
    
    output[i,]$rangelow = low
    output[i,]$rangehigh = high
    
    output[i,]$fitfamily = useFit$family[1]

    # output[i,]$p_0deg = pSN2(q = 0, 
    #      mu = useFit$mu.coefficients[2], 
    #      sigma = exp(useFit$sigma.coefficients), 
    #      nu =  exp(useFit$nu.coefficients), 
    #      lower.tail = FALSE)
    # output[i,]$p_4deg = pSN2(q = 4, 
    #                          mu = useFit$mu.coefficients[2], 
    #                          sigma = exp(useFit$sigma.coefficients), 
    #                          nu =  exp(useFit$nu.coefficients), 
    #                          lower.tail = FALSE)
    # output[i,]$p_7deg = pSN2(q = 7, 
    #                          mu = useFit$mu.coefficients[2], 
    #                          sigma = exp(useFit$sigma.coefficients), 
    #                          nu =  exp(useFit$nu.coefficients), 
    #                          lower.tail = FALSE)
  }
  
  output = output |> 
    mutate(p_0deg = pSN2(q = 0, mu = mu, sigma = sigma, nu = nu, lower.tail = FALSE)) |> 
    mutate(p_4deg = pSN2(q = 4, mu = mu, sigma = sigma, nu = nu, lower.tail = FALSE)) |> 
    mutate(p_7deg = pSN2(q = 7, mu = mu, sigma = sigma, nu = nu, lower.tail = FALSE))
  
  
  temp2 |> group_by(month) |> 
    gamlssML(family = 'SN2')
  
  pSN2(q = 100, mu = 86.6, sigma = exp(1.84), nu = exp(-0.2333)) 
  
  data(test)
  h<-gamlss(y~pb(x), sigma.formula=~pb(x), family=BCT, data=test) # fits 
  pdf.plot(obj=h , obs=c(100,23), min=0, max=150)
  
  # Calculate probability 
  temp3 = temp2 |> group_by(yday) |> 
    summarise(p_0deg = pnorm(0, mean= mean(temp, na.rm = T), sd = sd(temp, na.rm = T), lower.tail = FALSE),
              p_4deg = pnorm(4, mean= mean(temp, na.rm = T), sd = sd(temp, na.rm = T), lower.tail = FALSE),
              p_7deg = pnorm(7, mean= mean(temp, na.rm = T), sd = sd(temp, na.rm = T), lower.tail = FALSE)) |> 
    pivot_longer(cols = 2:4, names_to = 'Temp')
  
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
  download.file(url,infile6,method="curl")
  
  wind1 <-read_csv(infile6) |> 
    mutate(dateTime = mdy_hm(date_time)) |> 
    filter(year(dateTime) >= 1994)
  
  wind2 = wind1 |> 
    group_by(dateTime = as.Date(dateTime)) |> 
    summarise(wspd = log(max(wspd))) |> 
    mutate(month = month(dateTime), 
           year = year(dateTime), yday = yday(dateTime)) |> 
    mutate(useDate = `year<-`(dateTime, 2020)) 
  
  # Test normality histogram
  wind2 |> 
    ggplot() +
    geom_histogram(aes(x = wspd)) +
    facet_wrap(~month)
  
  # Perform a Shapiro-Wilk Test by goup
  SWresults = wind2 %>%
    group_by(month) %>%
    summarise(statistic = shapiro.test(wspd)$statistic,
              p.value = shapiro.test(wspd)$p.value)
  
  print(SWresults)
  
  # Calculate probability
  wind3 = wind2 |> group_by(yday) |> 
    summarise(p_02 = pnorm(log(2), mean = mean(wspd, na.rm = T), sd = sd(wspd, na.rm = T), lower.tail = FALSE),
              p_05 = pnorm(log(5), mean = mean(wspd, na.rm = T), sd = sd(wspd, na.rm = T), lower.tail = FALSE),
              p_10 = pnorm(log(10), mean = mean(wspd, na.rm = T), sd = sd(wspd, na.rm = T), lower.tail = FALSE)) |> 
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

p1 = makeTempplot(url = urlFrx.temp)
p2 = makeTempplot(url = urlBon.temp)
p3 = makeWindplot(url = urlFrx.wind)
p4 = makeWindplot(url = urlBon.wind)

(p1 + p2 + plot_layout(guides = 'collect')) / (p3 + p4 + plot_layout(guides = 'collect')) + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(plot.tag = element_text(size  = 8), legend.position = 'bottom', 
        legend.margin=margin(0,0,0,0))


ggsave('figures/Figure2.png', width = 6, height = 5)

