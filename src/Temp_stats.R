# Package ID: knb-lter-mcm.7010.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: High frequency measurements from Lake Fryxell Meteorological Station (FRLM), McMurdo Dry Valleys, Antarctica (1993-2022, ongoing).
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/17/1aafff5c9473045c30275cb2c839904d"   #fryxell
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")


getTempStats <- function(infile1) {
  
  # Get temperature data, convert to daily mean since timestep changes
  fryxell.temp <- read_csv(infile1, show_col_types = FALSE) |> 
    mutate(dateTime = mdy_hm(date_time)) |> 
    filter(year(dateTime) >= 1994) |> 
    group_by(date = as.Date(dateTime)) |> 
    summarise(airt3m = mean(airt3m)) |> 
    mutate(month = month(date), year = year(date)) |> 
    filter(!is.na(airt3m))
    
  #############
  # Check on NA data
  frx.years = fryxell.temp |> 
    filter(month %in% c(11,12,1,2)) |> 
    group_by(year) |> 
    summarise(n = n()) 
  table(frx.years$n)
    
  # (1) Mean summer temp
  frx1 = fryxell.temp |> 
    filter(month %in% c(11,12,1,2)) |>
    group_by(year) |> 
    mutate(n = n()) |> 
    filter(n >= 118) |> 
    ungroup() |> 
    summarise(temp = mean(airt3m, na.rm = T))
  
  print(paste0('Mean summer temp = ', round(frx1$temp, 2)))
  
  #############
  # Check on NA data
  frx.years = fryxell.temp |> 
    filter(month %in% c(3)) |> 
    group_by(year) |> 
    summarise(n = n()) 
  table(frx.years$n)
  
  # (2) Mean March temp
  frx2 = fryxell.temp |> 
    filter(month %in% c(3)) |>
    group_by(year) |> 
    mutate(n = n()) |> 
    filter(n >= 31) |> 
    ungroup() |> 
    summarise(temp = mean(airt3m, na.rm = T))
  
  print(paste0('Mean March temp = ', round(frx2$temp, 2)))
  
  #############
  # (3) Mean March 18 temp
  frx3 = fryxell.temp |> 
    filter(format(date,"%m-%d") == '03-18') |>
    summarise(temp = mean(airt3m, na.rm = T))
  
  print(paste0('Mean March 18 temp = ', round(frx3$temp, 2)))
  
  #############
  # (4) SD March 18 temp
  frx4 = fryxell.temp |> 
    filter(format(date,"%m-%d") == '03-18') |>
    summarise(sd = sd(airt3m, na.rm = T))
  
  print(paste0('SD March 18 temp = ', round(frx4$sd, 2)))
  
  #############
  # (5) SD March 18 temp
  frx5 = read_csv(infile1, show_col_types = FALSE) |> 
    mutate(dateTime = mdy_hm(date_time)) |> 
    filter(as.Date(dateTime) == as.Date('2022-03-18')) |>
    summarise(temp.mean = mean(airt3m, na.rm = T), 
              temp.max = max(airt3m, na.rm = T))
  
  print(paste0('Mean March 18 2022 temp = ', round(frx5$temp.mean, 2)))
  print(paste0('Max March 18 2022 temp = ', round(frx5$temp.max, 2)))
  print(paste0('deltaT March 18 2022 temp = ', round(frx5$temp.max, 2) - round(frx3$temp, 2)))
  print(paste0('deltaT March 18 2022 temp = ', round((frx5$temp.max - frx3$temp)/frx4$sd,2)))
}

getTempStats(infile1)
