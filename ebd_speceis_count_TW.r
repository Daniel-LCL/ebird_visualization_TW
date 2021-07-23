# ebd testing
library(magrittr)
library(sf)
library(dplyr)
library(data.table)
ebd_sf <- 
  fread('ebd_TW_relJun-2021.txt', encoding = 'UTF8') %>% 
  st_read() %>% 
  st_set_crs(4326, coords = c('LONGITUDE', 'LATITUDE')) %>% 
  st_transform(3826)

towns <- fread('//10.40.2.146/gis/Taiwan_Geo/鄉鎮世界/TOWN_MOI_1060525.shp')
counties <- fread('//10.40.2.146/gis/Taiwan_Geo/縣市界(經緯度)_內政部2014/county.shp')

