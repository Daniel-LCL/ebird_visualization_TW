# ebd testing
library(magrittr)
library(sf)
library(dplyr)
library(data.table)
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) #Map Data
library(rebird) #eBird API Client
library(tidyverse) #Data Processing
library(ggplot2) #Plotting
library(extrafont) #Plotting
library(scico) #Plotting

ebd_sf <- 
  fread('C:/Users/user/Desktop/TESRI_project_lu/others/ebd_practicing/ebd_TW_relJun-2021.txt', encoding = 'UTF-8') %>% 
  st_read() %>% 
  st_set_crs(4326, coords = c('LONGITUDE', 'LATITUDE')) %>% 
  st_transform(3826)

towns_sf <- st_read('//10.40.2.146/gis/Taiwan_Geo/鄉鎮市界/TOWN_MOI_1060525.shp')
counties_sf <- st_read('//10.40.2.146/gis/Taiwan_Geo/縣市界(經緯度)_內政部2014/county.shp')
towns_sf <- st_crop(towns_sf, xmin = 119.710605, ymin = 21.782934, xmax = 122.381709, ymax = 25.287902)
counties_sf <- st_crop(counties_sf, st_bbox(towns_sf))
plot(counties_sf$geometry)

eBird_API_Key <- "351hfslcr2sb" #Key can be obtained at https://ebird.org/api/keygen

#Here we're going to run our first api request, this will return a list of all the US Counties in eBird.
counties <- ebirdsubregionlist("subnational1", "TW", key = eBird_API_Key)
counties <- counties[c(-8, -18, -20),] # 移除直轄市修正後的縣
#Next, were just adding a new column in counties_sf (the county polygon tibble) to match the eBird county codes so we can merge the two later.
#counties_sf$merge <- paste("TW", counties_sf$state_abbv, substr(counties_sf$county_fips, 3, 5), sep = "-")
counties_sf <- left_join(counties_sf, counties, by = c('E_NAME' = 'name'))

# The next line is critical, however, due to the heavy use of the eBird API, 
# please limit the use of this loop as much as possible. You should only need to run it once.
for(i in 1:nrow(counties)){
  counties$Species[i] <- nrow(ebirdregionspecies(counties$code[i], key = eBird_API_Key))
}

# # This next loop is due to name change of this county, we need to revise the county FIPS code.
# for(i in 1:nrow(counties)){
#   if(counties$name[i] == "Oglala Lakota"){
#     counties$code[i] <- "US-SD-102"
#   }
# } 

# Now, the moment of truth, this line is going to join the counties and counties_sf data frames so we can plot our data.
counties_eBird <- inner_join(counties_sf, 
                             dplyr::group_by(counties, code) %>% summarise(Species=max(Species)), 
                             by=c("code"))
# Now the plot
counties_eBird
ggplot(counties_eBird) + geom_sf(aes(fill = Species), size = 0, alpha = 0.95)

p <- ggplot(counties_eBird) +
  # We're going to use geom_polygon to render our county shapes, geom_sf may also work, but I've found this to be easier.
  geom_sf(aes(fill = Species), size=0, alpha=0.95) +
  # Adding theme_void is essental for choropleth maps in ggplot2 as we don't need gridlines.
  theme_void() +
  # This will give us the legend and color palette.
  scale_fill_scico(palette = "bilbao", direction = 1, name="Bird Species", guide = guide_colourbar(direction = "horizontal", label.position = "bottom", title.position = 'top', nrow=1)) +
  # Adding labels
  labs(
    title = "Bird Species by County"#,
    # subtitle = "Number of Bird Species Observed on eBird by TW County",
    # caption = "Data: eBird & U.S. Census Bureau | Created by Oliver Burrus | https://tinyurl.com/2bskxuzf   "
  ) +
  # Adding the final touches
  theme(
    text = element_text(color = "#22211d", family="Maiandra GD"),
    plot.background = element_rect(fill = "#d1cec5", color = NA),
    panel.background = element_rect(fill = "#d1cec5", color = NA),
    legend.background = element_rect(fill = "#d1cec5", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.03, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.05, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text(size=10, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.8, 0.9))
  # ) +
  # # We need to finally add coord_map to maintain the aspect ratio of the polygons.
  # coord_map()
p
