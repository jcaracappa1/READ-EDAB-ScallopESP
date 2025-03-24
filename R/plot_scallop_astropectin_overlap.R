#Script to plot the habitable area for scallops 
library(terra)

file.shp = terra::project(terra::vect(here::here('geometry','MAB_Estimation_Areas_2022_UTM18_PDT_ET.shp')),' +proj=longlat +datum=WGS84 +no_defs ')

data.dir = here::here('data','SAMS','scallop')

years = 1994:2023
max.t = seq(16,19,0.5)

for(i in 1:length(max.t)){
  
  for(j in 1:length(years)){
    
    data.yr = rast(paste0(data.dir,'/GLORYS_BT_',max.t[i],'_mask_',years[j],'.nc'))
    
    
  }
}