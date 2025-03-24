#Script to calculate the habitable area across all MAB habitat
library(dplyr)
library(terra)

out.dir = here::here('data','habitat_area')

# file.shp = terra::project(terra::vect(here::here('geometry','MAB_Estimation_Areas_2022_UTM18_PDT_ET.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')

source(here::here('R','make_temp_mask_funs.R'))

years = 1993:2023

# min.vals = seq(5,7,0.5)
# max.vals = seq(16,19,0.5)
max.vals = 18

year.area.ls = list()
i=1
for(i in 1:length(years)){
  
  file.in = paste0('C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_',years[i],'.nc')
  
  file.date = seq.Date(as.Date(paste0(years[i],'-01-01')),as.Date(paste0(years[i],'-12-31')),by = 'd')
  file.date.j = as.numeric(format(file.date,format = '%j'))
  
  temp.ls = list()
  
  j=1
  for(j in 1:length(max.vals)){
    
    SAM.mask =make_temp_mask(file.in,
                             file.shp,
                             min.val = 0,
                             max.val = max.vals[j])  
    
    writeCDF(SAM.mask,paste0(out.dir,'/SAMS/scallop/shelf/GLORYS_BT_SAMS_mask_',max.vals[j],'_',years[i],'.nc'),overwrite =T)
    
    SAM.area = make_shp_area(file.in,
                             file.shp = file.shp,
                             units = 'km')

    SAM.temp.area = make_shp_temp_area(data = SAM.mask,
                                       area.shp = file.shp,
                                       units = 'km',
                                       type = 'ts')

    # SAM.temp.prop = SAM.temp.area/SAM.area

    temp.ls[[j]] = data.frame(year = years[i],
                              SAM = 'SAMS_all',
                              min.temp = 0,
                              max.temp = max.vals[j],
                              date = file.date[SAM.temp.area$layer],
                              area = SAM.temp.area$area,
                              area.pct = SAM.temp.area$area/SAM.area)
  }
  
  
  year.area.ls[[i]] = bind_rows(temp.ls)
  
}

SAMS.area = dplyr::bind_rows(year.area.ls)
#   dplyr::select(SAM.names,max.vals,year,area.prop)%>%
#   dplyr::rename(SAM = 'SAM.names',
#                 max.temp = 'max.vals',
#                 prop.habitable = 'area.prop')%>%
#   arrange(SAM,max.temp,year)

write.csv(SAMS.area,here::here('data','habitat_area','thermal_habitat_area_scallop_SAMS_combined.csv'),row.names = F)
