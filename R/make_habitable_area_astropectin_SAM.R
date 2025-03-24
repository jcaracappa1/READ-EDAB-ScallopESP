#Script to calculate the habitable area by year within each SAMS
# 
# out.dir = here::here('data','habitat_area')
# file.shp = terra::project(terra::vect(here::here('geometry','MAB_Scallop_Estimation_Areas_2022_80meters.shp'),crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')
# years = 1993:2023
# min.vals = seq(4,7,1)
# SAM.names = grep('_LT80',file.shp$SUBAREA,value =T)

make_habitable_area_astropectin_SAM = function(out.dir,file.shp, SAM.names,years,min.vals){
  
  
  library(dplyr)
  library(terra)
  
  source(here::here('R','make_temp_mask_funs.R'))
  
  names(file.shp)[1] = 'SUBAREA'
  
  SAM.combs = expand.grid(min.vals =min.vals,SAM.names =SAM.names)
  
  SAMs.area.ls = list()
  i=1
  for(i in 1:length(years)){
    
    file.in = paste0('C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_',years[i],'.nc')
    
    file.date = seq.Date(as.Date(paste0(years[i],'-01-01')),as.Date(paste0(years[i],'-12-31')),by = 'd')
    file.date.j = as.numeric(format(file.date,format = '%j'))
    
    year.ls = list()
  
    j=1
    for(j in 1:nrow(SAM.combs)){
      
      SAM.shp = file.shp[which(file.shp$SUBAREA == SAM.combs$SAM.names[j]),]
      
      SAM.mask =make_temp_mask(file.in,
                               SAM.shp,
                               min.val = SAM.combs$min.vals[j],
                               max.val = 30)  
      
      writeCDF(SAM.mask,paste0(out.dir,'/SAMS/astropectin/GLORYS_BT_',SAM.combs$SAM.names[j],'_',SAM.combs$min.vals[i],'_mask_',years[i],'.nc'),overwrite =T)
      
      # SAM.area = make_shp_area(file.in,
      #                          file.shp = SAM.shp,
      #                          units = 'km')
      # 
      # SAM.temp.area = make_shp_temp_area(data = SAM.mask,
      #                                    area.shp = SAM.shp,
      #                                    units = 'km',
      #                                    type = 'ts')
      # 
      # # SAM.temp.prop = SAM.temp.area/SAM.area
      # 
      # year.ls[[j]] = data.frame(year = years[i],
      #            SAM = SAM.combs$SAM.names[j],
      #            min.temp = SAM.combs$min.vals[j],
      #            max.temp = 30,
      #            date = file.date[SAM.temp.area$layer],
      #            area = SAM.temp.area$area,
      #            area.pct = SAM.temp.area$area/SAM.area)
    }
    
    
    SAMs.area.ls[[i]] = bind_rows(year.ls)
    
  }
  
  SAMS.area = dplyr::bind_rows(SAMs.area.ls)
  #   dplyr::select(SAM.names,max.vals,year,area.prop)%>%
  #   dplyr::rename(SAM = 'SAM.names',
  #                 max.temp = 'max.vals',
  #                 prop.habitable = 'area.prop')%>%
  #   arrange(SAM,max.temp,year)
  
  write.csv(SAMS.area,here::here('data','habitat_area','thermal_habitat_area_astropectin_SAMS.csv'),row.names = F)
}