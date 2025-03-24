#Script to calculate the habitable area by year within each SAMS

# out.dir = here::here('data','habitat_area')
# input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/'
# input.prefix = 'GLORYS_daily_BottomTemp_'
# file.shp = terra::project(terra::vect(here::here('geometry','Scallop_2024_MAB_Est_Areas_SAMS_CASA_UTM18_EDAB_80meters.shp'),crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')
# file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')
# years = 1993:2023
# min.vals = -5
# max.vals = seq(16,19,0.5)
# SAM.names = grep('_LT80',file.shp$SUBAREA,value =T)
# out.df.name = 'thermal_inhabitable_area_scallop_SAMS'
# j.day.start = as.numeric(format(as.Date('2020-09-01'),format = '%j'))
# j.day.end = as.numeric(format(as.Date('2020-11-30'),format = '%j'))

make_mask_area = function(out.dir,input.dir,input.prefix,file.shp,years,min.vals,max.vals,out.df.name,j.day.start, j.day.end){
  
  library(dplyr)
  library(terra)
  
  source(here::here('R','make_temp_mask_funs.R'))
  
  
  if(!dir.exists(out.dir)){dir.create(out.dir)}
  
  area.ls = list()
  i=1
  for(i in 1:length(years)){
    
    file.in = paste0(input.dir,input.prefix,years[i],'.nc')
    
    
    file.date = seq.Date(as.Date(paste0(years[i],'-01-01')),as.Date(paste0(years[i],'-12-31')),by = 'd')
    file.date.j = as.numeric(format(file.date,format = '%j'))
    
    year.ls = list()
    
    #loop min/max vals
    temp.combs = expand.grid(min.vals = min.vals, max.vals = max.vals)
    j=1
    for(j in 1:nrow(temp.combs)){
      temp.mask =make_temp_mask(file.in,
                                file.shp,
                                min.val = temp.combs$min.vals[j],
                                max.val = temp.combs$max.vals[j],
                                j.day.start = j.day.start,
                                j.day.end = j.day.end)  
      
      # plot(temp.mask)
      
      
      writeCDF(temp.mask,paste0(out.dir,'/GLORYS_BT_',temp.combs$min.vals[j],'_',temp.combs$max.vals[j],'_mask_',years[i],'.nc'),overwrite =T)
      
      area.stat = make_shp_area(file.in,
                                file.shp = file.shp,
                                units = 'km')
      
      temp.area = make_shp_temp_area(data = temp.mask,
                                     area.shp = file.shp,
                                     units = 'km',
                                     type = 'ts')
      
      # SAM.temp.prop = SAM.temp.area/SAM.area
      
      year.ls[[j]] = data.frame(year = years[i],
                                min.temp =  temp.combs$min.vals[j],
                                max.temp = temp.combs$max.vals[j],
                                date = file.date[temp.area$layer],
                                area = temp.area$area,
                                area.pct = temp.area$area/area.stat)
      }  
     
    area.ls[[i]] = bind_rows(year.ls)
    print(years[i])
      
    }
    
    
  area.out = dplyr::bind_rows(area.ls)
  #   dplyr::select(SAM.names,max.vals,year,area.prop)%>%
  #   dplyr::rename(SAM = 'SAM.names',
  #                 max.temp = 'max.vals',
  #                 prop.habitable = 'area.prop')%>%
  #   arrange(SAM,max.temp,year)
  
  write.csv(area.out,paste0(out.dir,out.df.name,'.csv'),row.names = F)
}