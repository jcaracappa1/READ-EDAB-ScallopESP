#Script to calculate the number of days where conditions are outside of habitable range

library(terra)
library(ggplot2)
library(mapdata)
# file.shp = sf::st_read(here::here('geometry','MAB_Estimation_Areas_2022_UTM18_PDT_ET.shp'))
# file.shp2 = sf::st_transform(file.shp,'+proj=longlat +datum=WGS84 +no_defs')
SAM.shp = terra::project(terra::vect(here::here('geometry','MAB_Estimation_Areas_2022_UTM18_PDT_ET.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
SAM.shp.df = as.data.frame(geom(SAM.shp))

neus.map = map_data('worldHires',region = c('USA','Canada'))

SAM.areas = SAM.shp$NewSAMS
data.dir = here::here('data','habitat_area','SAMS','scallop','shelf')

figure.dir = here::here('figures','habitat_area','threshold_maps','')

years = 1993:2023
max.t = seq(16.5,19,0.5)

i=j=1
for(i in 1:length(max.t)){
  
  pdf(paste0(figure.dir,'/Days_Exceeding_',max.t[i],'.pdf'))
  for(j in 1:length(years)){
    
    data.yr = rast(paste0(data.dir,'/GLORYS_BT_SAMS_mask_',max.t[i],'_',years[j],'_scallop.nc'))
    
    # plot(data.yr)
    
    data.binary = (data.yr*0)+1
    
    # plot(data.binary)
    
    data.binary.n = sum(data.binary,na.rm=T)
    
    # plot(data.binary.n)
    
    yr.days = ifelse(years[j]%%4 == 0, 366,365)
  
    data.inhabitable.n = yr.days - data.binary.n
    
    data.inhabitable.n.df = as.data.frame(data.inhabitable.n,xy =T)
    
    data.inhabitable.n.df$sum[which(data.inhabitable.n.df$sum == 0)] = NA
    
    p = ggplot(data=data.inhabitable.n.df, aes(x = x, y = y, fill = sum))+
      geom_tile()+
        scale_fill_gradient(name = 'Days exceeding T_max',low = 'orange',high = 'blue', na.value = 'grey80')+
      geom_polygon(data = SAM.shp.df, aes(x = x, y = y, group = geom), color = 'black', fill = NA, size = 1.2)+
      annotation_map(neus.map,fill = 'grey70',color = 'black')+
      # geom_polygon(data = neus.map, aes(x= long, y = lat, group = group),fill = NA, color = 'black')
      coord_equal()+
      theme_bw()+
      ggtitle(paste0('YEAR: ',years[i],', T_max = ',max.t[i]))+
      theme(legend.position = 'bottom')
   
    gridExtra::grid.arrange(p)   
  }
  dev.off()
}