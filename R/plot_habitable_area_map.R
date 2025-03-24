#Script to calculate the number of days where conditions are outside of habitable range

# file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')
# data.dir = here::here('data','habitat_area','SAMS','scallop','shelf')
# figure.dir = here::here('figures','habitat_area','threshold_maps','')
# years = 1993:2022
# max.vals = seq(16.5,19,0.5)

plot_habitable_area_map = function(file.shp, years,min.vals,max.vals,data.dir,figure.dir,output.dir,outside =F){
  library(terra)
  library(ggplot2)
  library(mapdata)
  # file.shp = sf::st_read(here::here('geometry','MAB_Estimation_Areas_2022_UTM18_PDT_ET.shp'))
  # file.shp2 = sf::st_transform(file.shp,'+proj=longlat +datum=WGS84 +no_defs')
  # SAM.shp = terra::project(terra::vect(here::here('geometry','MAB_Estimation_Areas_2022_UTM18_PDT_ET.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
  
  if(!dir.exists(figure.dir)){dir.create(figure.dir)}
  if(!dir.exists(output.dir)){dir.create(output.dir)}
  SAM.shp.df = as.data.frame(geom(file.shp))%>%
    tidyr::unite('group',c(geom,part))
  neus.map = map_data('worldHires',region = c('USA','Canada'))
  
  temp.combs = expand.grid(min.vals =min.vals, max.vals = max.vals)
  
  i=j=1
  for(i in 1:nrow(temp.combs)){
    
    if(outside){
      name = paste0('Days_Outside_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'.pdf')
    }else{
      name = paste0('Days_Between_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'.pdf')
    }
    pdf(paste0(figure.dir,'/',name))
    for(j in 1:length(years)){
      
      data.yr = rast(paste0(data.dir,'/GLORYS_BT_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'_mask_',years[j],'.nc'))
      
      # plot(data.yr)
      
      data.binary = (data.yr*0)+1
      
      # plot(data.binary)
      
      data.binary.n = sum(data.binary,na.rm=T)
      
      if(outside==T){
        cdf.name = paste0('days_outside_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'_',years[j],'.nc')
      }else{
        cdf.name = paste0('days_between_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'_',years[j],'.nc')
      }
      
      writeCDF(data.binary.n,paste0(output.dir,'/',cdf.name),overwrite =T)
      # plot(data.binary.n)
      
      # yr.days = ifelse(years[j]%%4 == 0, 366,365)
      yr.days = length(time(data.yr))
      
      if(outside ==T){
        data.n = yr.days - data.binary.n
        axis.name = 'Days Outside\nHabitable Range'
        title = paste0('T < ',temp.combs$min.vals[i],' or T > ',temp.combs$max.vals[i])
      }else{
        data.n = data.binary.n
        axis.name = 'Days Within\nHabitable Range'
        title = paste0(temp.combs$min.vals[i],' < T < ',temp.combs$max.vals[i])
      }
      
      data.n.df = as.data.frame(data.n,xy =T)
      
      data.n.df$sum[which(data.n.df$sum == 0)] = NA
      
      p = ggplot(data=data.n.df, aes(x = x, y = y, fill = sum))+
        geom_tile()+
        scale_fill_gradient(name = axis.name,low = 'orange',high = 'blue', na.value = 'grey80', limits = c(0,120))+
        geom_polygon(data = SAM.shp.df, aes(x = x, y = y, group = group), color = 'black', fill = NA, size = 0.4)+
        annotation_map(neus.map,fill = 'grey70',color = 'black')+
        # geom_polygon(data = neus.map, aes(x= long, y = lat, group = group),fill = NA, color = 'black')
        coord_equal()+
        theme_bw()+
        ggtitle(paste0('YEAR: ',years[j],', ',title))+
        theme(legend.position = 'bottom')
      
      gridExtra::grid.arrange(p)   
    }
    dev.off()
  }  
}
