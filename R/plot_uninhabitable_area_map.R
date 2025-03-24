library(terra)
library(ggplot2)
library(mapdata)

file.shp = file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')
min.vals = 18
max.vals = 50
data.dir = here::here('data','habitat_area','MAB','scallop_annual')
figure.dir = here::here('figures','RTA working paper')
output.dir = here::here('data','inhabitable_days','scallop','')

SAM.shp.df = as.data.frame(geom(file.shp))%>%
  tidyr::unite('group',c(geom,part))
neus.map = map_data('worldHires',region = c('USA','Canada'))

temp.combs = expand.grid(min.vals =min.vals, max.vals = max.vals)

outside = F
years = 1993:2023

data.n.ls = list()

i=j=1
for(i in 1:nrow(temp.combs)){
  
  if(outside){
    name = paste0('Days_Outside_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'.pdf')
  }else{
    name = paste0('Days_Between_',temp.combs$min.vals[i],'_',temp.combs$max.vals[i],'.pdf')
  }
  
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
    
    # writeCDF(data.binary.n,paste0(output.dir,'/',cdf.name),overwrite =T)
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
    
    data.n.df = as.data.frame(data.n,xy =T)%>%
      mutate(year = years[j])
    
    data.n.df$sum[which(data.n.df$sum == 0)] = NA
    
    data.n.ls[[j]] = data.n.df
  }
  
  data.n = bind_rows(data.n.ls)
    
  decade.df = data.frame(start = c(1993,2003,2013),
                         stop =c(2002,2012,2023))
  
  for(d in 1:nrow(decade.df)){
    ggplot(data=filter(data.n, year>=decade.df$start[d]& year<=decade.df$stop[d]), aes(x = x, y = y, fill = sum))+
      geom_tile()+
      scale_fill_gradient(name = 'Days Outiside Thermal Habitat',low = 'orange',high = 'blue', na.value = 'grey80', limits = c(0,100))+
      geom_polygon(data = SAM.shp.df, aes(x = x, y = y, group = group), color = 'black', fill = NA, size = 0.4)+
      annotation_map(neus.map,fill = 'grey70',color = 'black')+
      # geom_polygon(data = neus.map, aes(x= long, y = lat, group = group),fill = NA, color = 'black')
      coord_equal()+
      facet_wrap(~year)+
      theme_bw()+
      xlab('longitude')+
      ylab('latitude')+
      # ggtitle(paste0('YEAR: ',years[j],', ',title))+
      theme(legend.position = 'bottom')
    ggsave(paste0(figure.dir,'/scallop_uninhabitable_area_ndays_',decade.df$start[d],'_',decade.df$stop[d],'.png'),width = 8, height = 6,dpi =250)
    
  }

  data.n %>% group_by(year) %>% summarise(min.n = min(sum),max.n = max(sum))%>%print(n = 31)
    
  }
  
 