#Function to generate metrics relating to fall thermal conditions
#A) The number of days per cell in fallexceeding max.temp
#B) The number of consecutive days exceeding max.temp
#C) Degree days over max.temp

library(terra)
library(ggplot2)
library(mapdata)

# file.shp = terra::project(terra::vect(here::here('geometry','MAB_Scallop_Estimation_Areas_2022_80meters.shp'),crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')
file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')
SAM.shp.df = as.data.frame(geom(file.shp))
neus.map = map_data('worldHires',region = c('USA','Canada'))

data.dir = here::here('data','habitat_area','SAMS','scallop','shelf')
figure.dir = here::here('figures','habitat_area','threshold_maps','Fall','')
years = 1993:2022
max.vals = seq(16.5,19,0.5)

combs = expand.grid(max.vals = max.vals, years =years)

fall.nday.all.ls = list()
fall.max.cont.all.ls = list()
i = j =1
for(i in 1:length(max.vals)){

  plot.fall.ndays.ls = list()
  plot.fall.max.cont.ls = list()
  fall.nday.ls = list()
  fall.max.cont.ls = list()
  
  for(j in 1:length(years)){
    
    data = rast(paste0(data.dir,'/GLORYS_BT_SAMS_mask_',max.vals[i],'_',years[j],'.nc'))  
    data.time = as.Date(time(data))
    
    year.dates = seq.Date(as.Date(paste0(years[j],'-01-01'),tz ='UTC'),as.Date(paste0(years[j],'-12-31'),tz = 'UTC'),by = '1 day')
    fall.dates = year.dates[which(year.dates >= paste0(years[j],'-09-01') & year.dates <= paste0(years[j],'-11-30'))]
    which.data.fall = which(data.time %in% fall.dates)
    
    data.fall = subset(data,which.data.fall)
    
    #A) Number of fall days above threshold
    data.fall.binary = (data.fall*0)+1
    
    data.fall.ndays = length(fall.dates)-sum(data.fall.binary,na.rm=T)
    data.fall.ndays.df = as.data.frame(data.fall.ndays,xy =T)
    data.fall.ndays.df$sum[which(data.fall.ndays.df$sum ==0)] =NA
    
    plot.fall.ndays.ls[[j]] = ggplot(data=data.fall.ndays.df, aes(x = x, y = y, fill = sum))+
      geom_tile()+
      scale_fill_gradient(name = 'Days exceeding T_max',low = 'lightblue',high = 'darkblue', na.value = 'grey90', limits = c(0,length(fall.dates)))+
      geom_polygon(data = SAM.shp.df, aes(x = x, y = y, group = geom), color = 'black', fill = NA, size = 0.8)+
      annotation_map(neus.map,fill = 'grey70',color = 'black')+
      # geom_polygon(data = neus.map, aes(x= long, y = lat, group = group),fill = NA, color = 'black')
      coord_equal()+
      theme_bw()+
      ggtitle(paste0('YEAR: ',years[j],', T_max = ',max.vals[i]))+
      theme(legend.position = 'bottom')
    
    fall.nday.ls[[j]] = data.fall.ndays.df%>%
      mutate(year = years[j],
             max.t = max.vals[i])
    
    #B) Number of consecutive days
    data.fall.binary.df = as.data.frame(data.fall.binary,xy =T, time = T, cell = T)%>%
      tidyr::gather(date, mask, -x, -y, - cell) %>%
      mutate(mask = ifelse(is.na(mask),0,mask))
    
    cells = sort(unique(data.fall.binary.df$cell))
    
    cell.cont.max.df = data.frame(cell = cells,year = years[j],max.t = max.vals[i], max.cont.n = NA)
    k=634
    for(k in 1:length(cells)){
      
      data.fall.cell = data.fall.binary.df %>%
        filter(cell == cells[k] & mask ==0)%>%
        arrange(date)
      
      cell.date.j = as.numeric(format(as.Date(data.fall.cell$date),format = '%j'))
      cell.date.split =split(cell.date.j, cummax(c(1,diff(cell.date.j))))
      cell.date.split.n = lapply(cell.date.split,length)
      cell.cont.max.df$max.cont.n[k] = cell.date.split.n[which.max(cell.date.split.n)][[1]]
    }
    
    fall.max.cont.ls[[j]] = cell.cont.max.df
    
    plot.max.cont = cell.cont.max.df %>%
      left_join(as.data.frame(data.fall.binary[[1]],cell = T,xy =T))%>%
      mutate(max.cont.n = ifelse(max.cont.n == 0, NA, max.cont.n))
    
    plot.fall.max.cont.ls[[j]] = ggplot(data=plot.max.cont, aes(x = x, y = y, fill = max.cont.n))+
      geom_tile()+
      scale_fill_gradient(name = 'Max Consecutive Days\n Exceeding T_max',low = 'lightblue',high = 'darkblue', na.value = 'grey90', limits = c(0,length(fall.dates)))+
      geom_polygon(data = SAM.shp.df, aes(x = x, y = y, group = geom), color = 'black', fill = NA, size = 0.8)+
      annotation_map(neus.map,fill = 'grey70',color = 'black')+
      # geom_polygon(data = neus.map, aes(x= long, y = lat, group = group),fill = NA, color = 'black')
      coord_equal()+
      theme_bw()+
      ggtitle(paste0('YEAR: ',years[j],', T_max = ',max.vals[i]))+
      theme(legend.position = 'bottom')
    
  }
  fall.nday.all.ls[[i]] = bind_rows(fall.nday.ls)
  fall.max.cont.all.ls[[i]] = bind_rows(fall.max.cont.ls)
  
  pdf(paste0(figure.dir,'/Days_Exceeding_',max.vals[i],'_scallop_fall.pdf'))
  for(s in 1:length(plot.fall.ndays.ls)){gridExtra::grid.arrange(plot.fall.ndays.ls[[s]])}
  dev.off()
  
  pdf(paste0(figure.dir,'/Continuous_Days_Exceeding_',max.vals[i],'_scallop_fall.pdf'))
  for(s in 1:length(plot.fall.max.cont.ls)){gridExtra::grid.arrange(plot.fall.max.cont.ls[[s]])}
  dev.off()
}

fall.nday.all.df = bind_rows(fall.nday.all.ls)
fall.max.cont.all.df = bind_rows(fall.max.cont.all.ls)

write.csv(fall.nday.all.df,here::here('data','scallop_fall_ndays_exceeding.csv'),row.names =F)
write.csv(fall.max.cont.all.df,here::here('data','scallop_fall_consecutive_ndays_exceeding.csv'),row.names =F)
