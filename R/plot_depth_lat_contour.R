#Plot depth and latitude  temperature contours
library(ggplot2)
library(dplyr)
library(terra)
library(mapdata)

years = 1993:2023
file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')
SAM.names = grep('_LT80',file.shp$SUBAREA,value =T)
file.shp.L80 =file.shp[which(file.shp$SUBAREA %in% SAM.names)]

neus.map = map_data('worldHires',region = c('USA','Canada')) %>%
  filter(long >= -77 , long <= -68, lat >=30, lat <=45)

figure.dir = here::here('figures','/')

data.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/'
glorys.prefix = 'GLORYS_daily_BottomTemp_'

bathy = terra::rast('C:/USers/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc', subds = 'deptho')
bathy = terra::mask(terra::crop(bathy,file.shp),file.shp)
# x = ncdf4::nc_open('C:/USers/joseph.caracappa/Documents/Data/GLORYS/GLO-MFC_001_030_mask_bathy.nc')

data.all.ls =list()
data.all.hab.ls = list()

plot.mean.ls = list()
plot.max.ls = list()
plot.mean.hab.ls = list()
plot.max.hab.ls = list()
i=1

max.temp = 18

for(i in 1:length(years)){
  
  data.in = terra::rast(paste0(data.dir,glorys.prefix,years[i],'.nc'))
  
  data.crop = terra::crop(terra::mask(data.in,file.shp),bathy)
  
  data.outer = subset(data.crop,1)
  values(data.outer)[which(!is.na(values(data.outer)))]  = 1
  data.outer = as.polygons(data.outer)
  # data.outer=geom(data.outer)
  
  data.hab = terra::clamp(data.crop,lower = -Inf, upper = max.temp, values = F)
  data.times = time(data.crop)
  
  t=1
  data.t.ls = list()
  data.hab.ls = list()
  for(t in 1:length(data.times)){
    
    data.t = terra::subset(data.crop,t)  
    if(t==1 & i ==1){
      
       depth.int = 5
       lat.min = round(min(values(data.t),na.rm=T),1)
       lat.max = round(max(values(data.t),na.rm=T),1)
       
       depth.min = floor(min(values(bathy),na.rm=T)/depth.int)*depth.int
       depth.max = ceiling(max(values(bathy),na.rm=T)/depth.int)*depth.int
       
       lat.seq = seq(lat.min,lat.max,0.1)
       depth.seq = seq(depth.min,depth.max,depth.int)
       
       grid = expand.grid(lat = lat.seq,depth = depth.seq)
    }
    
    # dat = extract(data.t, y = cells(data.t),xy =T) %>%
    dat = as.data.frame(data.t, xy = T, cells = T)
    colnames(dat) = c('cell','lon','lat','value')
    
    # data.t.ls[[t]]
    dat2 = dat %>%
      mutate(date = data.times[t],
             depth = floor(extract(bathy, cell )[,1]/depth.int)*depth.int,
             lat = round(lat,1))%>%
      group_by(lat,depth,date)%>%
      summarise(value = mean(value,na.rm=T))
    
    data.t.ls[[t]] = dat2 %>%
      left_join(grid)
    
    dat.hab = dat %>%
      # filter(value < max.temp)%>%
      mutate(date = data.times[t],
             depth = floor(extract(bathy, cell )[,1]/depth.int)*depth.int,
             lat = round(lat,1))
    
    data.hab.ls[[t]] = dat.hab %>%
      left_join(grid)
    
  }
  
  data.df = bind_rows(data.t.ls) %>%
    mutate(date = as.Date(date),
           month = format(date,format = "%m"))
  
  data.df.month = data.df %>%
    group_by(month,lat,depth)%>%
    summarise(temp.mean = mean(value,na.rm=T),
              temp.max = max(value,na.rm=T))
  
  data.hab.df = bind_rows(data.hab.ls)%>%
    mutate(date = as.Date(date),
           month = format(date, format = '%m'))
  
  data.hab.month.df = data.hab.df %>%
    group_by(month, lat, lon, cell) %>%
    summarise(temp.mean = mean(value,na.rm=T),
              temp.max = max(value,na.rm=T)) %>%
    mutate(mean.over.max = temp.mean >= max.temp,
           max.over.max = temp.max >= max.temp)%>%
    mutate(month.n = month.name[as.numeric(month)])
  data.hab.month.df$month.n = factor(data.hab.month.df$month.n, levels = month.name)
  
  
  data.all.ls[[i]] = data.df.month
  data.all.hab.ls[[i]] = data.hab.month.df
  
  data.df.month = data.df.month %>%
    mutate(temp.mean2 = ifelse(temp.mean >= max.temp, NA, temp.mean),
           temp.max2 = ifelse(temp.max >= max.temp, NA, temp.max))
  
  plot.mean.ls[[i]] = ggplot(data= data.df.month,aes(x=depth,y=lat,z = temp.mean2, color = temp.mean2))+
    geom_contour_filled()+
    facet_wrap(~month)+
    # scale_fill_viridis_d(name = 'Bottom Temp',option = 'turbo')+
    scale_fill_brewer(name = 'Bottom Temperature (\u00B0C)',direction = -1, palette = 'RdYlBu',na.value = 'black')+
    ggtitle(paste0('Mean Bottom Temperature: ',years[i]))+
    xlab('Depth (m)')+
    ylab('Latitude (\u00B0N)')+
    theme_bw()+
    theme(legend.position = 'bottom')
  
  plot.max.ls[[i]] = ggplot(data= data.df.month,aes(x=depth,y=lat,z = temp.max2, color = temp.max2))+
    geom_contour_filled()+
    facet_wrap(~month)+
    # scale_color_viridis_c(name = 'Bottom Temp')+
    scale_fill_brewer(name = 'Bottom Temperature (\u00B0C)', palette = 'RdYlBu',direction = -1,na.value = 'black')+
    ggtitle(paste0('Maximum Bottom Temperature: ',years[i]))+
    xlab('Depth (m)')+
    ylab('Latitude (\u00B0N)')+
    theme_bw()+
    theme(legend.position = 'bottom')
  
  plot.mean.hab.ls[[i]] = ggplot()+
    annotation_map(neus.map,fill = 'forestgreen',color = 'white')+
    geom_tile(data = data.hab.month.df, aes(x=lon,y=lat,fill = temp.mean))+
    scale_fill_gradient2(name = 'Mean Temperature (\u00B0C)',low = 'white',mid = 'yellow',high = 'red',na.value = 'black', limits = c(4,20))+
    # geom_point(data= filter(data.hab.month.df, mean.over.max == T),aes(x=lon,y=lat),fill = 'black', pch = 21, size = 1.5)+
    facet_wrap(~month.n)+
    coord_sf()+
    ylab('')+
    xlab('')+
    # geom_sf(data=sf::st_as_sf(data.outer),fill = NA, color = 'black')+
    xlim(-75,-71)+
    ggtitle(paste0('Baseline Thermal Habitat: ',years[i]))+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  plot.max.hab.ls[[i]] = ggplot()+
    annotation_map(neus.map,fill = 'forestgreen',color = 'white')+
    geom_tile(data = data.hab.month.df, aes(x=lon,y=lat,fill = temp.max))+
    scale_fill_gradient2(name = 'Maximum Temperature (\u00B0C)',low = 'white',mid = 'yellow',high = 'red',na.value = 'black', limits = c(4,20))+
    # geom_point(data= filter(data.hab.month.df, mean.over.max == T),aes(x=lon,y=lat),fill = 'black', pch = 21, size = 1.5)+
    facet_wrap(~month.n)+
    coord_sf()+
    ylab('')+
    xlab('')+
    # geom_sf(data=sf::st_as_sf(data.outer),fill = NA, color = 'black')+
    xlim(-75,-71)+
    ggtitle(paste0('Unsuitable Thermal Habitat: ',years[i]))+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
}


pdf(paste0(figure.dir,'depth_lat_bottomT_monthly_mean.pdf'),width =8, height =8)
sapply(plot.mean.ls,function(x) gridExtra::grid.arrange(x))
dev.off()

pdf(paste0(figure.dir,'depth_lat_bottomT_monthly_max.pdf'),width =8, height =8)
sapply(plot.max.ls,function(x) gridExtra::grid.arrange(x))
dev.off()

pdf(paste0(figure.dir,'inhabitable_areas_monthly_mean.pdf'),width =12, height =12)
sapply(plot.mean.hab.ls,function(x) gridExtra::grid.arrange(x))
dev.off()

pdf(paste0(figure.dir,'inhabitable_areas_monthly_max.pdf'),width =12, height =12)
sapply(plot.max.hab.ls,function(x) gridExtra::grid.arrange(x))
dev.off()

for(i in 1:length(data.all.ls)){
  data.all.ls[[i]]$year = years[i]
  data.all.hab.ls[[i]]$year = years[i]
}
data.all = bind_rows(data.all.ls)
data.all.hab = bind_rows(data.all.hab.ls)

saveRDS(data.all,here::here('data','GLORYS_lat_depth_bottomT.rds'))
saveRDS(data.all.hab,here::here('data','GLORYS_habitable_area.rds'))

data.all = readRDS(here::here('data','GLORYS_lat_depth_bottomT.rds'))
data.all.hab = readRDS(here::here('data','GLORYS_habitable_area.rds'))

dec.df = data.frame(dec.start = c(1993,2003,2013),
                    dec.end = c(2002,2012,2022))%>%
  tidyr::unite(dec.name, dec.start,dec.end, sep = '-',remove =F)

for(i in 1:nrow(dec.df)){
  
  data.all.hab.dec = data.all.hab %>%
    filter(year >= dec.df$dec.start[i] & year <=dec.df$dec.end[i]) %>%
    group_by(month,lat,lon)%>%
    summarise(temp.mean =mean(temp.mean,na.rm=T),
              temp.max = mean(temp.max,na.rm=T))%>%
    mutate(mean.over.max = temp.mean >= max.temp,
           max.over.max = temp.max >= max.temp)%>%
    mutate(month.n = month.name[as.numeric(month)],
           temp.mean2 = ifelse(mean.over.max == T,NA,temp.mean),
           temp.max2 = ifelse(max.over.max == T,NA,temp.max))
  data.all.hab.dec$month.n = factor(data.all.hab.dec$month.n, levels = month.name)
  
  ggplot()+
    annotation_map(neus.map,fill = 'forestgreen',color = 'white')+
    geom_tile(data = data.all.hab.dec, aes(x=lon,y=lat,fill = temp.mean2))+
    scale_fill_gradient2(name = 'Mean Temperature (\u00B0C)',low = 'white',mid = 'yellow',high = 'red',na.value = 'black', limits = c(4,20))+
    # geom_sf(data=sf::st_as_sf(data.outer),fill = NA, color = 'black')+
    coord_sf()+
    ylab('')+
    xlab('')+
    # geom_point(data= filter(data.all.hab.month, mean.over.max == T),aes(x=lon,y=lat),fill = 'grey20',color = 'black', pch = 22, size = 2)+
    facet_wrap(~month.n)+
    xlim(-75,-71)+
    # ggtitle(paste0('Areas where mean temperature >= 18C: ',years[1],' - ',years[length(years)]))+
    ggtitle(paste0('Unsuitable Thermal Habitat: ',dec.df$dec.start[i],':',dec.df$dec.end[i]))+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  ggsave(paste0(figure.dir,'RTA working paper/GLORYS_Unsuitable_Thermal_Habitat_',dec.df$dec.start[i],'_',dec.df$dec.end[i],'.png'), width = 12, height = 10, units = 'in', dpi = 250)
  
  ggplot()+
    annotation_map(neus.map,fill = 'forestgreen',color = 'white')+
    geom_tile(data = data.all.hab.dec, aes(x=lon,y=lat,fill = temp.max2))+
    scale_fill_gradient2(name = 'Maximum Temperature (\u00B0C)',low = 'white',mid = 'yellow',high = 'red',na.value = 'black', limits = c(4,20))+
    # geom_sf(data=sf::st_as_sf(data.outer),fill = NA, color = 'black')+
    coord_sf()+
    ylab('')+
    xlab('')+
    # geom_point(data= filter(data.all.hab.month, mean.over.max == T),aes(x=lon,y=lat),fill = 'grey20',color = 'black', pch = 22, size = 2)+
    facet_wrap(~month.n)+
    xlim(-75,-71)+
    ggtitle(paste0('Baseline Thermal Habitat: ',dec.df$dec.start[i],':',dec.df$dec.end[i]))+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  ggsave(paste0(figure.dir,'RTA working paper/GLORYS_Baseline_Thermal_Habitat_',dec.df$dec.start[i],'_',dec.df$dec.end[i],'.png'), width = 12, height = 10, units = 'in', dpi = 250)
}

data.all.month = data.all %>%
  group_by(month,lat,depth)%>%
  summarise(temp.mean =mean(temp.mean,na.rm=T),
            temp.max = mean(temp.max,na.rm=T))%>%
  mutate(month.n = month.name[as.numeric(month)])
         # temp.mean2 = ifelse(temp.mean >= max.temp,NA,temp.mean),
         # temp.max2 = ifelse(temp.max >= max.temp,NA,temp.max))
data.all.month$month.n = factor(data.all.month$month.n, levels = month.name)


ggplot(data= data.all.month,aes(x=depth,y=lat,z = temp.mean))+
  geom_contour_filled(breaks = seq(4,20,2),na.rm=F)+
  facet_wrap(~month.n)+
  # scale_fill_viridis_d(name = 'Bottom Temp',option = 'turbo')+
  scale_fill_manual(name = 'Bottom Temperature (\u00B0C)',values = c(rev(RColorBrewer::brewer.pal(7,'RdYlBu')),'black','black'))+
  xlab('Depth (m)')+
  ylab('Latitude (\u00B0N)')+
  ggtitle('Mean Monthly Bottom Temp: 1993-2023')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'GLORYS_lat_depth_bottomT_mean_1993_2023.png'), width = 12, height = 8, units = 'in', dpi = 250)

ggplot(data= data.all.month,aes(x=depth,y=lat,z = temp.max))+
  geom_contour_filled(breaks = seq(4,20,2),na.rm=F)+
  facet_wrap(~month.n)+
  scale_fill_manual(name = 'Bottom Temperature (\u00B0C)',values = c(rev(RColorBrewer::brewer.pal(7,'RdYlBu')),'black','black'))+
  xlab('Depth (m)')+
  ylab('Latitude (\u00B0N)')+
  ggtitle('Max Monthly Bottom Temp: 1993-2023')+
  theme_bw()+
  theme(legend.position = 'bottom')
ggsave(paste0(figure.dir,'GLORYS_lat_depth_bottomT_max_1993_2023.png'), width = 12, height = 8, units = 'in', dpi = 250)


data.all.hab.month = data.all.hab %>%
  group_by(month,lat,lon)%>%
  summarise(temp.mean =mean(temp.mean,na.rm=T),
            temp.max = mean(temp.max,na.rm=T))%>%
  mutate(mean.over.max = temp.mean >= max.temp,
         max.over.max = temp.max >= max.temp)%>%
  mutate(month.n = month.name[as.numeric(month)],
         temp.mean2 = ifelse(mean.over.max == T,NA,temp.mean),
         temp.max2 = ifelse(max.over.max == T,NA,temp.max))
data.all.hab.month$month.n = factor(data.all.hab.month$month.n, levels = month.name)

plot.time.df = data.frame(season = c('Winter','Spring','Summer','Fall'), t0 = c(1,4,7,10), t1 = c(3,6,9,12))

for(i in 1:4){
  
  ggplot()+
    annotation_map(neus.map,fill = 'forestgreen',color = 'white')+
    geom_tile(data = filter(data.all.hab.month, as.numeric(month) %in% plot.time.df$t0[i]:plot.time.df$t1[i]), aes(x=lon,y=lat,fill = temp.mean2))+
    scale_fill_gradient2(name = 'Mean Temperature (\u00B0C)',low = 'white',mid = 'yellow',high = 'red',na.value = 'black', limits = c(4,20))+
    # geom_sf(data=sf::st_as_sf(data.outer),fill = NA, color = 'black')+
    coord_sf()+
    ylab('')+
    xlab('')+
    # geom_point(data= filter(data.all.hab.month, mean.over.max == T),aes(x=lon,y=lat),fill = 'grey20',color = 'black', pch = 22, size = 2)+
    facet_wrap(~month.n)+
    xlim(-75,-71)+
    # ggtitle(paste0('Areas where mean temperature >= 18C: ',years[1],' - ',years[length(years)]))+
    ggtitle(paste0('Unsuitable Thermal Habitat: ',plot.time.df$season[i],' 1993:2023'))+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  
  ggsave(paste0(figure.dir,'RTA working paper/GLORYS_Unsuitable_Thermal_Habitat_',plot.time.df$season[i],'_1993_2023.png'), width = 12, height = 10, units = 'in', dpi = 250)
  
  ggplot()+
    annotation_map(neus.map,fill = 'forestgreen',color = 'white')+
    geom_tile(data = filter(data.all.hab.month, as.numeric(month) %in% plot.time.df$t0[i]:plot.time.df$t1[i]), aes(x=lon,y=lat,fill = temp.max2))+
    scale_fill_gradient2(name = 'Maximum Temperature (\u00B0C)',low = 'white',mid = 'yellow',high = 'red',na.value = 'black', limits = c(4,20))+
    # geom_sf(data=sf::st_as_sf(data.outer),fill = NA, color = 'black')+
    coord_sf()+
    ylab('')+
    xlab('')+
    # geom_point(data= filter(data.all.hab.month, mean.over.max == T),aes(x=lon,y=lat),fill = 'grey20',color = 'black', pch = 22, size = 2)+
    facet_wrap(~month.n)+
    xlim(-75,-71)+
    ggtitle(paste0('Baseline Thermal Habitat: ',plot.time.df$season[i],' 1993:2023'))+
    theme_bw()+
    theme(legend.position = 'bottom',
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
  ggsave(paste0(figure.dir,'RTA working paper/GLORYS_Baseline_Thermal_Habitat_',plot.time.df$season[i],'_1993_2023.png'), width = 12, height = 10, units = 'in', dpi = 250)
  
}
