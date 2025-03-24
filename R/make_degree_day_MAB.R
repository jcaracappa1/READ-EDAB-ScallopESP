#Function to calculate degree days within a SAM area

# out.dir = here::here('data')
# out.df.name = 'degree_days_over_threshold_scallop_SAM'
# input.dir = here::here('data','inhabitable_area','SAMS','scallop','/')
# input.prefix = 'GLORYS_BT_'
# file.shp = terra::project(terra::vect(here::here('geometry','MAB_Scallop_Estimation_Areas_2022_80meters.shp'),crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')
# SAM.names = grep('_LT80',file.shp$SUBAREA,value =T)

make_degree_day_MAB = function(input.dir,input.prefix,input.file = NA, file.shp,SAM.names,years,shp.name.var,out.dir,out.df.name,figure.dir,plot = T){
  
  if(!is.na(input.file)){
    files = paste0(input.dir,input.file)
  }else{
    files = list.files(input.dir,input.prefix)  
  }
  
  out.df.ls = list()
  i =1
  for(i in 1:length(files)){
    
    file.min.val = strsplit(files[i],paste0(input.prefix,'|_|.nc'))[[1]][2]
    file.max.val = strsplit(files[i],paste0(input.prefix,'|_|.nc'))[[1]][3]
    file.year = strsplit(files[i],paste0(input.prefix,'|_|.nc'))[[1]]
    file.year = as.numeric(file.year[length(file.year)])
    
    data.file = terra::rast(paste0(input.dir,files[i]))
    
    data.sum = sum(data.file,na.rm=T)
    
    out.df.ls[[i]] = data.frame(
      year = file.year,
      ncell = sum(values(data.sum)*0+1,na.rm=T),
      min.val  = file.min.val,
      max.val = file.max.val,
      dd_mean = mean(values(data.sum),na.rm=T),
      dd_sd = sd(values(data.sum),na.rm=T)
    )
    print(i/length(files))
  }
  out.df = bind_rows(out.df.ls)
  
  write.csv(out.df,paste0(out.dir,out.df.name,'.csv'),row.names =F)
  
  out.lm = lm(dd_mean~year, out.df)
  print(summary(out.lm))
  if(plot == T){
    
    p  = ggplot(out.df, aes( x= year, y = dd_mean,ymin = dd_mean - dd_sd, ymax = dd_mean + dd_sd))+
      geom_ribbon(alpha = 0.5, fill = 'grey80')+
      geom_line()+
      stat_smooth(method = 'lm')+
      ylab('Degree Days over Threshold')+
      xlab('')+
      theme_bw()
    
    if(length(unique(out.df$min.val))>1){
      p + facet_wrap(~min.val)
    }else{
      p
    }
    
    ggsave(paste0(figure.dir,'/',out.df.name,'.png'),width = 8, height = 5, units = 'in', dpi = 350)
  }
}
