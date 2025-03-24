#Function to calculate degree days within a SAM area

# out.dir = here::here('data')
# out.df.name = 'degree_days_over_threshold_scallop_SAM'
# input.dir = here::here('data','inhabitable_area','SAMS','scallop','/')
# input.prefix = 'GLORYS_BT_'
# file.shp = terra::project(terra::vect(here::here('geometry','MAB_Scallop_Estimation_Areas_2022_80meters.shp'),crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')
# SAM.names = grep('_LT80',file.shp$SUBAREA,value =T)

make_degree_day_SAM = function(input.dir,input.prefix,file.shp,SAM.names,years,shp.name.var,out.dir,out.df.name,figure.dir,plot = T){
  
  files = list.files(input.dir,input.prefix)
  
  out.df.ls = list()
  i =1
  for(i in 1:length(files)){
    
    file.SAM = SAM.names[sapply(SAM.names,function(x) grepl(x,files[i])) ]
    if(length(file.SAM)==0){next()}
    file.min.val = strsplit(files[i],paste0(input.prefix,'|_|',file.SAM,'|.nc'))[[1]][4]
    file.max.val = strsplit(files[i],paste0(input.prefix,'|_|',file.SAM,'|.nc'))[[1]][5]
    file.year = as.numeric(strsplit(files[i],paste0(input.prefix,'|_|',file.SAM,'|.nc'))[[1]][7])
    
    data.file = terra::rast(paste0(input.dir,files[i]))
    
    data.sum = sum(data.file,na.rm=T)
    
    out.df.ls[[i]] = data.frame(
      year = file.year,
      SAM = file.SAM,
      min.val  = file.min.val,
      max.val = file.max.val,
      dd_mean = mean(values(data.sum),na.rm=T),
      dd_sd = sd(values(data.sum),na.rm=T)
    )
    print(i/length(files))
  }
  out.df = bind_rows(out.df.ls)
  
  write.csv(out.df,paste0(out.dir,out.df.name,'.csv'),row.names =F)
  
  if(plot == T){
    
    ggplot(out.df, aes( x= year, y = dd_mean,ymin = dd_mean - dd_sd, ymax = dd_mean + dd_sd))+
      geom_ribbon(alpha = 0.5, fill = 'grey50')+
      geom_line()+
      facet_grid(min.val~SAM)+
      ylab('Degree Days over Threshold')+
      xlab('')+
      theme_bw()
    
    ggsave(paste0(figure.dir,'/',out.df.name,'.png'),width = 16, height = 10, units = 'in', dpi = 350)
  }
}
