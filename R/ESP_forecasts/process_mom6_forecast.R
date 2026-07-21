#Function to process MOM6 forecast data into useable pieces

forecast_orig_file = 'W:/MOM6/seasonal_forecasts/i202604/tob.nwa.full.ss_fcast.daily.regrid.r20250710.enss.i202604.neus.nc'
nc_out_dir = 'W:/MOM6/seasonal_forecasts/i202604/tob/daily/'
df_out_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/'
output_prefix = 'nwa_neus_i202604_'
library(sf)
shp_orig = NEFSCspatial::scallop_strata |> 
  # dplyr::filter(grepl('MAB',NEWSAMS)) |> 
  dplyr::group_by(NEWSAMS) |> 
  dplyr::mutate(
    # '1:n()' guarantees a sequence like 1, 2, 3 for the exact size of the group
    NEWSAMS = if (dplyr::n() > 1) paste(NEWSAMS, 1:dplyr::n(), sep = '_') else NEWSAMS
  ) |> 
  dplyr::ungroup()
shp_files = list('scallop_strata' = terra::vect(shp_orig))
area_var = c('scallop_strata' = 'NEWSAMS')
threshold = 18
overwrite = F

process_mom6_forecast = function(forecast_orig_file,nc_out_dir, df_out_dir,output_prefix,shp_files,threshold = 18,overwrite = F){
  
  if(!dir.exists(nc_out_dir)){
    dir.create(nc_out_dir)
  }

  if(!dir.exists(df_out_dir)){
    dir.create(df_out_dir)
  }
  
  #get global attributes
  data.nc = ncdf4::nc_open(forecast_orig_file)
  ndays = ncdf4::ncvar_get(data.nc,'valid_time')
  init.date.string = ncdf4::ncatt_get(data.nc,'init','units')$value
  init.date = strsplit(init.date.string, split = '\\s+')[[1]][3] |> as.Date()
  file.times = init.date + ndays
  ncdf4::nc_close(data.nc)
  
  # shp.vect = terra::vect(shp.file)
  data = terra::rast(forecast_orig_file)
  
  #Get layer names
  name.strings = terra::names(data)

  # Extract the digits that come after each specific key
  pattern <- "^(.*)_lead=([0-9]+)_member=([0-9]+)$"

  # strcapture maps the capture groups directly to the prototype dataframe
  data.var.names <- strcapture(
    pattern = pattern,
    x = name.strings,
    proto = data.frame(
      variable = character(),
      lead     = numeric(),
      member   = numeric()
    )
  ) |>
    dplyr::mutate(var.name = name.strings)
    
    
  var.combs = expand.grid(variable = unique(data.var.names$variable), member = unique(data.var.names$member), which.shp = 1:length(shp_files))
    
  i=1
  #Loop through layers
  for(i in 1:nrow(var.combs)){

    this.shp = shp_files[[var.combs$which.shp[i]]]
    this.region = names(shp_files)[var.combs$which.shp[i]]
    which.names = which(names(this.shp) == area_var[var.combs$which.shp[i]])
    region.names = terra::as.data.frame(this.shp)[,which.names]

    var.subset.names = data.var.names |> 
      dplyr::filter(member == var.combs$member[i], variable == var.combs$variable[i]) |> 
      dplyr::pull(var.name)
    
    this.data = terra::subset(data,var.subset.names)
    terra::time(this.data) = file.times
    this.time = terra::time(this.data)
    this.time.month = as.numeric(format(as.Date(this.time),format = '%m'))
    
    is.anom = grepl('anom',var.combs$variable[i])
    
    ####RAW DATA ONLY####
    if(!is.anom){
      
      #Get ndays gridded and write
      message('Doing thresholded ndays for ',var.combs$variable[i],' member = ',var.combs$member[i])
      thresh.filename = paste0(nc_out_dir,'threshold/',output_prefix,this.region,'_nd',threshold,'_',var.combs$variable[i],'_member=',var.combs$member[i],'.nc')
      
      if(file.exists(thresh.filename) & !overwrite){
        print(paste0('File exists and overwrite = F, skipping: ',thresh.filename))
      }else{
        
        EDABUtilities::make_2d_deg_day_gridded(data.in = this.data,
                                                             var.name = var.combs$variable[i],
                                                             type = 'above',
                                                             ref.value = threshold,
                                                             metric = 'nd',
                                                             shp.file = this.shp,
                                                             write.out = T,
                                                             output.file = thresh.filename)
        
      }
      
      #Get ndays table and write
      metric.names = c('dd','nd','nd.con')
      
      j=1
      for( j in 1:length(metric.names)){
        message('Doing thresholded ',metric.names[j],' table for ',var.combs$variable[i],' member = ',var.combs$member[i])
        thresh.df.filename =  paste0(df_out_dir,'threshold/',output_prefix,this.region,'_',metric.names[j],threshold,'_',var.combs$variable[i],'_member=',var.combs$member[i],'.csv')
        
        if(file.exists(thresh.df.filename) & !overwrite ){
          print(paste0('File exists and overwrite = F, skipping: ',thresh.df.filename))
        }else{
          data.thresh.df = EDABUtilities::make_2d_deg_day_ts(data.in = this.data,
                                                             var.name = var.combs$variable[i],
                                                             type = 'above',
                                                             ref.value = threshold,
                                                             metric = metric.names[j],
                                                             shp.file = this.shp,
                                                             area.names  =region.names)[[1]]
          
          data.thresh.df = data.thresh.df |>  
            dplyr::mutate(year = format(init.date,'%Y'),
                          member = var.combs$member[i])
          write.csv(data.thresh.df, thresh.df.filename,row.names = F)
        }
        
        #Do Monthly deg
        message('Doing monthly ',metric.names[j],' gridded for ',var.combs$variable[i],' member = ',var.combs$member[i])
        thresh.month.df.filename = paste0(df_out_dir,'threshold/',output_prefix,this.region,'_monthly_',metric.names[j],threshold,'_',var.combs$variable[i],'_member=',var.combs$member[i],'.csv')
        
        
        if(file.exists(thresh.month.df.filename) & !overwrite ){
          print(paste0('File exists and overwrite = F, skipping: ',thresh.month.df.filename))
        }else{
          
          month.order = unique(this.time.month)
          
          month.metric.ls = list()
          k=1
          for(k in month.order){
            this.month.name = month.name[k]
            
            
            this.month.timestep = which(this.time.month == k)
            this.month.year = as.numeric(format(as.Date(this.time), format = '%Y'))[this.month.timestep][1]
            
            this.data.month = terra::subset(this.data,this.month.timestep)
            
            this.month.stats.df = EDABUtilities::make_2d_deg_day_ts(data.in = this.data.month,
                                                                    var.name = var.combs$variable[i],
                                                                    type = 'above',
                                                                    ref.value = threshold,
                                                                    metric = metric.names[j],
                                                                    shp.file = this.shp,
                                                                    area.names  = region.names)[[1]] |> 
              dplyr::mutate(month = k,
                            month.name = this.month.name,
                            year = this.month.year)
            
            month.metric.ls[[k]] = this.month.stats.df
          }
          
          month.metric.df = dplyr::bind_rows(month.metric.ls) |> 
            dplyr::mutate(init.year = format(init.date,'%Y'),
                          member = var.combs$member[i])
          
          
          write.csv(month.metric.df, thresh.month.df.filename,row.names = F)
        }
      }
      
    }
    
    ### ANY DATA ####
    
    
    #Do daily stats+
    message('Doing Daily Gridded Stats for ',var.combs$variable[i],' member = ',var.combs$member[i])
    stats.nc.filename =  paste0(nc_out_dir,'stats/',output_prefix,this.region,'_stats_',var.combs$variable[i],'_member=',var.combs$member[i],'.nc')
    if(file.exists(stats.nc.filename) & !overwrite ){
      print(paste0('File exists and overwrite = F, skipping: ',stats.nc.filename))
    }else{
     
      EDABUtilities::make_2d_summary_gridded(data.in = this.data,
                                                        var.name = var.combs$variable[i],
                                                        shp.file =this.shp,
                                                        area.names = region.names,
                                                        agg.time = 'years',
                                                        statistics = c('mean','sd','min','max'),
                                                        file.time = 'annual',
                                                        write.out = T,
                                                        output.files = stats.nc.filename)
      
      
    }
    
    #Do Monthly stats
    message('Doing monthly stats table for ',var.combs$variable[i],' member = ',var.combs$member[i])
    stats.df.filename = paste0(df_out_dir,'stats/',output_prefix,this.region,'_stats_',var.combs$variable[i],'_member=',var.combs$member[i],'.csv')
    if(file.exists(stats.df.filename) & !overwrite ){
      print(paste0('File exists and overwrite = F, skipping: ',stats.df.filename))
    }else{
      
      stats.df = EDABUtilities::make_2d_summary_ts(data.in = this.data,
                                                        var.name = var.combs$variable[i],
                                                        shp.file = this.shp,
                                                        agg.time = 'months',
                                                        statistics = c('mean','sd','min','max'),
                                                        file.time = 'annual',
                                                        write.out = F,
                                                        area.names  = region.names)[[1]]
      
     
      stats.df = stats.df |>  
        dplyr::mutate(year = format(init.date,'%Y'),
                      member = var.combs$member[i])
      write.csv(stats.df, stats.df.filename,row.names = F)
    }
    
    message('Doing Monthly Gridded Stats for ',var.combs$variable[i],' member = ',var.combs$member[i])
    stats.nc.monthly.filename =  paste0(nc_out_dir,'stats/',output_prefix,this.region,'_stats_monthly_',var.combs$variable[i],'_member=',var.combs$member[i],'.nc')
    if(file.exists(stats.nc.monthly.filename) & !overwrite ){
      print(paste0('File exists and overwrite = F, skipping: ',stats.nc.monthly.filename))
    }else{
      
      EDABUtilities::make_2d_summary_gridded(data.in = this.data,
                                             var.name = var.combs$variable[i],
                                             shp.file =this.shp,
                                             area.names = region.names,
                                             agg.time = 'months',
                                             statistics = c('mean','sd','min','max'),
                                             file.time = 'annual',
                                             write.out = T,
                                             output.files = stats.nc.monthly.filename)
      
      
    }
    
    
    

    print(var.combs[i,])
    
    

    
  }

}
