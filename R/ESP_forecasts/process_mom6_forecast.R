#Function to process MOM6 forecast data into useable pieces

forecast_orig_file = 'W:/MOM6/seasonal_forecasts/i202604/tob.nwa.full.ss_fcast.daily.regrid.r20250710.enss.i202604.neus.nc'
nc_out_dir = 'W:/MOM6/seasonal_forecasts/i202604/tob/daily/'
df_out_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/'
output_prefix = 'nwa_neus_i202604_'
shp.file <- system.file("data", "EPU_NOESTUARIES.shp", package = "EDABUtilities")
threshold = 18
overwrite = F

process_mom6_forecast = function(forecast_orig_file,nc_out_dir, df_out_dir,output_prefix,shp.file,threshold = 18,overwrite = F){
  
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }

  #get global attributes
  data.nc = ncdf4::nc_open(forecast_orig_file)
  ndays = ncdf4::ncvar_get(data.nc,'valid_time')
  init.date.string = ncdf4::ncatt_get(data.nc,'init','units')$value
  init.date = strsplit(init.day.string, split = '\\s+')[[1]][3] |> as.Date()
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
    
    
  var.combs = expand.grid(variable = unique(data.var.names$variable), member = unique(data.var.names$member))
  
  #Loop through layers
  for(i in 1:nrow(var.combs)){

    var.subset.names = data.var.names |> 
      dplyr::filter(member == var.combs$member[i], variable == var.combs$variable[i]) |> 
      dplyr::pull(var.name)
    
    this.data = terra::subset(data,var.subset.names)
    terra::time(this.data) = file.times

    is.anom = grepl('anom',var.combs$variable[i])
    
    if(!is.anom){
      
      #Get ndays gridded and write
      thresh.filename = paste0(nc_out_dir,'threshold/',output_prefix,'nd',threshold,'_',var.combs$variable[i],'_member=',var.combs$member[i],'.nc')
      
      if(file.exists(thresh.filename) & !overwrite){
        print(paste0('File exists and overwrite = F, skipping: ',thresh.filename))
      }else{
        
        data.thresh = EDABUtilities::make_2d_deg_day_gridded(data.in = this.data,
                                                             var.name = var.combs$variable[i],
                                                             type = 'above',
                                                             ref.value = threshold,
                                                             statistic = 'nd',
                                                             shp.file = shp.file,
                                                             write.out = T,
                                                             output.file = thresh.filename)
        
      }
      
      #Get ndays table and write
      thresh.df.filename =  paste0(df_out_dir,'threshold/',output_prefix,'nd',threshold,'_',var.combs$variable[i],'_member=',var.combs$member[i],'.csv')
      
      if(file.exists(thresh.df.filename) & !overwrite ){
        print(paste0('File exists and overwrite = F, skipping: ',thresh.df.filename))
      }else{
        data.thresh.df = EDABUtilities::make_2d_deg_day_ts(data.in = this.data,
                                                           var.name = var.combs$variable[i],
                                                           type = 'above',
                                                           ref.value = threshold,
                                                           metric = 'nd',
                                                           shp.file = shp.file,
                                                           area.names  = c('GOM','GB','MAB'))[[1]]
        
        data.thresh.df = data.thresh.df |>  
          dplyr::mutate(year = format(init.date,'%Y'),
                        member = var.combs$member[i])
        write.csv(data.thresh.df, thresh.df.filename,row.names = F)
      }
      
    }
    
    #Do daily stats+
    
    stats.nc.filename =  paste0(nc_out_dir,'stats/',output_prefix,'stats_',var.combs$variable[i],'_member=',var.combs$member[i],'.nc')
    if(file.exists(thresh.df.filename) & !overwrite ){
      print(paste0('File exists and overwrite = F, skipping: ',stats.nc.filename))
    }else{
     
      EDABUtilities::make_2d_summary_gridded(data.in = this.data,
                                                        var.name = var.combs$variable[i],
                                                        shp.file = shp.file,
                                                        agg.time = 'years',
                                                        statistics = c('mean','sd','min','max'),
                                                        file.time = 'annual',
                                                        write.out = T,
                                                        output.files = stats.nc.filename)
      
      
    }
    
    stats.df.filename = paste0(df_out_dir,'stats/',output_prefix,'stats_',var.combs$variable[i],'_member=',var.combs$member[i],'.csv')
    if(file.exists(stats.df.filename) & !overwrite ){
      print(paste0('File exists and overwrite = F, skipping: ',stats.df.filename))
    }else{
      
      stats.df = EDABUtilities::make_2d_summary_ts(data.in = this.data,
                                                        var.name = var.combs$variable[i],
                                                        shp.file = shp.file,
                                                        agg.time = 'months',
                                                        statistics = c('mean','sd','min','max'),
                                                        file.time = 'annual',
                                                        write.out = F,
                                                        area.names  = c('GOM','GB','MAB'))[[1]]
      
     
      stats.df = stats.df |>  
        dplyr::mutate(year = format(init.date,'%Y'),
                      member = var.combs$member[i])
      write.csv(stats.df, stats.df.filename,row.names = F)
    }

    print(var.combs[i,])
    
    

    
  }

  # 
  # data.nc = ncdf4::nc_open(forecast_orig_file)
  # names(data.nc$dim)
  # names(data.nc$var)
  # ncdf4::ncvar_get(data.nc,'valid_time')
  # ncdf4::ncvar_get(data.nc,'tob_anom')
  # ncdf4::nc_close(data.nc)
  # # terra::time(data)
}