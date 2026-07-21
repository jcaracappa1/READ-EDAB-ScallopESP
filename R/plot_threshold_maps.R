
data_dir = 'W:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold/'
fig_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/figures/'
nc_out_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold_stats/'
file_prefix = 'nwa_neus_i202604_scallop_strata_nd18_tob'
shp_file =  terra::vect(NEFSCspatial::scallop_strata)
day_thresholds = c(5,10,20)
overwrite_plot = T


plot_threshold_maps = function(data_dir, fig_dir,nc_out_dir,shp_file,overwrite_plot =T, day_thresholds = 5){

  #Get file information
  file.names = list.files(pattern = file_prefix, path = data_dir, full.names = TRUE)  
  file.basename = basename(file.names)
  file.id = sapply(file.basename, function(x) strsplit(x, split = '\\.')[[1]][1])
  file.member.string = unique(gsub(paste0(file_prefix, "_"), "", gsub(".nc", "", file.basename)))
  file.member = sapply(file.member.string, function(x) strsplit(x,split = '=')[[1]][2],USE.NAMES = F)
  file.str <- stringr::str_match(file.id, "_i(\\d{4})(\\d{2})_")
  thresh.str <- stringr::str_match(file.id,"_nd(\\d{2})_")
  
  file.df = data.frame(id = file.id, name = file.names, basename = file.basename, member = file.member,
                       init.year = as.numeric(file.str[,2]), init.month = as.numeric(file.str[,3]),threshold = as.numeric(thresh.str[,2]),
                       stringsAsFactors = F)
  
  
  
  #Get coastline data
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  #Get Area shapefile
  shp.vect = EDABUtilities:::import_shp(shp_file)
  
  plot_fn = function(data.plot, plot.title, key.label ){
    p = ggplot2::ggplot() +
      # Add the raster layer
      tidyterra::geom_spatraster(data = data.plot) +
      
      # Add the coastline. ggplot/sf will automatically reproject the coastline 
      # on-the-fly to match the raster's CRS
      ggplot2::geom_sf(data = land, color = "black", fill = 'seagreen', linewidth = 0.5) +
      
      # Add the vector polygons (overlay)
      tidyterra::geom_spatvector(data = shp.vect, fill = NA, color = "black", linewidth = 0.5) +
      
      # Set a nice color palette for the raster (e.g., viridis)
      ggplot2::scale_fill_viridis_c(na.value = "transparent", name = key.label) +
      
      # Ensure the map bounds are locked to your raster's extent, not the whole world
      ggplot2::coord_sf(
        xlim = terra::ext(data.plot)[1:2], 
        ylim = terra::ext(data.plot)[3:4], 
        expand = FALSE,
        crs = terra::crs(data.plot)
      ) +
      
      # Clean up the visual theme
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = plot.title,
        x = "Longitude",
        y = "Latitude"
      )
    return(p)
  }
  
  data.ls = list()
  i=1
  for( i in 1:nrow(file.df)){
    
    data = terra::rast(file.df$name[i])
    
    this.month = file.df$init.month[i]
    this.year = file.df$init.year[i]
    this.thresh = file.df$threshold[i]
    
    #Plotting
    fig.name = paste0(fig_dir,file.df$id[i],'.png')
    
    if(file.exists(fig.name) & !overwrite_plot){
      message(paste0('Figure already exists: ',fig.name))
    }else{
      # 2. Create the ggplot
      
      p = plot_fn(data,
                  plot.title = paste0(month.name[this.month],', ',this.year,' forecast: Days above ',this.thresh),
                  key.label = 'Days above threshold')
      ggplot2::ggsave(plot = p,filename =  fig.name)
    }
    
    terra::varnames(data) = paste0(terra::varnames(data),':member=',file.df$member[i])
    names(data) = paste0(names(data),':member=',file.df$member[i])
    data.ls[[i]] = data
  }
  
  #Look at all end members together
  #Mean and sd 
  data.all = terra::rast(data.ls)

  fig.name.mean = paste0(fig_dir,file_prefix,'_mean.png')
  fig.name.sd = paste0(fig_dir,file_prefix,'_sd.png')
  
  
  data.mean = terra::app(data.all,mean)
  data.sd = terra::app(data.all, sd)
  
  #write netcdf out
  nc.name.mean = paste0(nc_out_dir,file_prefix,'_mean.nc')
  nc.name.sd = paste0(nc_out_dir,file_prefix,'_sd.nc')
  
  terra::writeCDF(data.mean,filename = nc.name.mean, overwrite =T)
  terra::writeCDF(data.sd,filename = nc.name.sd, overwrite =T)
  
  if(overwrite_plot == T | !file.exists(fig.name.mean)){
    p.mean = plot_fn(data.mean,
                     plot.title = paste0(month.name[this.month],', ',this.year,' forecast: Mean Bottom Temperature'),
                     key.label = 'Bottom Temperature (°C)')
    ggplot2::ggsave(plot = p.mean,filename =  fig.name.mean)
  }
  
  if(overwrite_plot == T | !file.exists(fig.name.sd)){
    p.sd = plot_fn(data.sd,
                   plot.title = paste0(month.name[this.month],', ',this.year,' forecast: Stdev Bottom Temperature'),
                   key.label = 'Bottom Temperature (°C)')
    ggplot2::ggsave(plot = p.sd,filename =  fig.name.sd)
  }
  
  
  #Probability of being above N days
  for(thresh in day_thresholds){
    data.prob = terra::app(data.all, function(x) mean(x >= thresh, na.rm = T))
    fig.name.prob = paste0(fig_dir,file_prefix,'_prob_above_',thresh,'days.png')
    nc.name.prob = paste0(nc_out_dir,file_prefix,'_prob_above_',thresh,'days.nc')
    
    if(overwrite_plot == T| !file.exists(fig.name.prob)){
      p.prob = plot_fn(data.prob,
                       plot.title = paste0(month.name[this.month],', ',this.year,' forecast: Probability of being above ',this.thresh,' for ',thresh,' days'),
                       key.label = 'Probability')
      ggplot2::ggsave(plot = p.prob,filename =  fig.name.prob)
    }
    
    terra::writeCDF(data.prob, filename = nc.name.prob, overwrite =T)
    
    

  }
  
}