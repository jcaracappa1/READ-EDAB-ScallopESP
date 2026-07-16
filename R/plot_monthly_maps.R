data_dir = 'W:/MOM6/seasonal_forecasts/i202604/tob/monthly/stats/'
fig_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/figures/'
file_prefix = 'nwa_neus_i202604_scallop_strata_stats_monthly_'
shp_file =  terra::vect(NEFSCspatial::scallop_strata)
overwrite_plot = T
temp_thresh = 18

plot_threshold_maps = function(data_dir, fig_dir,shp_file,temp_thresh,overwrite_plot =T){
  
  file.names = list.files(pattern = paste0(file_prefix), path = data_dir, full.names = TRUE)  
  file.basename = basename(file.names)
  file.id = sapply(file.basename, function(x) strsplit(x, split = '\\.')[[1]][1])
  file.member.string = unique(gsub(paste0(file_prefix, "_"), "", gsub(".nc", "", file.basename)))
  file.member = sapply(file.member.string, function(x) strsplit(x,split = '=')[[1]][2],USE.NAMES = F)
  file.str <- stringr::str_match(file.id, "_i(\\d{4})(\\d{2})_")
  
  file.df = data.frame(id = file.id, name = file.names, basename = file.basename, member = file.member,
                       init.year = as.numeric(file.str[,2]), init.month = as.numeric(file.str[,3]),
                       stringsAsFactors = F)
  

  #Get coastline data
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  #Get Area shapefile
  shp.vect = EDABUtilities:::import_shp(shp_file)
  
  plot_fn = function(data.plot, plot.title, key.label, is.anom = F ){
    p = ggplot2::ggplot() +
      # Add the raster layer
      tidyterra::geom_spatraster(data = data.plot) +
      
      # Add the coastline. ggplot/sf will automatically reproject the coastline 
      # on-the-fly to match the raster's CRS
      ggplot2::geom_sf(data = land, color = "black", fill = 'grey70', linewidth = 0.5) +
      
      # Add the vector polygons (overlay)
      tidyterra::geom_spatvector(data = shp.vect, fill = NA, color = "black", linewidth = 0.5) +
      
      # Set a nice color palette for the raster (e.g., viridis)
      # Ensure the map bounds are locked to your raster's extent, not the whole world
      ggplot2::coord_sf(
        xlim = terra::ext(data.plot)[1:2], 
        ylim = terra::ext(data.plot)[3:4], 
        expand = FALSE,
        crs = terra::crs(data.plot)
      ) +
      ggplot2::facet_wrap(~lyr)+
      # Clean up the visual theme
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = plot.title,
        x = "Longitude",
        y = "Latitude"
      )
    
    if(is.anom){
      p = p + ggplot2::scale_fill_gradient2(low = 'blue',high = 'red',mid = 'grey',na.value = "transparent", name = key.label)
    }else{
      p = p + ggplot2::scale_fill_viridis_c(na.value = "transparent", name = key.label) 
    }
    return(p)
  }
  
  plot_fn_thresh = function(data.plot, plot.title, key.label ){
    p = ggplot2::ggplot() +
      # Add the raster layer
      tidyterra::geom_spatraster(data = data.plot) +
      
      # Add the coastline. ggplot/sf will automatically reproject the coastline 
      # on-the-fly to match the raster's CRS
      ggplot2::geom_sf(data = land, color = "black", fill = 'grey70', linewidth = 0.5) +
      
      # Add the vector polygons (overlay)
      tidyterra::geom_spatvector(data = shp.vect, fill = NA, color = "black", linewidth = 0.5) +
      
      ggplot2::scale_fill_gradient(low = 'red',high = 'red',na.value = 'transparent')+
      
      # Ensure the map bounds are locked to your raster's extent, not the whole world
      ggplot2::coord_sf(
        xlim = terra::ext(data.plot)[1:2], 
        ylim = terra::ext(data.plot)[3:4], 
        expand = FALSE,
        crs = terra::crs(data.plot)
      ) +
      ggplot2::facet_wrap(~lyr)+
      # Clean up the visual theme
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = plot.title,
        x = "Longitude",
        y = "Latitude"
      )
    return(p)
  }
  
  plot.combs = expand.grid(varnames = c('tob','tob_anom'),
                           statistics = c('mean','min','max','sd'),
                           stringsAsFactors = F) |> 
    dplyr::mutate(search.str = paste0(varnames,'_member'),
                  varname.str = paste0(varnames,'_',statistics))
  
  
  i=1
  for(i in 1:nrow(plot.combs)){
    
    these.files = file.df$name[grep(plot.combs$search.str[i],file.df$name)]
    data.ls = lapply(these.files, function(f) terra::rast(f, subds = plot.combs$varname.str[i]))
    data.rast = terra::rast(data.ls)
    month_index <-terra::time(data.rast)
    
    data.mean  <- terra::tapp(data.rast, month_index, fun = mean, na.rm = TRUE)
    
    names(data.mean) <- month.abb[month_index[1:12]]
    
    fig.name = paste0(fig_dir,file_prefix,plot.combs$varname.str[i],'.png')
    
    is.anom = grepl('anom',plot.combs$varnames[i])
    
    if(file.exists(fig.name) & !overwrite_plot){
      message(paste0('Figure already exists: ',fig.name))
    }else{
    p=plot_fn(data.mean,
            plot.title = paste0('MOM6 Forecast:',month.name[file.df$init.month[1]],', ',file.df$init.year[1],' - Mean ',plot.combs$varname.str[i]),
            key.label ='Bottom Temp',
            is.anom = is.anom
            )
    ggplot2::ggsave(plot = p,filename =  fig.name, width = 8, height = 6, units = 'in',dpi = 300)
    }
    
    if(plot.combs$statistics[i] != 'sd' & !is.anom){
      
      
      fig.name = paste0(fig_dir,file_prefix,plot.combs$varname.str[i],'_',temp_thresh,'C.png')
      
      if(file.exists(fig.name) & !overwrite_plot){
        message(paste0('Figure already exists: ',fig.name))
      }else{
        data.thresh = terra::clamp(data.mean,lower = temp_thresh, upper = Inf,values =F)*0+1
        p.thresh = plot_fn_thresh(data.thresh,
                       plot.title = paste0('MOM6 Forecast:',month.name[file.df$init.month[1]],', ',file.df$init.year[1],' - Mean ',plot.combs$varname.str[i], '> ',temp_thresh,'C'),
                       key.label = paste0('Bottom Temp >',temp_thresh))
        
        ggplot2::ggsave(plot = p.thresh,filename =  fig.name, width = 8, height = 6, units = 'in',dpi = 300)
      }
    }
  }
  
  
}
