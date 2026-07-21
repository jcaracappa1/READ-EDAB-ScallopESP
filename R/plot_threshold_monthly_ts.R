data_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold/'
fig_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/figures/'
df_out_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold_stats/'
file_prefix = 'nwa_neus_i202604_scallop_strata_monthly_'

shp_file =  terra::vect(NEFSCspatial::scallop_strata)
overwrite_plot = T
temp_thresh = 18

plot_threshold_monthly_ts = function(data_dir,fig_dir,df_out_dir, file_prefix,shp_file,day_thresholds,overwrite_plot,temp_thresh){
  
  thresh.files = list.files(path = data_dir,pattern = file_prefix, full.names = T)
  nd.files = grep(paste0('nd',temp_thresh),thresh.files,value =T)
  dd.files = grep(paste0('dd',temp_thresh),thresh.files,value =T)
  nd.con.files = grep(paste0('nd.con',temp_thresh),thresh.files,value =T)
  
  file.group.ls = list(nd.files = nd.files, dd.files = dd.files, nd.con.file = nd.con.files)
  
  #Do number of days
  i=1
  
  for(i in 1:length(file.group.ls)){
    data.thresh = lapply(file.group.ls[[i]],read.csv) |> dplyr::bind_rows() |> 
      dplyr::mutate(
        member = as.character(member),
        date = as.Date(paste0(year,'-',month,'-01')))
    
    data.thresh.mean = data.thresh |> 
      dplyr::group_by(area,date) |> 
      dplyr::summarise(value.mean = mean(value,na.rm=T),
                       value.sd = sd(value,na.rm=T)) |> 
      dplyr::mutate(value.lower = value.mean - value.sd,
                    value.upper = value.mean + value.sd) |> 
      dplyr::ungroup()
    
    this.metric = data.thresh$metric[1]
    this.metric.name = if(this.metric == 'dd'){
      'Degree Days'
    } else if(this.metric == 'nd'){
      'Number of Days'
    } else if(this.metric == 'nd.con'){
      'Number of Consecutive Days'
    }
    
    data.thresh.mean$metric = this.metric
    
    saveRDS(data.thresh,paste0(df_out_dir,file_prefix,this.metric,temp_thresh,'degC_monthly_ts.rds'))
    saveRDS(data.thresh.mean,paste0(df_out_dir,file_prefix,this.metric,temp_thresh,'degC_monthly_ts_mean.rds'))
    
    p.thresh.all =ggplot2::ggplot()+
      ggplot2::geom_line(data= data.thresh, ggplot2::aes(x = date, y = value, color = member))+
      ggplot2::geom_ribbon(data = data.thresh.mean, ggplot2::aes(x = date, ymin = value.lower, ymax = value.upper), alpha = 0.2)+
      ggplot2::geom_line(data = data.thresh.mean, ggplot2::aes(x = date, y = value.mean),color = 'black')+
      ggplot2::facet_wrap(~area)+
      ggplot2::ylab(paste0(this.metric.name,' Exceeding ',temp_thresh,'degC'))+
      ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b %Y')+
      ggplot2::xlab('')+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 1,size =10))
    
    fig.name = paste0(fig_dir, file_prefix,data.thresh$metric[1],temp_thresh,'.png')
    
    if(!file.exists(fig.name) | overwrite_plot){
      ggplot2::ggsave(plot = p.thresh.all,filename = fig.name, width = 8, height = 4)
    }
    
    #Non-zero areas
    data.thresh.nozero = data.thresh |> 
      dplyr::group_by(area) |> 
      dplyr::mutate(tot = sum(value,na.rm=T)) |> 
      dplyr::ungroup() |> 
      dplyr::filter(tot > 0)
    
    data.thresh.nozero.mean = data.thresh.mean |> 
      dplyr::filter(area %in% unique(data.thresh.nozero$area))
    
    p.thresh.nozero =ggplot2::ggplot()+
      ggplot2::geom_line(data= data.thresh.nozero, ggplot2::aes(x = date, y = value, color = member))+
      ggplot2::geom_ribbon(data = data.thresh.nozero.mean, ggplot2::aes(x = date, ymin = value.lower, ymax = value.upper), alpha = 0.2)+
      ggplot2::geom_line(data = data.thresh.nozero.mean, ggplot2::aes(x = date, y = value.mean),color = 'black')+
      ggplot2::facet_wrap(~area)+
      ggplot2::ylab(paste0(this.metric.name,' Exceeding ',temp_thresh,'degC'))+
      ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b %Y')+
      ggplot2::xlab('')+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 1,size = 10))
    
    fig.name = paste0(fig_dir, file_prefix,data.thresh$metric[1],temp_thresh,'_nonzero_areas.png')
    
    if(!file.exists(fig.name) | overwrite_plot){
      ggplot2::ggsave(plot = p.thresh.nozero,filename = fig.name, width = 8, height = 4)
    }
    
  }

}
