data_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/'
fig_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/figures/'
df_out_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold_stats/'
file_prefix = 'nwa_neus_i202604_scallop_strata_'

shp_file =  terra::vect(NEFSCspatial::scallop_strata)
day_thresholds = c(5,10,20)
overwrite_plot = T
temp_thresh = 18

plot_threshold_ts = function(data_dir,fig_dir,df_out_dir, file_prefix,shp_file,day_thresholds,overwrite_plot,temp_thresh){
  
  stats.dir =paste0(data_dir,'stats/')
  thresh.dir =paste0(data_dir,'threshold/')
  
  stats.files = list.files(path = stats.dir,pattern = paste0(file_prefix,'stats_tob_member'),full.names = T)
  thresh.files = list.files(path = thresh.dir,pattern = paste0(file_prefix,'nd18_tob_member'), full.names = T)
  
  #plot stats TS
  i=1
  data.stats = lapply(stats.files,read.csv) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(
      year = ifelse(time - time[1] %% 12 <0, year+1,year),
      month.name = month.name[time],
                date = as.Date(paste0(year,'-',month.name,'-01'),format = '%Y-%B-%d'),
      member = as.character(member))
  
  data.member.mean = data.stats |> 
    dplyr::group_by(area,date,statistics) |> 
    dplyr::summarise(value = mean(value,na.rm = T)) |> 
    dplyr::mutate(member = 'mean')
  
  data.plot = data.stats |> 
    dplyr::bind_rows(data.member.mean)
  
  stat.names = c('mean','min','max')
  
  i=1
  for(i in 1:length(stat.names)){}
  
  this.data = data.plot |> 
    dplyr::filter(statistics == stat.names[i]) |> 
    dplyr::mutate(member = factor(member))
  
  p.stat = ggplot2::ggplot(data = this.data, ggplot2::aes(x = date, y = value, color = member,linewidth = member))+
    ggplot2::geom_line()+
    ggplot2::facet_wrap(~area)+
    ggplot2::scale_color_manual(values = c(RColorBrewer::brewer.pal(10,'Paired'),'black'))+
    ggplot2::scale_linewidth_manual(values = c(rep(0.5,10),1))+
    ggplot2::geom_hline(yintercept = temp_thresh,lty = 2)+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 10))
  
  fig.name = paste0(fig_dir, file_prefix, stat.names[i],'_tob_ts.png')
  
  if(!file.exists(fig.name) | overwrite_plot){
    ggplot2::ggsave(plot = p.stat,filename = fig.name, width = 8, height = 4)
  }
  
  #Do number of days
  data.thresh = lapply(thresh.files,read.csv) |> dplyr::bind_rows() |> 
    dplyr::mutate(
      member = as.character(member))
  
  data.thresh.mean = data.thresh |> 
    dplyr::group_by(area) |> 
    dplyr::summarise(value.mean = mean(value,na.rm=T),
                     value.sd = sd(value,na.rm=T)) |> 
    dplyr::mutate(value.lower = value.mean - value.sd,
                  value.upper = value.mean + value.sd)
  
  p.thresh = ggplot2::ggplot()+
    ggplot2::geom_point(data= data.thresh, ggplot2::aes(x = area, y = value, color = member),size = 2)+
    ggplot2::geom_errorbar(data = data.thresh.mean, ggplot2::aes(x = area, y = value.mean, ymin = value.lower, ymax = value.upper), width = 0.2)+
    ggplot2::geom_point(data = data.thresh.mean, ggplot2::aes(x = area, y = value.mean))+
    ggplot2::ylab(paste0('Number of Days Exceeding ',temp_thresh,'degC'))+
    ggplot2::xlab('')+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 10))
    
    
  fig.name = paste0(fig_dir, file_prefix, 'nd',temp_thresh,'.png')
  
  if(!file.exists(fig.name) | overwrite_plot){
    ggplot2::ggsave(plot = p.thresh,filename = fig.name, width = 8, height = 4)
  }
  
  
}
