data_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold_stats/'
fig_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/figures/'
df_out_dir = 'V:/MOM6/seasonal_forecasts/i202604/tob/daily/threshold_stats/'
file_prefix = 'nwa_neus_i202604_scallop_strata_monthly_'

data.files <- list.files(
  path = data_dir, 
  pattern = "_ts\\.rds$", 
  full.names = TRUE
)

i=1
area.season.ls = list()
for( i in 1:length(data.files)){
  
  this.data = readRDS(data.files[i])
  this.metric = this.data$metric[1]
  
  this.data.season = this.data |> 
    dplyr::mutate(
      month = as.numeric(format(date,format = '%m')),
      year = format(date, format = '%Y'),
      season = dplyr::case_when(
        month %in% c(1:3) ~ 'Winter',
        month %in% c(4:6) ~ 'Spring',
        month %in% c(7:9) ~ 'Summer',
        month %in% c(10:12) ~ 'Fall'
      )
    ) |> 
    dplyr::filter(value >0) |> 
    dplyr::group_by(metric,year,season,area,member) |> 
    dplyr::summarise(value.mean = mean(value,na.rm=T),
                     value.min = min(value,na.rm=T),
                     value.max = max(value,na.rm=T)) |> 
    dplyr::group_by(metric,year,season,area) |> 
    dplyr::summarise(value.mean = mean(value.mean,na.rm=T),
                     value.min = mean(value.min,na.rm=T),
                     value.max = mean(value.max,na.rm=T))
  
  area.season.ls[[i]] = this.data.season
  
}

area.season.df = dplyr::bind_rows(area.season.ls)

#build contigency table for statistics
nd.scale = data.frame(min.val = c(0, 7, 14, 30),
                      max.val = c(7, 14, 30, Inf),
                      color = c('skyblue','forestgreen','orange','red'),
                      metric = 'nd')
nd.con.scale = data.frame(min.val = c(0, 7, 14, 30),
                          max.val = c(7, 14, 30, Inf),
                          color = c('skyblue','forestgreen','orange','red'),
                          metric = 'nd.con')
dd.scale = data.frame(min.val = c(0, 7, 14, 30),
                      max.val = c(7, 14, 30, Inf),
                      color = c('skyblue','forestgreen','orange','red'),
                      metric = 'dd')
scale.combined = rbind(nd.scale,nd.con.scale,dd.scale)

area.season.df_classified <- area.season.df |> 
  # 1. Classify value.mean
  dplyr::left_join(
    scale.combined|> dplyr::select(metric, min.val, max.val, color.mean = color),
    by = dplyr::join_by(metric, value.mean >= min.val, value.mean < max.val)
  )|>
  dplyr::left_join(
    scale.combined|> dplyr::select(metric, min.val, max.val, color.min = color),
    by = dplyr::join_by(metric, value.min >= min.val, value.min < max.val)
  )|>
  dplyr::left_join(
    scale.combined|> dplyr::select(metric, min.val, max.val, color.max = color),
    by = dplyr::join_by(metric, value.max >= min.val, value.max < max.val)
  ) |> 
  
  # Clean up the bounding columns that get pulled in from the joins
  dplyr::mutate(scale.lab = paste(min.val,max.val,sep = '-')) |> 
  dplyr::select(-dplyr::contains('.val')) |> 
  dplyr::mutate(facet.lab = paste(season,year,sep= '-'))|> 
  dplyr::ungroup() |> 
  dplyr::mutate(metric.names = dplyr::case_when(
    metric == 'dd' ~ 'Degree Days',
    metric == 'nd' ~ 'Days Exceeding',
    metric == 'nd.con' ~ 'Consecutive\nDays Exceeding',
    TRUE ~ NA_character_
  ))

area.season.df_classified <- area.season.df_classified |> 
  dplyr::mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))) |> 
  # 2. Sort the dataframe by year, then by season
  dplyr::arrange(year, season) |> 
  # 3. Turn facet.lab into a factor using the newly sorted, unique timeline
  dplyr::mutate(facet.lab = factor(facet.lab, levels = unique(facet.lab)))

area.season.df_classified$scale.lab = factor(area.season.df_classified$scale.lab,levels = c('0-7','7-14','14-30','30-Inf'))

color_key <- area.season.df_classified |> 
  dplyr::distinct(scale.lab, color.mean)
my_colors <- setNames(color_key$color.mean, color_key$scale.lab)

ggplot2::ggplot(data = area.season.df_classified, ggplot2::aes(x = metric.names, y = area, fill = scale.lab,labels = round(value.mean))) +
  ggplot2::geom_tile(color = 'black') +
  ggplot2::geom_text()+
  ggplot2::scale_fill_manual(
    values = my_colors, 
    name = "Stress Level" # Change legend title here
  ) + # guide="legend" forces the legend to appear
  ggplot2::facet_wrap(~facet.lab,nrow = 2)+
  ggplot2::ylab('')+
  ggplot2::xlab('')+
  ggplot2::ggtitle('Mean Heat Stress Conditions')+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = 'bottom')

ggplot2::ggsave(paste0(fig_dir,'threshold_metric_summary_SAMS_mean.png'),width = 9, height = 5, units = 'in')

ggplot2::ggplot(data = area.season.df_classified, ggplot2::aes(x = metric.names, y = area, fill = scale.lab,labels = round(value.max))) +
  ggplot2::geom_tile(color = 'black') +
  ggplot2::geom_text()+
  ggplot2::scale_fill_manual(
    values = my_colors, 
    name = "Stress Level" # Change legend title here
  ) + # guide="legend" forces the legend to appear
  ggplot2::facet_wrap(~facet.lab,nrow = 2)+
  ggplot2::ylab('')+
  ggplot2::xlab('')+
  ggplot2::ggtitle('Maximum Heat Stress Conditions')+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = 'bottom')

ggplot2::ggsave(paste0(fig_dir,'threshold_metric_summary_SAMS_max.png'),width = 9, height = 5, units = 'in')

