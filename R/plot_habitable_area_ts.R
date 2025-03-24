#plot functions for habitable area time series by SAM
library(ggplot2)
library(dplyr)

years = 1993:2023

fig.dir = here::here('figures','habitat_area')
##Scallop metrics

scallop.SAM = read.csv(here::here('data','habitat_area','SAMS','scallop','thermal_habitable_area_scallop_SAMS.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))

pdf(paste0(fig.dir,'/scallop/thermal_habitat_area_scallop_SAMS.pdf'), width = 10, height = 10)
i=1
for(i in 1:length(years)){
  scallop.SAM.yr = filter(scallop.SAM, year == years[i])
  
  scallop.SAM.p = ggplot(scallop.SAM.yr,aes(x = date, y = area.pct))+
    geom_line()+
    facet_grid(max.temp~SAM,labeller = 'label_both')+
    ylab('Proportion Area below threshold')+
    xlab('')+
    ggtitle(paste0('Scallop - ',years[i],' (min temp = 0)'))
  
  print(scallop.SAM.p)
}
dev.off()

ggplot(scallop.SAM,aes(x = date, y = area.pct))+
  geom_line()+
  facet_grid(max.temp~SAM)+
  theme_bw()+
  ylab('Proportion Area below threshold')+
  ggtitle('Scallop - min temp = 0')
ggsave(paste0(fig.dir,'/scallop/thermal_habitat_area_scallop_SAMS_allyears.png'),width = 16,height = 10)

scallop.SAM.all = read.csv(here::here('data','habitat_area','MAB','scallop','thermal_habitable_area_scallop_MAB.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))

ggplot(scallop.SAM.all, aes( x= date, y = area.pct))+
  geom_line()+
  facet_wrap(~max.temp, labeller = 'label_both')+
  ylab('Proportion Area below threshold')+
  ggtitle('Scallop - min temp = 0')

  ggsave(paste0(fig.dir,'/scallop/thermal_habitat_area_scallop_MAB.png'))
  
  ggplot(filter(scallop.SAM.all,max.temp == 18), aes( x= date, y = area.pct))+
    geom_line()+
    # facet_wrap(~max.temp, labeller = 'label_both')+
    ylab('Proportion Area below threshold')+
    ggtitle('Proportion of Scallop Habitat Below 18C')+
    theme_bw()
  ggsave(paste0(fig.dir,'/scallop/thermal_habitat_area_scallop_MAB_18.png'),width = 8, height =5,units = 'in',dpi =300)
  
##Astropectin metrics
astropectin.SAM = read.csv(here::here('data','habitat_area','SAMS','astropectin','thermal_habitable_area_astropectin_SAMS.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))

pdf(paste0(fig.dir,'/astropectin/thermal_habitat_area_astropectin_SAMS.pdf'), width = 10, height = 10)
for(i in 1:length(years)){
  astropectin.SAM.yr = filter(astropectin.SAM, year == years[i])
  
  astropectin.SAM.p = ggplot(astropectin.SAM.yr,aes(x = date, y = area.pct))+
    geom_line()+
    facet_grid(min.temp~SAM,labeller = 'label_both')+
    ylab('Proportion Area below threshold')+
    xlab('')+
    ggtitle(paste0('astropectin - ',years[i], ' (max temp = 30)'))
  
  print(astropectin.SAM.p)
}
dev.off()

astropectin.SAM.all = read.csv(here::here('data','habitat_area','MAB','astropectin','thermal_habitable_area_astropectin_MAB.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))

ggplot(astropectin.SAM.all, aes( x= date, y = area.pct))+
  geom_line()+
  facet_wrap(~min.temp, labeller = 'label_both')+
  ylab('Proportion Area below threshold')+
  ggtitle('Astropectin - max temp = 30')

ggsave(paste0(fig.dir,'/astropectin/thermal_habitat_area_astropectin_MAB.png'))
