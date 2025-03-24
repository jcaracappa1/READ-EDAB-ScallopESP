#plot functions for habitable area time series by SAM
library(ggplot2)
library(dplyr)

years = 1993:2023

fig.dir = here::here('figures','RTA working paper')
##Scallop metrics

scallop.SAM.all = read.csv(here::here('data','inhabitable_area','MAB','scallop','thermal_inhabitable_area_scallop_MAB.csv'),as.is =T)%>%
  filter(min.temp == 18) %>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))

lm.d = lm(area.pct~date,scallop.SAM.all)
summary(lm.d)


#Daily area above 18C
ggplot(scallop.SAM.all, aes( x= date, y = area.pct))+
  geom_line()+
  stat_smooth(method ='lm')+
  ylab('Proportion of Area Above 18\u00B0C')+
  theme_bw()+
  xlab('')
ggsave(paste0(fig.dir,'/thermal_unihabitable_area_scallop_MAB_18C_daily.png'))
  
#Annual max area above 18C
scallop.SAM.yr = scallop.SAM.all %>%
  group_by(year)%>%
  summarise(area.pct = max(area.pct,na.rm=T))

lm.y = lm(area.pct~year,scallop.SAM.yr)
summary(lm.y)
ggplot(scallop.SAM.yr, aes( x= year, y = area.pct))+
    geom_line()+
    stat_smooth(method ='lm')+
    ylab('Maximum Annual Proportion of Area Above 18\u00B0C')+
    theme_bw()+
    xlab('')
ggsave(paste0(fig.dir,'/thermal_unihabitable_area_scallop_MAB_18C_annual_max.png'))
  
#Seasonal area above 18C
season.df = data.frame(season.name = rep(c('Winter','Spring','Summer','Fall'),each =3),season.num = rep(1:4,each =3),month.num = 1:12, month.name = month.name)

scallop.SAM.season = scallop.SAM.all %>%
  mutate(date = as.Date(date),
         month.num = as.numeric(format(date,format = '%m')))%>%
  left_join(season.df)%>%
  group_by(year,season.name)%>%
  summarise(area.pct.max = max(area.pct,na.rm=T),
            area.pct.mean = mean(area.pct,na.rm=T))
scallop.SAM.season$season.name = factor(scallop.SAM.season$season.name,levels = c('Winter','Spring','Summer','Fall'))

fall = scallop.SAM.season %>% filter(season.name == 'Fall')
lm.fall.mean = lm(area.pct.mean ~ year,fall)
summary(lm.fall.mean)
lm.fall.max = lm(area.pct.max~year,fall)
summary(lm.fall.max)
ggplot(scallop.SAM.season, aes( x= year, y = area.pct.mean))+
  geom_line()+
  facet_wrap(~season.name)+
  stat_smooth(method ='lm')+
  ylab('Mean Annual Proportion of Area Above 18\u00B0C')+
  theme_bw()+
  xlab('')
ggsave(paste0(fig.dir,'/thermal_unihabitable_area_scallop_MAB_18C_seasonal_mean.png'))

ggplot(scallop.SAM.season, aes( x= year, y = area.pct.max))+
  geom_line()+
  facet_wrap(~season.name)+
  stat_smooth(method ='lm')+
  ylab('Maximum Annual Proportion of Area Above 18\u00B0C')+
  theme_bw()+
  xlab('')
ggsave(paste0(fig.dir,'/thermal_unihabitable_area_scallop_MAB_18C_seasonal_max.png'))

