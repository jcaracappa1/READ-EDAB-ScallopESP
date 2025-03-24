#Plot a time series of the minimum habitable area by year
library(ggplot2)
library(dplyr)

fig.dir = here::here('figures','habitat_area','scallop')

scallop.SAM = read.csv(here::here('data','habitat_area','SAMS','scallop','thermal_habitable_area_scallop_SAMS.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))%>%
  group_by(SAM,year,max.temp)%>%
  summarise(min.pct = min(area.pct))


ggplot(scallop.SAM,aes(x = year, y = min.pct))+
  geom_line()+
  facet_grid(max.temp~SAM)+
  theme_bw()+
  ylab('Proportion Area below threshold')+
  ggtitle('Scallop - min temp = 0')     
ggsave(paste0(fig.dir,'/minimum_thermal_habitat_area_scallop_SAMS_allyears.png'),width = 16,height = 10)

#Coast wide
scallop.MAB = read.csv(here::here('data','habitat_area','MAB','scallop','thermal_habitable_area_scallop_MAB.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))%>%
  group_by(year,max.temp)%>%
  summarise(min.pct = min(area.pct))

ggplot(scallop.MAB,aes(x = year, y = min.pct))+
  geom_line()+
  facet_wrap(~max.temp, labeller = label_both)+
  theme_bw()+
  ylab('Proportion Area below threshold')+
  ggtitle('Scallop - min temp = 0')     
ggsave(paste0(fig.dir,'/minimum_thermal_habitat_area_scallop_MAB_allyears.png'),width = 16,height = 10)

ggplot(scallop.MAB,aes(x = year, y = 1-min.pct))+
  geom_line()+
  stat_smooth(method = 'lm')+
  facet_wrap(~max.temp, labeller = label_both)+
  theme_bw()+
  ylab('Proportion Area Above threshold')+
  ggtitle('Scallop - min temp = 0')     
ggsave(paste0(fig.dir,'/maximum_thermal_inhabitatable_area_scallop_MAB_allyears.png'),width = 16,height = 10)

#Astropectin
fig.dir = here::here('figures','habitat_area','astropectin')

astropectin.SAM = read.csv(here::here('data','habitat_area','SAMS','astropectin','thermal_habitable_area_astropectin_SAMS.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         min.temp = factor(max.temp))%>%
  group_by(SAM,year,min.temp)%>%
  summarise(min.pct = min(area.pct))


ggplot(astropectin.SAM,aes(x = year, y = min.pct))+
  geom_line()+
  facet_grid(min.temp~SAM)+
  theme_bw()+
  ylab('Proportion Area above threshold')+
  ggtitle('astropectin - min temp = 0')     
ggsave(paste0(fig.dir,'/minimum_thermal_habitat_area_astropectin_SAMS_allyears.png'),width = 16,height = 10)

#Coast wide
astropectin.MAB = read.csv(here::here('data','habitat_area','MAB','astropectin','thermal_habitable_area_astropectin_MAB.csv'),as.is =T)%>%
  mutate(date = as.Date(date),
         max.temp = factor(max.temp))%>%
  group_by(year,max.temp)%>%
  summarise(min.pct = min(area.pct))

ggplot(astropectin.MAB,aes(x = year, y = min.pct))+
  geom_line()+
  facet_wrap(~max.temp, labeller = label_both)+
  theme_bw()+
  ylab('Proportion Area below threshold')+
  ggtitle('astropectin - min temp = 0')     
ggsave(paste0(fig.dir,'/minimum_thermal_habitat_area_astropectin_MAB_allyears.png'),width = 16,height = 10)
