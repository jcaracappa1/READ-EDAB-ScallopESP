#Make scallop habitable area wrapper script
library(terra)
library(dplyr)
library(ggplot2)
figure.out.dir = here::here('figures','NEFMC_12_2025')
file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp')),'+proj=longlat +datum=WGS84 +no_defs ')
# file.shp = terra::vect(here::here('geometry','Scallop_2024_MAB_Est_Areas_SAMS_CASA_UTM18_EDAB_80meters.shp'))
# file.shp = terra::project(terra::vect(here::here('geometry','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp'),crs = '+proj=longlat'),'+proj=longlat +datum=WGS84 +no_defs ')
# file.shp =terra::vect(here::here('geometry','MAB_Est_Areas_SAMS_CASA_UTM18_EDAB.shp'),crs = '+proj=longlat')
# SAM.names = grep('_LT80',file.shp$SAMS,value =T)
SAM.names = file.shp$SAMS
years = 2025
# max.val.scallop = seq(16,19,0.5)
max.val.scallop = c(17,18,19)
min.val.astropectin = seq(4,7,1)
j.day.start = as.numeric(format(as.Date('2020-09-01'),format = '%j'))
j.day.end = as.numeric(format(as.Date('2020-11-30'),format = '%j'))
input.dir = "W:/GLORYS/glorys_bottomT/cmems_mod_glo_phy_anfc_0.083deg_P1D-m/bottomT/"
output.dir = here::here('data','NEFMC_12_2025')
####Scallop Habitable####
#make_habitable_area_scallop_SAM
source(here::here('R','make_SAM_mask_area.R'))
source(here::here('R','make_mask_area.R'))
make_SAM_mask_area(
  out.dir = here::here('data','habitat_area','SAMS','scallop','/'),
  # input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.dir = input.dir,
  # input.prefix = 'GLORYS_daily_BottomTemp_',
  input.prefix = 'GLORYS_REANALYSIS_DAILY_cmems_mod_glo_phy_anfc_0.083deg_P1D-m_bottomT_',
  file.shp = file.shp,
  years = years,
  min.vals = 0,
  max.vals = max.val.scallop,
  SAM.names = SAM.names,
  out.df.name = 'thermal_habitable_area_scallop_SAMS',
  shp.name.var =1,
  j.day.start = j.day.start,
  j.day.end = j.day.end
)
#make_habitable_area_scallop_SAM
make_mask_area(
  out.dir = here::here('data','habitat_area','MAB','scallop','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = 0,
  max.vals = max.val.scallop,
  out.df.name = 'thermal_habitable_area_scallop_MAB',
  j.day.start = j.day.start,
  j.day.end = j.day.end
)

####Scallop Inhabitable #####

#make inhabitable area outside above temp range
make_SAM_mask_area(
  out.dir = here::here('data','inhabitable_area','SAMS','scallop','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = max.val.scallop,
  max.vals = 50,
  SAM.names = SAM.names,
  out.df.name = 'thermal_inhabitable_area_scallop_SAMS',
  shp.name.var =1,
  j.day.start = j.day.start,
  j.day.end = j.day.end
)

#Outside 18deg for working paper
make_mask_area(
  out.dir = here::here('data','habitat_area','MAB','scallop_annual','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = 18,
  max.vals = 50,
  out.df.name = 'scallop_unihabitable_18',
  j.day.start = 1,
  j.day.end = 365
)

#make_habitable_area_scallop_SAM
make_mask_area(
  out.dir = here::here('data','inhabitable_area','MAB','scallop','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = max.val.scallop,
  max.vals = 50,
  out.df.name = 'thermal_inhabitable_area_scallop_MAB',
  j.day.start = 1,
  j.day.end = 365
)

####Astropectin Habitable ####
make_SAM_mask_area(
  out.dir = here::here('data','habitat_area','SAMS','astropectin','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = min.val.astropectin,
  max.vals = 50,
  SAM.names = SAM.names,
  out.df.name = 'thermal_habitable_area_astropectin_SAMS',
  shp.name.var =1,
  j.day.start = j.day.start,
  j.day.end = j.day.end
)
make_mask_area(
  out.dir = here::here('data','habitat_area','MAB','astropectin','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = min.val.astropectin,
  max.vals = 50,
  out.df.name = 'thermal_habitable_area_astropectin_MAB',
  j.day.start = j.day.start,
  j.day.end = j.day.end
)
#Outside 18deg for working paper
make_mask_area(
  out.dir = here::here('data','habitat_area','MAB','scallop_annual','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = 18,
  max.vals = 50,
  out.df.name = 'scallop_unihabitable_18',
  j.day.start = 1,
  j.day.end = 365
)

#### Astropectin Inhabitable ####
make_SAM_mask_area(
  out.dir = here::here('data','inhabitable_area','SAMS','astropectin','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = 0,
  max.vals = min.val.astropectin,
  SAM.names = SAM.names,
  out.df.name = 'thermal_inhabitable_area_astropectin_SAMS',
  shp.name.var =1,
  j.day.start = j.day.start,
  j.day.end = j.day.end
)
make_mask_area(
  out.dir = here::here('data','inhabitable_area','MAB','astropectin','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = -5,
  max.vals = min.val.astropectin,
  out.df.name = 'thermal_inhabitable_area_astropectin_MAB',
  j.day.start = j.day.start,
  j.day.end = j.day.end
)

####Astropectin & Scallop Habitable ####
make_SAM_mask_area(
  out.dir = here::here('data','habitat_area','SAMS','astropectin-scallop','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = c(4,6),
  max.vals = c(17,19),
  SAM.names = SAM.names,
  out.df.name = 'thermal_habitable_area_astropectin_scallop_SAMS',
  shp.name.var =1,
  j.day.start = j.day.start,
  j.day.end = j.day.end
)
make_mask_area(
  out.dir = here::here('data','habitat_area','MAB','astropectin-scallop','/'),
  input.dir = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/',
  input.prefix = 'GLORYS_daily_BottomTemp_',
  file.shp = file.shp,
  years = years,
  min.vals = c(4,6),#min.val.astropectin,
  max.vals = c(17,19),#max.val.scallop,
  out.df.name = 'thermal_habitable_area_astropectin_scallop_MAB',
  j.day.start = j.day.start,
  j.day.end = j.day.end
)



#plot_habitable_area_map_scallop
source(here::here('R','plot_habitable_area_map.R'))
plot_habitable_area_map(
  file.shp= file.shp,
  years = years,
  min.vals = 0,
  max.vals = max.val.scallop,
  data.dir = here::here('data','habitat_area','MAB','scallop'),
  figure.dir = here::here('figures','inhabitable_area','scallop',''),
  output.dir = here::here('data','inhabitable_days','scallop',''),
  outside =T
)

#plot_habitable_area_map_astropectin
plot_habitable_area_map(
  file.shp= file.shp,
  years = years,
  min.vals = min.val.astropectin,
  max.vals = 50,
  data.dir = here::here('data','habitat_area','MAB','astropectin'),
  figure.dir = here::here('figures','inhabitable_area','astropectin',''),
  output.dir = here::here('data','inhabitable_days','astropectin',''),
  outside = T
)

#plot_min_habitable_area
source(here::here('R','plot_min_habitable_area.R'))

#plot_scallop_degree_days
source(here::here('R','make_degree_day_MAB.R'))
make_degree_day_MAB(
  out.dir = here::here('data'),
  out.df.name = 'degree_days_over_threshold_scallop_MAB',
  input.dir = here::here('data','inhabitable_area','MAB','scallop','/'),
  input.prefix = 'GLORYS_BT_',
  file.shp = file.shp,
  SAM.names = SAM.names,
  plot =T,
  figure.dir = here::here('figures','degree_day')
)

make_degree_day_MAB(
  out.dir = here::here('data'),
  out.df.name = 'degree_days_over_18C_scallop_MAB',
  input.dir = here::here('data','inhabitable_area','MAB','scallop','/'),
  input.prefix = 'GLORYS_BT_18_50_mask',
  file.shp = file.shp,
  SAM.names = SAM.names,
  plot =T,
  figure.dir = here::here('figures','NEFMC_12_2025')
)

#make astropectin degree days #Doesnt work
make_degree_day_MAB(
  out.dir = here::here('data'),
  out.df.name = 'degree_days_over_threshold_astropectin_SAM',
  input.dir = here::here('data','inhabitable_area','SAMS','astropectin','/'),
  input.prefix = 'GLORYS_BT_',
  file.shp = file.shp,
  SAM.names = SAM.names,
  plot =T,
  figure.dir = here::here('figures','degree_day')
)
  