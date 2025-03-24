#Plot the daily overlap between scallop and astropectin habitats based on threshold combinations
library(terra)
library(ggplot2)

star.min.t = c(4,5,6,7)
scallop.max.t = seq(16.5,19,0.5)

years = 1993:2022

combs = expand.grid(star.min.t = star.min.t, scallop.max.t = scallop.max.t)

i=y=1
for(y in 1:length(years)){

  for(i in 1:nrow(combs)){
    
    star.data = rast(here::here('data','habitat_area','SAMS','astropectin','shelf',paste0('GLORYS_BT_SAMS_mask_',combs$star.min.t[i],'_',years[y],'.nc')))
    scallop.data = rast(here::here('data','habitat_area','SAMS','scallop','shelf',paste0('GLORYS_BT_SAMS_mask_',combs$scallop.max.t[i],'_',years[y],'.nc')))
    
    ####CHANGE TO JUST LOOK AT WINDOW BETWEEN MIN ASTROPECTIN TEMPERATURE AND MAX SCALLOP TEMPERATURE IN COMBINATION SIMILAR TO MAKE_HATIABLE-AREA-SCALLOP_MAB
  }
}
