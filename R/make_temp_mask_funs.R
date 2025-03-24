#Function to extract value range from GLORYS data


# file.in = 'C:/Users/Joseph.Caracappa/Documents/Data/GLORYS/GLORYS_daily/GLORYS_daily_BottomTemp_2022.nc'
# min.val = 18
# max.val = 30
# file.shp = terra::project(terra::vect(here::here('geometry','archive','MAB_ESTIMATION_AREAS_2023_UTM18_PDT_NYB.shp')),' +proj=longlat +datum=WGS84 +no_defs ')
# units = 'km'

make_temp_mask = function(file.in, file.shp, min.val, max.val, j.day.start,j.day.end){
  
  data.rast = terra::rast(file.in)
  
  data.time.j = as.numeric(format(as.Date(time(data.rast)),format = '%j'))
  which.time = data.time.j[which(data.time.j>= j.day.start & data.time.j <= j.day.end)]
  data.crop.time = terra::subset(data.rast,which.time)
  
  data.crop = terra::crop(terra::mask(data.crop.time,file.shp, touches = T),file.shp)
  
  data.window =  terra::clamp(data.crop, lower = min.val, upper = max.val,values = F)
  
  # plot(subset(data.window,10))
  
  return(data.window)

}

#Function to calculate area for shape
make_shp_area = function(file.in,file.shp,units){
  
  data.rast = terra::rast(file.in)
  data.rast = terra::subset(data.rast,1)
  
  data.crop = terra::crop(terra::mask(data.rast,file.shp, touches = T),file.shp)
  
  shp.area = terra::expanse(data.crop,unit = units)$area
  
  return(shp.area)
}

#Function to extract the percent of habitat that is within range all year
#if type = 'total' -> total area where habitat is always in temperature range
#if type = 'ts' -> time series of total area that meets conditions per day

# data = make_temp_mask(file.in,file.shp,min.val,max.val)
# area.shp = file.shp


make_shp_temp_area = function(data, area.shp,units, type = 'total'){
  
  data.binary = (data *0)+1
  # plot(subset(data.binary,300))
  
  if(type == 'total'){
    ntime = length(terra::time(data.binary))
    
    data.sum =app(data.binary,sum,na.rm=T)
    data.less = (data.sum == ntime)
    data.all = data.sum * data.less
    # plot(data.all)
    
    values(data.all)[values(data.all) == 0] = NA
    # plot(data.all)
    # plot(area.shp,add =T)
    # values(data.mask)
    
    area.out = expanse(data.all, unit = units)$area
    
  }else if(type == 'ts'){
    
    area.out = expanse(data.binary, unit = units)
  }
  return(area.out)
  
}
