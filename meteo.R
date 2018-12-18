## METEO FUNCTIONS ##

library(reticulate)
library(oce)

#
# How to install bee_meteo python library?
# pip install -i https://217.182.160.171:9002/bgruser/stable/ --cert 217.182.160.171.pem bee_meteo
# 

# Deg2Rad
degrees.to.radians<-function(degrees=45,minutes=30){
  if(!is.numeric(minutes)) stop("Please enter a numeric value for minutes!\n")
  if(!is.numeric(degrees)) stop("Please enter a numeric value for degrees!\n")
  decimal<-minutes/60
  c.num<-degrees+decimal
  radians<-c.num*pi/180
  radians
}

# Get real historical weather data
get_weather <- function(ts_from, ts_to, lat, lon, tz="UTC"){
  api_key = "861fe2c5c052c028a60a834ab6ab324d"
  CAMS_registered_mails = c("gerardmor@gmail.com","gmor@cimne.upc.edu","fbgerardmor@gmail.com","gmor@macs.udl.cat")
  use_virtualenv("/home/gerard/.virtualenvs/beeMeteo/")
  bee_meteo <- reticulate::import("bee_meteo")
  wdf <- bee_meteo$historical_weather(
    api_key=api_key,
    cams_registered_mails=CAMS_registered_mails,
    lat=lat,
    lon=lon,
    ts_from=ts_from,
    ts_to=ts_to,
    tz=tz,
    units="si",
    format="list",
    csv_export=T
  )$hourly
  if (!is.null(wdf)){
    
    # Read the downloaded data and generate the weather dataframe
    wdf <- do.call(cbind,wdf)
    df <- data.frame(
      time=as.POSIXct(as.character(wdf[,'time']), format="%Y-%m-%dT%H:%M:%S.000Z", tz="UTC"),
      temperature=as.numeric(as.character(wdf[,'temperature'])),
      humidity=as.numeric(as.character(wdf[,'humidity'])),
      GHI=as.numeric(as.character(wdf[,'GHI'])),
      BNI=as.numeric(as.character(wdf[,'BNI'])),
      windSpeed=as.numeric(as.character(wdf[,'windSpeed'])),
      windDirection=as.numeric(as.character(wdf[,'windBearing']))
    )
    
    # Sun azimuth and elevation
    sun_df<-as.data.frame(do.call(cbind,sunAngle(t=df$time,latitude = lat,longitude = lon,useRefraction = T)))
    sun_df$altitude <- ifelse(sun_df$altitude<0,0,sun_df$altitude)
    df$sunElev <- sun_df$altitude
    df$sunAzimuth <- ifelse(sun_df$altitude>0,sun_df$azimuth,0)
    
    # Beam Vertical Irradiance in North, east, south and west
    df$BVI <- df$BNI*cos(degrees.to.radians(df$sunElev))
    BVIew <- df$BVI*sin(degrees.to.radians(180-df$sunAzimuth))
    BVIsn <- df$BVI*cos(degrees.to.radians(180-df$sunAzimuth))
    df$BVIe<- ifelse(BVIew>=0,BVIew,0)
    df$BVIw<- ifelse(BVIew<0,abs(BVIew),0)
    df$BVIs<- ifelse(BVIsn>=0,BVIsn,0)
    df$BVIn<- ifelse(BVIsn<0,abs(BVIsn),0)

    # windSpeed in North, east, south and west
    windSpeed_ew <- df$windSpeed*sin(degrees.to.radians(180-df$windDirection))
    windSpeed_sn <- df$windSpeed*cos(degrees.to.radians(180-df$windDirection))
    df$windSpeed_e<- ifelse(windSpeed_ew>=0,windSpeed_ew,0)
    df$windSpeed_w<- ifelse(windSpeed_ew<0,abs(windSpeed_ew),0)
    df$windSpeed_s<- ifelse(windSpeed_sn>=0,windSpeed_sn,0)
    df$windSpeed_n<- ifelse(windSpeed_sn<0,abs(windSpeed_sn),0)
    
    # Delete deprecated objects and return df
    rm(wdf)
    gc(verbose = F)
    py_gc <- import("gc")
    py_gc$collect()
    return (df)
  }
  
}