#load packages

############
library(terra)


library(tidyverse)

library(amt)

library(lubridate)

library(ggplot2)

library(dplyr)

library(raster)
library(leaflet)

library(Rcpp)
library(lubridate)
library(knitr)

library(raster)

########
# Specify the folder containing your raster files
setwd("C:\\Users\\CWise\\OneDrive - California Department of Fish and Wildlife\\Research projects\\R4\\CDFW-Davis_Mountain Lion\\covs")

#### load rasters
clc<-raster("clc_utm.tif")
names(clc)<-"clc"
d2mnroad<-raster("d2min_road_utm.tif")
names(d2mnroad)<-"D2mn_road"
d2mjroad<-raster("d2mjroad_utm.tif")
names(d2mjroad)<-"d2mjroad"
ftc<-raster("ftc_utm.tif")
names(ftc)<-"ftc"
tpi<-raster("tpi90_utm.tif")
names(tpi)<-"tpi"
traffic<-raster("traffic3000_utm.tif")
names(traffic)<-"traffic"


tri<-raster("tri_utm.tif")
names(tri)<-"tri"

rdens<-raster("rdens_utm.tif")
names(rdens)<-"rdens"


imperv<-raster("imperv_utm.tif")
names(imperv)<-"imperv"

fvsrb<-raster("fvsrb90_utm.tif")
names(fvsrb)<-"fvsrb"
crop<-raster("dist_crop_utm.tif")
names(crop)<-"crop"

tpi6k<-raster("tpi6000_utm.tif")
names(tpi6k)<-"tpi6k"
cropcv<-raster("crop6000_utm.tif")
names(cropcv)<-"cropcv"




## load locations

# adult range 

adult_rs<-read_csv("C:\\Users\\CWise\\Desktop\\projects\\data\\UCD_lion_Data\\lion_UCD_916utm.csv")
names(adult_rs)
adult_rs$Time<-as.POSIXlt(adult_rs$Time, tz = "", format ="%m/%d/%Y %H:%M")

adult_rs <- adult_rs %>% drop_na()
summary(is.na(adult_rs))
head(adult_rs)
plot(adult_rs$X,adult_rs$Y)



#turn into tracks
trk <- adult_rs %>% mk_track(X, Y, Time, ID, all_cols = FALSE)
head(trk)

# Create a track from the adult_rs dataset
tail(trk)

trk1 <- trk %>% nest(data = -"ID")

##############


#

# Resample the track data to consistent time
trk2 <- trk1 %>% mutate(steps = map(data, function(x)
  x %>% track_resample(rate = minutes(180), tolerance = minutes(10)) %>% steps_by_burst()))



ssf<-trk2 %>% unnest(cols = steps)


ssf1 <- ssf %>%  random_steps(n_control = 15)

plot(ssf1$x1_,ssf1$y1_) 
##########################################################################
#extract covariates to locations, for some reason both doesn't work in the package 
#version I am using so roundabout way
#extract starts
ssf1 <- ssf1 %>% extract_covariates(clc, where = "start")


ssf1 <- ssf1 %>% extract_covariates(d2mjroad, where = "start")


ssf1 <- ssf1 %>% extract_covariates(d2mnroad, where = "start")


ssf1 <- ssf1 %>% extract_covariates(ftc, where = "start")
ssf1 <- ssf1 %>% extract_covariates(tpi, where = "start")
ssf1 <- ssf1 %>% extract_covariates(traffic, where = "start")
ssf1 <- ssf1 %>% extract_covariates(tri, where = "start")
ssf1 <- ssf1 %>% extract_covariates(rdens, where = "start")
ssf1 <- ssf1 %>% extract_covariates(fvsrb, where = "start")
ssf1 <- ssf1 %>% extract_covariates(imperv, where = "start")
ssf1 <- ssf1 %>% extract_covariates(crop, where = "start")
ssf1 <- ssf1 %>% extract_covariates(tpi6k, where = "start")
ssf1 <- ssf1 %>% extract_covariates(cropcv, where = "start")
names(ssf1)


ssf1<- ssf1 %>% rename( 
  cropcv_start=cropcv,
  clc_start=clc,
  d2mjroad_start=d2mjroad,
  D2mn_road_start=D2mn_road,
  ftc_start =  ftc,
  tpi_start=tpi,
  traffic_start=traffic,
  tri_start=tri,
  rdens_start=  rdens,
  fvsrb_start=fvsrb,
  imperv_start=imperv,
  crop_start=crop,
  tpi6k_start=tpi6k)

names(ssf1)

#xtract ends
ssf1 <- ssf1 %>% extract_covariates(clc, where = "end")


ssf1 <- ssf1 %>% extract_covariates(d2mjroad, where = "end")


ssf1 <- ssf1 %>% extract_covariates(d2mnroad, where = "end")


ssf1 <- ssf1 %>% extract_covariates(ftc, where = "end")
ssf1 <- ssf1 %>% extract_covariates(tpi, where = "end")
ssf1 <- ssf1 %>% extract_covariates(traffic, where = "end")
ssf1 <- ssf1 %>% extract_covariates(tri, where = "end")
ssf1 <- ssf1 %>% extract_covariates(rdens, where = "end")
ssf1 <- ssf1 %>% extract_covariates(fvsrb, where = "end")
ssf1 <- ssf1 %>% extract_covariates(imperv, where = "end")
ssf1 <- ssf1 %>% extract_covariates(crop, where = "end")
ssf1 <- ssf1 %>% extract_covariates(tpi6k, where = "end")
ssf1 <- ssf1 %>% extract_covariates(cropcv, where = "end")
ssf1<- ssf1 %>% rename( 
  
  d2mjroad_end=d2mjroad,
  clc_end=clc,
  
  D2mn_road_end=D2mn_road,
  ftc_end =  ftc,
  tpi_end=tpi,
  traffic_end=traffic,
  tri_end=tri,
  rdens_end=  rdens,
  fvsrb_end=fvsrb,
  imperv_end=imperv,
  crop_end=crop,
  tpi6k_end=tpi6k,
  cropcv_end=cropcv)

names(ssf1)


sf1 <- ssf1 %>% 
  mutate( cos_ta = cos(ta_),  log_sl = log(sl_))
#############

#save dataspace
saveRDS(sf1,file="G:\\Mountain Lion\\CDFW-Davis_Mountain Lion\\lion_UCD_extract.rds")
#######################

#stack landscape rasters for your environment


env.list<- list(rast(clc),rast(tpi),rast(d2mjroad),rast(d2mnroad),rast(crop),rast(ftc),rast(traffic),rast(tri),rast(rdens),rast(fvsrb),rast(imperv))
env.all<-rast(env.list)
plot(env.all)



##################################################


######pick the IDs of the individuals you want to simulate



######pick the IDs of the individuals you want to simulate
unique_ids<-c("M325", "M326", "F327", "M329" ,"F330" ,"M331" ,"M332" ,"M334" ,"F339" ,"M383",
              "F384" ,"F385" ,"F386" ,"F388", "M389",  "M401" )

#################################################################






# Create an empty data frame to store the results
results.df <- data.frame()

# Loop through each unique ID




Mlist<-data.frame()

# Loop through each unique ID
for(id in unique_ids) {
  # Subset the data for the current ID
  ind <- subset(sf1, ID == id)
  # Set up the start value for each ID (modify as needed)
  #uses second location as start since first doesn't have an associated TA
  start <- make_start(ind[2, ])
  
  
  
  
  
  # Fit the conditional logistic regression model for the current ID
  m_0 <- fit_clogit(ind, formula =  case_ ~ -1 + 
                      x2_ + y2_ + I(x2_^2 + y2_^2) + 
                      cos(ta_) + sl_ + log(sl_) +
                      cos(ta_):log(sl_) + 
                      tri_start:log(sl_) +  
                      clc_start:log(sl_) +  
                      rdens_start:cos(ta_) + 
                      ftc_start:cos(ta_) + 
                      tpi_start:cos(ta_) + 
                      traffic_start:cos(ta_) +  
                      tri_start:cos(ta_) +  
                      D2mn_road_end + 
                      d2mjroad_end + 
                      ftc_end + 
                      # tpi_end + 
                      traffic_end +  
                      tri_end +  
                      fvsrb_end + 
                      imperv_end + 
                      clc_end + 
                      crop_end+
                      strata(step_id_))
  
  # Print the summary of the fitted model

  M_sum<-  summary(m_0)
  Mlist<-list(M_sum,Mlist)
  # Calculate the redistribution kernel
  k2 <- redistribution_kernel(m_0, map = env.all, start = start, 
                              landscape = "continuous", tolerance.outside = .75,
                              n.control = 1e4)
  
  # Run the simulation (change the loop number to adjust simulation count)
  for(i in 1:10) {  # You can adjust '1' for more simulations
    #nsteps is how many consecutive locations you will simulate, 240 is 1 month for this data
    result <- simulate_path(k2, n.steps =240)
    
    # Append the result along with ID and run number to the results data frame
    res <- cbind(result, ID = id, RunNumber = i,s_x=start$x_,s_y=start$y_)
    results.df <- rbind(results.df, res)
  }
}


# create plot of raster, start locations and simulated locations
plot(env.all[[6]])

points(ssfv$x1_,ssfv$y1_,col="green")

points(start$x_,start$y_,col="yellow")

points(results.df$x_,results.df$y_,col="blue")
points(results.df$s_x,results.df$s_y,col="yellow")

#######################################################################


#   for novel locations


####### create virtual starting locations, formatted as a series of locations
#so you have a start and turn angle

vids<-read.csv("C:\\Users\\CWise\\Desktop\\projects\\data\\UCD_lion_Data\\virt_starts5.csv")
head(vids)
vids$Time<-as.POSIXlt(vids$Time, tz = "", format ="%Y-%m-%d %H:%M")
names(vids)<-c("OID","CID","X","Y","ID","Time")
vids<- vids %>% drop_na()
summary(is.na(vids))

#turn into tracks
trkv <- vids %>% mk_track(X, Y, Time, ID, all_cols = FALSE)
head(trkv)

# Create a track from the adult_rs dataset
tail(trk)

trk1v <- trkv %>% nest(data = -"ID")

##############


# Resample the track data
trk2v <- trk1v %>% mutate(steps = map(data, function(x)
  x %>% track_resample(rate = minutes(180), tolerance = minutes(180)) %>% steps_by_burst()))

ssfv<-trk2v %>% unnest(cols = steps)

# Get unique virtual IDs you want to have as starting areas


unique_vids<-c( "vert18",  "vert20", "vert26", "vert27", "vert28", 
                "vert31", "vert32", "vert33", "vert39", "vert44", "vert45", "vert46", "vert55", "vert56",
                "vert57", "vert58", "vert67", "vert68", "vert69", "vert70", "vert71", "vert81")


#############################################################


# Loop through each unique ID
for(id in unique_vids) {
  # Subset the data for the current ID
  vind <- subset(ssfv, ID == id)
  
  # Set up the start value for each ID (modify as needed)
  start <- make_start(vind[2, ])
  
  
  
  
  
  
  
  
  
  # Loop through each unique ID
  for(id in unique_ids) {
    # Subset the data for the current ID
    ind <- subset(f6, ID == id)
    
    
    # Fit the conditional logistic regression model for the current ID
    m_0 <- fit_clogit(ind, formula =  case_ ~ -1 + 
                        x2_ + y2_ + I(x2_^2 + y2_^2) + 
                        cos(ta_) + sl_ + log(sl_) +
                        cos(ta_):log(sl_) + 
                        tri_start:log(sl_) +  
                        clc_start:log(sl_) +  
                        rdens_start:cos(ta_) + 
                        ftc_start:cos(ta_) + 
                        tpi_start:cos(ta_) + 
                        traffic_start:cos(ta_) +  
                        tri_start:cos(ta_) +  
                        D2mn_road_end + 
                        d2mjroad_end + 
                        ftc_end + 
                        # tpi_end + 
                        traffic_end +  
                        tri_end +  
                        fvsrb_end + 
                        imperv_end + 
                        clc_end + 
                        crop_end+
                        strata(step_id_))
    
    # Print the summary of the fitted model
   
  M_sum<-  summary(m_0)
  Mlist<-list(M_sum,Mlist)
    # Calculate the redistribution kernel
    k2 <- redistribution_kernel(m_0, map = env.all, start = start, 
                                landscape = "continuous", tolerance.outside = .75,
                                n.control = 1e4)
    
    # Run the simulation (change the loop number to adjust simulation count)
    for(i in 1:1) {  # You can adjust '1' for more simulations
      result <- simulate_path(k2, n.steps =240)
      
      # Append the result along with ID and run number to the results data frame
      res <- cbind(result, ID = id, RunNumber = i,Vstart=unique(vind$ID),s_x=start$x_,s_y=start$y_)
      results.df <- rbind(results.df, res)
    }
  }
}
?simulate_path
head(results.df) 
?redistribution_kernel
plot(env.all[[6]])

points(ssfv$x1_,ssfv$y1_,col="green")

points(start$x_,start$y_,col="yellow")

points(results.df$x_,results.df$y_,col="blue")
points(results.df$s_x,results.df$s_y,col="yellow")


unique(results.df$ID)

write.csv(results.df,file="C:\\Users\\CWise\\Desktop\\projects\\data\\UCD_lion_Data\\simtest10fold.csv")

