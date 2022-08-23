### Clear memory
rm(list = ls())

# Libraries ---------------------------------------------------------------
suppressPackageStartupMessages(library(sf)) # for spatial things
suppressPackageStartupMessages(library(lwgeom)) # for advanced spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(fitdistrplus)) # for log normal distributions
suppressPackageStartupMessages(library(ggplot2)) # for plotting data
suppressPackageStartupMessages(library(purrr)) # for nested dataframes
suppressPackageStartupMessages(library(stringr))# for editing columns 
suppressPackageStartupMessages(library(tidyverse))# for manipulating data

#reading journey to work and journey to education data
jtwdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/JTW_VISTA_1220_coord.csv",header=T, na.strings="N/A")
jtedata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/JTE_VISTA_1220_coord.csv",header=T, na.strings="N/A")

#reordigng columns and adding missing columns in each data
jtwdata <- jtwdata %>%  
  add_column(destplace = NA, .after ="destlga")

jtedata <- jtedata %>%
  add_column(duration = NA, .after ="arrtime")%>%
  add_column(dist10 = NA,dist11 = NA,dist12 = NA, .after ="dist9")%>%
  add_column(dur10 = NA,dur11 = NA,dur12 = NA, .after ="dur9")%>%
  add_column(mode10 = NA, mode11 = NA,mode12 = NA, .after="mode9")%>%
  add_column(place10 = NA, place11 = NA, place12 = NA, .after="place9")%>%
  add_column(purp10 = NA, purp11 = NA, purp12 = NA, .after="purp9")%>%
  add_column(stop10 = NA, stop11 = NA, stop12 = NA, .after="stop9")%>%
  add_column(time10 = NA ,time11 = NA, time12 = NA, .after="time9")

jtedata <- jtedata %>%
  relocate(jtetraveltime, jte_at, jteelapsedtime, jtedist, jtemode, .after = hhid)%>%
  relocate(dephour, deptime, .after = duration)


#replacing jte/jtw in both dataset with jtwe
names(jtedata) = gsub(pattern = "jte", replacement = "jtwe", x = names(jtedata))
names(jtwdata) = gsub(pattern = "jtw", replacement = "jtwe", x = names(jtwdata))

jtwedata <- rbind(jtwdata,jtedata)

df <- jtwedata %>%
  mutate(
    orig_long = origlong,
    orig_lat = origlat,
    dest_long = destlong,
    dest_lat = destlat
  )
############################################################################################################################################
# filtering data based on geographic extent-----------------------------------------------------------------------------------------------------

studyRegion <- st_read("C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/GIS/data/absRegionsReprojected.sqlite",layer="GCCSA_2016_AUST") %>%
  st_buffer(1)

orig_within_region <- df %>%
  st_as_sf(coords=c("origlong","origlat"),crs=4326) %>%
  st_transform(28355) %>%
  st_snap_to_grid(0.1) %>%
  filter(lengths(st_intersects(., studyRegion,prepared=TRUE,sparse=TRUE)) > 0)

dest_within_region <- df %>%
  st_as_sf(coords=c("destlong","destlat"),crs=4326) %>%
  st_transform(28355) %>%
  st_snap_to_grid(0.1) %>%
  filter(lengths(st_intersects(., studyRegion,prepared=TRUE,sparse=TRUE)) > 0)

############################################################################################################################################
# joining trip data with household&person data-----------------------------------------------------------------------------------------------------
HHdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/H_VISTA_1220_Coord.csv")
Pdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/P_VISTA_1220_Coord.csv")

# note: change orig_within_region to dest_within_region for modelling based on BE features on destinations
###########################################################################################################
jtwe_HHJoined <- merge(orig_within_region,HHdata, by="hhid")
jtwe_HHP_Joined <- merge(jtwe_HHJoined,Pdata, by="persid")

# filtering data based on surveyperiod
jtwe_HHP_1620 <- subset(jtwe_HHP_Joined, surveyperiod=="2016-17" | surveyperiod=="2017-18" | surveyperiod=="2018-19" | surveyperiod=="2019-20")

# filtering work journeys started from home
stopdata<- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/S_VISTA_1220_Coord.csv")
stopdata <- stopdata[, c("persid","stopno","origpurp1")]
stopdata<- subset(stopdata, stopno==1)
stopdata<- subset(stopdata, origpurp1=="At Home")
jtwe_stop <- merge(jtwe_HHP_1620, stopdata, by="persid")
hb_jtwe<- subset(jtwe_stop, stopno==1)
hb_jtwe = hb_jtwe[,!(names(hb_jtwe) %in% c("persid","stopno","origpurp1"))]

# recoding linkmode var (might be changed)

#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Vehicle Driver"] = "Vehicle Driver"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Motorcycle"] = "Vehicle Driver"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Vehicle Passenger"] = "Vehicle Passenger"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Taxi"] = "Vehicle Passenger"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Walking"] = "Walking"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Jogging"] = "Walking"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Bicycle"] = "Bicycle"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Public Bus"] = "Bus"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="School Bus"] = "Bus"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Train"] = "Train"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Tram"] = "Tram"
#hb_jtwe$mainmode[hb_jtwe$jtwemode=="Other"] = "Other"

hb_jtwe <- hb_jtwe %>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Motorcycle', 'Vehicle Driver', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Taxi', 'Vehicle Passenger', .))) %>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Jogging', 'Walking', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Public Bus', 'PT', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Mobility Scooter', 'Bicycle', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'School Bus', 'PT', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Train', 'PT', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'),funs(ifelse(. == 'Tram', 'PT', .)))
hb_jtwe <- hb_jtwe[!(hb_jtwe$jtwemode=="Other"),]

# concatenating modes
hb_jtwe <- hb_jtwe %>%
  unite("combinedmode", c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10','mode11','mode12'), sep ='_', na.rm = TRUE, remove = FALSE)
hb_jtwe$combinedmode2 <- sapply(hb_jtwe$combinedmode, function(x) paste(unique(unlist(str_split(x,"_"))), collapse = "_"))
hb_jtwe$combinedmode2 <- gsub('_Other', '', hb_jtwe$combinedmode2)
hb_jtwe$combinedmode2 <- gsub('Other_', '', hb_jtwe$combinedmode2)

rep_str = c('Vehicle Driver'='Vehicle Driver','Vehicle Passenger'='Vehicle Passenger','Walking'='Walking', 'Bicycle'='Bicycle','Bicycle_PT'= 'PT_walk_Bike', 
            'Bicycle_PT_Walking'='PT_walk_Bike', 'Bicycle_Walking_PT'='PT_walk_Bike', 'PT_Walking'='PT_walk_Bike', 'Vehicle Driver_PT_Walking'='PT_Car', 'Bicycle_PT'='PT_Walk_Bike',
            'Vehicle Driver_Vehicle Passenger_PT_Walking'='PT_Car','Vehicle Driver_Walking_PT'='PT_Car','Vehicle Passenger_PT_Walking'='PT_Car',
            'Vehicle Passenger_Vehicle Driver_Walking_PT'='PT_Car', 'Vehicle Passenger_Walking_PT'='PT_walk_Bike', 'Walking_PT'='PT_walk_Bike','Walking_PT_Vehicle Driver'='PT_Car',
            'Walking_PT_Vehicle Passenger'='PT_walk_Bike','Vehicle Driver_Bicycle'='Vehicle Driver','Vehicle Driver_Vehicle Passenger'='Vehicle Driver',
            'Vehicle Driver_Vehicle Passenger_Walking'='Vehicle Driver','Vehicle Driver_Walking'='Vehicle Driver','Vehicle Driver_Walking_Vehicle Passenger'='Vehicle Driver',
            'Vehicle Passenger_Vehicle Driver_Walking'='Vehicle Driver','Vehicle Passenger_Walking'='Vehicle Passenger','Vehicle Passenger_Walking_Vehicle Driver'=
            'Vehicle Driver','Walking_Bicycle'='Walking', 'Bicycle_Walking'='Bicycle', 'Walking_Vehicle Driver'='Vehicle Driver','Walking_Vehicle Passenger'='Vehicle Passenger',
            'Vehicle Passenger_Vehicle Driver'='Vehicle Driver','Vehicle Passenger_PT_Walking'='PT_Walk_Bike','Vehicle Passenger_Walking_PT'='PT_Walk_Bike','Vehicle Driver_Vehicle Passenger'=
            'Vehicle Driver','Vehicle Passenger_PT_Walking'='PT_Walk_Bike','Vehicle Driver_PT_walking'='PT_Car','PT_walk_Bike_Walking'='PT_walk_Bike','Vehicle Driver_PT_walk_Bike'='PT_Car',
            'Vehicle Passenger_PT_walk_Bike'='PT_Car','PT_walk_Bike_Vehicle Driver'='PT_Car','PT_walk_Bike_Vehicle Passenger'='PT_Car','Vehicle Passenger_PT_Car'='PT_Car','Vehicle Driver_PT'='PT_Car',
            'Vehicle Passenger_PT'='PT_Car')

hb_jtwe$combinedmode3 <- str_replace_all(hb_jtwe$combinedmode2, rep_str)

# recoding carlicence var
hb_jtwe$licence[hb_jtwe$carlicence=="Full Licence"] = 1
hb_jtwe$licence[hb_jtwe$carlicence=="Green Probationary Licence"] = 1
hb_jtwe$licence[hb_jtwe$carlicence=="Learners Permit"] = 1
hb_jtwe$licence[hb_jtwe$carlicence=="Red Probationary Licence"] = 1
hb_jtwe$licence[hb_jtwe$carlicence=="No Car Licence"] = 0

# recoding No of cars in HH
hb_jtwe$carsno[hb_jtwe$cars == 0] = 0
hb_jtwe$carsno[hb_jtwe$cars == 1] = 1
hb_jtwe$carsno[hb_jtwe$cars== 2] = 2
hb_jtwe$carsno[hb_jtwe$cars >=3] = 3

#recoding sex var
hb_jtwe$gender[hb_jtwe$sex=="M"] = 1
hb_jtwe$gender[hb_jtwe$sex=="F"] = 0

#recoding household income var
hb_jtwe$hhinc_an = hb_jtwe$hhinc*52
hb_jtwe$hhincat[hb_jtwe$hhinc_an<=59999] = 1
hb_jtwe$hhincat[60000<=hb_jtwe$hhinc_an & hb_jtwe$hhinc_an<=94999] = 2
hb_jtwe$hhincat[95000<=hb_jtwe$hhinc_an & hb_jtwe$hhinc_an<=139999] = 3
hb_jtwe$hhincat[140000<=hb_jtwe$hhinc_an & hb_jtwe$hhinc_an<=189999] = 4
hb_jtwe$hhincat[hb_jtwe$hhinc_an>=190000] = 5

# exporting work and education trips to csv format
hb_jtwe$geometry <- gsub(',', '-', hb_jtwe$geometry)
write.csv(hb_jtwe,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/hb_jtwe.csv")
