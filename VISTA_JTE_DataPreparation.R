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
suppressPackageStartupMessages(library(stringr))

jtedata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/JTE_VISTA_1220_coord.csv",header=T, na.strings="N/A")
df <- jtedata %>%
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
jte_HHJoined <- merge(orig_within_region,HHdata, by="hhid")
jte_HHP_Joined <- merge(jte_HHJoined,Pdata, by="persid")

# filtering data based on surveyperiod
jte_HHP_1620 <- subset(jte_HHP_Joined, surveyperiod=="2016-17" | surveyperiod=="2017-18" | surveyperiod=="2018-19" | surveyperiod=="2019-20")

# filtering work journeys started from home
stopdata<- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/S_VISTA_1220_Coord.csv")
stopdata <- stopdata[, c("persid","stopno","origpurp1")]
stopdata<- subset(stopdata, stopno==1)
stopdata<- subset(stopdata, origpurp1=="At Home")
jte_stop <- merge(jte_HHP_1620, stopdata, by="persid")
hb_jte<- subset(jte_stop, stopno==1)
hb_jte = hb_jte[,!(names(hb_jte) %in% c("persid","stopno","origpurp1"))]

# recoding linkmode var (might be changed)

#hb_jte$mainmode[hb_jte$jtemode=="Vehicle Driver"] = "Vehicle Driver"
#hb_jte$mainmode[hb_jte$jtemode=="Motorcycle"] = "Vehicle Driver"
#hb_jte$mainmode[hb_jte$jtemode=="Vehicle Passenger"] = "Vehicle Passenger"
#hb_jte$mainmode[hb_jte$jtemode=="Taxi"] = "Vehicle Passenger"
#hb_jte$mainmode[hb_jte$jtemode=="Walking"] = "Walking"
#hb_jte$mainmode[hb_jte$jtemode=="Jogging"] = "Walking"
#hb_jte$mainmode[hb_jte$jtemode=="Bicycle"] = "Bicycle"
#hb_jte$mainmode[hb_jte$jtemode=="Public Bus"] = "Bus"
#hb_jte$mainmode[hb_jte$jtemode=="School Bus"] = "Bus"
#hb_jte$mainmode[hb_jte$jtemode=="Train"] = "Train"
#hb_jte$mainmode[hb_jte$jtemode=="Tram"] = "Tram"
#hb_jte$mainmode[hb_jte$jtemode=="Other"] = "Other"

hb_jte <- hb_jte %>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Motorcycle', 'Vehicle Driver', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Taxi', 'Vehicle Passenger', .))) %>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Jogging', 'Walking', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Public Bus', 'PT', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Mobility Scooter', 'Bicycle', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'School Bus', 'PT', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Train', 'PT', .)))%>%
  mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'),funs(ifelse(. == 'Tram', 'PT', .)))

hb_jte <- hb_jte[!(hb_jte$jtemode=="Other"),]

# concatenating modes
hb_jte <- hb_jte %>%
  unite("combinedmode", c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9'), sep ='_', na.rm = TRUE, remove = FALSE)
hb_jte$combinedmode2 <- sapply(hb_jte$combinedmode, function(x) paste(unique(unlist(str_split(x,"_"))), collapse = "_"))
hb_jte$combinedmode2 <- gsub('_Other', '', hb_jte$combinedmode2)
hb_jte$combinedmode2 <- gsub('Other_', '', hb_jte$combinedmode2)

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
            'Vehicle Passenger_PT_walk_Bike'='PT_Car','PT_walk_Bike_Vehicle Driver'='PT_Car','PT_walk_Bike_Vehicle Passenger'='PT_Car','Vehicle Passenger_PT_Car'='PT_Car', 'Vehicle Driver_PT'='PT_Car',
            'Vehicle Passenger_PT'='PT_Car')

hb_jte$combinedmode3 <- str_replace_all(hb_jte$combinedmode2, rep_str)

# recoding carlicence var
hb_jte$licence[hb_jte$carlicence=="Full Licence"] = 1
hb_jte$licence[hb_jte$carlicence=="Green Probationary Licence"] = 1
hb_jte$licence[hb_jte$carlicence=="Learners Permit"] = 1
hb_jte$licence[hb_jte$carlicence=="Red Probationary Licence"] = 1
hb_jte$licence[hb_jte$carlicence=="No Car Licence"] = 0

# recoding No of cars in HH
hb_jte$carsno[hb_jte$cars == 0] = 0
hb_jte$carsno[hb_jte$cars == 1] = 1
hb_jte$carsno[hb_jte$cars== 2] = 2
hb_jte$carsno[hb_jte$cars >=3] = 3

#recoding sex var
hb_jte$gender[hb_jte$sex=="M"] = 1
hb_jte$gender[hb_jte$sex=="F"] = 0

#recoding household income var
hb_jte$hhinc_an = hb_jte$hhinc*52
hb_jte$hhincat[hb_jte$hhinc_an<=59999] = 1
hb_jte$hhincat[60000<=hb_jte$hhinc_an & hb_jte$hhinc_an<=94999] = 2
hb_jte$hhincat[95000<=hb_jte$hhinc_an & hb_jte$hhinc_an<=139999] = 3
hb_jte$hhincat[140000<=hb_jte$hhinc_an & hb_jte$hhinc_an<=189999] = 4
hb_jte$hhincat[hb_jte$hhinc_an>=190000] = 5

# exporting work and education trips to csv format
hb_jte$geometry <- gsub(',', '-', hb_jte$geometry)
write.csv(hb_jte,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/hb_jte.csv")
