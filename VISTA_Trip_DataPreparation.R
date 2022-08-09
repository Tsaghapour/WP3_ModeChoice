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

tripdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/T_VISTA_MAIN.csv")
df <- tripdata %>%
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

Trip_HHJoined <- merge(orig_within_region,HHdata, by="hhid")
Trip_HHP_Joined <- merge(Trip_HHJoined,Pdata, by="persid")

# filtering data based on surveyperiod
trip_HHP_1620 <- subset(Trip_HHP_Joined, surveyperiod=="2016-17" | surveyperiod=="2017-18" | surveyperiod=="2018-19" | surveyperiod=="2019-20")

# filtering work&education trips using trippurp
WorkEducation <- subset(trip_HHP_1620, trippurp=="Education"|trippurp=="Work Related")

# recoding linkmode var (might be changed)
WorkEducation$mainmode[WorkEducation$linkmode=="Vehicle Driver"] = "Vehicle Driver"
WorkEducation$mainmode[WorkEducation$linkmode=="Motorcycle"] = "Vehicle Driver"
WorkEducation$mainmode[WorkEducation$linkmode=="Vehicle Passenger"] = "Vehicle Passenger"
WorkEducation$mainmode[WorkEducation$linkmode=="Taxi"] = "Vehicle Passenger"
WorkEducation$mainmode[WorkEducation$linkmode=="Walking"] = "Walking"
WorkEducation$mainmode[WorkEducation$linkmode=="Jogging"] = "Walking"
WorkEducation$mainmode[WorkEducation$linkmode=="Bicycle"] = "Bicycle"
WorkEducation$mainmode[WorkEducation$linkmode=="Public Bus"] = "Bus"
WorkEducation$mainmode[WorkEducation$linkmode=="School Bus"] = "Bus"
WorkEducation$mainmode[WorkEducation$linkmode=="Train"] = "Train"
WorkEducation$mainmode[WorkEducation$linkmode=="Tram"] = "Tram"
WorkEducation$mainmode[WorkEducation$linkmode=="Other"] = "Other"

WorkEducation <- WorkEducation[!(WorkEducation$mainmode=="Other"),]

# recoding carlicence var
WorkEducation$licence[WorkEducation$carlicence=="Full Licence"] = 1
WorkEducation$licence[WorkEducation$carlicence=="Green Probationary Licence"] = 1
WorkEducation$licence[WorkEducation$carlicence=="Learners Permit"] = 1
WorkEducation$licence[WorkEducation$carlicence=="Red Probationary Licence"] = 1
WorkEducation$licence[WorkEducation$carlicence=="No Car Licence"] = 0

# recoding No of cars in HH
WorkEducation$carsno[WorkEducation$cars == 0] = 0
WorkEducation$carsno[WorkEducation$cars == 1] = 1
WorkEducation$carsno[WorkEducation$cars== 2] = 2
WorkEducation$carsno[WorkEducation$cars >=3] = 3

#recoding sex var
WorkEducation$gender[WorkEducation$sex=="M"] = 1
WorkEducation$gender[WorkEducation$sex=="F"] = 0

#recoding household income var
WorkEducation$hhinc_an = WorkEducation$hhinc*52
WorkEducation$hhincat[WorkEducation$hhinc_an<=59999] = 1
WorkEducation$hhincat[60000<=WorkEducation$hhinc_an & WorkEducation$hhinc_an<=94999] = 2
WorkEducation$hhincat[95000<=WorkEducation$hhinc_an & WorkEducation$hhinc_an<=139999] = 3
WorkEducation$hhincat[140000<=WorkEducation$hhinc_an & WorkEducation$hhinc_an<=189999] = 4
WorkEducation$hhincat[WorkEducation$hhinc_an>=190000] = 5


# exporting work and education trips to csv format
write.csv(WorkEducation,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/WorkEducation.csv")

# home-based work and education trips
HB_WorkEducation <- subset(WorkEducation, origpurp1=="At Home")

# exporting Home-based work and education trips to csv format
HB_WorkEducation$geometry <- gsub(',', '-', HB_WorkEducation$geometry)
write.csv(HB_WorkEducation,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/HB_WorkEducation.csv")

