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
suppressPackageStartupMessages(library(readr)) 
suppressPackageStartupMessages(library(stringr)) 

tripdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/T_VISTA_MAIN.csv",header=T, na.strings="N/A")
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

#trips within greater Melbourne
write.csv(orig_within_region,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/origmelb_trips.csv",row.names= F)
write.csv(dest_within_region,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/destmelb_trips.csv",row.names=F)
orig <- read_csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/origmelb_trips.csv", col_names = T,show_col_types = F)
dest <- read_csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/destmelb_trips.csv", col_names = T,show_col_types = F)
melbtrips <- semi_join(orig,dest, by="tripid")
############################################################################################################################################
# joining trip data with household&person data-----------------------------------------------------------------------------------------------------
hhdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/H_VISTA_1220_Coord.csv")
pdata <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/P_VISTA_1220_Coord.csv")
tripshh <- merge(melbtrips,hhdata, by="hhid")
tripshhp <- merge(tripshh,pdata, by="persid")

# filtering data based on surveyperiod
trips <- subset(tripshhp, surveyperiod=="2016-17" | surveyperiod=="2017-18" | surveyperiod=="2018-19" | surveyperiod=="2019-20")

#recoding linkmode var (old tree structure)
#trips$mainmode[mandatory_trips$linkmode=="Vehicle Driver"] = "card"
#trips$mainmode[trips$linkmode=="Motorcycle"] = "card"
#trips$mainmode[trips$linkmode=="Vehicle Passenger"] = "carp"
#trips$mainmode[trips$linkmode=="Taxi"] = "carp"
#trips$mainmode[trips$linkmode=="Walking"] = "walk"
#trips$mainmode[trips$linkmode=="Jogging"] = "walk"
#trips$mainmode[trips$linkmode=="Bicycle"] = "bike"
#trips$mainmode[trips$linkmode=="Public Bus"] = "bus"
#trips$mainmode[trips$linkmode=="School Bus"] = "bus"
#trips$mainmode[trips$linkmode=="Train"] = "train"
#trips$mainmode[trips$linkmode=="Tram"] = "tram"
#trips$mainmode[trips$linkmode=="Other"] = "Other"
#trips <- WE_trips[!(WE_trips$mainmode=="Other"),]

#recoding modes
trips <- trips %>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Motorcycle', 'Vehicle Driver', .)))%>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Taxi', 'Vehicle Passenger', .))) %>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Jogging', 'Walking', .)))%>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Public Bus', 'PT', .)))%>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Mobility Scooter', 'Bicycle', .)))%>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'School Bus', 'PT', .)))%>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Train', 'PT', .)))%>%
   mutate_at(c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'),funs(ifelse(. == 'Tram', 'PT', .)))
 trips <- trips[!(trips$linkmode=="Other"),]
 
# concatenating modes
 trips <- trips %>%
   unite("combinedmode", c('mode1', 'mode2', 'mode3','mode4','mode5','mode6','mode7','mode8','mode9','mode10'), sep ='_', na.rm = TRUE, remove = FALSE)
 trips$combinedmode2 <- sapply(trips$combinedmode, function(x) paste(unique(unlist(str_split(x,"_"))), collapse = "_"))
 trips$combinedmode2 <- gsub('_Other', '', trips$combinedmode2)
 trips$combinedmode2 <- gsub('Other_', '', trips$combinedmode2)
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
             'Vehicle Passenger_PT'='PT_Car','Bicycle_Vehicle Driver'='Vehicle Driver','PT_Vehicle Driver'='PT_Car','PT_Car_Vehicle Passenger'='PT_Car','Vehicle Passenger_Bicycle'='Bicycle',
             'PT_Car_Bicycle'='PT_Car','PT_Vehicle Passenger'='PT_Car','PT_walk_Bike_Bicycle'='PT_walk_Bike')

trips$mainmode <- str_replace_all(trips$combinedmode2, rep_str)
trips$mainmode[trips$mainmode=="PT"] = "PT_walk_Bike"

#generaing age groups 
trips$agegroup[trips$age<=14] = 1
trips$agegroup[15<=trips$age & trips$age<=24] = 2
trips$agegroup[25<=trips$age & trips$age<=34] = 3
trips$agegroup[35<=trips$age & trips$age<=44] = 3
trips$agegroup[45<=trips$age & trips$age<=54] = 4
trips$agegroup[55<=trips$age & trips$age<=64] = 5
trips$agegroup[trips$age>=65] = 6

# recoding sex var
trips$female[trips$sex=="F"] = 1
trips$female[trips$sex=="M"] = 0

# recoding carlicence var
trips$licence[trips$carlicence=="Full Licence"] = 1
trips$licence[trips$carlicence=="Green Probationary Licence"] = 1
trips$licence[trips$carlicence=="Learners Permit"] = 1
trips$licence[trips$carlicence=="Red Probationary Licence"] = 1
trips$licence[trips$carlicence=="No Car Licence"] = 0

# recoding No of cars in HH
trips$carsno[trips$cars == 0] = 0
trips$carsno[trips$cars == 1] = 1
trips$carsno[trips$cars== 2] = 2
trips$carsno[trips$cars >=3] = 3

# recoding No of bikes in HH
trips$bikes[trips$adultbikes == 0] = 0
trips$bikes[trips$adultbikes == 1] = 1
trips$bikes[trips$adultbikes== 2] = 2
trips$bikes[trips$adultbikes >=3] = 3

# recoding No of bikes in HH
trips$hh.size[trips$adultbikes == 0] = 0
trips$hh.size[trips$adultbikes == 1] = 1
trips$hh.size[trips$adultbikes== 2] = 2
trips$hh.size[trips$adultbikes >=3] = 3

#recoding sex var
trips$gender[trips$sex=="M"] = 1
trips$gender[trips$sex=="F"] = 0

#recoding household income var
trips$hhinc_an = trips$hhinc*52
trips$hhincat[trips$hhinc_an<=59999] = 1
trips$hhincat[60000<=trips$hhinc_an & trips$hhinc_an<=94999] = 2
trips$hhincat[95000<=trips$hhinc_an & trips$hhinc_an<=139999] = 3
trips$hhincat[140000<=trips$hhinc_an & trips$hhinc_an<=189999] = 4
trips$hhincat[trips$hhinc_an>=190000] = 5
trips$hhincat[is.na(trips$hhinc_an)] = 6 # missing/refused to respond

#recoding work type
trips$work.type[trips$worktype=="Fixed Hours"] = 1 #working full time
trips$work.type[trips$worktype=="Flexible Hours"|trips$worktype=="Work from Home"] = 2 
trips$work.type[trips$worktype=="Rostered Shifts"] = 3
trips$work.type[trips$worktype=="Not in Work Force"] = 4

#recoding mainmode
trips$mainmode2[trips$mainmode == "Vehicle Driver"] = 1
trips$mainmode2[trips$mainmode == "Vehicle Passenger"] = 2
trips$mainmode2[trips$mainmode == "Walking"] = 3
trips$mainmode2[trips$mainmode == "Bicycle"] = 4
trips$mainmode2[trips$mainmode == "PT_Car"] = 5
trips$mainmode2[trips$mainmode == "PT_walk_Bike"] = 6

#joining with route attributes
route_attributes <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/route_attributes.csv")
trips <- merge(trips, route_attributes, by="tripid")
#joining car and pt travel times 
ptcar_time <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/carandpt_time.csv")
trips <- merge(trips, ptcar_time, by="tripid")

#generating availability of modes (should be edited)
trips$availcard <- 1
trips$availcard[trips$time_car == 0] = 0
#trips$availcard[trips$carsno == 0] = 0
#trips$availcard[trips$mainmode2 == 1] = 1
trips$availcarp <- 1
trips$availcarp[trips$time_car == 0] = 0
#trips$availcarp[trips$carsno == 0] = 0
#trips$availcarp[trips$mainmode2 == 2] = 1
trips$availwalk <- 1
trips$availwalk[trips$troutewalk_short_tt_s == 0] = 0
#trips$availwalk[trips$troutewalk_fast_tt_s == 0] = 0 #to be activated before running models with fast route attributes
#trips$availwalk[trips$troutewalk_jibe_tt_s == 0] = 0 #to be activated before running models with jibe route attributes
#trips$availwalk[trips$mainmode == 3] = 1
trips$availbike <- 1
trips$availbike[trips$troutebike_short_tt_s == 0] = 0
#trips$availbike[trips$troutebike_fast_tt_s == 0] = 0 #to be activated before running models with fast route attributes
#trips$availbike[trips$troutebike_jibe_tt_s == 0] = 0 #to be activated before running models with jibe route attributes
#trips$availbike[trips$bikes == 0] = 0
#trips$availbike[trips$mainmode2 == 4] = 1
trips$availptcar <- 1
trips$availptcar[trips$time_ptcar == 0] = 0
#trips$availptcar[trips$mainmode2 == 5] = 1
trips$availptwalk <- 1
trips$availptwalk[trips$time_ptwalk == 0] = 0
#trips$availptwalk[trips$mainmode2 == 6] = 1
trips <- trips[!(trips$time_car==0 & trips$mainmode2==1),]
trips <- trips[!(trips$time_car==0 & trips$mainmode2==2),]
trips <- trips[!(trips$time_ptcar==0 & trips$mainmode2==5),]
trips <- trips[!(trips$time_ptwalk==0 & trips$mainmode2==6),]

#modifying stress junction var
trips$stressjct_walk_short <- trips$troutewalk_short_stressjct*trips$troutewalk_short_distance_m
trips$stressjct_bike_short <- trips$troutebike_short_stressjct*trips$troutebike_short_distance_m
trips$stressjct_walk_fast <- trips$troutewalk_fast_stressjct*trips$troutewalk_fast_distance_m
trips$stressjct_bike_fast <- trips$troutebike_fast_stressjct*trips$troutebike_fast_distance_m
trips$stressjct_walk_jibe <- trips$troutewalk_jibe_stressjct*trips$troutewalk_jibe_distance_m
trips$stressjct_bike_jibe <- trips$troutebike_jibe_stressjct*trips$troutebike_jibe_distance_m

# generating log transforamtion of distance
trips$logdist_walk_short = log(trips$troutewalk_short_distance_m)
trips$logdist_bike_short = log(trips$troutewalk_short_distance_m)
trips$logdist_walk_fast = log(trips$troutewalk_fast_distance_m)
trips$logdist_bike_fast = log(trips$troutewalk_fast_distance_m)
trips$logdist_walk_jibe = log(trips$troutewalk_jibe_distance_m)
trips$logdist_bike_jibe = log(trips$troutewalk_jibe_distance_m)

#generating a weighting variable based on walk and bike log distances
trips <- with(trips,trips[order(trips$logdist_walk_short,trips$logdist_bike_short),])
trips <- trips %>% rowwise() %>%
  mutate(weight_short = mean(c(logdist_walk_short, logdist_bike_short)))
trips <- with(trips,trips[order(trips$logdist_walk_fast,trips$logdist_bike_fast),])
trips <- trips %>% rowwise() %>%
  mutate(weight_fast = mean(c(logdist_walk_fast, logdist_bike_fast)))
trips <- with(trips,trips[order(trips$logdist_walk_jibe,trips$logdist_bike_jibe),])
trips <- trips %>% rowwise() %>%
  mutate(weight_jibe = mean(c(logdist_walk_jibe, logdist_bike_jibe)))

# mandatory tours
trips <- trips[order(trips$persid),]
mandatory_tours <- trips %>%
  group_by(persid)
mandatory_tours <- subset(mandatory_tours, origpurp1 == "At Home" | origpurp1 == "Work Related" | origpurp1 == "Education")
mandatory_tours <- subset(mandatory_tours, destpurp1 == "At or Go Home" | destpurp1 == "Work Related" | destpurp1 == "Education")
mandatory_tours <- semi_join(mandatory_tours,route_attributes, by="tripid")
write.csv(mandatory_tours,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_tours.csv")

#joining with gnaf point for area-based measures
#orig long&lat of mandatory trips were joined to gnaf points using near analysis in qgis 
#gnaf <- read.csv("C:/Users/e18933/OneDrive - RMIT University/WORK/Lucy/Data/ganf_points_selectedBEmeasures.csv",header=T, na.strings="N/A")
#mandatory_gnaf <- read_csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/Mandatory_tours_origgnaf.csv")
#mandatory_gnaf <- rename(mandatory_gnaf, gnaf_pid = HubName)
#mandatory_gnaf <- rename(mandatory_gnaf, origdist_gnaf = HubDist)
#trips_alljoined <-merge(mandatory_gnaf, gnaf, by="gnaf_pid")

# filtering trips started from home
hb_trips <- mandatory_tours %>% 
  group_by(persid)
hb_trips <- hb_trips[hb_trips$tripno == 1 & hb_trips$origpurp1 == "At Home", ] 

# filtering mandatory(work&education) trips 
mandatory_trips <- subset(hb_trips, trippurp=="Education"|trippurp=="Work Related")
#mandatory_trips <- mandatory_trips %>%
#  relocate(gnaf_pid, .before = origdist_gnaf)

# exporting work and education trips to csv format
write.csv(mandatory_trips,file = "C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_trips.csv")


