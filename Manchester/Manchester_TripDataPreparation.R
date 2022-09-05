### Clear memory
rm(list = ls())

suppressPackageStartupMessages(library(sf)) # for spatial things
suppressPackageStartupMessages(library(lwgeom)) # for advanced spatial things
suppressPackageStartupMessages(library(dplyr)) # for manipulating data
suppressPackageStartupMessages(library(tidyr)) # for pivoting data
suppressPackageStartupMessages(library(fitdistrplus)) # for log normal distributions
suppressPackageStartupMessages(library(ggplot2)) # for plotting data
suppressPackageStartupMessages(library(purrr)) # for nested dataframes
suppressPackageStartupMessages(library(stringr))# for editing columns 
suppressPackageStartupMessages(library(tidyverse))# for manipulating data
suppressPackageStartupMessages(library(expss))# for manipulating data

#Writing and reading csv
trads <- readRDS("C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/TRADS_safe_routed.rds")
write.csv(trads[["trips"]],file = "C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/trip_manchester.csv",row.names=F)
write.csv(trads[["households"]],file = "C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/hh_manchester.csv",row.names=F)
write.csv(trads[["indiv"]],file = "C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/pers_manchester.csv",row.names=F)
trips <- read.csv("C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/trip_manchester.csv",header=T)
households <- read.csv("C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/hh_manchester.csv",header=T)
indiv <- read.csv("C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/pers_manchester.csv",header=T)

#creating unique id for individuals
indiv <- indiv %>%
  unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(indiv.id, .after = hh.id)
trips <- trips %>%
  unite("indiv.id", c('hh.id', 'p.id'), sep ='', na.rm = TRUE, remove = FALSE)%>%
  relocate(indiv.id, .after = hh.id)

#merging trip data with household and person data
trips_hh <- merge(trips,households, by="hh.id")
trips_hh_p <- merge(trips_hh,indiv, by="indiv.id")

#recoding sex var
trips_hh_p$sex[trips_hh_p$p.female=="TRUE"] = 1
trips_hh_p$sex[trips_hh_p$p.female=="FALSE"] = 0
trips_hh_p <- trips_hh_p[!(is.na(trips_hh_p$sex)),]

#trips_hh_p$sex <- factor(trips_hh_p$sex, levels = c(1,0),labels = c("female", "male"))

#recoding age var
trips_hh_p$agegroup[trips_hh_p$p.age_group=="5-9"|trips_hh_p$p.age_group=="10-14"] = 1
trips_hh_p$agegroup[trips_hh_p$p.age_group=="15-19"|trips_hh_p$p.age_group=="20-24"] = 2
trips_hh_p$agegroup[trips_hh_p$p.age_group=="25-29"|trips_hh_p$p.age_group=="30-34"] = 3
trips_hh_p$agegroup[trips_hh_p$p.age_group=="35-39"|trips_hh_p$p.age_group=="40-44"] = 4
trips_hh_p$agegroup[trips_hh_p$p.age_group=="45-49"|trips_hh_p$p.age_group=="50-54"] = 5
trips_hh_p$agegroup[trips_hh_p$p.age_group=="55-59"|trips_hh_p$p.age_group=="60-64"] = 6
trips_hh_p$agegroup[trips_hh_p$p.age_group=="65-69"|trips_hh_p$p.age_group=="70-74"|trips_hh_p$p.age_group=="75-79"|trips_hh_p$p.age_group=="80-44"|
trips_hh_p$p.age_group=="85+"] = 7
trips_hh_p <- trips_hh_p[!(trips_hh_p$agegroup==""),]

#trips_hh_p$agegroup <- factor(trips_hh_p$agegroup,
#levels = c(1,2,3,4,5,6,7),labels = c("5-14", "15-24", "25-34","35-44","45-54","55-64","65+"))

#recoding work type
trips_hh_p$worktype[trips_hh_p$p.ws_workOver30h=="TRUE"] = 1 #working full time
trips_hh_p$worktype[trips_hh_p$p.ws_work16to30h=="TRUE"|trips_hh_p$p.ws_workUnder16h=="TRUE"|trips_hh_p$p.ws_volunteer=="TRUE"] = 2 #working part time/casual/volunteer
trips_hh_p$worktype[trips_hh_p$p.ws_retired=="TRUE"] = 3
trips_hh_p$worktype[trips_hh_p$p.ws_studyFullTime=="TRUE"|trips_hh_p$p.ws_studyPartTime=="TRUE"] = 4 #studying full/part time
trips_hh_p$worktype[trips_hh_p$p.ws_homeMaker=="TRUE"|trips_hh_p$p.ws_unemployed=="TRUE"|trips_hh_p$p.ws_other=="TRUE"|trips_hh_p$p.ws_longTermDisabled=="TRUE"] = 5 #not in work force/other

#trips_hh_p$worktype <- factor(trips_hh_p$worktype,
#levels = c(1,2,3,4,5),labels = c("full time", "part time/casual/volunteer", "retired","studying full/part time","not in work force/other"))

# recoding No of cars in HH
trips_hh_p$carsno[trips_hh_p$hh.cars == 0] = 0
trips_hh_p$carsno[trips_hh_p$hh.cars == 1] = 1
trips_hh_p$carsno[trips_hh_p$hh.cars== 2] = 2
trips_hh_p$carsno[trips_hh_p$hh.cars >=3] = 3 #more than 3 cars

# recoding No of bikes in HH
trips_hh_p$bikesno[trips_hh_p$hh.bikes == 0] = 0
trips_hh_p$bikesno[trips_hh_p$hh.bikes == 1] = 1
trips_hh_p$bikesno[trips_hh_p$hh.bikes== 2] = 2
trips_hh_p$bikesno[trips_hh_p$hh.bikes ==3] = 3
trips_hh_p$bikesno[trips_hh_p$hh.bikes >=4] = 4 #more than 4 bikes

#recoding household income var
trips_hh_p$hhincome[trips_hh_p$hh.income=="less than £5000"|trips_hh_p$hh.income=="£5000 to £9999"|trips_hh_p$hh.income=="£10000 to £14999"] = 1 #hh income less than £14999 
trips_hh_p$hhincome[trips_hh_p$hh.income=="£15000 to £19999"|trips_hh_p$hh.income=="£20000 to £24999"] = 2  
trips_hh_p$hhincome[trips_hh_p$hh.income=="£25000 to £34999"] = 3 
trips_hh_p$hhincome[trips_hh_p$hh.income=="£35000 to £49999"] = 4  
trips_hh_p$hhincome[trips_hh_p$hh.income=="£50000 to £74999"|trips_hh_p$hh.income=="£75000"] = 5 #hh income more than £50000 
trips_hh_p$hhincome[trips_hh_p$hh.income=="unknown"] = 6 # missing/refused to respond

#trips_hh_p$hhincome <- factor(trips_hh_p$hhincome,
#levels = c(1,2,3,4,5,6),labels = c("less than £14999", "£15000 to £24999", "£25000 to £34999","£35000 to £49999",
#        "more than £50000","missing/refused to respond"))

#recoding household structure var
trips_hh_p$hhstructure[trips_hh_p$hh.structure2=="2 adults, 1 child"|trips_hh_p$hh.structure2=="2 adults, 2 children"|trips_hh_p$hh.structure2=="2 adults, 3+ children"|
trips_hh_p$hh.structure2=="3+ adults, 1+ children"|trips_hh_p$hh.structure2=="Single parent family"] = 1 # hh with children
trips_hh_p$hhstructure[trips_hh_p$hh.structure2=="Single Adult 16 to 64"|trips_hh_p$hh.structure2=="Single Adult 65+"|trips_hh_p$hh.structure2=="Three of more Adults" |
trips_hh_p$hh.structure2=="Two Adults Hoh or HRP 16 to 64"|trips_hh_p$hh.structure2=="Two Adults Hoh or HRP 65+"] = 0 # hh without children

#trips_hh_p$hhstructure <- factor(trips_hh_p$hhstructure, levels = c(1,0),labels = c("households with children", "households without children"))

#generating mainmode 
trips_hh_p$mainmode[trips_hh_p$t.m_carDriver=="TRUE"|trips_hh_p$t.m_motorcycle=="TRUE"] = 1 
trips_hh_p$mainmode[trips_hh_p$t.m_carPassenger=="TRUE"|trips_hh_p$t.m_taxi=="TRUE"] = 2
trips_hh_p$mainmode[trips_hh_p$t.m_walk=="TRUE"] = 3
trips_hh_p$mainmode[trips_hh_p$t.m_cycle=="TRUE"] = 4
trips_hh_p$mainmode[trips_hh_p$t.m_train=="TRUE"|trips_hh_p$t.m_metrolink=="TRUE"|trips_hh_p$t.m_bus=="TRUE"] = 5 
trips_hh_p <- trips_hh_p[!(trips_hh_p$t.m_main=="Other"),]

#trips_hh_p$mainmode <- factor(trips_hh_p$mainmode, levels = c(1,2,3,4,5),labels = c("card", "carp", "walk","bike","ptwalk"))

#generating availability of modes
trips_hh_p$availcard <- 1
trips_hh_p$availcard[trips_hh_p$t.route.car_time == 0] = 0
trips_hh_p$availcard[trips_hh_p$mainmode == 1] = 1
trips_hh_p$availcarp <- 1
trips_hh_p$availcarp[trips_hh_p$t.route.car_time == 0] = 0
trips_hh_p$availcarp[trips_hh_p$mainmode == 2] = 1
trips_hh_p$availwalk <- 1
trips_hh_p$availwalk[trips_hh_p$troutewalk_short_time == 0] = 0
trips_hh_p$availwalk[trips_hh_p$mainmode == 3] = 1
trips_hh_p$availbike <- 1
trips_hh_p$availbike[trips_hh_p$troutebike_short_time == 0] = 0
trips_hh_p$availbike[trips_hh_p$mainmode == 4] = 1
trips_hh_p$availpt <- 1
trips_hh_p$availpt[trips_hh_p$troutept_totaltraveltime == 0] = 0
trips_hh_p$availpt[trips_hh_p$mainmode == 5] = 1

#replacing NAs in time and cost variables with 0
trips_hh_p$t.route.car_time[is.na(trips_hh_p$t.route.car_time)] = 0
trips_hh_p$t.route.car_cost[is.na(trips_hh_p$t.route.car_cost)] = 0
trips_hh_p$t.route.walk_short_time[is.na(trips_hh_p$t.route.walk_short_time)] = 0
trips_hh_p$t.route.walk_short_cost[is.na(trips_hh_p$t.route.walk_short_cost)] = 0
trips_hh_p$t.route.bike_short_time[is.na(trips_hh_p$t.route.bike_short_cost)] = 0
trips_hh_p$t.route.bike_short_cost[is.na(trips_hh_p$t.route.bike_short_cost)] = 0
trips_hh_p$t.route.pt_totalTravelTime[is.na(trips_hh_p$pt_totalTravelTime)] = 0
trips_hh_p <- trips_hh_p[!is.na(trips_hh_p$t.route.bike_short_POI),]

#trips_hh_p$t.route.pt_accessDistance[is.na(trips_hh_p$t.route.pt_accessDistance)] = 0
#trips_hh_p$t.route.t.route.pt_egressDistance[is.na(trips_hh_p$t.route.pt_egressDistance)] = 0
#generating pt cost (travel cost was not provided: total distance to pt stop and distance to destination used as the cost of PT)
#trips_hh_p$troutept_accessdistance <- as.numeric(trips_hh_p$t.route.pt_accessDistance)
#trips_hh_p$troutept_accessdistance[is.na(trips_hh_p$troutept_accessdistance)] = 0
#trips_hh_p$troutept_egressdistance <- as.numeric(trips_hh_p$t.route.pt_egressDistance)
#trips_hh_p$troutept_egressdistance[is.na(trips_hh_p$t.route.pt_egressDistance)] = 0
#trips_hh_p$t.route.pt_totalTravelCost <- trips_hh_p$troutept_accessdistance + trips_hh_p$troutept_egressdistance

#extracting work and education trips
tripsWE<- subset(trips_hh_p, t.startPurpose=="Home"& t.endPurpose=="Usual place of work"|t.endPurpose=="Unpaid, voluntary work"|
t.endPurpose=="Education as pupil, student" | t.endPurpose== "Work - Business, other")
#write_labelled_csv(tripsWE,file = "C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/mandatory_trips.csv",row.names=FALSE, single_file = TRUE)
write.csv(tripsWE,file = "C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Manchester/mandatory_trips.csv",row.names=FALSE)

