# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
### Clear memory
rm(list = ls())
#rm(list= ls()[!(ls() %in% c('modelmnl','modeljib','modeljib2'))])

### Load Apollo library
library(apollo)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
library(readr)
library(dplyr)
mandatory_trips <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_trips.csv/mandatory_trips.csv",stringsAsFactors = TRUE)
#mandatory_trips <- read_csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_trips.csv",col_types = cols(...1 = col_skip()))
database <- mandatory_trips

#generating unique tripid
# database <- mutate(database, tripID = row_number())

#relocating columns 
# database <- database %>%
#   relocate(tripID, .before = persid)

### Initialize code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "modeljib",
  modelDescr = "Nested logit with socio-demographics and jibe route-based measures on mode choice",
  indivID    = "tripid",
  panelData  = FALSE,
  weights    = "weight_jibe",
  nCores     = 6
)

outputDir <- "JIBE/DATA Analysis/NewVISTA/JIBE_WP3_ModeChoice/JIBE_WP3_ModeChoice"

#### DEFINE MODEL PARAMETERS
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta =c(cons_carp = 0, cons_walk = 0, cons_bike = 0, cons_ptcar = 0, cons_ptwalk = 0, 
               j_age1_carp = 0, j_age1_walk = 0, j_age1_bike = 0, j_age1_ptwalk = 0, j_age1_ptcar = 0,
               j_age2_carp = 0, j_age2_walk = 0, j_age2_bike = 0, j_age2_ptwalk = 0, j_age2_ptcar = 0,
               j_age4_carp = 0, j_age4_walk = 0, j_age4_bike = 0, j_age4_ptwalk = 0, j_age4_ptcar = 0,
               j_age5_carp = 0, j_age5_walk = 0, j_age5_bike = 0, j_age5_ptwalk = 0, j_age5_ptcar = 0,
               j_age6_carp = 0, j_age6_walk = 0, j_age6_bike = 0, j_age6_ptwalk = 0, j_age6_ptcar = 0,
               j_female_carp = 0, j_female_walk = 0, j_female_bike = 0, j_female_ptwalk = 0, j_female_ptcar = 0,
               # j_hhsize_carp = 0, j_hhsize_walk = 0, j_hhsize_bike = 0, j_hhsize_ptwalk = 0, j_hhsize_ptcar = 0,
               # j_hhstructure_carp = 0, j_hhstructure_walk = 0, j_hhstructure_bike = 0, j_hhstructure_ptwalk = 0, j_hhstructure_ptcar = 0
               j_inc1_carp = 0, j_inc1_walk = 0, j_inc1_bike = 0, j_inc1_ptwalk = 0, j_inc1_ptcar = 0,
               j_inc2_carp = 0, j_inc2_walk = 0, j_inc2_bike = 0, j_inc2_ptwalk = 0, j_inc2_ptcar = 0,
               j_inc3_carp = 0, j_inc3_walk = 0, j_inc3_bike = 0, j_inc3_ptwalk = 0, j_inc3_ptcar = 0,
               j_inc4_carp = 0, j_inc4_walk = 0, j_inc4_bike = 0, j_inc4_ptwalk = 0, j_inc4_ptcar = 0,
               # j_inc6_carp = 0, j_inc6_walk = 0, j_inc6_bike = 0, j_inc6_ptwalk = 0, j_inc6_ptcar = 0,
               j_cars_carp = 0, j_cars_walk = 0, j_cars_bike = 0, j_cars_ptwalk = 0, j_cars_ptcar = 0,
               j_bikes_carp = 0, j_bikes_walk = 0, j_bikes_bike = 0, j_bikes_ptwalk = 0, j_bikes_ptcar = 0,
               j_work2_carp = 0, j_work2_walk = 0, j_work2_bike = 0, j_work2_ptwalk = 0, j_work2_ptcar = 0,
               j_work3_carp = 0, j_work3_walk = 0, j_work3_bike = 0, j_work3_ptwalk = 0, j_work3_ptcar = 0,
               j_work4_carp = 0, j_work4_walk = 0, j_work4_bike = 0, j_work4_ptwalk = 0, j_work4_ptcar = 0,
               j_vgvi_walk = 0, j_vgvi_bike = 0,
               # j_light_walk = 0, j_light_bike = 0,
               j_shannon_walk = 0, j_shannon_bike = 0,
               # j_crime_walk = 0, j_crime_bike = 0,
               # j_attract_walk = 0, j_attract_bike = 0,
               j_streslnk_walk = 0, j_streslnk_bike = 0,
               j_stresjct_walk = 0, j_stresjct_bike = 0,
               # j_sumstress_walk = 0, j_sumstress_bike = 0,
               j_poi_walk = 0, j_poi_bike = 0,
               j_negpoi_walk = 0, j_negpoi_bike = 0,
               # j_sumpois_walk = 0, j_sumpois_bike = 0,
               # j_freightpoi_walk = 0, j_freightpoi_bike = 0, 
               #j_costcarp = 0, j_costwalk = 0, j_costbike = 0, j_costpt = 0,
               j_timecarp = 0, j_timewalk = 0, j_timebike = 0, j_timeptwalk = 0, j_timeptcar = 0,
               lambda_car = 1,
               lambda_pt = 1)

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

### Read in starting values for cons only model output file
#apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"C:/Users/e18933/OneDrive - RMIT University/WORK/JIBE/DATA Analysis/R/Apollo/MNL_Model",overwriteFixed=FALSE)

#### GROUP AND VALIDATE INPUTS
apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Create alternative specific constants and coefficients 
  ###age
  age1_carp = j_age1_carp * (agegroup == 1); age1_walk = j_age1_walk * (agegroup == 1);age1_bike = j_age1_bike * (agegroup == 1);age1_ptwalk = j_age1_ptwalk * (agegroup == 1);age1_ptcar = j_age1_ptcar * (agegroup == 1)
  age2_carp = j_age2_carp * (agegroup == 2); age2_walk = j_age2_walk * (agegroup == 2);age2_bike = j_age2_bike * (agegroup == 2);age2_ptwalk = j_age2_ptwalk * (agegroup == 2);age2_ptcar = j_age2_ptcar * (agegroup == 2)
  age4_carp = j_age4_carp * (agegroup == 4); age4_walk = j_age4_walk * (agegroup == 4);age4_bike = j_age4_bike * (agegroup == 4);age4_ptwalk = j_age4_ptwalk * (agegroup == 4);age4_ptcar = j_age4_ptcar * (agegroup == 4)
  age5_carp = j_age5_carp * (agegroup == 5); age5_walk = j_age5_walk * (agegroup == 5);age5_bike = j_age5_bike * (agegroup == 5);age5_ptwalk = j_age5_ptwalk * (agegroup == 5);age5_ptcar = j_age5_ptcar * (agegroup == 5)
  age6_carp = j_age6_carp * (agegroup == 6); age6_walk = j_age6_walk * (agegroup == 6);age6_bike = j_age6_bike * (agegroup == 6);age6_ptwalk = j_age6_ptwalk * (agegroup == 6);age6_ptcar = j_age6_ptcar * (agegroup == 6)
  
  # ###sex
  female_carp = j_female_carp * female; female_walk = j_female_walk * female; female_bike = j_female_bike * female; female_ptwalk = j_female_ptwalk * female;female_ptcar = j_female_ptcar * female
  # 
  # ###hhstructure
  # #hhstructure_carp = j_hhstructure_carp * (hhstructure ==1); hhstructure_walk = j_hhstructure_walk * (hhstructure ==1); hhstructure_bike = j_hhstructure_bike * (hhstructure==1); hhstructure_ptwalk = j_hhstructure_ptwalk * (hhstructure==1);hhstructure_ptcar = j_hhstructure_ptcar * (hhstructure==1)
  # 
  # ###hhsize
  # hhsize_carp = j_hhsize_carp * hh.size; hhsize_walk = j_hhsize_walk * hh.size; hhsize_bike = j_hhsize_bike * hh.size; hhsize_ptwalk = j_hhsize_ptwalk * hh.size; hhsize_ptcar = j_hhsize_ptcar * hh.size
  # 
  # ###hhincome
  inc1_carp = j_inc1_carp * (hhincat == 1); inc1_walk = j_inc1_walk * (hhincat == 1); inc1_bike = j_inc1_bike * (hhincat == 1); inc1_ptwalk = j_inc1_ptwalk * (hhincat == 1); inc1_ptcar = j_inc1_ptcar * (hhincat == 1)
  inc2_carp = j_inc2_carp * (hhincat == 2); inc2_walk = j_inc2_walk * (hhincat == 2); inc2_bike = j_inc2_bike * (hhincat == 2); inc2_ptwalk = j_inc2_ptwalk * (hhincat == 2); inc2_ptcar = j_inc2_ptcar * (hhincat == 2)
  inc3_carp = j_inc3_carp * (hhincat == 3); inc3_walk = j_inc3_walk * (hhincat == 3); inc3_bike = j_inc3_bike * (hhincat == 3); inc3_ptwalk = j_inc3_ptwalk * (hhincat == 3); inc3_ptcar = j_inc3_ptcar * (hhincat == 3)
  inc4_carp = j_inc4_carp * (hhincat == 4); inc4_walk = j_inc4_walk * (hhincat == 4); inc4_bike = j_inc4_bike * (hhincat == 4); inc4_ptwalk = j_inc4_ptwalk * (hhincat == 4); inc4_ptcar = j_inc4_ptcar * (hhincat == 4)
  # inc6_carp = j_inc6_carp * (hhincat == 6); inc6_walk = j_inc6_walk * (hhincat == 6); inc6_bike = j_inc6_bike * (hhincat == 6); inc6_ptwalk = j_inc6_ptwalk * (hhincat == 6); inc6_ptcar = j_inc6_ptcar * (hhincat == 6)
  
  ###carno
  carsno_carp = j_cars_carp * carsno ; carsno_walk = j_cars_walk * carsno; carsno_bike = j_cars_bike * carsno; carsno_ptwalk = j_cars_ptwalk * carsno; carsno_ptcar = j_cars_ptcar * carsno
  
  ###bikeno
  bikesno_carp = j_bikes_carp * bikes; bikesno_walk = j_bikes_walk * bikes; bikesno_bike = j_bikes_bike * bikes; bikesno_ptwalk = j_bikes_ptwalk * bikes; bikesno_ptcar = j_bikes_ptcar * bikes
  
  ###worktype
  worktype2_carp = j_work2_carp * (work.type == 2); worktype2_walk = j_work2_walk * (work.type == 2); worktype2_bike = j_work2_bike * (work.type == 2); worktype2_ptwalk = j_work2_ptwalk * (work.type == 2); worktype2_ptcar = j_work2_ptcar * (work.type == 2)
  worktype3_carp = j_work3_carp * (work.type == 3); worktype3_walk = j_work3_walk * (work.type == 3); worktype3_bike = j_work3_bike * (work.type == 3); worktype3_ptwalk = j_work3_ptwalk * (work.type == 3); worktype3_ptcar = j_work3_ptcar * (work.type == 3)
  worktype4_carp = j_work4_carp * (work.type == 4); worktype4_walk = j_work4_walk * (work.type == 4); worktype4_bike = j_work4_bike * (work.type == 4); worktype4_ptwalk = j_work4_ptwalk * (work.type == 4); worktype4_ptcar = j_work4_ptcar * (work.type == 4)
  
  ####Linked-based measures
  ###################################################################################################
  ######original attributes 
  vgvi_walk = j_vgvi_walk * troutewalk_jibe_vgvi; vgvi_bike = j_vgvi_bike * troutebike_jibe_vgvi
  # light_walk = j_light_walk * troutewalk_jibe_lighting; light_bike = j_light_bike * troutebike_jibe_lighting
  shannon_walk = j_shannon_walk *troutewalk_jibe_shannon; shannon_bike = j_shannon_bike *troutebike_jibe_shannon
  # crime_walk = j_crime_walk *t.route.walk_jibe_crime; crime_bike = j_crime_bike *t.route.bike_jibe_crime 
  # attract_walk = j_attract_walk *troutewalk_jibe_attractiveness; attract_bike = j_attract_bike *troutebike_jibe_attractiveness
  streslnk_walk = j_streslnk_walk *troutewalk_jibe_stresslink; streslnk_bike = j_streslnk_bike *troutebike_jibe_stresslink
  stresjct_walk = j_stresjct_walk *stressjct_walk_jibe; stresjct_bike = j_stresjct_bike *stressjct_bike_jibe
  # sumstress_walk = j_sumstress_walk * sumstress_walk_jibe; sumstress_bike = j_sumstress_bike * sumstress_bike_jibe
  poi_walk = j_poi_walk *troutewalk_jibe_pois; poi_bike = j_poi_bike *troutebike_jibe_pois;
  negpoi_walk = j_negpoi_walk *troutewalk_jibe_negpois; negpoi_bike = j_negpoi_bike *troutebike_jibe_negpois
  # freightpoi_walk = j_freightpoi_walk *troutewalk_jibe_freightPOIs; freightpoi_bike = j_freightpoi_bike *troutebike_jibe_freightPOIs
  # sumpois_walk = j_sumpois_walk * sumpois_walk_jibe; sumpois_bike = j_sumpois_bike * sumpois_walk_jibe
  
  ######log transformation
  # vgvi_walk = j_vgvi_walk *log(1+troutewalk_jibe_vgvi); vgvi_bike = j_vgvi_bike *log(1+troutebike_jibe_vgvi)
  # light_walk = j_light_walk * log(1+troute.walk_jibe_lighting); light_bike = j_light_bike * log(1+troutebike_jibe_lighting) 
  # shannon_walk = j_shannon_walk * log(1+troutewalk_jibe_shannon); shannon_bike = j_shannon_bike * log(1+troutebike_jibe_shannon)
  # crime_walk = j_crime_walk * log(1+troutewalk_jibe_crime); crime_bike = j_crime_bike * log(1+troutebike_jibe_crime) 
  # attract_walk = j_attract_walk * log(1+troutewalk_jibe_attractiveness); attract_bike = j_attract_bike * (1+troutebike_jibe_attractiveness) 
  # streslnk_walk = j_streslnk_walk * log(1+troutewalk_jibe_stresslink); streslnk_bike = j_streslnk_bike * log(1+troutebike_jibe_stresslink)
  # stresjct_walk = j_stresjct_walk * log(1+troutewalk_jibe_stressjct); stresjct_bike = j_stresjct_bike *log(1+troute.bike_jibe_stressjct)
  # poi_walk = j_poi_walk *log(1+troutewalk_jibe_pois); poi_bike = j_poi_bike *log(1+troutebike_jibe_pois)
  # negpoi_walk = j_negpoi_walk * log(1+troutewalk_jibe_negpois); negpoi_bike = j_negpoi_bike * log(1+troutebike_jibe_negpois)
  #freightpoi_walk = j_freightpoi_walk * log(1+troutewalk_jibe_freightPOIs); freightpoi_bike = j_freightpoi_bike * log(1+troutebike_jibe_freightPOIs)
  #sumstress_walk = j_sumstress_walk * log(1+sumstress_walk_jibe); sumstress_bike = j_sumstress_bike * log(1+sumstress_bike_jibe)
  #sumpois_walk = j_sumpois_walk * log(1+sumpois_walk_jibe); sumpois_bike = j_sumpois_bike * log(1+sumpois_bike_jibe)
  
  ###traveltime
  carptime = j_timecarp * time_car; walktime = j_timewalk * troutewalk_jibe_tt_s; biketime = j_timebike * troutebike_jibe_tt_s; ptcartime = j_timeptcar * time_ptcar; ptwalktime = j_timeptwalk * time_ptwalk;
  
  ###travelcost
  #carpcost = j_costcarp * t.route.car_cost; walkcost = j_costwalk * t.route.walk_jibe_cost; bikecost = j_costbike * t.route.bike_jibe_cost; ptwalkcost = j_costpt * t.route.pt_walkDistance
  
  ### List of utilities: M1 (these must use the same names as in nl_settings, order is irrelevant)
  V = list()
  V[['card']]  = 0
  V[['carp']]  = cons_carp + age1_carp + age2_carp + age4_carp + age5_carp + age6_carp + female_carp + inc1_carp + inc2_carp + inc3_carp + inc4_carp +
    worktype2_carp + worktype3_carp + worktype4_carp + carsno_carp + bikesno_carp + worktype2_carp + worktype4_carp + carptime
  V[['walk']]  = cons_walk + age1_walk + age2_walk + age4_walk + age5_walk + age6_walk + female_walk +inc1_walk + inc2_walk + inc3_walk + inc4_walk + worktype2_walk + worktype3_walk + worktype4_walk +
    carsno_walk + bikesno_walk + worktype2_walk + worktype4_walk + vgvi_walk + shannon_walk + poi_walk + negpoi_walk+ streslnk_walk + stresjct_walk + walktime
  V[['bike']] = cons_bike + age1_bike + age2_bike + age4_bike + age5_bike + age6_bike + female_bike + inc1_bike + inc2_bike + inc3_bike + inc4_bike + worktype2_bike + worktype3_bike + worktype4_bike +
    carsno_bike + bikesno_bike + worktype2_bike + worktype4_bike + vgvi_bike + shannon_bike + poi_bike + negpoi_bike + streslnk_bike + stresjct_bike + biketime
  V[['ptcar']] = cons_ptcar + age1_ptcar + age2_ptcar + age4_ptcar + age5_ptcar + age6_ptcar + female_ptwalk + inc1_ptcar + inc2_ptcar + inc3_ptcar + inc4_ptcar +
    worktype2_ptcar + worktype3_ptcar + worktype4_ptcar + carsno_ptcar + bikesno_ptcar + worktype2_ptcar + worktype4_ptcar + ptcartime
  V[['ptwalk']] = cons_ptwalk + age1_ptwalk + age2_ptwalk + age4_ptwalk + age5_ptwalk + age6_ptwalk + female_ptcar + inc1_ptwalk + inc2_ptwalk + inc3_ptwalk + inc4_ptwalk +
    worktype2_ptwalk + worktype3_ptwalk + worktype4_ptwalk + carsno_ptwalk + bikesno_ptwalk + worktype2_ptwalk + worktype4_ptwalk + ptwalktime
  
  ### Specify nests for NL model
  nlNests      = list(root=1, car = lambda_car, pt = lambda_pt)
  #, active=lambda_active)
  
  ### Specify tree structure for NL model
  nlStructure = list()
  nlStructure[["root"]]   = c("car","walk","bike","pt")
  nlStructure[["car"]] = c("card","carp")
  nlStructure[["pt"]] = c("ptwalk","ptcar")
  
  ### Define settings for NL model component
  nl_settings = list(
    alternatives  = c(card= 1, carp= 2, walk = 3, bike = 4, ptcar = 5, ptwalk = 6),
    choiceVar     = mainmode2,
    avail         = list(card=availcard, carp=availcarp, walk=availwalk, bike=availbike, ptcar=availptcar, ptwalk=availptwalk),
    #avail         = list(card=1, carp=1, walk=1, bike=1, ptcar=1, ptwalk=1),
    V             = V,
    nlNests       = nlNests,
    nlStructure   = nlStructure
  )
  
  ### Compute probabilities using NL model
  P[['model']] = apollo_nl(nl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_weighting(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

modeljib = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
#             ,estimate_settings=list(constraints=c(
#            "lambda_car < 1 + 1e-10", "lambda_car > -1e-10")))
#            "lambda_pt < 1 + 1e-10", "lambda_pt > -1e-10")))

# ################################################################# #
#### MODEL PEREDICTION                                           ####
# ################################################################# #
forecast = apollo_prediction(modeljib,
                             apollo_probabilities,
                             apollo_inputs)

write.csv(forecast,file = "/Users/tayebeh/Library/CloudStorage/OneDrive-RMITUniversity/WORK/JIBE/DATA Analysis/R/NewVISTA/probabilities_modeljibM4.csv")

#### Likelihood ratio tests against MNL model
# ################################################################# #
lrTest = apollo_lrTest(modeljib,modelmnl)                                                                                                                                                                                                                                                                                                                           

#### MODEL OUTPUTS                                               ####
# ################################################################# #
apollo_modelOutput(modeljib,modelOutput_settings = list(printT1=1))
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILES)                               ----
# ----------------------------------------------------------------- #
apollo_saveOutput(modeljib)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(modeljib)

# Print estimates
modeljib_estimates <- modeljib$estimate
modeljib_tTest <- modeljib$estimate / modeljib$se
modeljib_pValue <- 2 * (1 - stats::pnorm(abs(modeljib$estimate/modeljib$se)))
modeljib_sig <- case_when(modeljib_pValue <= 0.001 ~ "***",
                          modeljib_pValue <= 0.01 ~ "**",
                          modeljib_pValue <= 0.05 ~ "*",
                          modeljib_pValue <= 0.1 ~ ".",
                          TRUE ~ "")
estimateValues <- paste(round(modeljib_estimates,3),"[",round(modeljib_tTest,3),modeljib_sig,"]")
estimateValues[modeljib_estimates == 0] <- NA
names(estimateValues) = names(apollo_beta)
estimate_lambdas <- estimateValues[startsWith(names(apollo_beta),"lambda")]
write.csv(estimateValues,file = "/Users/tayebeh/Library/CloudStorage/OneDrive-RMITUniversity/WORK/JIBE/DATA Analysis/R/NewVISTA/estimatevalues_modeljib_m4.csv")
