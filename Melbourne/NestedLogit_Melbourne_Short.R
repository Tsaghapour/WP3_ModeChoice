# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
### Clear memory
rm(list = ls())
#rm(list= ls()[!(ls() %in% c('modelmnl','modelsht','modelsht2'))])

### Load Apollo library
library(apollo)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
library(readr)
library(dplyr)
mandatory_trips <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_trips.csv",stringsAsFactors = TRUE)
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
  modelName  = "modelsht",
  modelDescr = "Nested logit with socio-demographics and short route-based measures on mode choice",
  indivID    = "tripid",
  panelData  = FALSE,
  weights    = "weight_short",
  nCores     = 6
)

outputDir <- "JIBE/DATA Analysis/NewVISTA/JIBE_WP3_ModeChoice/JIBE_WP3_ModeChoice"

#### DEFINE MODEL PARAMETERS
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta =c(cons_carp = 0, cons_walk = 0, cons_bike = 0, cons_ptcar = 0, cons_ptwalk = 0, 
               s_age1_carp = 0, s_age1_walk = 0, s_age1_bike = 0, s_age1_ptwalk = 0, s_age1_ptcar = 0,
               s_age2_carp = 0, s_age2_walk = 0, s_age2_bike = 0, s_age2_ptwalk = 0, s_age2_ptcar = 0,
               s_age4_carp = 0, s_age4_walk = 0, s_age4_bike = 0, s_age4_ptwalk = 0, s_age4_ptcar = 0,
               s_age5_carp = 0, s_age5_walk = 0, s_age5_bike = 0, s_age5_ptwalk = 0, s_age5_ptcar = 0,
               s_age6_carp = 0, s_age6_walk = 0, s_age6_bike = 0, s_age6_ptwalk = 0, s_age6_ptcar = 0,
               s_female_carp = 0, s_female_walk = 0, s_female_bike = 0, s_female_ptwalk = 0, s_female_ptcar = 0,
               # s_hhsize_carp = 0, s_hhsize_walk = 0, s_hhsize_bike = 0, s_hhsize_ptwalk = 0, s_hhsize_ptcar = 0,
               # s_hhstructure_carp = 0, s_hhstructure_walk = 0, s_hhstructure_bike = 0, s_hhstructure_ptwalk = 0, s_hhstructure_ptcar = 0
               s_inc1_carp = 0, s_inc1_walk = 0, s_inc1_bike = 0, s_inc1_ptwalk = 0, s_inc1_ptcar = 0,
               s_inc2_carp = 0, s_inc2_walk = 0, s_inc2_bike = 0, s_inc2_ptwalk = 0, s_inc2_ptcar = 0,
               s_inc3_carp = 0, s_inc3_walk = 0, s_inc3_bike = 0, s_inc3_ptwalk = 0, s_inc3_ptcar = 0,
               s_inc4_carp = 0, s_inc4_walk = 0, s_inc4_bike = 0, s_inc4_ptwalk = 0, s_inc4_ptcar = 0,
               # s_inc6_carp = 0, s_inc6_walk = 0, s_inc6_bike = 0, s_inc6_ptwalk = 0, s_inc6_ptcar = 0,
               s_cars_carp = 0, s_cars_walk = 0, s_cars_bike = 0, s_cars_ptwalk = 0, s_cars_ptcar = 0,
               s_bikes_carp = 0, s_bikes_walk = 0, s_bikes_bike = 0, s_bikes_ptwalk = 0, s_bikes_ptcar = 0,
               s_work2_carp = 0, s_work2_walk = 0, s_work2_bike = 0, s_work2_ptwalk = 0, s_work2_ptcar = 0,
               s_work3_carp = 0, s_work3_walk = 0, s_work3_bike = 0, s_work3_ptwalk = 0, s_work3_ptcar = 0,
               s_work4_carp = 0, s_work4_walk = 0, s_work4_bike = 0, s_work4_ptwalk = 0, s_work4_ptcar = 0,
               s_vgvi_walk = 0, s_vgvi_bike = 0,
               # s_light_walk = 0, s_light_bike = 0,
               s_shannon_walk = 0, s_shannon_bike = 0,
               # s_crime_walk = 0, s_crime_bike = 0,
               # s_attract_walk = 0, s_attract_bike = 0,
               s_streslnk_walk = 0, s_streslnk_bike = 0,
               s_stresjct_walk = 0, s_stresjct_bike = 0,
               # s_sumstress_walk = 0, s_sumstress_bike = 0,
               s_poi_walk = 0, s_poi_bike = 0,
               s_negpoi_walk = 0, s_negpoi_bike = 0,
               # s_sumpois_walk = 0, s_sumpois_bike = 0,
               # s_freightpoi_walk = 0, s_freightpoi_bike = 0, 
               #s_costcarp = 0, s_costwalk = 0, s_costbike = 0, s_costpt = 0,
               s_timecarp = 0, s_timewalk = 0, s_timebike = 0, s_timeptwalk = 0, s_timeptcar = 0,
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
  age1_carp = s_age1_carp * (agegroup == 1); age1_walk = s_age1_walk * (agegroup == 1);age1_bike = s_age1_bike * (agegroup == 1);age1_ptwalk = s_age1_ptwalk * (agegroup == 1);age1_ptcar = s_age1_ptcar * (agegroup == 1)
  age2_carp = s_age2_carp * (agegroup == 2); age2_walk = s_age2_walk * (agegroup == 2);age2_bike = s_age2_bike * (agegroup == 2);age2_ptwalk = s_age2_ptwalk * (agegroup == 2);age2_ptcar = s_age2_ptcar * (agegroup == 2)
  age4_carp = s_age4_carp * (agegroup == 4); age4_walk = s_age4_walk * (agegroup == 4);age4_bike = s_age4_bike * (agegroup == 4);age4_ptwalk = s_age4_ptwalk * (agegroup == 4);age4_ptcar = s_age4_ptcar * (agegroup == 4)
  age5_carp = s_age5_carp * (agegroup == 5); age5_walk = s_age5_walk * (agegroup == 5);age5_bike = s_age5_bike * (agegroup == 5);age5_ptwalk = s_age5_ptwalk * (agegroup == 5);age5_ptcar = s_age5_ptcar * (agegroup == 5)
  age6_carp = s_age6_carp * (agegroup == 6); age6_walk = s_age6_walk * (agegroup == 6);age6_bike = s_age6_bike * (agegroup == 6);age6_ptwalk = s_age6_ptwalk * (agegroup == 6);age6_ptcar = s_age6_ptcar * (agegroup == 6)

  # ###sex
  female_carp = s_female_carp * female; female_walk = s_female_walk * female; female_bike = s_female_bike * female; female_ptwalk = s_female_ptwalk * female;female_ptcar = s_female_ptcar * female
  # 
  # ###hhstructure
  # #hhstructure_carp = s_hhstructure_carp * (hhstructure ==1); hhstructure_walk = s_hhstructure_walk * (hhstructure ==1); hhstructure_bike = s_hhstructure_bike * (hhstructure==1); hhstructure_ptwalk = s_hhstructure_ptwalk * (hhstructure==1);hhstructure_ptcar = s_hhstructure_ptcar * (hhstructure==1)
  # 
  # ###hhsize
  # hhsize_carp = s_hhsize_carp * hh.size; hhsize_walk = s_hhsize_walk * hh.size; hhsize_bike = s_hhsize_bike * hh.size; hhsize_ptwalk = s_hhsize_ptwalk * hh.size; hhsize_ptcar = s_hhsize_ptcar * hh.size
  # 
  # ###hhincome
  inc1_carp = s_inc1_carp * (hhincat == 1); inc1_walk = s_inc1_walk * (hhincat == 1); inc1_bike = s_inc1_bike * (hhincat == 1); inc1_ptwalk = s_inc1_ptwalk * (hhincat == 1); inc1_ptcar = s_inc1_ptcar * (hhincat == 1)
  inc2_carp = s_inc2_carp * (hhincat == 2); inc2_walk = s_inc2_walk * (hhincat == 2); inc2_bike = s_inc2_bike * (hhincat == 2); inc2_ptwalk = s_inc2_ptwalk * (hhincat == 2); inc2_ptcar = s_inc2_ptcar * (hhincat == 2)
  inc3_carp = s_inc3_carp * (hhincat == 3); inc3_walk = s_inc3_walk * (hhincat == 3); inc3_bike = s_inc3_bike * (hhincat == 3); inc3_ptwalk = s_inc3_ptwalk * (hhincat == 3); inc3_ptcar = s_inc3_ptcar * (hhincat == 3)
  inc4_carp = s_inc4_carp * (hhincat == 4); inc4_walk = s_inc4_walk * (hhincat == 4); inc4_bike = s_inc4_bike * (hhincat == 4); inc4_ptwalk = s_inc4_ptwalk * (hhincat == 4); inc4_ptcar = s_inc4_ptcar * (hhincat == 4)
  # inc6_carp = s_inc6_carp * (hhincat == 6); inc6_walk = s_inc6_walk * (hhincat == 6); inc6_bike = s_inc6_bike * (hhincat == 6); inc6_ptwalk = s_inc6_ptwalk * (hhincat == 6); inc6_ptcar = s_inc6_ptcar * (hhincat == 6)

  ###carno
  carsno_carp = s_cars_carp * carsno ; carsno_walk = s_cars_walk * carsno; carsno_bike = s_cars_bike * carsno; carsno_ptwalk = s_cars_ptwalk * carsno; carsno_ptcar = s_cars_ptcar * carsno
  
  ###bikeno
  bikesno_carp = s_bikes_carp * bikes; bikesno_walk = s_bikes_walk * bikes; bikesno_bike = s_bikes_bike * bikes; bikesno_ptwalk = s_bikes_ptwalk * bikes; bikesno_ptcar = s_bikes_ptcar * bikes
  
  ###worktype
  worktype2_carp = s_work2_carp * (work.type == 2); worktype2_walk = s_work2_walk * (work.type == 2); worktype2_bike = s_work2_bike * (work.type == 2); worktype2_ptwalk = s_work2_ptwalk * (work.type == 2); worktype2_ptcar = s_work2_ptcar * (work.type == 2)
  worktype3_carp = s_work3_carp * (work.type == 3); worktype3_walk = s_work3_walk * (work.type == 3); worktype3_bike = s_work3_bike * (work.type == 3); worktype3_ptwalk = s_work3_ptwalk * (work.type == 3); worktype3_ptcar = s_work3_ptcar * (work.type == 3)
  worktype4_carp = s_work4_carp * (work.type == 4); worktype4_walk = s_work4_walk * (work.type == 4); worktype4_bike = s_work4_bike * (work.type == 4); worktype4_ptwalk = s_work4_ptwalk * (work.type == 4); worktype4_ptcar = s_work4_ptcar * (work.type == 4)

  ####Linked-based measures
  ###################################################################################################
  ######original attributes 
  vgvi_walk = s_vgvi_walk * troutewalk_short_vgvi; vgvi_bike = s_vgvi_bike * troutebike_short_vgvi
  # light_walk = s_light_walk * troutewalk_short_lighting; light_bike = s_light_bike * troutebike_short_lighting
  shannon_walk = s_shannon_walk *troutewalk_short_shannon; shannon_bike = s_shannon_bike *troutebike_short_shannon
  # crime_walk = s_crime_walk *t.route.walk_short_crime; crime_bike = s_crime_bike *t.route.bike_short_crime 
  # attract_walk = s_attract_walk *troutewalk_short_attractiveness; attract_bike = s_attract_bike *troutebike_short_attractiveness
  streslnk_walk = s_streslnk_walk *troutewalk_short_stresslink; streslnk_bike = s_streslnk_bike *troutebike_short_stresslink
  stresjct_walk = s_stresjct_walk *stressjct_walk_short; stresjct_bike = s_stresjct_bike *stressjct_bike_short
  #sumstress_walk = s_sumstress_walk * sumstress_walk_short; sumstress_bike = s_sumstress_bike * sumstress_bike_short
  poi_walk = s_poi_walk *troutewalk_short_pois; poi_bike = s_poi_bike *troutebike_short_pois;
  negpoi_walk = s_negpoi_walk *troutewalk_short_negpois; negpoi_bike = s_negpoi_bike *troutebike_short_negpois
  #freightpoi_walk = s_freightpoi_walk *troutewalk_short_freightPOIpois; freightpoi_bike = s_freightpoi_bike *troutebike_short_freightPOIs
  #sumpois_walk = s_sumpois_walk * sumpois_walk_short; sumpois_bike = s_sumpois_bike * sumpois_walk_short
 
  ######log transformation
  #vgvi_walk = s_vgvi_walk *log(1+troutewalk_short_vgvi); vgvi_bike = s_vgvi_bike *log(1+troutebike_short_vgvi)
  #light_walk = s_light_walk * log(1+troute.walk_short_lighting); light_bike = s_light_bike * log(1+troutebike_short_lighting) 
  #shannon_walk = s_shannon_walk * log(1+troutewalk_short_shannon); shannon_bike = s_shannon_bike * log(1+troutebike_short_shannon)  
  #crime_walk = s_crime_walk * log(1+troutewalk_short_crime); crime_bike = s_crime_bike * log(1+troutebike_short_crime) 
  #attract_walk = s_attract_walk * log(1+troutewalk_short_attractiveness); attract_bike = s_attract_bike * (1+troutebike_short_attractiveness) 
  #streslnk_walk = s_streslnk_walk * log(1+troutewalk_short_stressLink); streslnk_bike = s_streslnk_bike * log(1+troutebike_short_stressLink) 
  #stresjct_walk = s_stresjct_walk * log(1+troutewalk_short_stressJct); stresjct_bike = s_stresjct_bike *log(1+troute.bike_short_stressJct)
  #poi_walk = s_poi_walk *log(1+troutewalk_short_POIs); poi_bike = s_poi_bike *log(1+troutebike_short_POIs)
  #negpoi_walk = s_negpoi_walk * log(1+troutewalk_short_negPOIs); negpoi_bike = s_negpoi_bike * log(1+troutebike_short_negPOIs)
  #freightpoi_walk = s_freightpoi_walk * log(1+troutewalk_short_freightPOIs); freightpoi_bike = s_freightpoi_bike * log(1+troutebike_short_freightPOIs)
  #sumstress_walk = s_sumstress_walk * log(1+sumstress_walk_short); sumstress_bike = s_sumstress_bike * log(1+sumstress_bike_short)
  #sumpois_walk = s_sumpois_walk * log(1+sumpois_walk_short); sumpois_bike = s_sumpois_bike * log(1+sumpois_bike_short)
  
  ###traveltime
  carptime = s_timecarp * time_car; walktime = s_timewalk * troutewalk_short_tt_s; biketime = s_timebike * troutebike_short_tt_s; ptcartime = s_timeptcar * time_ptcar; ptwalktime = s_timeptwalk * time_ptwalk;
  
  ###travelcost
  #carpcost = s_costcarp * t.route.car_cost; walkcost = s_costwalk * t.route.walk_short_cost; bikecost = s_costbike * t.route.bike_short_cost; ptwalkcost = s_costpt * t.route.pt_walkDistance
  
  ### List of utilities: these must use the same names as in nl_settings, order is irrelevant
  V = list()
  V[['card']]  = 0
  V[['carp']]  = cons_carp + age1_carp + age2_carp + age4_carp + age5_carp + age6_carp + female_carp + inc1_carp + inc2_carp + inc3_carp + inc4_carp +
    worktype2_carp + worktype3_carp + worktype4_carp + carsno_carp + bikesno_carp + worktype2_carp + worktype4_carp + carptime
  V[['walk']]  = cons_walk + age1_walk + age2_walk + age4_walk + age5_walk + age6_walk + female_walk +inc1_walk + inc2_walk + inc3_walk + inc4_walk + worktype2_walk + worktype3_walk + worktype4_walk +
    carsno_walk + bikesno_walk + worktype2_walk + worktype4_walk + vgvi_walk + shannon_walk + poi_walk + negpoi_walk + streslnk_walk + stresjct_walk + walktime
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

modelsht = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
                           #             ,estimate_settings=list(constraints=c(
                           #            "lambda_car < 1 + 1e-10", "lambda_car > -1e-10")))
                           #            "lambda_pt < 1 + 1e-10", "lambda_pt > -1e-10")))

# ################################################################# #
#### MODEL PEREDICTION                                           ####
# ################################################################# #
forecast = apollo_prediction(modelsht,
                                     apollo_probabilities,
                                     apollo_inputs)
                                      
write.csv(forecast,file = "/Users/tayebeh/Library/CloudStorage/OneDrive-RMITUniversity/WORK/JIBE/DATA Analysis/R/NewVISTA/probabilities_modelshtm4.csv")

#### Likelihood ratio tests against MNL model
# ################################################################# #
lrTest = apollo_lrTest(modelsht,modelmnl)                                                                                                                                                                                                                                                                                                                           

#### MODEL OUTPUTS                                               ####
# ################################################################# #
apollo_modelOutput(modelsht,modelOutput_settings = list(printT1=1))
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILES)                               ----
# ----------------------------------------------------------------- #
apollo_saveOutput(modelsht)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(modelsht)

# Print estimates
modelsht_estimates <- modelsht$estimate
modelsht_tTest <- modelsht$estimate / modelsht$se
modelsht_pValue <- 2 * (1 - stats::pnorm(abs(modelsht$estimate/modelsht$se)))
modelsht_sig <- case_when(modelsht_pValue <= 0.001 ~ "***",
                          modelsht_pValue <= 0.01 ~ "**",
                          modelsht_pValue <= 0.05 ~ "*",
                          modelsht_pValue <= 0.1 ~ ".",
                          TRUE ~ "")
estimateValues <- paste(round(modelsht_estimates,3),"[",round(modelsht_tTest,3),modelsht_sig,"]")
estimateValues[modelsht_estimates == 0] <- NA
names(estimateValues) = names(apollo_beta)
estimate_lambdas <- estimateValues[startsWith(names(apollo_beta),"lambda")]
write.csv(estimateValues,file = "/Users/tayebeh/Library/CloudStorage/OneDrive-RMITUniversity/WORK/JIBE/DATA Analysis/R/NewVISTA/estimatevalues_modelsht_m4.csv")
