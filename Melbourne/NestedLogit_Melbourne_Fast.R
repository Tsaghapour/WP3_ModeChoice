# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
### Clear memory
rm(list = ls())
#rm(list= ls()[!(ls() %in% c('modelmnl','modelfst','modelfst2'))])

### Load Apollo library
library(apollo)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
library(readr)
library(dplyr)
# mandatory_trips <- read.csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_trips.csv",stringsAsFactors = TRUE)
mandatory_trips <- read_csv("C:/Users/e18933/OneDrive - RMIT University/DOT_VISTA/Processed Data/mandatory_trips.csv",col_types = cols(...1 = col_skip()))
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
  modelName  = "modelfst",
  modelDescr = "Nested logit with socio-demographics and fast route-based measures on mode choice",
  indivID    = "tripid",
  panelData  = FALSE,
  weights    = "weight_fast",
  nCores     = 6
)

outputDir <- "JIBE/DATA Analysis/NewVISTA/JIBE_WP3_ModeChoice/JIBE_WP3_ModeChoice"

#### DEFINE MODEL PARAMETERS
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta =c(cons_carp = 0, cons_walk = 0, cons_bike = 0, cons_ptcar = 0, cons_ptwalk = 0, 
               f_age1_carp = 0, f_age1_walk = 0, f_age1_bike = 0, f_age1_ptwalk = 0, f_age1_ptcar = 0,
               f_age2_carp = 0, f_age2_walk = 0, f_age2_bike = 0, f_age2_ptwalk = 0, f_age2_ptcar = 0,
               f_age4_carp = 0, f_age4_walk = 0, f_age4_bike = 0, f_age4_ptwalk = 0, f_age4_ptcar = 0,
               f_age5_carp = 0, f_age5_walk = 0, f_age5_bike = 0, f_age5_ptwalk = 0, f_age5_ptcar = 0,
               f_age6_carp = 0, f_age6_walk = 0, f_age6_bike = 0, f_age6_ptwalk = 0, f_age6_ptcar = 0,
               f_female_carp = 0, f_female_walk = 0, f_female_bike = 0, f_female_ptwalk = 0, f_female_ptcar = 0,
               # f_hhsize_carp = 0, f_hhsize_walk = 0, f_hhsize_bike = 0, f_hhsize_ptwalk = 0, f_hhsize_ptcar = 0,
               # f_hhstructure_carp = 0, f_hhstructure_walk = 0, f_hhstructure_bike = 0, f_hhstructure_ptwalk = 0, f_hhstructure_ptcar = 0
               f_inc1_carp = 0, f_inc1_walk = 0, f_inc1_bike = 0, f_inc1_ptwalk = 0, f_inc1_ptcar = 0,
               f_inc2_carp = 0, f_inc2_walk = 0, f_inc2_bike = 0, f_inc2_ptwalk = 0, f_inc2_ptcar = 0,
               f_inc3_carp = 0, f_inc3_walk = 0, f_inc3_bike = 0, f_inc3_ptwalk = 0, f_inc3_ptcar = 0,
               f_inc4_carp = 0, f_inc4_walk = 0, f_inc4_bike = 0, f_inc4_ptwalk = 0, f_inc4_ptcar = 0,
               # f_inc6_carp = 0, f_inc6_walk = 0, f_inc6_bike = 0, f_inc6_ptwalk = 0, f_inc6_ptcar = 0,
               f_cars_carp = 0, f_cars_walk = 0, f_cars_bike = 0, f_cars_ptwalk = 0, f_cars_ptcar = 0,
               f_bikes_carp = 0, f_bikes_walk = 0, f_bikes_bike = 0, f_bikes_ptwalk = 0, f_bikes_ptcar = 0,
               f_work2_carp = 0, f_work2_walk = 0, f_work2_bike = 0, f_work2_ptwalk = 0, f_work2_ptcar = 0,
               f_work3_carp = 0, f_work3_walk = 0, f_work3_bike = 0, f_work3_ptwalk = 0, f_work3_ptcar = 0,
               f_work4_carp = 0, f_work4_walk = 0, f_work4_bike = 0, f_work4_ptwalk = 0, f_work4_ptcar = 0,
               f_vgvi_walk = 0, f_vgvi_bike = 0,
               # f_light_walk = 0, f_light_bike = 0,
               f_shannon_walk = 0, f_shannon_bike = 0,
               # f_crime_walk = 0, f_crime_bike = 0,
               # f_attract_walk = 0, f_attract_bike = 0,
               f_streslnk_walk = 0, f_streslnk_bike = 0,
               f_stresjct_walk = 0, f_stresjct_bike = 0,
               # f_sumstress_walk = 0, f_sumstress_bike = 0,
               f_poi_walk = 0, f_poi_bike = 0,
               f_negpoi_walk = 0, f_negpoi_bike = 0,
               # f_sumpois_walk = 0, f_sumpois_bike = 0,
               # f_freightpoi_walk = 0, f_freightpoi_bike = 0, 
               #f_costcarp = 0, f_costwalk = 0, f_costbike = 0, f_costpt = 0,
               f_timecarp = 0, f_timewalk = 0, f_timebike = 0, f_timeptwalk = 0, f_timeptcar = 0,
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
  age1_carp = f_age1_carp * (agegroup == 1); age1_walk = f_age1_walk * (agegroup == 1);age1_bike = f_age1_bike * (agegroup == 1);age1_ptwalk = f_age1_ptwalk * (agegroup == 1);age1_ptcar = f_age1_ptcar * (agegroup == 1)
  age2_carp = f_age2_carp * (agegroup == 2); age2_walk = f_age2_walk * (agegroup == 2);age2_bike = f_age2_bike * (agegroup == 2);age2_ptwalk = f_age2_ptwalk * (agegroup == 2);age2_ptcar = f_age2_ptcar * (agegroup == 2)
  age4_carp = f_age4_carp * (agegroup == 4); age4_walk = f_age4_walk * (agegroup == 4);age4_bike = f_age4_bike * (agegroup == 4);age4_ptwalk = f_age4_ptwalk * (agegroup == 4);age4_ptcar = f_age4_ptcar * (agegroup == 4)
  age5_carp = f_age5_carp * (agegroup == 5); age5_walk = f_age5_walk * (agegroup == 5);age5_bike = f_age5_bike * (agegroup == 5);age5_ptwalk = f_age5_ptwalk * (agegroup == 5);age5_ptcar = f_age5_ptcar * (agegroup == 5)
  age6_carp = f_age6_carp * (agegroup == 6); age6_walk = f_age6_walk * (agegroup == 6);age6_bike = f_age6_bike * (agegroup == 6);age6_ptwalk = f_age6_ptwalk * (agegroup == 6);age6_ptcar = f_age6_ptcar * (agegroup == 6)
  
  # ###sex
  female_carp = f_female_carp * female; female_walk = f_female_walk * female; female_bike = f_female_bike * female; female_ptwalk = f_female_ptwalk * female;female_ptcar = f_female_ptcar * female
  # 
  # ###hhstructure
  # #hhstructure_carp = f_hhstructure_carp * (hhstructure ==1); hhstructure_walk = f_hhstructure_walk * (hhstructure ==1); hhstructure_bike = f_hhstructure_bike * (hhstructure==1); hhstructure_ptwalk = f_hhstructure_ptwalk * (hhstructure==1);hhstructure_ptcar = f_hhstructure_ptcar * (hhstructure==1)
  # 
  # ###hhsize
  # hhsize_carp = f_hhsize_carp * hh.size; hhsize_walk = f_hhsize_walk * hh.size; hhsize_bike = f_hhsize_bike * hh.size; hhsize_ptwalk = f_hhsize_ptwalk * hh.size; hhsize_ptcar = f_hhsize_ptcar * hh.size
  # 
  # ###hhincome
  inc1_carp = f_inc1_carp * (hhincat == 1); inc1_walk = f_inc1_walk * (hhincat == 1); inc1_bike = f_inc1_bike * (hhincat == 1); inc1_ptwalk = f_inc1_ptwalk * (hhincat == 1); inc1_ptcar = f_inc1_ptcar * (hhincat == 1)
  inc2_carp = f_inc2_carp * (hhincat == 2); inc2_walk = f_inc2_walk * (hhincat == 2); inc2_bike = f_inc2_bike * (hhincat == 2); inc2_ptwalk = f_inc2_ptwalk * (hhincat == 2); inc2_ptcar = f_inc2_ptcar * (hhincat == 2)
  inc3_carp = f_inc3_carp * (hhincat == 3); inc3_walk = f_inc3_walk * (hhincat == 3); inc3_bike = f_inc3_bike * (hhincat == 3); inc3_ptwalk = f_inc3_ptwalk * (hhincat == 3); inc3_ptcar = f_inc3_ptcar * (hhincat == 3)
  inc4_carp = f_inc4_carp * (hhincat == 4); inc4_walk = f_inc4_walk * (hhincat == 4); inc4_bike = f_inc4_bike * (hhincat == 4); inc4_ptwalk = f_inc4_ptwalk * (hhincat == 4); inc4_ptcar = f_inc4_ptcar * (hhincat == 4)
  # inc6_carp = f_inc6_carp * (hhincat == 6); inc6_walk = f_inc6_walk * (hhincat == 6); inc6_bike = f_inc6_bike * (hhincat == 6); inc6_ptwalk = f_inc6_ptwalk * (hhincat == 6); inc6_ptcar = f_inc6_ptcar * (hhincat == 6)
  
  ###carno
  carsno_carp = f_cars_carp * carsno ; carsno_walk = f_cars_walk * carsno; carsno_bike = f_cars_bike * carsno; carsno_ptwalk = f_cars_ptwalk * carsno; carsno_ptcar = f_cars_ptcar * carsno
  
  ###bikeno
  bikesno_carp = f_bikes_carp * bikes; bikesno_walk = f_bikes_walk * bikes; bikesno_bike = f_bikes_bike * bikes; bikesno_ptwalk = f_bikes_ptwalk * bikes; bikesno_ptcar = f_bikes_ptcar * bikes
  
  ###worktype
  worktype2_carp = f_work2_carp * (work.type == 2); worktype2_walk = f_work2_walk * (work.type == 2); worktype2_bike = f_work2_bike * (work.type == 2); worktype2_ptwalk = f_work2_ptwalk * (work.type == 2); worktype2_ptcar = f_work2_ptcar * (work.type == 2)
  worktype3_carp = f_work3_carp * (work.type == 3); worktype3_walk = f_work3_walk * (work.type == 3); worktype3_bike = f_work3_bike * (work.type == 3); worktype3_ptwalk = f_work3_ptwalk * (work.type == 3); worktype3_ptcar = f_work3_ptcar * (work.type == 3)
  worktype4_carp = f_work4_carp * (work.type == 4); worktype4_walk = f_work4_walk * (work.type == 4); worktype4_bike = f_work4_bike * (work.type == 4); worktype4_ptwalk = f_work4_ptwalk * (work.type == 4); worktype4_ptcar = f_work4_ptcar * (work.type == 4)
  
  ####Linked-based measures
  ###################################################################################################
  ######original attributes 
  vgvi_walk = f_vgvi_walk * troutewalk_fast_vgvi; vgvi_bike = f_vgvi_bike * troutebike_fast_vgvi
  # light_walk = f_light_walk * troutewalk_fast_lighting; light_bike = f_light_bike * troutebike_fast_lighting
  shannon_walk = f_shannon_walk *troutewalk_fast_shannon; shannon_bike = f_shannon_bike *troutebike_fast_shannon
  # crime_walk = f_crime_walk *t.route.walk_fast_crime; crime_bike = f_crime_bike *t.route.bike_fast_crime 
  # attract_walk = f_attract_walk *troutewalk_fast_attractiveness; attract_bike = f_attract_bike *troutebike_fast_attractiveness
  streslnk_walk = f_streslnk_walk *troutewalk_fast_stresslink; streslnk_bike = f_streslnk_bike *troutebike_fast_stresslink
  stresjct_walk = f_stresjct_walk *stressjct_walk_fast; stresjct_bike = f_stresjct_bike *stressjct_bike_fast
  #sumstress_walk = f_sumstress_walk * sumstress_walk_fast; sumstress_bike = f_sumstress_bike * sumstress_bike_fast
  poi_walk = f_poi_walk *troutewalk_fast_pois; poi_bike = f_poi_bike *troutebike_fast_pois;
  negpoi_walk = f_negpoi_walk *troutewalk_fast_negpois; negpoi_bike = f_negpoi_bike *troutebike_fast_negpois
  #freightpoi_walk = f_freightpoi_walk *troutewalk_fast_freightPOIpois; freightpoi_bike = f_freightpoi_bike *troutebike_fast_freightPOIs
  #sumpois_walk = f_sumpois_walk * sumpois_walk_fast; sumpois_bike = f_sumpois_bike * sumpois_walk_fast
  
  ######log transformation
  #vgvi_walk = f_vgvi_walk *log(1+troutewalk_fast_vgvi); vgvi_bike = f_vgvi_bike *log(1+troutebike_fast_vgvi)
  #light_walk = f_light_walk * log(1+troute.walk_fast_lighting); light_bike = f_light_bike * log(1+troutebike_fast_lighting) 
  #shannon_walk = f_shannon_walk * log(1+troutewalk_fast_shannon); shannon_bike = f_shannon_bike * log(1+troutebike_fast_shannon)  
  #crime_walk = f_crime_walk * log(1+troutewalk_fast_crime); crime_bike = f_crime_bike * log(1+troutebike_fast_crime) 
  #attract_walk = f_attract_walk * log(1+troutewalk_fast_attractiveness); attract_bike = f_attract_bike * (1+troutebike_fast_attractiveness) 
  #streslnk_walk = f_streslnk_walk * log(1+troutewalk_fast_stressLink); streslnk_bike = f_streslnk_bike * log(1+troutebike_fast_stressLink) 
  #stresjct_walk = f_stresjct_walk * log(1+troutewalk_fast_stressJct); stresjct_bike = f_stresjct_bike *log(1+troute.bike_fast_stressJct)
  #poi_walk = f_poi_walk *log(1+troutewalk_fast_POIs); poi_bike = f_poi_bike *log(1+troutebike_fast_POIs)
  #negpoi_walk = f_negpoi_walk * log(1+troutewalk_fast_negPOIs); negpoi_bike = f_negpoi_bike * log(1+troutebike_fast_negPOIs)
  #freightpoi_walk = f_freightpoi_walk * log(1+troutewalk_fast_freightPOIs); freightpoi_bike = f_freightpoi_bike * log(1+troutebike_fast_freightPOIs)
  #sumstress_walk = f_sumstress_walk * log(1+sumstress_walk_fast); sumstress_bike = f_sumstress_bike * log(1+sumstress_bike_fast)
  #sumpois_walk = f_sumpois_walk * log(1+sumpois_walk_fast); sumpois_bike = f_sumpois_bike * log(1+sumpois_bike_fast)
  
  ###traveltime
  carptime = f_timecarp * time_car; walktime = f_timewalk * troutewalk_fast_tt_s; biketime = f_timebike * troutebike_fast_tt_s; ptcartime = f_timeptcar * time_ptcar; ptwalktime = f_timeptwalk * time_ptwalk;
  
  ###travelcost
  #carpcost = f_costcarp * t.route.car_cost; walkcost = f_costwalk * t.route.walk_fast_cost; bikecost = f_costbike * t.route.bike_fast_cost; ptwalkcost = f_costpt * t.route.pt_walkDistance
  
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

modelfst = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
#             ,estimate_settings=list(constraints=c(
#            "lambda_car < 1 + 1e-10", "lambda_car > -1e-10")))
#            "lambda_pt < 1 + 1e-10", "lambda_pt > -1e-10")))

# ################################################################# #
#### MODEL PEREDICTION                                           ####
# ################################################################# #
forecast = apollo_prediction(modelfst,
                             apollo_probabilities,
                             apollo_inputs)

write.csv(forecast,file = "/Users/tayebeh/Library/CloudStorage/OneDrive-RMITUniversity/WORK/JIBE/DATA Analysis/R/NewVISTA/probabilities_modelfstm4.csv")

#### Likelihood ratio tests against MNL model
# ################################################################# #
lrTest = apollo_lrTest(modelfst,modelmnl)                                                                                                                                                                                                                                                                                                                           

#### MODEL OUTPUTS                                               ####
# ################################################################# #
apollo_modelOutput(modelfst,modelOutput_settings = list(printT1=1))
# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILES)                               ----
# ----------------------------------------------------------------- #
apollo_saveOutput(modelfst)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(modelfst)

# Print estimates
modelfst_estimates <- modelfst$estimate
modelfst_tTest <- modelfst$estimate / modelfst$se
modelfst_pValue <- 2 * (1 - stats::pnorm(abs(modelfst$estimate/modelfst$se)))
modelfst_sig <- case_when(modelfst_pValue <= 0.001 ~ "***",
                          modelfst_pValue <= 0.01 ~ "**",
                          modelfst_pValue <= 0.05 ~ "*",
                          modelfst_pValue <= 0.1 ~ ".",
                          TRUE ~ "")
estimateValues <- paste(round(modelfst_estimates,3),"[",round(modelfst_tTest,3),modelfst_sig,"]")
estimateValues[modelfst_estimates == 0] <- NA
names(estimateValues) = names(apollo_beta)
estimate_lambdas <- estimateValues[startsWith(names(apollo_beta),"lambda")]
write.csv(estimateValues,file = "/Users/tayebeh/Library/CloudStorage/OneDrive-RMITUniversity/WORK/JIBE/DATA Analysis/R/NewVISTA/estimatevalues_modelfst_m4.csv")
