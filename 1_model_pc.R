##########################
#### Packages         ####
##########################
library(tidyverse)

##########################
#### Data             ####
##########################
# data on consumption which has been callibrated to market research data
load("inputs/base_consumption.Rda")

# data from the IAC study which provides prices by drinker and wealth group
load("inputs/price_changes.Rda")

# this is a regression model computed using the South African Demographic and Health Survey 2016 data
load("inputs/peak_model.Rda")

##########################
#### Data dictionary  ####
##########################
# here are some definitions for some of the key variables

# base price is price per standard drink

# lit_SADHS       total litres per year using SADHS data
# lit_SADHS_rec   total litres per year excluding homebrew
# lit_SADHS_hb    total litres per year of homebrew
# 
# lit             total litres per year after callibrating to euromonitor using a gamma shift
# lit_rec         total litres per year after callibrating (recorded only)
# lit_hb          total litres per year after callibrating (homebrew only)

# dk              drinks per year using uplifted consumption
# dk_rec          drinks per year using uplifted consumption recorded only
# dk_hb           drinks per year using uplifted consumption homebrew only

# these variables are used in combination with the drink peak regression model to
# predict peak drinking at each new policy level

# dk_peak_SADHS       peak drinks using just SADHS (all alcohol)
# dk_peak_SADHS_pred  peak drinks predicted using just SADHS data
# dk_peak_base_pred   predicted peak drinks using uplifted consumption and applying model
# dk_peak_base        peak drink base, dk_peak_SADHS*(dk_peak_base_pred/dk_peak_SADHS_pred)

# heavy_binge_mod     is an ordered factor for whether people are abstainers, heavy, binge or mod
#                     drinker, it uses the shifted data categorisations

############################################################
# Generating baseline consumption
############################################################

# telling R the variable is an ordered factor
base_consumption$heavy_binge_mod <-
  ordered(base_consumption$heavy_binge_mod, levels = c("abstainer", "moderate", "occ_binge", "heavy"), 
          labels = c("Abstainer", "Moderate", "Occasional Binge", "Heavy"))

price_changes$heavy_binge_mod <-
  ordered(price_changes$heavy_binge_mod, levels = c("abstainer", "moderate", "occ_binge", "heavy"), 
          labels = c("Abstainer", "Moderate", "Occasional Binge", "Heavy"))

# combining the consumption for each individual with the mean prices paid by wealth group
# and drinker group
modelling <-
  left_join(base_consumption, price_changes, by = c("heavy_binge_mod", "wealth"))

# creating a variable for number of drinks in a year
modelling$dk <- modelling$lit*1000/15

# then splitting between homebrew and recorded alcohol
modelling$dk_hb <- modelling$dk * modelling$dk_perc_hb
modelling$dk_rec <- modelling$dk - modelling$dk_hb

# checking drinks peak is never less than mean drinks per day
modelling %>% filter(dk/365 > dk_peak_base) %>% count(sex)

# calculating base spend
modelling$base_spend <-  modelling$mean_base_price * modelling$dk_rec


############################################################
# bringing in the elasticities
############################################################

# For the three categories of consumers, we use the following
# price elasticities, namely -0.22 for binge drinkers,
# -0.18 for other heavy drinkers and -0.4 for moderate drinkers
# but for the ECEA we have adjusted them for wealth quintiles

# these are the ones used in the base case
elasticities <- modelling %>% dplyr::select(heavy_binge_mod, wealth) %>% distinct()

# creating a vector
elasticities$elasticity <- ifelse(elasticities$heavy_binge_mod == "Moderate" & elasticities$wealth == "poorest", -0.53,
                           ifelse(elasticities$heavy_binge_mod == "Moderate" & elasticities$wealth == "poorer", -0.53,
                           ifelse(elasticities$heavy_binge_mod == "Moderate" & elasticities$wealth == "middle", -0.31,
                           ifelse(elasticities$heavy_binge_mod == "Moderate" & elasticities$wealth == "richer", -0.31,
                           ifelse(elasticities$heavy_binge_mod == "Moderate" & elasticities$wealth == "richest", -0.31,
                           ifelse(elasticities$heavy_binge_mod == "Occasional Binge" & elasticities$wealth == "poorest", -0.29,
                           ifelse(elasticities$heavy_binge_mod == "Occasional Binge" & elasticities$wealth == "poorer", -0.29,
                           ifelse(elasticities$heavy_binge_mod == "Occasional Binge" & elasticities$wealth == "middle", -0.17,
                           ifelse(elasticities$heavy_binge_mod == "Occasional Binge" & elasticities$wealth == "richer", -0.17,
                           ifelse(elasticities$heavy_binge_mod == "Occasional Binge" & elasticities$wealth == "richest", -0.17,
                           ifelse(elasticities$heavy_binge_mod == "Heavy" & elasticities$wealth == "poorest", -0.24,
                           ifelse(elasticities$heavy_binge_mod == "Heavy" & elasticities$wealth == "poorer", -0.24,
                           ifelse(elasticities$heavy_binge_mod == "Heavy" & elasticities$wealth == "middle", -0.14,
                           ifelse(elasticities$heavy_binge_mod == "Heavy" & elasticities$wealth == "richer", -0.14,
                           ifelse(elasticities$heavy_binge_mod == "Heavy" & elasticities$wealth == "richest", -0.14, NA)))))))))))))))

# joining the elasticities into the main modelling dataframe
modelling <-
  left_join(modelling, elasticities, by = c("heavy_binge_mod", "wealth"))

############################################################
# calculating perc change in mean price for my results paper
############################################################

perc_change_price_table <- modelling %>% mutate(perc_change_R5 = (mean_R5_mup - mean_base_price)/mean_base_price) %>% 
  mutate(perc_change_R10 = (mean_R10_mup - mean_base_price)/mean_base_price) %>%
  mutate(perc_change_R15 = (mean_R15_mup - mean_base_price)/mean_base_price)
  
perc_change_price_table %>% group_by(heavy_binge_mod, wealth) %>% summarise(weighted.mean(perc_change_R10, pop_wt))


############################################################
# calculating consumption following a change in mean price
############################################################
# this section of code estimates the recorded drink at each 
# policy level by taking the drinks at baseline and then adding 
# on the change in consumption using the elasticity of demand 
# and the percentage change in price

# R5 minimum price
modelling <- modelling %>% mutate(dk_rec_R5 = dk_rec +
                                    (dk_rec * elasticity * ((mean_R5_mup - mean_base_price) /
                                                                 mean_base_price
                                    )))

# change NA to zero
modelling$dk_rec_R5[is.na(modelling$dk_rec_R5)] <- 0

# sense check
summary(modelling$dk_rec_R5)

# R10 minimum price
modelling <- modelling %>% mutate(dk_rec_R10 = dk_rec +
                                    (dk_rec * elasticity * ((mean_R10_mup - mean_base_price) /
                                                                 mean_base_price
                                    )))

# change NA to zero
modelling$dk_rec_R10[is.na(modelling$dk_rec_R10)] <- 0

# sense check
summary(modelling$dk_rec_R10)

# R15 minimum price
modelling <- modelling %>% mutate(dk_rec_R15 = dk_rec +
                                    (dk_rec * elasticity * ((mean_R15_mup - mean_base_price) /
                                                                 mean_base_price
                                    )))

# change NA to zero
modelling$dk_rec_R15[is.na(modelling$dk_rec_R15)] <- 0

# sense check
summary(modelling$dk_rec_R15)

############################################################
# calculating expenditure at each level
############################################################

modelling <-
  modelling %>% mutate(R5_spend = dk_rec_R5 * mean_R5_mup)
modelling <-
  modelling %>% mutate(R10_spend = dk_rec_R10 * mean_R10_mup)
modelling <-
  modelling %>% mutate(R15_spend = dk_rec_R15 * mean_R15_mup)

############################################################
# calculating change in expenditure
############################################################
# in absolute terms
modelling <-
  modelling %>% mutate(change_spend_R5 = R5_spend - base_spend)
modelling <-
  modelling %>% mutate(change_spend_R10 =  R10_spend - base_spend)
modelling <-
  modelling %>% mutate(change_spend_R15 =  R15_spend - base_spend)

# in percentage terms
modelling <-
  modelling %>% mutate(perc_change_spend_R5 = (R5_spend - base_spend)/base_spend)
modelling <-
  modelling %>% mutate(perc_change_spend_R10 =  (R10_spend - base_spend)/base_spend)
modelling <-
  modelling %>% mutate(perc_change_spend_R15 =  (R15_spend - base_spend)/base_spend)

# check
modelling %>% filter(dk_rec > 0) %>% group_by(heavy_binge_mod) %>%
  summarise(mean(change_spend_R5/52),
            mean(change_spend_R10/52),
            mean(change_spend_R15/52))

# check
modelling %>% filter(dk_rec > 0) %>% 
  group_by(heavy_binge_mod) %>%
  summarise(mean(perc_change_spend_R5),
            mean(perc_change_spend_R10),
            mean(perc_change_spend_R15))


############################################################
# adding homebrew back into consumption total
############################################################

# I am adding homebrew back into the model but I am also 
# adding an extra amount of consumption to account for 
# homebrew drinkers increasing their homebrew consumption 
# by 30% of the drop in recorded consumption

# to test this assumption for sensitivity analysis I varied 0.3 
# to 0 and to 1.

modelling <-
  modelling %>% mutate(dk_R5 = ifelse(dk_hb > 0, dk_rec_R5 + dk_hb + 0.3*(dk_rec - dk_rec_R5), dk_rec_R5))

# sense check. dk_R5 should be less than dk which is baseline 
# drinking including both homebrew and recorded
modelling %>% filter(dk_hb > 0) %>% dplyr::select(dk_R5, dk_rec_R5, dk_hb, dk_rec, dk)
                       
                       
modelling <-
  modelling %>% mutate(dk_R10 = ifelse(dk_hb > 0, dk_rec_R10 + dk_hb + 0.3*(dk_rec - dk_rec_R10), dk_rec_R10))
# check
modelling %>% filter(dk_hb > 0) %>% dplyr::select(dk_R10, dk_rec_R10, dk_hb, dk_rec, dk)
                       
                       
modelling <-
  modelling %>% mutate(dk_R15 = ifelse(dk_hb > 0, dk_rec_R15 + dk_hb + 0.3*(dk_rec - dk_rec_R15), dk_rec_R15))
# check
modelling %>% filter(dk_hb > 0) %>% dplyr::select(dk_R15, dk_rec_R15, dk_hb, dk_rec, dk)


# checking
modelling %>% filter(dk_rec > 0) %>% group_by(heavy_binge_mod) %>%
  summarise(mean(dk_R5),
            mean(dk_R10),
            mean(dk_R15))

# just wanting to see who drinks the homebrew
modelling %>% filter(dk_rec > 0, dk_hb > 0) %>% group_by(heavy_binge_mod, wealth) %>%
  summarise(mean(dk_hb/dk))
# looks like everyone but as a percentage of total drinking it is inversely related to wealth
# also it is inversley related to age

############################################################
# calculating change in consumption
############################################################
# in absolute terms
modelling <-
  modelling %>% mutate(change_cons_R5 = dk_R5 - dk)
modelling <-
  modelling %>% mutate(change_cons_R10 = dk_R10 - dk)
modelling <-
  modelling %>% mutate(change_cons_R15 = dk_R15 - dk)

# check
modelling %>% filter(dk_rec > 0) %>%
  group_by(heavy_binge_mod, wealth) %>%
  summarise(weighted.mean(change_cons_R5, pop_wt),
            weighted.mean(change_cons_R10, pop_wt),
            weighted.mean(change_cons_R15, pop_wt))

# in percentage terms
modelling <-
  modelling %>% mutate(perc_change_cons_R5 = (dk_R5 - dk)/dk)
modelling <-
  modelling %>% mutate(perc_change_cons_R10 = (dk_R10 - dk)/dk)
modelling <-
  modelling %>% mutate(perc_change_cons_R15 = (dk_R15 - dk)/dk) 

# check
modelling %>% filter(dk > 0) %>% 
  group_by(heavy_binge_mod, wealth) %>%
  summarise(weighted.mean(perc_change_cons_R5, pop_wt),
            weighted.mean(perc_change_cons_R10, pop_wt),
            weighted.mean(perc_change_cons_R15, pop_wt))


############################################################
# calculating new total litres drunk at each scenario             
############################################################
modelling$lit_R5 <- modelling$dk_R5*15/1000
modelling$lit_R10 <- modelling$dk_R10*15/1000
modelling$lit_R15<- modelling$dk_R15*15/1000

############################################################
# predicting peak consumption using the prediction model
############################################################
# Following the method used in SAPM 
# (see Brennan et al. a mathematical description...)

# first I create a dataframe with just the variables which
# are used in the refression model "peak_model" and then 
# I rename them so they match the variable names used
# in the model

# R5 minimum price
# selecting the variables
temp_R5 <- modelling %>% filter(lit > 0) %>% 
  dplyr::select('caseid', 'lit_R5', 'age_band_4', 'sex', 'dk_5_more')
# renaming the variables
temp_R5 <- rename(temp_R5, lit_SADHS = lit_R5)
# running the model
temp_R5$dk_peak_R5_pred <- predict(peak_model, temp_R5)
# selecting just the id and the new predicted variable
temp_R5 <- temp_R5 %>% dplyr::select('caseid', 'dk_peak_R5_pred')
# joining this back into the main dataframe
modelling <- dplyr::full_join(modelling, temp_R5, by = "caseid")
# adjusting the prediction by the error in prediction at baseline
modelling <- modelling %>% mutate(dk_peak_R5 = dk_peak_base*(dk_peak_R5_pred/dk_peak_base_pred))

# repeating for R10 minimum price
temp_R10 <- modelling %>% filter(lit > 0) %>% 
  dplyr::select('caseid', 'lit_R10', 'age_band_4', 'sex', 'dk_5_more')
temp_R10 <- rename(temp_R10, lit_SADHS = lit_R10)

temp_R10$dk_peak_R10_pred <- predict(peak_model, temp_R10)
temp_R10 <- temp_R10 %>% dplyr::select('caseid', 'dk_peak_R10_pred')
modelling <- dplyr::full_join(modelling, temp_R10, by = "caseid")
modelling <- modelling %>% mutate(dk_peak_R10 = dk_peak_base*(dk_peak_R10_pred/dk_peak_base_pred))

# repeating for R15 minimum price
temp_R15 <- modelling %>% filter(lit > 0) %>% 
  dplyr::select('caseid', 'lit_R15', 'age_band_4', 'sex', 'dk_5_more')
temp_R15 <- rename(temp_R15, lit_SADHS = lit_R15)

temp_R15$dk_peak_R15_pred <- predict(peak_model, temp_R15)
temp_R15 <- temp_R15 %>% dplyr::select('caseid', 'dk_peak_R15_pred')
modelling <- dplyr::full_join(modelling, temp_R15, by = "caseid")
modelling <- modelling %>% mutate(dk_peak_R15 = dk_peak_base*(dk_peak_R15_pred/dk_peak_base_pred))

# check
modelling %>% filter(lit > 0) %>% group_by(heavy_binge_mod) %>% 
  summarise(weighted.mean(dk_peak_base, pop_wt),
            weighted.mean(dk_peak_R5, pop_wt),
            weighted.mean(dk_peak_R10, pop_wt),
            weighted.mean(dk_peak_R15, pop_wt))

modelling %>% filter(lit > 0) %>% group_by(dk_peak_base > 5) %>% count(heavy_binge_mod)
modelling %>% filter(lit > 0) %>% group_by(dk_peak_R5 > 5) %>% count(heavy_binge_mod)
modelling %>% filter(lit > 0) %>% group_by(dk_peak_R10 > 5) %>% count(heavy_binge_mod)
modelling %>% filter(lit > 0) %>% group_by(dk_peak_R15 > 5) %>% count(heavy_binge_mod)

# check that the dk_peak is not less than the daily mean
modelling %>% filter(dk_peak_base < dk/365) %>% count(sex)
modelling %>% filter(dk_peak_R5 < dk_R5/365) %>% count(sex)
modelling %>% filter(dk_peak_R10 < dk_R10/365) %>% count(sex)
modelling %>% filter(dk_peak_R15 < dk_R15/365) %>% count(sex)

############################################################
# change in industry and government revenue                         
############################################################
# calculate total spend for each scenario, population weighted
# increased to take account of the fact we only modelled 80% 
# of the total consumption
base_spend <- sum(modelling$base_spend*modelling$pop_wt, na.rm = TRUE)/80*100
R5_spend <- sum(modelling$R5_spend*modelling$pop_wt, na.rm = TRUE)/80*100
R10_spend <- sum(modelling$R10_spend*modelling$pop_wt, na.rm = TRUE)/80*100
R15_spend <- sum(modelling$R15_spend*modelling$pop_wt, na.rm = TRUE)/80*100

# create a vector
policy <- c("base", "R5", "R10", "R15")
# make it a dataframe
rev_data <- as.data.frame(policy)
# add in the spend
rev_data$spend <- c(base_spend, R5_spend, R10_spend, R15_spend)
# calculating the change in spend
rev_data$spend_change <- rev_data$spend - rev_data$spend[rev_data$policy == "base"]

# create a new column which calculates the VAT collected for every scenario
# vat is 15%
rev_data$VAT <- rev_data$spend/115*15

# I know from the Budget report 
# http://www.treasury.gov.za/documents/National%20Budget/2020/review/FullBR.pdf
# that the excise tax collected from alcohol from 2018/19 was R25,998,477,000

# allocate this to an object
base_excise_tax <- 25998477000

# calcuate the total volume consumed (in drinks) at each scenario
base_dk_alcohol <- sum(modelling$dk_rec*modelling$pop_wt, na.rm = TRUE)/80*100
R5_dk_alcohol <- sum(modelling$dk_rec_R5*modelling$pop_wt, na.rm = TRUE)/80*100
R10_dk_alcohol <- sum(modelling$dk_rec_R10*modelling$pop_wt, na.rm = TRUE)/80*100
R15_dk_alcohol <- sum(modelling$dk_rec_R15*modelling$pop_wt, na.rm = TRUE)/80*100

# add them into the dataframe
rev_data$dk_alcohol <- c(base_dk_alcohol, R5_dk_alcohol, R10_dk_alcohol, R15_dk_alcohol)

# and the volume of alcohol in litres of pure alcohol
rev_data$lit_pure_alc <- rev_data$dk_alcohol*15/1000

# create perc change in volume, all in relation to baseline
rev_data$perc_change_vol <- (rev_data$dk_alcohol - base_dk_alcohol)/base_dk_alcohol

# calculate new excise tax by multiplying the percentage change in volume of 
# alcohol with baseline excise tax. This assumes a fixed ratio between volume
# of alcohol consumed and excise tax revenue. 
rev_data$excise_tax <- base_excise_tax*rev_data$perc_change_vol

# calculate retailer revenue as spend - excise tax - VAT
rev_data$retail_rev <- rev_data$spend - rev_data$VAT - rev_data$excise_tax

# calculating the change
rev_data$retail_change <- rev_data$retail_rev - rev_data$retail_rev[policy == "base"]
rev_data$vat_change <- rev_data$VAT - rev_data$VAT[policy == "base"]
rev_data$excise_tax_change <- rev_data$excise_tax - rev_data$excise_tax[policy == "base"]
rev_data$net_tax <- rev_data$vat_change + rev_data$excise_tax_change

############################################################
# saving the data      
############################################################

# selecting only the variables I need to take through to 
# the consumption to harm model
consumption_scenarios <- modelling %>% 
dplyr::select('pop_wt', 'sex', 'age', 'age_band_4', 'age_band', 'wealth', 'wealtha', 'province',
              'urban_rural', 'edu', 'pop_group', 'pop_wt',
              'heavy_binge_mod', 'dk_ever', 'dk_hb',
              'dk_rec', 'dk', 'dk_rec_R5', 'dk_rec_R10', 'dk_rec_R15', 'dk_R5', 'dk_R10', 'dk_R15',
              'dk_peak_base', 'dk_peak_R5', 'dk_peak_R10', 'dk_peak_R15')

# savig this so I can do some more detailed data visualistaion
save(modelling, file = "intermediate/modelling.Rda")
# saving this to take through to the consumption to harm model
save(consumption_scenarios, file = "X:/ScHARR/STU_CMP16NKG/Model/consumption_harm/inputs/consumption_scenarios.Rda")
save(consumption_scenarios, file = "outputs/consumption_scenarios.Rda")
