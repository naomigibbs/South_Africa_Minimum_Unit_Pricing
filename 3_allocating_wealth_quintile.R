# code to get the proportion of the population in each wealth quintile/drinker_type/sex/age_band
# also this is combined with probability of death at each age which does
# not vary by wealth quintile

# the age sex structure is from statistics south africa and the life tables
# i have used are iHME life tables


##########################
#### Packages         ####
##########################
library(tidyverse)
library(readxl)

##########################
#### Data             ####
##########################

load("inputs/consumption_scenarios.Rda")

males_2018 <- read_excel("inputs/SSA_age_sex_structure_2018.xlsx",
                         sheet = "age_sex_structure_m")

females_2018 <- read_excel("inputs/SSA_age_sex_structure_2018.xlsx",
                         sheet = "age_sex_structure_f")

#####################################################################################
#### first of all checking the sample size
#####################################################################################
# just looking at the data

table(consumption_scenarios$age, consumption_scenarios$heavy_binge_mod, consumption_scenarios$wealth, consumption_scenarios$sex)

table(consumption_scenarios$age_band, consumption_scenarios$wealth, consumption_scenarios$sex)

table(consumption_scenarios$wealth, consumption_scenarios$heavy_binge_mod, consumption_scenarios$sex)

# I am trying to get the 2018 population to reflect the SADHS population profile in terms of who is 
# what type of drinker. So I am finding proportions of sex/wealth/drinker types by age and then 
# applying this to the reported population in 2018. I have used age_bands in SADHS and matched it with age_bands in 
# the population stats.

# I have also created an age band which aggregates the over 50s as there are not many observations in the 
# older age groups
consumption_scenarios$age_band_reduced <- ifelse(consumption_scenarios$age >= 50, "50+", consumption_scenarios$age_band)
males_2018$age_band_reduced <- ifelse(males_2018$age >= 50, "50+", males_2018$age_band)
females_2018$age_band_reduced <- ifelse(females_2018$age >= 50, "50+", females_2018$age_band)

# check
table(consumption_scenarios$age_band, consumption_scenarios$age_band_reduced)

#####################################################################################
#### creating the proportions of the SADHS who are in each wealth quintile
#### by age group, taking into account the population weights
#####################################################################################
#### males

# before we begin the population of over 14 males we are looking at is 19,645,159
males_2018 %>% filter(age > 14) %>% summarise(sum(pop_age))
# the total poulation we are looking at is 31,073,820. Athough remember that includes some future generations, 
# added in because the policy effect is for ages 15+ and we want the model to run for 20 years.
sum(males_2018$pop_age)

# first of all I will calculate the proportion of the SADHS survey who fall into each
# group making it nationally representative using the population weights
male_props <- consumption_scenarios %>% filter(sex == "male") %>% 
  group_by(wealth, age_band_reduced, heavy_binge_mod, .drop = FALSE) %>% summarise(sum(pop_wt))

# rename the variable wt_sample_pop 
male_props <- rename(male_props, pop = "sum(pop_wt)")

# check that I have not lost anyone along the way, these two should be the same
sum(male_props$pop)
consumption_scenarios %>% filter(sex == "male") %>% summarise(sum(pop_wt))

# calculating the proportion of people within each age_band_reduced group that fall into each wealth/drinker_type quintile
male_props <- male_props %>% group_by(age_band_reduced) %>% mutate(prop_wealth_drinker = pop/sum(pop)) %>% ungroup()

# check the sum of the proportions equals one within each age band
male_props <- male_props %>% group_by(age_band_reduced) %>% mutate(check = sum(prop_wealth_drinker)) %>% ungroup()
summary(male_props$check)

# select only the variables required
male_props <- male_props %>% dplyr::select(wealth, age_band_reduced, heavy_binge_mod, prop_wealth_drinker)

# join the proportions to the population table
males_2018 <- dplyr::full_join(male_props, males_2018, by = c('age_band_reduced'))

# calculate the population within each wealth/drinker group
males_2018$pop_wealth_drinker <- round(males_2018$prop_wealth_drinker*males_2018$pop_age, digits = 0)

# need to check i have the same population total as 19,645,159
sum(males_2018$pop_wealth_drinker, na.rm = TRUE )
# only out by a few due to rounding

# as i want the model to run through by 20 years I need to apply the same splits to the pre 15 year old population

# create a dataframe that lists all wealth categories and drinker types for every age
all <- males_2018 %>% expand(age, wealth, heavy_binge_mod)

# only keep those ages less than 15
all <- all %>% filter(age < 15)

# rejoin the dataframes by age
males_2018 <- full_join(males_2018, all, by = c("age"))

# sorting out the fact we have duplcate columns
males_2018 <- males_2018 %>% mutate(heavy_binge_mod = if_else(age < 15, heavy_binge_mod.y, heavy_binge_mod.x))
males_2018 <- males_2018 %>% mutate(wealth = if_else(age < 15, wealth.y, wealth.x))

# select only the required variables
males_2018 <- males_2018 %>% dplyr::select(wealth, age_band_reduced, age, age_band, heavy_binge_mod, 
                                           prob_death, population, pop_age, prop_wealth_drinker, pop_wealth_drinker)

# ok now i have to split the population by the wealth/drinker proportions for under 15s. 
# I will use the 15 - 19 proportions for all ages less than 15

males_2018 <- males_2018 %>% group_by(wealth, heavy_binge_mod) %>% 
  mutate(pop_wealth_drinker = ifelse(age < 15, pop_age*(prop_wealth_drinker[which(age == 15)]), pop_wealth_drinker)) %>% 
  ungroup()

# check, these should be the same
males_2018 %>% filter(age == 14) %>% summarise(min(pop_age), max(pop_age), mean(pop_age))
males_2018 %>% filter(age == 14) %>% summarise(sum(pop_wealth_drinker))

# check the total is 31 million people
sum(males_2018$pop_wealth_drinker)

#####################################################################################
#### females
#####################################################################################
# before we begin the population of over 14 females we are looking at is 21,036,930
females_2018 %>% filter(age > 14) %>% summarise(sum(pop_age))
# the total poulation we are looking at is 32366218. Athough remember that includes some future generations, 
# added in because the policy effect is for ages 15+ and we want the model to run for 20 years.
sum(females_2018$pop_age)

# first of all I will calculate the proportion of the SADHS survey who fall into each
# group making it nationally representative using the population weights
female_props <- consumption_scenarios %>% filter(sex == "female") %>% 
  group_by(wealth, age_band_reduced, heavy_binge_mod, .drop = FALSE) %>% summarise(sum(pop_wt))

# rename the variable wt_sample_pop 
female_props <- rename(female_props, pop = "sum(pop_wt)")

# check that I have not lost anyone along the way, these two should be the same
sum(female_props$pop)
consumption_scenarios %>% filter(sex == "female") %>% summarise(sum(pop_wt))

# calculating the proportion of people within each age_band_reduced group that fall into each wealth/drinker_type quintile
female_props <- female_props %>% group_by(age_band_reduced) %>% mutate(prop_wealth_drinker = pop/sum(pop)) %>% ungroup()

# check the sum of the proportions equals one within each age band
female_props <- female_props %>% group_by(age_band_reduced) %>% mutate(check = sum(prop_wealth_drinker)) %>% ungroup()
summary(female_props$check)

# select only the variables required
female_props <- female_props %>% dplyr::select(wealth, age_band_reduced, heavy_binge_mod, prop_wealth_drinker)

# join the proportions to the population table
females_2018 <- dplyr::full_join(female_props, females_2018, by = c('age_band_reduced'))

# calculate the population within each wealth/drinker group
females_2018$pop_wealth_drinker <- round(females_2018$prop_wealth_drinker*females_2018$pop_age, digits = 0)

# need to check i have the same population total as 19,645,159
sum(females_2018$pop_wealth_drinker, na.rm = TRUE )
# only out by a few due to rounding

# as i want the model to run through by 20 years I need to apply the same splits to the pre 15 year old population

# create a dataframe that lists all wealth categories and drinker types for every age
all <- females_2018 %>% expand(age, wealth, heavy_binge_mod)

# only keep those ages less than 15
all <- all %>% filter(age < 15)

# rejoin the dataframes by age
females_2018 <- full_join(females_2018, all, by = c("age"))

# sorting out the fact we have duplcate columns
females_2018 <- females_2018 %>% mutate(heavy_binge_mod = if_else(age < 15, heavy_binge_mod.y, heavy_binge_mod.x))
females_2018 <- females_2018 %>% mutate(wealth = if_else(age < 15, wealth.y, wealth.x))

# select only the required variables
females_2018 <- females_2018 %>% dplyr::select(wealth, age_band_reduced, age, age_band, heavy_binge_mod, 
                                           prob_death, population, pop_age, prop_wealth_drinker, pop_wealth_drinker)

# ok now i have to split the population by the wealth/drinker proportions for under 15s. 
# I will use the 15 - 19 proportions for all ages less than 15

females_2018 <- females_2018 %>% group_by(wealth, heavy_binge_mod) %>% 
  mutate(pop_wealth_drinker = ifelse(age < 15, pop_age*(prop_wealth_drinker[which(age == 15)]), pop_wealth_drinker)) %>% 
  ungroup()

# check, these should be the same
females_2018 %>% filter(age == 14) %>% summarise(min(pop_age), max(pop_age), mean(pop_age))
females_2018 %>% filter(age == 14) %>% summarise(sum(pop_wealth_drinker))

# check the total is 31 million people
sum(females_2018$pop_wealth_drinker)


#####################################################################################
#### saving the files
#####################################################################################

save(males_2018, file = "intermediate/males_2018.Rda")
save(females_2018, file = "intermediate/females_2018.Rda")
