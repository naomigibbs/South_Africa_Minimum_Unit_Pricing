# This is getting the populations ready with the probabilities at baseline and
# at each policy level ready to feed into the model

##########################
#### Packages         ####
##########################
library(tidyverse)

##########################
#### Data             ####
##########################
load("intermediate/females_2018.Rda")
load("intermediate/males_2018.Rda")

load("intermediate/baseline_probabilities.Rda")

load("intermediate/pif_hiv.Rda")
load("intermediate/pif_ii.Rda")
load("intermediate/pif_road.Rda")
load("intermediate/pif_liver.Rda")
load("intermediate/pif_bcancer.Rda")

################################################################################
#### females
################################################################################
# creating a variable for sex
females_2018$sex <- "female"

# bring in the pifs 
females_2018 <- dplyr::left_join(females_2018, pif_hiv, by = c('heavy_binge_mod', 'sex', 'wealth'))
females_2018 <- dplyr::left_join(females_2018, pif_ii, by = c('heavy_binge_mod', 'sex', 'wealth'))
females_2018 <- dplyr::left_join(females_2018, pif_road, by = c('heavy_binge_mod', 'sex', 'wealth'))
females_2018 <- dplyr::left_join(females_2018, pif_liver, by = c('heavy_binge_mod', 'sex', 'wealth'))
females_2018 <- dplyr::left_join(females_2018, pif_bcancer, by = c('heavy_binge_mod', 'sex', 'wealth'))

# bring in the probability of death/prevelance at baseline for each health outcome
# first I need to change baseline_probabilites from long to wide format
baseline_deaths <- baseline_probabilities %>% select(-prob_cases_wealth_dktype) %>% spread(disease, prob_deaths_wealth_dktype)
baseline_cases <- baseline_probabilities %>% select(-prob_deaths_wealth_dktype) %>% spread(disease, prob_cases_wealth_dktype)

# renaming the variables
baseline_deaths <- rename(baseline_deaths, prob_death_hiv = 'hiv')
baseline_deaths <- rename(baseline_deaths, prob_death_ii = 'ii')
baseline_deaths <- rename(baseline_deaths, prob_death_road = 'road')
baseline_deaths <- rename(baseline_deaths, prob_death_liver = 'liver')
baseline_deaths <- rename(baseline_deaths, prob_death_bcancer = 'bcancer')

baseline_cases <- rename(baseline_cases, prob_hiv = 'hiv')
baseline_cases <- rename(baseline_cases, prob_ii = 'ii')
baseline_cases <- rename(baseline_cases, prob_road = 'road')
baseline_cases <- rename(baseline_cases, prob_liver = 'liver')
baseline_cases <- rename(baseline_cases, prob_bcancer = 'bcancer')

# attach the dataframes together
females_2018 <- left_join(females_2018, baseline_deaths, by = c('sex', 'wealth', 'heavy_binge_mod'))
females_2018 <- left_join(females_2018, baseline_cases, by = c('sex', 'wealth', 'heavy_binge_mod'))

################################################################################
#### caclulating the prob of death at each policy scenario
################################################################################

# seperate the risk of health outcomes of interest from the risk of all other causes of mortality
females_2018$prob_death_non_alc <- females_2018$prob_death - females_2018$prob_death_ii - females_2018$prob_death_hiv - females_2018$prob_death_road - females_2018$prob_death_liver - females_2018$prob_death_bcancer

# check that the prob of death is not negative. Since we can not ressurect people in the model
summary(females_2018$prob_death_non_alc)
# we need to recode all those less than 0 to zero
females_2018$prob_death_non_alc[(females_2018$prob_death_non_alc < 0)] <- 0
# check
summary(females_2018$prob_death_non_alc)
# there are a number of NAs but that is because the policy only applies to age 15 up
# and we have ages 0 - 14 plus an extra five years which are minus's at the start so
# that the model can run forward 20 years

# calculate the prob of death under R5/10/15 minimum price and extreme scenario
females_2018$prob_death_hiv_R5 <- females_2018$prob_death_hiv * females_2018$pif_hiv_R5
females_2018$prob_death_hiv_R10 <- females_2018$prob_death_hiv * females_2018$pif_hiv_R10
females_2018$prob_death_hiv_R15 <- females_2018$prob_death_hiv * females_2018$pif_hiv_R15
females_2018$prob_death_hiv_extreme <- females_2018$prob_death_hiv * females_2018$pif_hiv_extreme

females_2018$prob_death_ii_R5 <- females_2018$prob_death_ii * females_2018$pif_ii_R5
females_2018$prob_death_ii_R10 <- females_2018$prob_death_ii * females_2018$pif_ii_R10
females_2018$prob_death_ii_R15 <- females_2018$prob_death_ii * females_2018$pif_ii_R15
females_2018$prob_death_ii_extreme <- females_2018$prob_death_ii * females_2018$pif_ii_extreme

females_2018$prob_death_road_R5 <- females_2018$prob_death_road * females_2018$pif_road_R5
females_2018$prob_death_road_R10 <- females_2018$prob_death_road * females_2018$pif_road_R10
females_2018$prob_death_road_R15 <- females_2018$prob_death_road * females_2018$pif_road_R15
females_2018$prob_death_road_extreme <- females_2018$prob_death_road * females_2018$pif_road_extreme

females_2018$prob_death_liver_R5 <- females_2018$prob_death_liver * females_2018$pif_liver_R5
females_2018$prob_death_liver_R10 <- females_2018$prob_death_liver * females_2018$pif_liver_R10
females_2018$prob_death_liver_R15 <- females_2018$prob_death_liver * females_2018$pif_liver_R15
females_2018$prob_death_liver_extreme <- females_2018$prob_death_liver * females_2018$pif_liver_extreme

females_2018$prob_death_bcancer_R5 <- females_2018$prob_death_bcancer * females_2018$pif_bcancer_R5
females_2018$prob_death_bcancer_R10 <- females_2018$prob_death_bcancer * females_2018$pif_bcancer_R10
females_2018$prob_death_bcancer_R15 <- females_2018$prob_death_bcancer * females_2018$pif_bcancer_R15
females_2018$prob_death_bcancer_extreme <- females_2018$prob_death_bcancer * females_2018$pif_bcancer_extreme

################################################################################
#### caclulating the prob of having the disease at each scenario
################################################################################

# calculate the prob of having the condition (prev) under R5/10/15 minimum price and extreme scenario
females_2018$prob_hiv_R5 <- females_2018$prob_hiv * females_2018$pif_hiv_R5
females_2018$prob_hiv_R10 <- females_2018$prob_hiv * females_2018$pif_hiv_R10
females_2018$prob_hiv_R15 <- females_2018$prob_hiv * females_2018$pif_hiv_R15
females_2018$prob_hiv_extreme <- females_2018$prob_hiv * females_2018$pif_hiv_extreme

females_2018$prob_ii_R5 <- females_2018$prob_ii * females_2018$pif_ii_R5
females_2018$prob_ii_R10 <- females_2018$prob_ii * females_2018$pif_ii_R10
females_2018$prob_ii_R15 <- females_2018$prob_ii * females_2018$pif_ii_R15
females_2018$prob_ii_extreme <- females_2018$prob_ii * females_2018$pif_ii_extreme

females_2018$prob_road_R5 <- females_2018$prob_road * females_2018$pif_road_R5
females_2018$prob_road_R10 <- females_2018$prob_road * females_2018$pif_road_R10
females_2018$prob_road_R15 <- females_2018$prob_road * females_2018$pif_road_R15
females_2018$prob_road_extreme <- females_2018$prob_road * females_2018$pif_road_extreme

females_2018$prob_liver_R5 <- females_2018$prob_liver * females_2018$pif_liver_R5
females_2018$prob_liver_R10 <- females_2018$prob_liver * females_2018$pif_liver_R10
females_2018$prob_liver_R15 <- females_2018$prob_liver * females_2018$pif_liver_R15
females_2018$prob_liver_extreme <- females_2018$prob_liver * females_2018$pif_liver_extreme

females_2018$prob_bcancer_R5 <- females_2018$prob_bcancer * females_2018$pif_bcancer_R5
females_2018$prob_bcancer_R10 <- females_2018$prob_bcancer * females_2018$pif_bcancer_R10
females_2018$prob_bcancer_R15 <- females_2018$prob_bcancer * females_2018$pif_bcancer_R15
females_2018$prob_bcancer_extreme <- females_2018$prob_bcancer * females_2018$pif_bcancer_extreme

# choosing jyst the variables needed
female_population <- females_2018 %>% arrange(age) %>% 
  select(age, pop_wealth_drinker, wealth, heavy_binge_mod, starts_with("prob_"), starts_with("pif_"))
# change all NA's to zero 
female_population[is.na(female_population)] = 0
# rename pop_wealthquintile pop_t0
female_population <- rename(female_population, pop_t0 = 'pop_wealth_drinker')

################################################################################
#### males
################################################################################
# creating a variable for sex
males_2018$sex <- "male"

# bring in the pifs 
males_2018 <- dplyr::left_join(males_2018, pif_hiv, by = c('heavy_binge_mod', 'sex', 'wealth'))
males_2018 <- dplyr::left_join(males_2018, pif_ii, by = c('heavy_binge_mod', 'sex', 'wealth'))
males_2018 <- dplyr::left_join(males_2018, pif_road, by = c('heavy_binge_mod', 'sex', 'wealth'))
males_2018 <- dplyr::left_join(males_2018, pif_liver, by = c('heavy_binge_mod', 'sex', 'wealth'))
males_2018 <- dplyr::left_join(males_2018, pif_bcancer, by = c('heavy_binge_mod', 'sex', 'wealth'))

# bring in the probability of death/prevelance at baseline for each health outcome
# first I need to change baseline_probabilites from long to wide format
baseline_deaths <- baseline_probabilities %>% select(-prob_cases_wealth_dktype) %>% spread(disease, prob_deaths_wealth_dktype)
baseline_cases <- baseline_probabilities %>% select(-prob_deaths_wealth_dktype) %>% spread(disease, prob_cases_wealth_dktype)

# renaming the variables
baseline_deaths <- rename(baseline_deaths, prob_death_hiv = 'hiv')
baseline_deaths <- rename(baseline_deaths, prob_death_ii = 'ii')
baseline_deaths <- rename(baseline_deaths, prob_death_road = 'road')
baseline_deaths <- rename(baseline_deaths, prob_death_liver = 'liver')
baseline_deaths <- rename(baseline_deaths, prob_death_bcancer = 'bcancer')

baseline_cases <- rename(baseline_cases, prob_hiv = 'hiv')
baseline_cases <- rename(baseline_cases, prob_ii = 'ii')
baseline_cases <- rename(baseline_cases, prob_road = 'road')
baseline_cases <- rename(baseline_cases, prob_liver = 'liver')
baseline_cases <- rename(baseline_cases, prob_bcancer = 'bcancer')

# attach the dataframes together
males_2018 <- left_join(males_2018, baseline_deaths, by = c('sex', 'wealth', 'heavy_binge_mod'))
males_2018 <- left_join(males_2018, baseline_cases, by = c('sex', 'wealth', 'heavy_binge_mod'))

################################################################################
#### caclulating the prob of death at each policy scenario
################################################################################

# seperate the risk of health outcomes of interest from the risk of all other causes of mortality
males_2018$prob_death_non_alc <- males_2018$prob_death - males_2018$prob_death_ii - males_2018$prob_death_hiv - males_2018$prob_death_road - males_2018$prob_death_liver - males_2018$prob_death_bcancer

# check that the prob of death is not negative. Since we can not ressurect people in the model
summary(males_2018$prob_death_non_alc)
# we need to recode all those less than 0 to zero
males_2018$prob_death_non_alc[(males_2018$prob_death_non_alc < 0)] <- 0
# check
summary(males_2018$prob_death_non_alc)
# there are a number of NAs but that is because the policy only applies to age 15 up
# and we have ages 0 - 14 plus an extra five years which are minus's at the start so
# that the model can run forward 20 years

# calculate the prob of death under R5/10/15 minimum price and extreme scenario
males_2018$prob_death_hiv_R5 <- males_2018$prob_death_hiv * males_2018$pif_hiv_R5
males_2018$prob_death_hiv_R10 <- males_2018$prob_death_hiv * males_2018$pif_hiv_R10
males_2018$prob_death_hiv_R15 <- males_2018$prob_death_hiv * males_2018$pif_hiv_R15
males_2018$prob_death_hiv_extreme <- males_2018$prob_death_hiv * males_2018$pif_hiv_extreme

males_2018$prob_death_ii_R5 <- males_2018$prob_death_ii * males_2018$pif_ii_R5
males_2018$prob_death_ii_R10 <- males_2018$prob_death_ii * males_2018$pif_ii_R10
males_2018$prob_death_ii_R15 <- males_2018$prob_death_ii * males_2018$pif_ii_R15
males_2018$prob_death_ii_extreme <- males_2018$prob_death_ii * males_2018$pif_ii_extreme

males_2018$prob_death_road_R5 <- males_2018$prob_death_road * males_2018$pif_road_R5
males_2018$prob_death_road_R10 <- males_2018$prob_death_road * males_2018$pif_road_R10
males_2018$prob_death_road_R15 <- males_2018$prob_death_road * males_2018$pif_road_R15
males_2018$prob_death_road_extreme <- males_2018$prob_death_road * males_2018$pif_road_extreme

males_2018$prob_death_liver_R5 <- males_2018$prob_death_liver * males_2018$pif_liver_R5
males_2018$prob_death_liver_R10 <- males_2018$prob_death_liver * males_2018$pif_liver_R10
males_2018$prob_death_liver_R15 <- males_2018$prob_death_liver * males_2018$pif_liver_R15
males_2018$prob_death_liver_extreme <- males_2018$prob_death_liver * males_2018$pif_liver_extreme

males_2018$prob_death_bcancer_R5 <- males_2018$prob_death_bcancer * males_2018$pif_bcancer_R5
males_2018$prob_death_bcancer_R10 <- males_2018$prob_death_bcancer * males_2018$pif_bcancer_R10
males_2018$prob_death_bcancer_R15 <- males_2018$prob_death_bcancer * males_2018$pif_bcancer_R15
males_2018$prob_death_bcancer_extreme <- males_2018$prob_death_bcancer * males_2018$pif_bcancer_extreme

################################################################################
#### caclulating the prob of having the disease at each scenario
################################################################################

# calculate the prob of having the condition (prev) under R5/10/15 minimum price and extreme scenario
males_2018$prob_hiv_R5 <- males_2018$prob_hiv * males_2018$pif_hiv_R5
males_2018$prob_hiv_R10 <- males_2018$prob_hiv * males_2018$pif_hiv_R10
males_2018$prob_hiv_R15 <- males_2018$prob_hiv * males_2018$pif_hiv_R15
males_2018$prob_hiv_extreme <- males_2018$prob_hiv * males_2018$pif_hiv_extreme

males_2018$prob_ii_R5 <- males_2018$prob_ii * males_2018$pif_ii_R5
males_2018$prob_ii_R10 <- males_2018$prob_ii * males_2018$pif_ii_R10
males_2018$prob_ii_R15 <- males_2018$prob_ii * males_2018$pif_ii_R15
males_2018$prob_ii_extreme <- males_2018$prob_ii * males_2018$pif_ii_extreme

males_2018$prob_road_R5 <- males_2018$prob_road * males_2018$pif_road_R5
males_2018$prob_road_R10 <- males_2018$prob_road * males_2018$pif_road_R10
males_2018$prob_road_R15 <- males_2018$prob_road * males_2018$pif_road_R15
males_2018$prob_road_extreme <- males_2018$prob_road * males_2018$pif_road_extreme

males_2018$prob_liver_R5 <- males_2018$prob_liver * males_2018$pif_liver_R5
males_2018$prob_liver_R10 <- males_2018$prob_liver * males_2018$pif_liver_R10
males_2018$prob_liver_R15 <- males_2018$prob_liver * males_2018$pif_liver_R15
males_2018$prob_liver_extreme <- males_2018$prob_liver * males_2018$pif_liver_extreme

males_2018$prob_bcancer_R5 <- males_2018$prob_bcancer * males_2018$pif_bcancer_R5
males_2018$prob_bcancer_R10 <- males_2018$prob_bcancer * males_2018$pif_bcancer_R10
males_2018$prob_bcancer_R15 <- males_2018$prob_bcancer * males_2018$pif_bcancer_R15
males_2018$prob_bcancer_extreme <- males_2018$prob_bcancer * males_2018$pif_bcancer_extreme

# choosing just the variables needed
male_population <- males_2018 %>% arrange(age) %>% 
  select(age, pop_wealth_drinker, wealth, heavy_binge_mod, starts_with("prob_"), starts_with("pif_"))
# change all NA's to zero 
male_population[is.na(male_population)] = 0
# rename pop_wealthquintile pop_t0
male_population <- rename(male_population, pop_t0 = 'pop_wealth_drinker')

######################################################################
#### saving the population data
######################################################################
save(female_population, file = "intermediate/female_population.Rda" )
save(male_population, file = "intermediate/male_population.Rda" )



