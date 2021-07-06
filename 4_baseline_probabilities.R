# this file is to calculate the probability of death/cases at baseline

# health outcomes are:
# hiv
# ii
# road injury
# liver cirrhosis
# breast cancer

##########################
#### Packages         ####
##########################
library(tidyverse)
library(readxl)

##########################
#### Data             ####
##########################

mortality_cases <- read_excel("inputs/health_data_inputs_2018.xlsx", sheet = "aggregate_levels")
population_2018 <- read_excel("inputs/SSA_age_sex_structure_2018.xlsx",
                           sheet = "aggregate_population")
load("inputs/prev_wealthquintile.Rda")
load("intermediate/rr_drinker_type.Rda")
load("inputs/consumption_scenarios.Rda")


##########################
#### Data dictionary  ####
##########################
# pop                 this is the population within the five year age band
# pop_age             this is the population for each single year of age
# prop_wealth_dktype  the proportion of the population in that single year of age 
#                     that falls within that particular drinker type and wealth group
# heavy_binge_mod     drinker type, abstainer, heavy, occ_binge or moderate
# wealth              wealth quintile from poorest to richest

# deaths_hiv_ageband  number of deaths in a five year ageband (repeated for all 5 conditions)
# prev_hiv_ageband    number of cases in a five year ageband (repeated for all 5 conditions)
# deaths_hiv          number of deaths in a single year of age (repeated for all 5 conditions)
# prev_hiv            number of cases in a single year of age (repeated for all 5 conditions)



##################################################################################
#### Step one: Understanding the SADHS population by quintile                 ####
##################################################################################

# checking sample size
consumption_scenarios %>% group_by(wealth, heavy_binge_mod) %>% count(sex)
# just about ok

# create the proportion of the population in each sex, wealth group, drinker type
props <- consumption_scenarios %>% 
  group_by(sex, wealth, .drop = FALSE) %>% summarise(sum(pop_wt))

# rename the variable
props <- rename(props, population_SADHS = "sum(pop_wt)")

# check that I have not lost anyone along the way, these two should be the same
sum(props$population_SADHS)
consumption_scenarios %>% summarise(sum(pop_wt))

# make each group into a proportion of the population seperately for male and female
props <- props %>% group_by(sex) %>% mutate(perc_share_pop = population_SADHS/sum(population_SADHS))

# check
props %>% group_by(sex) %>% summarise(sum(perc_share_pop))

# select only variables needed
props <- props %>% select('sex', 'wealth', 'perc_share_pop')

##################################################################################
#### Step two (a) allocating the proportions of cases by wealth quintiles     ####
##################################################################################
# this is accompanied by a spreadsheet with working if you need to refer back
# saved in 
# X:\ScHARR\STU_CMP16NKG\Model\consumption_harm\inputs\allocating_by_wealth_workings.xlsx"

# gathering the prevelance wealth quintile so that it is in long format
prev_wealthquintile <- tidyr::gather(prev_wealthquintile, 'disease', 'quintile_proportion', 2:6)

# rename the disease values
prev_wealthquintile$disease[prev_wealthquintile$disease == 'prop_ii'] <- 'ii'
prev_wealthquintile$disease[prev_wealthquintile$disease == 'prop_hiv'] <- 'hiv'
prev_wealthquintile$disease[prev_wealthquintile$disease == 'prop_road'] <- 'road'
prev_wealthquintile$disease[prev_wealthquintile$disease == 'prop_liver'] <- 'liver'
prev_wealthquintile$disease[prev_wealthquintile$disease == 'prop_bcancer'] <- 'bcancer'

# joining together the SADHS data with the proportions in each wealth quintile
props <- left_join(props, prev_wealthquintile, by = 'wealth')

# joining in the mortality by disease and sex
props <- left_join(props, mortality_cases, by = c('sex', 'disease'))

# calculating the cases if quintiles were of equal size
props <- props %>% mutate(cases_equal_quintiles = quintile_proportion * cases)

# check
mortality_cases 
props %>% group_by(disease, sex) %>% summarise(sum(cases_equal_quintiles))

# add in the population size if the quintiles were of equal size in 2018
props <- left_join(props, population_2018, by = 'sex')
props <- props %>% mutate(pop_equal_quintile = population/5)

# create the population in each quintile if we have the same distribution between the quintiles
# as in the SADHS data
props <- props %>% mutate(pop_SADHS_proportions = population * perc_share_pop)

# calculating the probability of cases if quintiles were equal
props <- props %>% mutate(prob_cases_equal_quintiles = cases_equal_quintiles/pop_equal_quintile)

# calculating expected cases using quintiles that reflect SADHS proportions
# this is more complicated and if you need additional suppport look at the excel spreadsheet referenced above
props <- props %>% mutate(expected_cases_SADHS = ifelse(wealth == 'poorest', pop_SADHS_proportions * prob_cases_equal_quintiles, 
                                                 
                                               ifelse(wealth == 'poorer', prob_cases_equal_quintiles[wealth == 'poorest'] *         
                                                (pop_equal_quintile[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorest']) +
                                                 prob_cases_equal_quintiles[wealth == 'poorer'] * 
                                                 (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] - pop_equal_quintile[wealth == 'poorest']), 
                                                
                                                ifelse(wealth == 'middle', prob_cases_equal_quintiles[wealth == 'poorer'] *
                                                  (pop_equal_quintile[wealth == 'poorest'] + pop_equal_quintile[wealth == 'poorer'] - 
                                                     pop_SADHS_proportions[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorer']) +
                                                  prob_cases_equal_quintiles[wealth == 'middle'] *
                                                  (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] + pop_SADHS_proportions[wealth == 'middle'] - 
                                                     pop_equal_quintile[wealth == 'poorest'] - pop_equal_quintile[wealth == 'poorer']), 
                                                  
                                                  ifelse(wealth == 'richer', prob_cases_equal_quintiles[wealth == 'middle'] *
                                                           (pop_equal_quintile[wealth == 'poorest'] + pop_equal_quintile[wealth == 'poorer'] + pop_equal_quintile[wealth == 'middle'] - 
                                                              pop_SADHS_proportions[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorer'] - pop_SADHS_proportions[wealth == 'middle']) +
                                                           prob_cases_equal_quintiles[wealth == 'richer'] *
                                                           (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] + 
                                                              pop_SADHS_proportions[wealth == 'middle'] + pop_SADHS_proportions[wealth == 'richer'] - 
                                                              pop_equal_quintile[wealth == 'poorest'] - pop_equal_quintile[wealth == 'poorer'] - pop_equal_quintile[wealth == 'middle']),
                                                         
                                                         ifelse(wealth == 'richest', prob_cases_equal_quintiles[wealth == 'richer'] *
                                                                  (pop_equal_quintile[wealth == 'poorest'] + pop_equal_quintile[wealth == 'poorer'] + 
                                                                     pop_equal_quintile[wealth == 'middle'] + pop_equal_quintile[wealth == 'richer'] - 
                                                                     pop_SADHS_proportions[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorer'] - 
                                                                     pop_SADHS_proportions[wealth == 'middle'] - pop_SADHS_proportions[wealth == 'richer']) +
                                                                  prob_cases_equal_quintiles[wealth == 'richest'] *
                                                                  (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] + 
                                                                     pop_SADHS_proportions[wealth == 'middle'] + pop_SADHS_proportions[wealth == 'richer'] + pop_SADHS_proportions[wealth == 'richest']  - 
                                                                     pop_equal_quintile[wealth == 'poorest'] - pop_equal_quintile[wealth == 'poorer'] - 
                                                                     pop_equal_quintile[wealth == 'middle'] - pop_equal_quintile[wealth == 'richer']), 1))))))

# check
props %>% group_by(sex, disease) %>% summarise(sum(expected_cases_SADHS))
props %>% group_by(sex, disease) %>% summarise(sum(cases_equal_quintiles)) 
mortality_cases

# finally calculating the probability of cases in population split using SADHS proportions
props$prob_cases_SADHS_quintiles <- props$expected_cases_SADHS/props$pop_SADHS_proportions


##################################################################################
#### Step two (b) allocating the proportions of deaths by wealth quintiles    ####
##################################################################################

# this is repeating the above steps but changing cases for deaths

# calculating the deaths if quintiles were of equal size
props <- props %>% mutate(deaths_equal_quintiles = quintile_proportion * deaths)

# check
mortality_cases
props %>% group_by(sex, disease) %>% summarise(sum(deaths_equal_quintiles))

# add in the population size if the quintiles were of equal size in 2018

props <- props %>% mutate(pop_equal_quintile = population/5)

# create the population in each quintile if we have the same distribution between the quintiles
# as in the SADHS data
props <- props %>% mutate(pop_SADHS_proportions = population * perc_share_pop)

# calculating the probability of deaths if quintiles were equal
props <- props %>% mutate(prob_deaths_equal_quintiles = deaths_equal_quintiles/pop_equal_quintile)

# calculating expected deaths using quintiles that reflect SADHS proportions
# this is more complicated and if you need additional suppport look at the excel spreadsheet referenced above
props <- props %>% mutate(expected_deaths_SADHS = ifelse(wealth == 'poorest', pop_SADHS_proportions * prob_deaths_equal_quintiles, 
                                                        
                                                        ifelse(wealth == 'poorer', prob_deaths_equal_quintiles[wealth == 'poorest'] *         
                                                                 (pop_equal_quintile[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorest']) +
                                                                 prob_deaths_equal_quintiles[wealth == 'poorer'] * 
                                                                 (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] - pop_equal_quintile[wealth == 'poorest']), 
                                                               
                                                               ifelse(wealth == 'middle', prob_deaths_equal_quintiles[wealth == 'poorer'] *
                                                                        (pop_equal_quintile[wealth == 'poorest'] + pop_equal_quintile[wealth == 'poorer'] - 
                                                                           pop_SADHS_proportions[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorer']) +
                                                                        prob_deaths_equal_quintiles[wealth == 'middle'] *
                                                                        (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] + pop_SADHS_proportions[wealth == 'middle'] - 
                                                                           pop_equal_quintile[wealth == 'poorest'] - pop_equal_quintile[wealth == 'poorer']), 
                                                                      
                                                                      ifelse(wealth == 'richer', prob_deaths_equal_quintiles[wealth == 'middle'] *
                                                                               (pop_equal_quintile[wealth == 'poorest'] + pop_equal_quintile[wealth == 'poorer'] + pop_equal_quintile[wealth == 'middle'] - 
                                                                                  pop_SADHS_proportions[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorer'] - pop_SADHS_proportions[wealth == 'middle']) +
                                                                               prob_deaths_equal_quintiles[wealth == 'richer'] *
                                                                               (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] + 
                                                                                  pop_SADHS_proportions[wealth == 'middle'] + pop_SADHS_proportions[wealth == 'richer'] - 
                                                                                  pop_equal_quintile[wealth == 'poorest'] - pop_equal_quintile[wealth == 'poorer'] - pop_equal_quintile[wealth == 'middle']),
                                                                             
                                                                             ifelse(wealth == 'richest', prob_deaths_equal_quintiles[wealth == 'richer'] *
                                                                                      (pop_equal_quintile[wealth == 'poorest'] + pop_equal_quintile[wealth == 'poorer'] + 
                                                                                         pop_equal_quintile[wealth == 'middle'] + pop_equal_quintile[wealth == 'richer'] - 
                                                                                         pop_SADHS_proportions[wealth == 'poorest'] - pop_SADHS_proportions[wealth == 'poorer'] - 
                                                                                         pop_SADHS_proportions[wealth == 'middle'] - pop_SADHS_proportions[wealth == 'richer']) +
                                                                                      prob_deaths_equal_quintiles[wealth == 'richest'] *
                                                                                      (pop_SADHS_proportions[wealth == 'poorest'] + pop_SADHS_proportions[wealth == 'poorer'] + 
                                                                                         pop_SADHS_proportions[wealth == 'middle'] + pop_SADHS_proportions[wealth == 'richer'] + pop_SADHS_proportions[wealth == 'richest']  - 
                                                                                         pop_equal_quintile[wealth == 'poorest'] - pop_equal_quintile[wealth == 'poorer'] - 
                                                                                         pop_equal_quintile[wealth == 'middle'] - pop_equal_quintile[wealth == 'richer']), 1))))))

# check
props %>% group_by(sex, disease) %>% summarise(sum(expected_deaths_SADHS))
props %>% group_by(sex, disease) %>% summarise(sum(deaths_equal_quintiles)) 
mortality_cases

# finally calculating the probability of deaths in population split using SADHS proportions
props$prob_deaths_SADHS_quintiles <- props$expected_deaths_SADHS/props$pop_SADHS_proportions


##################################################################################
#### Step three: Understanding SADHS drinker type proportions                 ####
##################################################################################

consumption_scenarios %>% group_by(wealth, heavy_binge_mod) %>% count(sex)
# just about ok

# create the proportion of the population in each sex, wealth group, drinker type
prop_dktype <- consumption_scenarios %>% 
  group_by(sex, wealth, heavy_binge_mod, .drop = FALSE) %>% summarise(sum(pop_wt)) %>% ungroup()

# rename the variable
prop_dktype <- rename(prop_dktype, population_SADHS = "sum(pop_wt)")

# check that I have not lost anyone along the way, these two should be the same
sum(prop_dktype$population_SADHS)
consumption_scenarios %>% summarise(sum(pop_wt))

# make each group into a proportion of the population seperately for male and female
prop_dktype <- prop_dktype %>% group_by(sex, wealth) %>% mutate(perc_share_dktype = population_SADHS/sum(population_SADHS)) %>% ungroup()

# check that the proportions add up to 1 within each wealth, sex group
prop_dktype %>% group_by(sex, wealth) %>% summarise(sum(perc_share_dktype))

# select only variables needed
prop_dktype <- prop_dktype %>% dplyr::select(-'population_SADHS')



##################################################################################
#### Step four: splitting deaths/cases by drinker type                        ####
##################################################################################

rr_drinker_type <- tidyr::gather(rr_drinker_type, 'disease', 'rr', 4:8)
# rename the disease values
rr_drinker_type$disease[rr_drinker_type$disease == 'rr_ii'] <- 'ii'
rr_drinker_type$disease[rr_drinker_type$disease == 'rr_hiv'] <- 'hiv'
rr_drinker_type$disease[rr_drinker_type$disease == 'rr_road'] <- 'road'
rr_drinker_type$disease[rr_drinker_type$disease == 'rr_liver'] <- 'liver'
rr_drinker_type$disease[rr_drinker_type$disease == 'rr_bcancer'] <- 'bcancer'

# check that the proportions sum to one within each wealth sex disease group
rr_drinker_type %>% group_by(sex, wealth, disease) %>% summarise(sum(rr))

# joining the rr by drinker type onto the props dataframe so the deaths/cases can be 
# distributed further by drinker type
props <- left_join(props, rr_drinker_type, by = c('sex', 'disease', 'wealth'))

# now redistributing the deaths and cases by drinker type
props$deaths_dktype <- props$expected_deaths_SADHS*props$rr
props$cases_dktype <- props$expected_cases_SADHS*props$rr

# check that the total deaths and cases have remained the same
props %>% group_by(disease, sex) %>% summarise(sum(deaths_dktype))
props %>% group_by(disease, sex) %>% summarise(sum(expected_deaths_SADHS/4)) 

##################################################################################
#### Step five: splitting population between drinker types                    ####
##################################################################################

# attach the perc share to the props dataframe
props <- left_join(props, prop_dktype, by = c('sex', 'wealth', 'heavy_binge_mod'))

# calculate the population within each quintile, drinker group
props$pop_SADHS_proportions_dktype <- props$pop_SADHS_proportions * props$perc_share_dktype

# check
props %>% group_by(sex, wealth) %>% summarise(sum(pop_SADHS_proportions_dktype))
props %>% group_by(sex, wealth) %>% summarise(sum(pop_SADHS_proportions/4)) 

##################################################################################
#### Step six: calculating probability and cases by sex/wealth/dk_type        ####
##################################################################################

props$prob_cases_wealth_dktype <- props$cases_dktype / props$pop_SADHS_proportions_dktype
props$prob_deaths_wealth_dktype <- props$deaths_dktype / props$pop_SADHS_proportions_dktype

# checking the range, probabilities should lie between 0 and 1, 
props %>% summarise(min(prob_cases_wealth_dktype), mean(prob_cases_wealth_dktype), max(prob_cases_wealth_dktype))
props %>% summarise(min(prob_deaths_wealth_dktype), mean(prob_deaths_wealth_dktype), max(prob_deaths_wealth_dktype))

# now checking each disease and seeing if it makes sense. It should be that abstainers
# have the lowest risk and heavy drinkers the highest risk
# checking hiv
props %>% filter(disease == 'hiv') %>% group_by(wealth, heavy_binge_mod) %>% 
  summarise(min(prob_cases_wealth_dktype), max(prob_cases_wealth_dktype), 
            min(prob_deaths_wealth_dktype), max(prob_deaths_wealth_dktype))

# checking ii
props %>% filter(disease == 'ii') %>% group_by(wealth, heavy_binge_mod) %>% 
  summarise(min(prob_cases_wealth_dktype), max(prob_cases_wealth_dktype),
            min(prob_deaths_wealth_dktype), max(prob_deaths_wealth_dktype))

# checking road
props %>% filter(disease == 'road') %>% group_by(wealth, heavy_binge_mod) %>% 
  summarise(min(prob_cases_wealth_dktype), max(prob_cases_wealth_dktype),
            min(prob_deaths_wealth_dktype), max(prob_deaths_wealth_dktype))

# checking liver
props %>% filter(disease == 'liver') %>% group_by(wealth, heavy_binge_mod) %>% 
  summarise(min(prob_cases_wealth_dktype), max(prob_cases_wealth_dktype),
            min(prob_deaths_wealth_dktype), max(prob_deaths_wealth_dktype))

# checking bcancer
props %>% filter(disease == 'bcancer') %>% group_by(wealth, heavy_binge_mod) %>% 
  summarise(min(prob_cases_wealth_dktype), max(prob_cases_wealth_dktype),
            min(prob_deaths_wealth_dktype), max(prob_deaths_wealth_dktype))


##################################################################################
#### saving the output I need                                                 ####
##################################################################################

baseline_probabilities <- props %>% dplyr::select('sex', 'wealth', 'heavy_binge_mod', 'disease', 
                                 'prob_deaths_wealth_dktype', 'prob_cases_wealth_dktype')

save(baseline_probabilities, file = "intermediate/baseline_probabilities.Rda")
