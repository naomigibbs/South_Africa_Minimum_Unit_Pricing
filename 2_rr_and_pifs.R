# This script file creates all the relative risks and pifs for the model
# Currently there are five disease/injury conditions

# hiv
# intentional injuries (includes IPV and self harm)
# road injuries
# liver cirrhosis
# breast cancer

##########################
#### Packages         ####
##########################
library(tidyverse)

##########################
#### Data             ####
##########################
load("inputs/consumption_scenarios.Rda")

##########################
#### Data dictionary  ####
##########################
# some of the key variable used in this script file are:

# dk_ever      whether the person has ever drunk alcohol
# dk           baseline number of drinks per year (drink = 15ml of aclcohol)
# dk_R5        number of drinks per year with R5 minimum price
# dk_R10       number of drinks per year with R10 minimum price
# dk_R15       number of drinks per year with R15 minimum price
# dk_peak_base greatest number of drinks drunk at any one time

# rr_ii_base   this gives the relative risk for intentional injury at baseline
# pif_ii_R5    this gives the potential impact fraction for intentional injury at R5 policy, 
#              grouped by sex, drinker type and wealth quintiles 

#######################################################################
#### creating relative risk columns for intentional injuries       ####
#######################################################################

# latest lancet paper gives relative risks for intentional 
# injuries

# https://www.thelancet.com/cms/10.1016/S2468-2667(19)30231-2/attachment/b10678dd-a6db-4de2-aeeb-fc0e4d0e76e8/mmc1.pdf

# they are different for binge drinkers and non-binge drinkers
# so i need to use my peak drinking variable to decide which one to apply

# the relative risk is calculated for grams per day of alcohol at baseline
health_outcomes <- consumption_scenarios %>% 
  mutate(rr_ii_base = ifelse(dk_peak_base >= 5, exp(.00199800266267306*(dk/365*12)+0.647103242058538), exp(.00199800266267306*(dk/365*12))))
# for abstainers the relative risk should be 1 at all levels
health_outcomes$rr_ii_base[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk at R5
health_outcomes <- health_outcomes %>% 
  mutate(rr_ii_R5 = ifelse(dk_peak_R5 >= 5, exp(.00199800266267306*(dk_R5/365*12)+0.647103242058538), exp(.00199800266267306*(dk_R5/365*12))))     
# for abstainers the relative risk should be 1 at all levels
health_outcomes$rr_ii_R5[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk at R10
health_outcomes <- health_outcomes %>% 
  mutate(rr_ii_R10 = ifelse(dk_peak_R10 >= 5, exp(.00199800266267306*(dk_R10/365*12)+0.647103242058538), exp(.00199800266267306*(dk_R10/365*12))))       
health_outcomes$rr_ii_R10[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk at R15
health_outcomes <- health_outcomes %>% 
  mutate(rr_ii_R15 = ifelse(dk_peak_R15 >= 5, exp(.00199800266267306*(dk_R15/365*12)+0.647103242058538), exp(.00199800266267306*(dk_R15/365*12))))  
health_outcomes$rr_ii_R15[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk for the extreme outcome that everyone stops drinking
health_outcomes <- health_outcomes %>% 
  mutate(rr_ii_extreme = 1)

# check that the rr are within the expected range. for non binge drinkers the
# maximum rr should be 1.127362. For binge drinkers the maximum rr should be 2.577458
# and the minimum should be 1.91
health_outcomes %>% group_by(dk_peak_base>=5) %>% summarise(min(rr_ii_base), mean(rr_ii_base), max(rr_ii_base))

health_outcomes %>% group_by(dk_peak_R5>=5) %>% summarise(min(rr_ii_R5), mean(rr_ii_R5), max(rr_ii_R5))

health_outcomes %>% group_by(dk_peak_R10>=5) %>% summarise(min(rr_ii_R10), mean(rr_ii_R10), max(rr_ii_R10))

health_outcomes %>% group_by(dk_peak_R15>=5) %>% summarise(min(rr_ii_R15), mean(rr_ii_R15), max(rr_ii_R15))

health_outcomes %>% summarise(min(rr_ii_extreme), mean(rr_ii_extreme), max(rr_ii_extreme))


#######################################################################
#### creating relative risk columns for road injuries              ####
#######################################################################

# latest lancet paper gives relative risks for road injuries

# https://www.thelancet.com/cms/10.1016/S2468-2667(19)30231-2/attachment/b10678dd-a6db-4de2-aeeb-fc0e4d0e76e8/mmc1.pdf

# they are different for binge drinkers and non-binge drinkers
# so i need to use my peak drinking variable to decide which one to apply

# the relative risk is calculated for grams per day of alcohol, firstly at baseline
health_outcomes <- health_outcomes %>% 
  mutate(rr_road_base = ifelse(dk_peak_base >= 5, exp(0.00299550897979837*(dk/365*12)+0.959350221334602), exp(0.00299550897979837*(dk/365*12))))
# for abstainers the relative risk should be 1 at all levels
health_outcomes$rr_road_base[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk at R5
health_outcomes <- health_outcomes %>% 
  mutate(rr_road_R5 = ifelse(dk_peak_R5 >= 5, exp(0.00299550897979837*(dk_R5/365*12)+0.959350221334602), exp(0.00299550897979837*(dk_R5/365*12))))     
health_outcomes$rr_road_R5[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk at R10
health_outcomes <- health_outcomes %>% 
  mutate(rr_road_R10 = ifelse(dk_peak_R10 >= 5, exp(0.00299550897979837*(dk_R10/365*12)+0.959350221334602), exp(0.00299550897979837*(dk_R10/365*12))))       
health_outcomes$rr_road_R10[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk at R15
health_outcomes <- health_outcomes %>% 
  mutate(rr_road_R15 = ifelse(dk_peak_R15 >= 5, exp(0.00299550897979837*(dk_R15/365*12)+0.959350221334602), exp(0.00299550897979837*(dk_R15/365*12))))  
health_outcomes$rr_road_R15[(health_outcomes$heavy_binge_mod == "Abstainer")] <- 1

# calculating the relative risk for the extreme outcome that everyone stops drinking
health_outcomes <- health_outcomes %>% 
  mutate(rr_road_extreme = 1)


# check that the rr are within the expected range. for non binge drinkers the
# maximum rr should be 1.196895 For binge drinkers the maximum rr should be 4.09
# and the minimum should be 2.61
health_outcomes %>% group_by(dk_peak_base>=5) %>% summarise(min(rr_road_base), mean(rr_road_base), max(rr_road_base))

health_outcomes %>% group_by(dk_peak_R5>=5) %>% summarise(min(rr_road_R5), mean(rr_road_R5), max(rr_road_R5))

health_outcomes %>% group_by(dk_peak_R10>=5) %>% summarise(min(rr_road_R10), mean(rr_road_R10), max(rr_road_R10))

health_outcomes %>% group_by(dk_peak_R15>=5) %>% summarise(min(rr_road_R15), mean(rr_road_R15), max(rr_road_R15))

health_outcomes %>% summarise(min(rr_road_extreme), mean(rr_road_extreme), max(rr_road_extreme))


#######################################################################
#### creating relative risk columns for liver cirrhosis            ####
#######################################################################

# latest lancet paper gives different relative risks for liver cirrhosis
# BUT BEWARE there is a typo for female drinkers, x<= 1 should read x>1
# for the second equation, you can double check it by reading across
# the graphs

# https://www.thelancet.com/cms/10.1016/S2468-2667(19)30231-2/attachment/b10678dd-a6db-4de2-aeeb-fc0e4d0e76e8/mmc1.pdf

# they are different depending on male/female and depending on level of alcohol
# so it is best to create a function
generate_rr_liver <- function(data, drinkinglevel, policy, dk_ever){
  # first of all we have to deal with the fact that we want to quote the policy
  # in the new variable
  policy <- enquo(policy)
  rr_name <- paste0("rr_liver_", quo_name(policy))
  
  mutate(data, 
         !! rr_name := ifelse(drinkinglevel/365*12 <= 1 & sex == "male", 1 + (drinkinglevel/365*12)*exp((1.687111 + 1.106413)*((1+0.1699981689453125)/100)),
                              ifelse(drinkinglevel/365*12 > 1 & sex == "male", exp((1.687111 + 1.106413)*((drinkinglevel/365*12 + 0.1699981689453125)/100)),
                                     ifelse(drinkinglevel/365*12 <= 1 & sex == "female", 1 + (drinkinglevel/365*12)*exp((2.351821 + 0.9002139)*sqrt((1 + 0.1699981689453125)/100)),
                                            ifelse(drinkinglevel/365*12 > 1 & sex == "female", exp((2.351821 + 0.9002139)*sqrt((drinkinglevel/365*12 + 0.1699981689453125)/100)), 1)))))
  
}

# now we are running the function at all different minimum price 
# levels to generate the relative risks
health_outcomes <- generate_rr_liver(health_outcomes, health_outcomes$dk, base)
health_outcomes <- generate_rr_liver(health_outcomes, health_outcomes$dk_R5, R5)
health_outcomes <- generate_rr_liver(health_outcomes, health_outcomes$dk_R10, R10)
health_outcomes <- generate_rr_liver(health_outcomes, health_outcomes$dk_R15, R15)

# adding in the extreme case where everyone stops drinking former drinkers have
# a relative risk of 3.26 for males and females
health_outcomes <- health_outcomes %>% 
  mutate(rr_liver_extreme = ifelse(dk > 0, 3.26, 1))

# allowing for the sick quitter effect. We can get an indication of former drinkers
# using some of the questions in SADHS. The relative risk of former drinkers is 3.26
# it is interesting to note that this will move their relative risk up above many of 
# the moderate drinkers
health_outcomes %>% filter(dk > 0) %>%  group_by(rr_liver_base < 3.26) %>% count(sex)
# looking at how many of the abstainers this would apply to
health_outcomes %>% filter(dk == 0) %>% group_by(dk_ever) %>% count(sex)

# looking at the distribution of rr for drinkers
ggplot(subset(health_outcomes, rr_liver_base > 1), aes(x = rr_liver_base))+
  geom_histogram(binwidth = 1)
# recode all people who ever drunk but who do not now to 3.26 to incorporate
# the sick quitter effect
health_outcomes <- health_outcomes %>% 
  mutate(rr_liver_base = ifelse(dk_ever == "yes" & dk == 0, 3.26, 
                                ifelse(dk_ever == "no", 1, rr_liver_base)))
# check
health_outcomes %>% filter(dk == 0) %>% group_by(dk_ever) %>% summarise(min(rr_liver_base), mean(rr_liver_base), max(rr_liver_base))

# and at R5
health_outcomes <- health_outcomes %>% 
  mutate(rr_liver_R5 = ifelse(dk_ever == "yes" & dk == 0, 3.26, 
                              ifelse(dk_ever == "no", 1, rr_liver_R5)))
# check
health_outcomes %>% filter(dk == 0) %>% group_by(dk_ever) %>% summarise(min(rr_liver_R5), mean(rr_liver_R5), max(rr_liver_R5))

# and at R10
health_outcomes <- health_outcomes %>% 
  mutate(rr_liver_R10 = ifelse(dk_ever == "yes" & dk == 0, 3.26, 
                               ifelse(dk_ever == "no", 1, rr_liver_R10)))
# check
health_outcomes %>% filter(dk == 0) %>% group_by(dk_ever) %>% summarise(min(rr_liver_R10), mean(rr_liver_R10), max(rr_liver_R10))

# and at R15
health_outcomes <- health_outcomes %>% 
  mutate(rr_liver_R15 = ifelse(dk_ever == "yes" & dk == 0, 3.26, 
                               ifelse(dk_ever == "no", 1, rr_liver_R15)))
# check
health_outcomes %>% filter(dk == 0) %>% group_by(dk_ever) %>% summarise(min(rr_liver_R15), mean(rr_liver_R15), max(rr_liver_R15))

# and at extreme
health_outcomes <- health_outcomes %>% 
  mutate(rr_liver_extreme = ifelse(dk_ever == "yes" & dk == 0, 3.26, rr_liver_extreme))

# check
health_outcomes %>% filter(dk == 0) %>% group_by(dk_ever) %>% summarise(min(rr_liver_extreme), mean(rr_liver_extreme), max(rr_liver_extreme))

# The liver cirrhosis relative risk function depends on whether the daily grams of alcohol is less than or equal to
# 1 or above one. it also varies by sex. But it turns out in this sample there are no drinkers who drink less than 
# or equal to 1 gram of alcohol per day.
# the below shows there are no drinkers who have less than or equal to 1 gram of alcohol per day
health_outcomes %>% filter(dk > 0) %>% group_by(sex) %>% summarise(min(dk/365*12), mean(dk/365*12), max(dk/365*12))

# men drinking more than 1:                 min: 1.03     max: 66.35
# women drinking more than 1:               min: 1.42     max: 53.79
# also don't get confused by the former drinkers who have a risk of 3.26

# check that all the min and max are in the expected range
health_outcomes %>% group_by(sex) %>% filter(dk > 1) %>% summarise(min(rr_liver_base), mean(rr_liver_base), max(rr_liver_base))
health_outcomes %>% group_by(sex) %>% filter(dk_R5 > 1) %>% summarise(min(rr_liver_R5), mean(rr_liver_R5), max(rr_liver_R5))
health_outcomes %>% group_by(sex) %>% filter(dk_R10 > 1) %>% summarise(min(rr_liver_R10), mean(rr_liver_R10), max(rr_liver_R10))
health_outcomes %>% group_by(sex) %>% filter(dk_R15 > 1) %>% summarise(min(rr_liver_R15), mean(rr_liver_R15), max(rr_liver_R15))
health_outcomes %>% summarise(min(rr_liver_extreme), mean(rr_liver_extreme), max(rr_liver_extreme))

#######################################################################
#### creating relative risk columns for hiv                        ####
#######################################################################
# file:///C:/Users/cmp16nkg/Downloads/bmjopen-2018-February-8-2--inline-supplementary-material-1%20(1).pdf
# using the work by charlotte probst, not just the lancet article, because she breaks down
# the relative risk by SES

# for instructions on the code used see
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
# and
# https://community.rstudio.com/t/using-mutate-in-a-function-with-the-new-colname-as-an-argument/10399/2

# creating a function for generating the relative risks for baseline, R5, R10 and R15
generate_rr_hiv <- function(data, drinkinglevel, policy){
  # first of all we have to deal with the fact that we want to quote the policy
  # in the new variable
  policy <- enquo(policy)
  rr_name <- paste0("rr_hiv_", quo_name(policy))
  
  mutate(data, 
         !! rr_name := ifelse(wealth >= "middle" & drinkinglevel/365*12 > 61 & sex == "male", 1.54,
                              ifelse(wealth >= "middle" & drinkinglevel/365*12 > 49 & sex == "female", 1.54,
                                     ifelse(wealth < "middle" & drinkinglevel/365*12 > 61 & sex == "male", 2.99,
                                            ifelse(wealth < "middle" & drinkinglevel/365*12 > 49 & sex == "female", 2.99,
                                                   ifelse(wealth < "middle" & drinkinglevel > 0, 1.94, 1))))))
  
}


# now we are running the function at all different minimum price levels to generate the relatie risk
health_outcomes <- generate_rr_hiv(health_outcomes, health_outcomes$dk, base)
health_outcomes <- generate_rr_hiv(health_outcomes, health_outcomes$dk_R5, R5)
health_outcomes <- generate_rr_hiv(health_outcomes, health_outcomes$dk_R10, R10)
health_outcomes <- generate_rr_hiv(health_outcomes, health_outcomes$dk_R15, R15)

# adding in the extreme case where everyone stops drinking
health_outcomes <- health_outcomes %>% 
  mutate(rr_hiv_extreme = 1)

# check that there are some that have changed
health_outcomes %>% filter(rr_hiv_base != rr_hiv_R15)
# check the values look sensible
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_hiv_base)
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_hiv_R5)
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_hiv_R10)
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_hiv_R15)

health_outcomes %>% filter(sex == "male") %>% group_by(wealth) %>% filter(dk > 0) %>% count(rr_hiv_R15)

# giving abstainers the relative risk of 1
health_outcomes$rr_hiv_base <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_hiv_base)
health_outcomes$rr_hiv_R5 <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_hiv_R5)
health_outcomes$rr_hiv_R10 <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_hiv_R10)
health_outcomes$rr_hiv_R15 <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_hiv_R15)

# checking, this one is slightly trckier to check as the threshold is different for men and women
# and the figure is different depending on your wealth quintiles.
health_outcomes %>% filter(sex == "female") %>% group_by(wealth, dk/365*12 > 49) %>% summarise(min(rr_hiv_base), mean(rr_hiv_base), max(rr_hiv_base))
health_outcomes %>% filter(sex == "male") %>% group_by(wealth, dk/365*12 > 61) %>% summarise(min(rr_hiv_base), mean(rr_hiv_base), max(rr_hiv_base))

health_outcomes %>% filter(sex == "female") %>% group_by(wealth, dk_R5/365*12 > 49) %>% summarise(min(rr_hiv_R5), mean(rr_hiv_R5), max(rr_hiv_R5))
health_outcomes %>% filter(sex == "male") %>% group_by(wealth, dk_R5/365*12 > 61) %>% summarise(min(rr_hiv_R5), mean(rr_hiv_R5), max(rr_hiv_R5))

health_outcomes %>% filter(sex == "female") %>% group_by(wealth, dk_R10/365*12 > 49) %>% summarise(min(rr_hiv_R10), mean(rr_hiv_R10), max(rr_hiv_R10))
health_outcomes %>% filter(sex == "male") %>% group_by(wealth, dk_R10/365*12 > 61) %>% summarise(min(rr_hiv_R10), mean(rr_hiv_R10), max(rr_hiv_R10))

health_outcomes %>% filter(sex == "female") %>% group_by(wealth, dk_R15/365*12 > 49) %>% summarise(min(rr_hiv_R15), mean(rr_hiv_R15), max(rr_hiv_R15))
health_outcomes %>% filter(sex == "male") %>% group_by(wealth, dk_R15/365*12 > 61) %>% summarise(min(rr_hiv_R15), mean(rr_hiv_R15), max(rr_hiv_R15))

health_outcomes %>% filter(sex == "female") %>% summarise(min(rr_hiv_extreme), mean(rr_hiv_extreme), max(rr_hiv_extreme))
health_outcomes %>% filter(sex == "male") %>% summarise(min(rr_hiv_extreme), mean(rr_hiv_extreme), max(rr_hiv_extreme))

# check visually
ggplot(data = health_outcomes, aes(x = rr_hiv_base, fill = wealth)) +
  geom_histogram()+
  facet_grid(~sex)

#######################################################################
#### creating relative risk columns for breast cancer              ####
#######################################################################

# https://www.thelancet.com/cms/10.1016/S2468-2667(19)30231-2/attachment/b10678dd-a6db-4de2-aeeb-fc0e4d0e76e8/mmc1.pdf
# I have sense checked the formula by running them with some example consumptions
# and checking them against the relative risk plots in the lancet appendix
# remember the alcohol attributable part only applies to females

# creating a function for generating the relative risks for baseline, R5, R10 and R15
generate_rr_bcancer <- function(data, drinkinglevel, policy){
  # first of all we have to deal with the fact that we want to quote the policy
  # in the new variable
  policy <- enquo(policy)
  rr_name <- paste0("rr_bcancer_", quo_name(policy))
  
  mutate(data, 
         !! rr_name := ifelse(sex == "female", exp((drinkinglevel/365*12)*0.01018), 1))
                              
}


# now we are running the function at all different minimum price levels
health_outcomes <- generate_rr_bcancer(health_outcomes, health_outcomes$dk, base)
health_outcomes <- generate_rr_bcancer(health_outcomes, health_outcomes$dk_R5, R5)
health_outcomes <- generate_rr_bcancer(health_outcomes, health_outcomes$dk_R10, R10)
health_outcomes <- generate_rr_bcancer(health_outcomes, health_outcomes$dk_R15, R15)

# adding in the extreme case where everyone stops drinking
health_outcomes <- health_outcomes %>% 
  mutate(rr_bcancer_extreme = 1)

# making all abstainers relative risk for breast cancer equal to 1
health_outcomes$rr_bcancer_base <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_bcancer_base)
health_outcomes$rr_bcancer_R5 <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_bcancer_R5)
health_outcomes$rr_bcancer_R10 <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_bcancer_R10)
health_outcomes$rr_bcancer_R15 <- ifelse(health_outcomes$heavy_binge_mod == "Abstainer", 1, health_outcomes$rr_bcancer_R15)

# check that there are some that have changed
health_outcomes %>% filter(rr_bcancer_base != rr_bcancer_R15)
# check the values look sensible
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_bcancer_base)
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_bcancer_R5)
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_bcancer_R10)
health_outcomes %>% group_by(sex, wealth) %>% filter(dk > 0) %>% count(rr_bcancer_R15)

# check that the maximum and minimum value look correct. For drinkers the minimum would be 1 and the maximum
# would be 4.604
health_outcomes %>% group_by(dk>0) %>% summarise(min(rr_bcancer_base), mean(rr_bcancer_base), max(rr_bcancer_base))
health_outcomes %>% group_by(dk>0) %>% summarise(min(rr_bcancer_R5), mean(rr_bcancer_R5), max(rr_bcancer_R5))
health_outcomes %>% group_by(dk>0) %>% summarise(min(rr_bcancer_R10), mean(rr_bcancer_R10), max(rr_bcancer_R10))
health_outcomes %>% group_by(dk>0) %>% summarise(min(rr_bcancer_R15), mean(rr_bcancer_R15), max(rr_bcancer_R15))

# check visually
health_outcomes %>% filter(sex == "female") %>% group_by(wealth) %>% filter(dk > 0) %>% count(rr_bcancer_R15)
ggplot(data = health_outcomes, aes(x = rr_bcancer_base)) +
  geom_histogram()+
  facet_grid(~sex)

#######################################################################
#### calculating mean relative risks                               ########
#######################################################################

# getting relative risks by drinker type, sex and wealth
# first of all checking sufficient sample size
table(health_outcomes$heavy_binge_mod, health_outcomes$wealth, health_outcomes$sex)

# summing up all of the risk for each of the drinker type, sex and wealth groups
rr_drinker_type <- health_outcomes %>% group_by(heavy_binge_mod, wealth, sex) %>% summarise(sum(rr_ii_base * pop_wt), 
                                                                                            sum(rr_hiv_base * pop_wt), 
                                                                                            sum(rr_road_base * pop_wt),
                                                                                            sum(rr_liver_base * pop_wt), 
                                                                                            sum(rr_bcancer_base  * pop_wt))

# renaming the variable or the below transformation will not work
rr_drinker_type <- rename(rr_drinker_type, rr_ii = "sum(rr_ii_base * pop_wt)")
rr_drinker_type <- rename(rr_drinker_type, rr_hiv = "sum(rr_hiv_base * pop_wt)")
rr_drinker_type <- rename(rr_drinker_type, rr_road = "sum(rr_road_base * pop_wt)")
rr_drinker_type <- rename(rr_drinker_type, rr_bcancer = "sum(rr_bcancer_base * pop_wt)")
rr_drinker_type <- rename(rr_drinker_type, rr_liver = "sum(rr_liver_base * pop_wt)")

# transforming these to proportions of 1 within each wealth quintile, sex group
# as later we will allocate the baseline morbidity and mortality, 
# which will already have been split by wealth into drinker groups
rr_drinker_type <- rr_drinker_type %>% group_by(sex, wealth) %>% 
  mutate(rr_ii = rr_ii/sum(rr_ii)) %>% 
  mutate(rr_hiv = rr_hiv/sum(rr_hiv)) %>% 
  mutate(rr_road = rr_road/sum(rr_road)) %>% 
  mutate(rr_bcancer = rr_bcancer/sum(rr_bcancer)) %>% 
  mutate(rr_liver = rr_liver/sum(rr_liver))

# check
rr_drinker_type %>% group_by(sex, wealth) %>% summarise(sum(rr_ii), sum(rr_hiv), sum(rr_road), sum(rr_bcancer), sum(rr_liver))

#######################################################################
####   creating potential impact fractions for intentional injury  ####
#######################################################################
# pifs are created by wealth and drinker group as the policy relevent subgroups 

# for a R5 minimum price
pif_ii <- health_outcomes %>% group_by(sex, wealth, heavy_binge_mod) %>% 
  mutate(pif_ii_R5 = sum(rr_ii_R5 * pop_wt)/sum(rr_ii_base * pop_wt)) %>% 
  ungroup()
         
# check
a <- pif_ii %>% 
  select(pif_ii_R5, wealth, sex, heavy_binge_mod) %>% 
  distinct()
a

# for a R10 minimum price
pif_ii  <- pif_ii %>% group_by(sex, wealth, heavy_binge_mod) %>% 
  mutate(pif_ii_R10 = sum(rr_ii_R10 * pop_wt)/sum(rr_ii_base * pop_wt)) %>% 
  ungroup()

# for a R15 minimum price
pif_ii <- pif_ii %>% group_by(sex, wealth, heavy_binge_mod) %>% 
  mutate(pif_ii_R15 = sum(rr_ii_R15 * pop_wt)/sum(rr_ii_base * pop_wt)) %>% 
  ungroup()

# for an extreme scenario where noone is drinking at all
pif_ii <- pif_ii %>% group_by(sex, wealth, heavy_binge_mod) %>% 
  mutate(pif_ii_extreme = sum(rr_ii_extreme * pop_wt)/sum(rr_ii_base * pop_wt)) %>% 
  ungroup()

# creating the pif for baseline
pif_ii$pif_ii <- 1

# selecting the variables I want and combining them into one dataframe
pif_ii <- pif_ii %>% select('heavy_binge_mod', 'sex', 'wealth', 'pif_ii', 'pif_ii_R5', 'pif_ii_R10', 'pif_ii_R15', 'pif_ii_extreme') %>% 
  distinct()
 
#######################################################################
####   creating potential impact fractions for road injury         ####
#######################################################################
 # for a R5 minimum price
 pif_road <- health_outcomes %>% group_by(sex, wealth, heavy_binge_mod) %>% 
   mutate(pif_road_R5 = sum(rr_road_R5 * pop_wt)/sum(rr_road_base * pop_wt)) %>% 
   ungroup()
 
 # check
 a <- pif_road %>% 
   select(pif_road_R5, wealth, sex, heavy_binge_mod) %>% 
   distinct()
 a

 # for a R10 minimum price
 pif_road  <- pif_road %>% group_by(sex, wealth, heavy_binge_mod) %>% 
   mutate(pif_road_R10 = sum(rr_road_R10 * pop_wt)/sum(rr_road_base * pop_wt)) %>% 
   ungroup()
 
 # for a R15 minimum price
 pif_road <- pif_road %>% group_by(sex, wealth, heavy_binge_mod) %>% 
   mutate(pif_road_R15 = sum(rr_road_R15 * pop_wt)/sum(rr_road_base * pop_wt)) %>% 
   ungroup()
 
 # for an extreme scenario where noone is drinking at all
 pif_road <- pif_road %>% group_by(sex, wealth, heavy_binge_mod) %>% 
   mutate(pif_road_extreme = sum(rr_road_extreme * pop_wt)/sum(rr_road_base * pop_wt)) %>% 
   ungroup()
 
 # creating the road for baseline
 pif_road$pif_road <- 1
 
 # selecting the variables I want and combining them into one dataframe
 pif_road <- pif_road %>% select('heavy_binge_mod', 'sex', 'wealth', 'pif_road', 'pif_road_R5', 'pif_road_R10', 'pif_road_R15', 'pif_road_extreme') %>% 
   distinct()

 
#######################################################################
####   creating potential impact fractions for hiv                 ####
#######################################################################
 # for a R5 minimum price
 pif_hiv <- health_outcomes %>% group_by(sex, wealth, heavy_binge_mod) %>% 
   mutate(pif_hiv_R5 = sum(rr_hiv_R5 * pop_wt)/sum(rr_hiv_base * pop_wt)) %>% 
   ungroup()
 
 # check
 a <- pif_hiv %>% 
   select(pif_hiv_R5, wealth, sex, heavy_binge_mod) %>% 
   distinct()
 a
 
 # for a R10 minimum price
 pif_hiv  <- pif_hiv %>% group_by(heavy_binge_mod, sex, wealth) %>% 
   mutate(pif_hiv_R10 = sum(rr_hiv_R10 * pop_wt)/sum(rr_hiv_base * pop_wt)) %>% 
   ungroup()
 
 # for a R15 minimum price
 pif_hiv <- pif_hiv %>% group_by(heavy_binge_mod, sex, wealth) %>% 
   mutate(pif_hiv_R15 = sum(rr_hiv_R15 * pop_wt)/sum(rr_hiv_base * pop_wt)) %>% 
   ungroup()
 
 # for an extreme scenario where everyone stops drinking
 pif_hiv <- pif_hiv %>% group_by(heavy_binge_mod, sex, wealth) %>% 
   mutate(pif_hiv_extreme = sum(rr_hiv_extreme * pop_wt)/sum(rr_hiv_base * pop_wt)) %>% 
   ungroup()
 
 # creating the hiv for baseline
 pif_hiv$pif_hiv <- 1
 
 # selecting the variables I want and combining them into one dataframe
 pif_hiv <- pif_hiv %>% select('heavy_binge_mod', 'sex', 'wealth', 'pif_hiv', 'pif_hiv_R5', 'pif_hiv_R10', 'pif_hiv_R15', 'pif_hiv_extreme') %>% 
   distinct()


#######################################################################
####   creating potential impact fractions for liver cirrhosis     ####
#######################################################################
# for a R5 minimum price
pif_liver <- health_outcomes %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_liver_R5 = sum(rr_liver_R5 * pop_wt)/sum(rr_liver_base * pop_wt)) %>% 
  ungroup()

# check
a <- pif_liver %>% 
  select(heavy_binge_mod, pif_liver_R5, wealth, sex) %>% 
  distinct()
a

# for a R10 minimum price
pif_liver  <- pif_liver %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_liver_R10 = sum(rr_liver_R10 * pop_wt)/sum(rr_liver_base * pop_wt)) %>% 
  ungroup()

# for a R15 minimum price
pif_liver <- pif_liver %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_liver_R15 = sum(rr_liver_R15 * pop_wt)/sum(rr_liver_base * pop_wt)) %>% 
  ungroup()

# for an extreme scenario where everyone stops drinking
pif_liver <- pif_liver %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_liver_extreme = sum(rr_liver_extreme * pop_wt)/sum(rr_liver_base * pop_wt)) %>% 
  ungroup()

# creating the pif for baseline
pif_liver$pif_liver <- 1

# selecting the variables I want and combining them into one dataframe
pif_liver <- pif_liver %>% select('heavy_binge_mod', 'sex', 'wealth', 'pif_liver', 'pif_liver_R5', 'pif_liver_R10', 'pif_liver_R15', 'pif_liver_extreme') %>% 
  distinct()



#######################################################################
####   creating potential impact fractions for breast cancer       ####
#######################################################################
# for a R5 minimum price
pif_bcancer <- health_outcomes %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_bcancer_R5 = sum(rr_bcancer_R5 * pop_wt)/sum(rr_bcancer_base * pop_wt)) %>% 
  ungroup()

# check
a <- pif_bcancer %>% 
  select(heavy_binge_mod, pif_bcancer_R5, wealth, sex) %>% 
  distinct()
a

# for a R10 minimum price
pif_bcancer  <- pif_bcancer %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_bcancer_R10 = sum(rr_bcancer_R10 * pop_wt)/sum(rr_bcancer_base * pop_wt)) %>% 
  ungroup()

# for a R15 minimum price
pif_bcancer <- pif_bcancer %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_bcancer_R15 = sum(rr_bcancer_R15 * pop_wt)/sum(rr_bcancer_base * pop_wt)) %>% 
  ungroup()

# for an extreme scenario where everyone stops drinking
pif_bcancer <- pif_bcancer %>% group_by(heavy_binge_mod, sex, wealth) %>% 
  mutate(pif_bcancer_extreme = sum(rr_bcancer_extreme * pop_wt)/sum(rr_bcancer_base * pop_wt)) %>% 
  ungroup()

# creating the pif for baseline
pif_bcancer$pif_bcancer <- 1

# selecting the variables I want and combining them into one dataframe
pif_bcancer <- pif_bcancer %>% select('heavy_binge_mod', 'sex', 'wealth', 'pif_bcancer', 'pif_bcancer_R5', 'pif_bcancer_R10', 'pif_bcancer_R15', 'pif_bcancer_extreme') %>% 
  distinct()


#######################################################################
#### saving all these pifs and health outcomes
#######################################################################

save(pif_ii, file = "intermediate/pif_ii.Rda")
save(pif_road, file = "intermediate/pif_road.Rda")
save(pif_hiv, file = "intermediate/pif_hiv.Rda")
save(pif_liver, file = "intermediate/pif_liver.Rda")
save(pif_bcancer, file = "intermediate/pif_bcancer.Rda")

save(rr_drinker_type, file = "intermediate/rr_drinker_type.Rda")


