# This is where the life tables are generated under each policy scenario and calculate 
# deaths form each of the five conditions

# Prevelance is also calculated under each scenario

##########################
#### Packages         ####
##########################
library(tidyverse)

##########################
#### Data             ####
##########################
load("intermediate/female_population.Rda")
load("intermediate/male_population.Rda")


##################################################################################
#### creating a function to project population for 20 years                   
##################################################################################

# base case projection

run_projection <-
  function(data, prob_death_non_alc, pif_ii, pif_hiv, pif_road, pif_liver, pif_bcancer, group_var1, group_var2) {
   prob_death_non_alc <- enquo(prob_death_non_alc)
   pif_ii <- enquo(pif_ii)
   pif_hiv <- enquo(pif_hiv)
   pif_road <- enquo(pif_road)
   pif_liver <- enquo(pif_liver)
   pif_bcancer <- enquo(pif_bcancer)
   group_var1 <- enquo(group_var1)
   group_var2 <- enquo(group_var2)

     data %>% 
      arrange(age) %>% 
      group_by((!! group_var1), (!! group_var2)) %>%
     
       # time period 1 (theres got to be a better way to do this!)  
      mutate(deaths_ii_t1 =  lag(pop_t0 * prob_death_ii * (!! pif_ii))) %>% 
      mutate(deaths_hiv_t1 =  lag(pop_t0 * prob_death_hiv * (!! pif_hiv))) %>% 
      mutate(deaths_road_t1 =  lag(pop_t0 * prob_death_road * (!! pif_road))) %>% 
      mutate(deaths_liver_t1 = lag(pop_t0 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.21)))) %>% 
      mutate(deaths_bcancer_t1 = lag(pop_t0 * prob_death_bcancer)) %>% 
      mutate(deaths_non_alc_t1 =  lag(pop_t0 * (!! prob_death_non_alc))) %>%  
      mutate(pop_t1 =  if_else(age >= 16, lag(pop_t0) - deaths_non_alc_t1 - deaths_ii_t1 - deaths_hiv_t1 - deaths_road_t1 
                               - deaths_liver_t1 - deaths_bcancer_t1,
        if_else(age >= 1 & age < 16, lag(pop_t0) * (1 - prob_death), lag(pop_t0)))) %>% 
       
     # time period 2
       mutate(deaths_ii_t2 =  lag(pop_t1 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t2 =  lag(pop_t1 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t2 =  lag(pop_t1 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t2 = lag(pop_t1 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.34)))) %>% 
       mutate(deaths_bcancer_t2 = lag(pop_t1 * prob_death_bcancer )) %>% 
       mutate(deaths_non_alc_t2 =  lag(pop_t1 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t2 =  if_else(age >= 16, lag(pop_t1) - deaths_non_alc_t2 - deaths_ii_t2 - deaths_hiv_t2 - deaths_road_t2
                                - deaths_liver_t2 - deaths_bcancer_t2,
                                if_else(age >= 1 & age < 16, lag(pop_t1) * (1 - prob_death), lag(pop_t1)))) %>% 
       
     # time period 3
       mutate(deaths_ii_t3 =  lag(pop_t2 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t3 =  lag(pop_t2 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t3 =  lag(pop_t2 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t3 = lag(pop_t2 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.43)))) %>% 
       mutate(deaths_bcancer_t3 = lag(pop_t2 * prob_death_bcancer )) %>% 
       mutate(deaths_non_alc_t3 =  lag(pop_t2 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t3 =  if_else(age >= 16, lag(pop_t2) - deaths_non_alc_t3 - deaths_ii_t3 - deaths_hiv_t3 - deaths_road_t3
                                - deaths_liver_t3 - deaths_bcancer_t3,
                                if_else(age >= 1 & age < 16, lag(pop_t2) * (1 - prob_death), lag(pop_t2)))) %>% 
       
     # time period 4
       mutate(deaths_ii_t4 =  lag(pop_t3 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t4 =  lag(pop_t3 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t4 =  lag(pop_t3 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t4 = lag(pop_t3 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.50)))) %>% 
       mutate(deaths_bcancer_t4 = lag(pop_t3 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t4 =  lag(pop_t3 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t4 =  if_else(age >= 16, lag(pop_t3) - deaths_non_alc_t4 - deaths_ii_t4 - deaths_hiv_t4 - deaths_road_t4
                                - deaths_liver_t4 - deaths_bcancer_t4,
                                if_else(age >= 1 & age < 16, lag(pop_t3) * (1 - prob_death), lag(pop_t3)))) %>% 
       
      # time period 5
       mutate(deaths_ii_t5 =  lag(pop_t4 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t5 =  lag(pop_t4 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t5 =  lag(pop_t4 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t5 = lag(pop_t4 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.56)))) %>% 
       mutate(deaths_bcancer_t5 = lag(pop_t4 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t5 =  lag(pop_t4 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t5 =  if_else(age >= 16, lag(pop_t4) - deaths_non_alc_t5 - deaths_ii_t5 - deaths_hiv_t5 - deaths_road_t5
                                - deaths_liver_t5 - deaths_bcancer_t5,
                                if_else(age >= 1 & age < 16, lag(pop_t4) * (1 - prob_death), lag(pop_t4)))) %>% 
       
       # time period 6
       mutate(deaths_ii_t6 =  lag(pop_t5 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t6 =  lag(pop_t5 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t6 =  lag(pop_t5 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t6 = lag(pop_t5 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.61)))) %>% 
       mutate(deaths_bcancer_t6 = lag(pop_t5 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t6 =  lag(pop_t5 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t6 =  if_else(age >= 16, lag(pop_t5) - deaths_non_alc_t6 - deaths_ii_t6 - deaths_hiv_t6 - deaths_road_t6
                                - deaths_liver_t6 - deaths_bcancer_t6,
                                if_else(age >= 1 & age < 16, lag(pop_t5) * (1 - prob_death), lag(pop_t5)))) %>% 
       
       # time period 7
       mutate(deaths_ii_t7 =  lag(pop_t6 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t7 =  lag(pop_t6 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t7 =  lag(pop_t6 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t7 = lag(pop_t6 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * prob_death_ii * 0.65)))) %>% 
       mutate(deaths_bcancer_t7 = lag(pop_t6 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t7 =  lag(pop_t6 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t7 =  if_else(age >= 16, lag(pop_t6) - deaths_non_alc_t7 - deaths_ii_t7 - deaths_hiv_t7 - deaths_road_t7
                                - deaths_liver_t7 - deaths_bcancer_t7,
                                if_else(age >= 1 & age < 16, lag(pop_t6) * (1 - prob_death), lag(pop_t6)))) %>% 
       
       # time period 8
       mutate(deaths_ii_t8 =  lag(pop_t7 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t8 =  lag(pop_t7 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t8 =  lag(pop_t7 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t8 = lag(pop_t7 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.69)))) %>% 
       mutate(deaths_bcancer_t8 = lag(pop_t7 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t8 =  lag(pop_t7 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t8 =  if_else(age >= 16, lag(pop_t7) - deaths_non_alc_t8 - deaths_ii_t8 - deaths_hiv_t8 - deaths_road_t8
                                - deaths_liver_t8 - deaths_bcancer_t8,
                                if_else(age >= 1 & age < 16, lag(pop_t7) * (1 - prob_death), lag(pop_t7)))) %>% 
       
       
       # time period 9
       mutate(deaths_ii_t9 =  lag(pop_t8 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t9 =  lag(pop_t8 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t9 =  lag(pop_t8 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t9 = lag(pop_t8 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.73)))) %>% 
       mutate(deaths_bcancer_t9 = lag(pop_t8 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t9 =  lag(pop_t8 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t9 =  if_else(age >= 16, lag(pop_t8) - deaths_non_alc_t9 - deaths_ii_t9 - deaths_hiv_t9 - deaths_road_t9
                                - deaths_liver_t9 - deaths_bcancer_t9,
                                if_else(age >= 1 & age < 16, lag(pop_t8) * (1 - prob_death), lag(pop_t8)))) %>%    
       
       # time period 10
       mutate(deaths_ii_t10 =  lag(pop_t9 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t10 =  lag(pop_t9 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t10 =  lag(pop_t9 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t10 = lag(pop_t9 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.76)))) %>% 
       mutate(deaths_bcancer_t10 = lag(pop_t9 * prob_death_bcancer )) %>%
       mutate(deaths_non_alc_t10 =  lag(pop_t9 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t10 =  if_else(age >= 16, lag(pop_t9) - deaths_non_alc_t10 - deaths_ii_t10 - deaths_hiv_t10 - deaths_road_t10
                                 - deaths_liver_t10 - deaths_bcancer_t10,
                                if_else(age >= 1 & age < 16, lag(pop_t9) * (1 - prob_death), lag(pop_t9)))) %>%    
       
       
       # time period 11
       mutate(deaths_ii_t11 =  lag(pop_t10 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t11 =  lag(pop_t10 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t11 =  lag(pop_t10 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t11 = lag(pop_t10 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.79)))) %>% 
       mutate(deaths_bcancer_t11 = lag(pop_t10 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.1)))) %>%
       mutate(deaths_non_alc_t11 =  lag(pop_t10 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t11 =  if_else(age >= 16, lag(pop_t10) - deaths_non_alc_t11 - deaths_ii_t11 - deaths_hiv_t11 - deaths_road_t11
                                 - deaths_liver_t11 - deaths_bcancer_t11,
                                 if_else(age >= 1 & age < 16, lag(pop_t10) * (1 - prob_death), lag(pop_t10)))) %>%   
       
       # time period 12
       mutate(deaths_ii_t12 =  lag(pop_t11 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t12 =  lag(pop_t11 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t12 =  lag(pop_t11 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t12 = lag(pop_t11 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.82)))) %>% 
       mutate(deaths_bcancer_t12 = lag(pop_t11 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.2)))) %>%
       mutate(deaths_non_alc_t12 =  lag(pop_t11 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t12 =  if_else(age >= 16, lag(pop_t11) - deaths_non_alc_t12 - deaths_ii_t12 - deaths_hiv_t12 - deaths_road_t12
                                 - deaths_liver_t12 - deaths_bcancer_t12,
                                 if_else(age >= 1 & age < 16, lag(pop_t11) * (1 - prob_death), lag(pop_t11)))) %>%
       
       # time period 13
       mutate(deaths_ii_t13 =  lag(pop_t12 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t13 =  lag(pop_t12 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t13 =  lag(pop_t12 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t13 = lag(pop_t12 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.85)))) %>% 
       mutate(deaths_bcancer_t13 = lag(pop_t12 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.3)))) %>%
       mutate(deaths_non_alc_t13 =  lag(pop_t12 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t13 =  if_else(age >= 16, lag(pop_t12) - deaths_non_alc_t13 - deaths_ii_t13 - deaths_hiv_t13 - deaths_road_t13
                                 - deaths_liver_t13 - deaths_bcancer_t13,
                                 if_else(age >= 1 & age < 16, lag(pop_t12) * (1 - prob_death), lag(pop_t12)))) %>%
       
       # time period 14
       mutate(deaths_ii_t14 =  lag(pop_t13 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t14 =  lag(pop_t13 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t14 =  lag(pop_t13 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t14 = lag(pop_t13 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.88)))) %>% 
       mutate(deaths_bcancer_t14 = lag(pop_t13 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.4)))) %>%
       mutate(deaths_non_alc_t14 =  lag(pop_t13 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t14 =  if_else(age >= 16, lag(pop_t13) - deaths_non_alc_t14 - deaths_ii_t14 - deaths_hiv_t14 - deaths_road_t14
                                 - deaths_liver_t14 - deaths_bcancer_t14,
                                 if_else(age >= 1 & age < 16, lag(pop_t13) * (1 - prob_death), lag(pop_t13)))) %>%
       
       # time period 15
       mutate(deaths_ii_t15 =  lag(pop_t14 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t15 =  lag(pop_t14 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t15 =  lag(pop_t14 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t15 = lag(pop_t14 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.90)))) %>% 
       mutate(deaths_bcancer_t15 = lag(pop_t14 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.5)))) %>%
       mutate(deaths_non_alc_t15 =  lag(pop_t14 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t15 =  if_else(age >= 16, lag(pop_t14) - deaths_non_alc_t15 - deaths_ii_t15 - deaths_hiv_t15 - deaths_road_t15
                                 - deaths_liver_t15 - deaths_bcancer_t15,
                                 if_else(age >= 1 & age < 16, lag(pop_t14) * (1 - prob_death), lag(pop_t14)))) %>%
       
       # time period 16
       mutate(deaths_ii_t16 =  lag(pop_t15 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t16 =  lag(pop_t15 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t16 =  lag(pop_t15 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t16 = lag(pop_t15 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.92)))) %>% 
       mutate(deaths_bcancer_t16 = lag(pop_t15 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.6)))) %>%
       mutate(deaths_non_alc_t16 =  lag(pop_t15 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t16 =  if_else(age >= 16, lag(pop_t15) - deaths_non_alc_t16 - deaths_ii_t16 - deaths_hiv_t16 - deaths_road_t16
                                 - deaths_liver_t16 - deaths_bcancer_t16,
                                 if_else(age >= 1 & age < 16, lag(pop_t15) * (1 - prob_death), lag(pop_t15)))) %>%
       
       # time period 17
       mutate(deaths_ii_t17 =  lag(pop_t16 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t17 =  lag(pop_t16 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t17 =  lag(pop_t16 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t17 = lag(pop_t16 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.94)))) %>% 
       mutate(deaths_bcancer_t17 = lag(pop_t16 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.7)))) %>%
       mutate(deaths_non_alc_t17 =  lag(pop_t16 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t17 =  if_else(age >= 16, lag(pop_t16) - deaths_non_alc_t17 - deaths_ii_t17 - deaths_hiv_t17 - deaths_road_t17
                                 - deaths_liver_t17 - deaths_bcancer_t17,
                                 if_else(age >= 1 & age < 16, lag(pop_t16) * (1 - prob_death), lag(pop_t16)))) %>%
       
       # time period 18
       mutate(deaths_ii_t18 =  lag(pop_t17 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t18 =  lag(pop_t17 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t18 =  lag(pop_t17 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t18 = lag(pop_t17 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.96)))) %>% 
       mutate(deaths_bcancer_t18 = lag(pop_t17 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.8)))) %>%
       mutate(deaths_non_alc_t18 =  lag(pop_t17 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t18 =  if_else(age >= 16, lag(pop_t17) - deaths_non_alc_t18 - deaths_ii_t18 - deaths_hiv_t18 - deaths_road_t18
                                 - deaths_liver_t18 - deaths_bcancer_t18,
                                 if_else(age >= 1 & age < 16, lag(pop_t17) * (1 - prob_death), lag(pop_t17)))) %>%
       
       # time period 19
       mutate(deaths_ii_t19 =  lag(pop_t18 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t19 =  lag(pop_t18 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t19 =  lag(pop_t18 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t19 = lag(pop_t18 * prob_death_liver * (1 - ((1 - ( !! pif_liver)) * 0.98)))) %>% 
       mutate(deaths_bcancer_t19 = lag(pop_t18 * prob_death_bcancer * (1 - ((1 - ( !! pif_bcancer)) * 0.9)))) %>%
       mutate(deaths_non_alc_t19 =  lag(pop_t18 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t19 =  if_else(age >= 16, lag(pop_t18) - deaths_non_alc_t19 - deaths_ii_t19 - deaths_hiv_t19 - deaths_road_t19
                                 - deaths_liver_t19 - deaths_bcancer_t19,
                                 if_else(age >= 1 & age < 16, lag(pop_t18) * (1 - prob_death), lag(pop_t18)))) %>%
       
       # time period 20
       mutate(deaths_ii_t20 =  lag(pop_t19 * prob_death_ii * (!! pif_ii))) %>% 
       mutate(deaths_hiv_t20 =  lag(pop_t19 * prob_death_hiv * (!! pif_hiv))) %>% 
       mutate(deaths_road_t20 =  lag(pop_t19 * prob_death_road * (!! pif_road))) %>%
       mutate(deaths_liver_t20 = lag(pop_t19 * prob_death_liver * ( !! pif_liver))) %>% 
       mutate(deaths_bcancer_t20 = lag(pop_t19 * prob_death_bcancer * ( !! pif_bcancer))) %>%
       mutate(deaths_non_alc_t20 =  lag(pop_t19 * (!! prob_death_non_alc))) %>%  
       mutate(pop_t20 =  if_else(age >= 16, lag(pop_t19) - deaths_non_alc_t20 - deaths_ii_t20 - deaths_hiv_t20 - deaths_road_t20
                                 - deaths_liver_t20 - deaths_bcancer_t20,
                                 if_else(age >= 1 & age < 16, lag(pop_t19) * (1 - prob_death), lag(pop_t19)))) %>%
  
      ungroup() %>% 
      mutate_at(vars(matches("pop_t"), matches("deaths_")), round, 2) %>%
      filter(age >= 15) %>%
      select(matches("pop_t"), matches("deaths_"), 'age', 'wealth', 'heavy_binge_mod')
    
  }  

##################################################################################
#### generating populations at various policy levels                          
##################################################################################

# females
results_base_f <- run_projection(female_population, prob_death_non_alc, pif_ii, pif_hiv, pif_road, pif_liver, pif_bcancer, wealth, heavy_binge_mod)
results_R5_f <- run_projection(female_population, prob_death_non_alc, pif_ii_R5, pif_hiv_R5, pif_road_R5, pif_liver_R5, pif_bcancer_R5, wealth, heavy_binge_mod)
results_R10_f <- run_projection(female_population, prob_death_non_alc, pif_ii_R10, pif_hiv_R10, pif_road_R10, pif_liver_R10, pif_bcancer_R10, wealth, heavy_binge_mod)
results_R15_f <- run_projection(female_population, prob_death_non_alc, pif_ii_R15, pif_hiv_R15, pif_road_R15, pif_liver_R15, pif_bcancer_R15, wealth, heavy_binge_mod)
results_extreme_f <- run_projection(female_population, prob_death_non_alc, pif_ii_extreme, pif_hiv_extreme, pif_road_extreme, pif_liver_extreme, pif_bcancer_extreme, wealth, heavy_binge_mod)

# sensecheck the results tables for baseline with the mortality from the health data inputs.

results_base_f %>% summarise(sum(deaths_ii_t1), sum(deaths_hiv_t1), sum(deaths_road_t1), sum(deaths_bcancer_t1), sum(deaths_liver_t1)) 
results_extreme_f %>% summarise(sum(deaths_ii_t1), sum(deaths_hiv_t1), sum(deaths_road_t1), sum(deaths_bcancer_t1), sum(deaths_liver_t1)) 


# males
results_base_m <- run_projection(male_population, prob_death_non_alc, pif_ii, pif_hiv, pif_road, pif_liver, pif_bcancer, wealth, heavy_binge_mod)
results_R5_m <- run_projection(male_population, prob_death_non_alc, pif_ii_R5, pif_hiv_R5, pif_road_R5, pif_liver_R5, pif_bcancer_R5, wealth, heavy_binge_mod)
results_R10_m <- run_projection(male_population, prob_death_non_alc, pif_ii_R10, pif_hiv_R10, pif_road_R10, pif_liver_R10, pif_bcancer_R10, wealth, heavy_binge_mod)
results_R15_m <- run_projection(male_population, prob_death_non_alc, pif_ii_R15, pif_hiv_R15, pif_road_R15, pif_liver_R15, pif_bcancer_R15, wealth, heavy_binge_mod)
results_extreme_m <- run_projection(male_population, prob_death_non_alc, pif_ii_extreme, pif_hiv_extreme, pif_road_extreme, pif_liver_extreme, pif_bcancer_extreme, wealth, heavy_binge_mod)

# sensecheck the results tables for baseline with the mortality from the health data inputs.

results_base_m %>% summarise(sum(deaths_ii_t1), sum(deaths_hiv_t1), sum(deaths_road_t1), sum(deaths_bcancer_t1), sum(deaths_liver_t1)) 
results_extreme_m %>% summarise(sum(deaths_ii_t1), sum(deaths_hiv_t1), sum(deaths_road_t1), sum(deaths_bcancer_t1), sum(deaths_liver_t1)) 

##################################################################################
#### creating a function to calculate deaths under each policy   
##################################################################################

# creating a function which sums all the results for each scenario over the 20 year
# model projection

deaths <- function(policy_results, policy, sex){
  
  policy_results$policy <- paste0("", policy, "")
  policy_results$sex <- paste0("", sex, "")
  policy_results$deaths_ii <- rowSums(policy_results[,grep('deaths_ii', names(policy_results))])
  policy_results$deaths_hiv <- rowSums(policy_results[,grep('deaths_hiv', names(policy_results))])
  policy_results$deaths_road <- rowSums(policy_results[,grep('deaths_road', names(policy_results))])
  policy_results$deaths_liver <- rowSums(policy_results[,grep('deaths_liver', names(policy_results))])
  policy_results$deaths_bcancer <- rowSums(policy_results[,grep('deaths_bcancer', names(policy_results))])
  policy_results <- dplyr::select(policy_results, 'wealth', 'heavy_binge_mod', 'age', 'deaths_ii', 'deaths_hiv', 
                                  'deaths_road', 'deaths_liver', 'deaths_bcancer', 'policy', 'sex')
}

##################################################################################
#### generating the results dataframes                                        
##################################################################################

female_base <- deaths(results_base_f, "base", "female")
female_R5 <- deaths(results_R5_f, "R5", "female")
female_R10 <- deaths(results_R10_f, "R10", "female")
female_R15 <- deaths(results_R15_f, "R15", "female")
female_extreme <- deaths(results_extreme_f, "extreme", "female")

male_base <- deaths(results_base_m, "base", "male")
male_R5 <- deaths(results_R5_m, "R5", "male")
male_R10 <- deaths(results_R10_m, "R10", "male")
male_R15 <- deaths(results_R15_m, "R15", "male")
male_extreme <- deaths(results_extreme_m, "extreme", "male")




##################################################################################
#### putting all the mortality results into one dataframe                     
##################################################################################
# Appending all the datasets together

results_mortality <- bind_rows(male_base, male_R5, male_R10, male_R15, male_extreme, 
                               female_base, female_R5, female_R10, female_R15, female_extreme)
# check results make sense, by looking at each of the health outcomes in turn
results_mortality %>% group_by(policy, sex) %>%
summarise(sum(deaths_bcancer, na.rm = TRUE))
# the breast cancer results look odd for males but this is beacuase their risk does not
# change with alcohol consumptions so as the price goes up and their are more people alive
# there are more men to die from breast cancer.
# this is the number of lives you could save if everyone completely stopped all drinking instantly.
# This seems about right for one year
sum(results_extreme_m$deaths_ii_t1, na.rm = TRUE)
sum(results_extreme_f$deaths_ii_t1, na.rm = TRUE)
# this is the number of deaths at current
sum(results_base_m$deaths_ii_t1, na.rm = TRUE)
sum(results_base_f$deaths_ii_t1, na.rm = TRUE)


#################################################################################
#### female prevelance                       
#################################################################################

# now thinking about morbidity the same pifs are relevant
# selecting the probabilities for each of the five conditions
# I am just selecting the variables I need. i also don't want anyone aged -5 to 14
probs <- female_population %>% select(starts_with("prob_hiv"), starts_with("prob_ii"), 
                                 starts_with("prob_road"), starts_with("prob_liver"), 
                                 starts_with("prob_bcancer"), starts_with("pif_"), age, wealth, heavy_binge_mod) %>% filter(age>14)

#### base ####
# create a population table for each scenario
base_pop <- results_base_f %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_base <- base_pop
prev_hiv_base[1:5, 1:5] # just seeing what it looks like

prev_hiv_base[,1:20] <- prev_hiv_base[,1:20] * probs$prob_hiv # multiplying the population columns
# from time t1 to t20 by baseline probability of hiv
prev_hiv_base[1:5, 1:5] #  checking they look about right
names(prev_hiv_base)
namesVec <- names(prev_hiv_base)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_base) <- namesVec


prev_ii_base <- base_pop
prev_ii_base[,1:20] <- prev_ii_base[,1:20] * probs$prob_ii
names(prev_ii_base)
namesVec <- names(prev_ii_base)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_base) <- namesVec


prev_road_base <- base_pop
prev_road_base[,1:20] <- prev_road_base[,1:20] * probs$prob_road
names(prev_road_base)
namesVec <- names(prev_road_base)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_base) <- namesVec


prev_liver_base <- base_pop
prev_liver_base[,1:20] <- prev_liver_base[,1:20] * probs$prob_liver
names(prev_liver_base)
namesVec <- names(prev_liver_base)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_base) <- namesVec


prev_bcancer_base <- base_pop
prev_bcancer_base[,1:20] <- prev_bcancer_base[,1:20] * probs$prob_bcancer
names(prev_bcancer_base)
namesVec <- names(prev_bcancer_base)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_base) <- namesVec


# joining them altogether into one dataframe
prev_base_f <- bind_cols(prev_hiv_base, prev_ii_base, prev_road_base, prev_liver_base, prev_bcancer_base)
prev_base_f <- rename(prev_base_f, age = "age...113")
prev_base_f <- rename(prev_base_f, wealth = "wealth...114")
prev_base_f <- rename(prev_base_f, heavy_binge_mod = "heavy_binge_mod...115")

prev_base_f <- prev_base_f %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)

# sense check
# it is worth pausing at this point to compare prevelance at base in year one with
# the prevelance data we used as an input. it should be in the same ballpark
# female: hiv 4,142,100, ii: 960,767, road: 546,048, liver: 41,738, bcancer: 60,614

sum(prev_base_f$prev_hiv_t1)
sum(prev_base_f$prev_ii_t1)
sum(prev_base_f$prev_road_t1)
sum(prev_base_f$prev_liver_t1)
sum(prev_base_f$prev_bcancer_t1)


#### R5 ####

R5_pop <- results_R5_f %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_R5 <- R5_pop
prev_hiv_R5[1:5, 1:5] # just seeing what it looks like
prev_hiv_R5[,1:20] <- prev_hiv_R5[,1:20] * probs$prob_hiv * probs$pif_hiv_R5
prev_hiv_R5[1:5, 1:5] #  checking they look about right
names(prev_hiv_R5)
namesVec <- names(prev_hiv_R5)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_R5) <- namesVec


prev_ii_R5 <- R5_pop
prev_ii_R5[,1:20] <- prev_ii_R5[,1:20] * probs$prob_ii * probs$pif_ii_R5
names(prev_ii_R5)
namesVec <- names(prev_ii_R5)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_R5) <- namesVec


prev_road_R5 <- R5_pop
prev_road_R5[,1:20] <- prev_road_R5[,1:20] * probs$prob_road * probs$pif_road_R5
names(prev_road_R5)
namesVec <- names(prev_road_R5)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_R5) <- namesVec

prev_liver_R5 <- R5_pop
prev_liver_R5[,1] <- prev_liver_R5[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.21))
prev_liver_R5[,2] <- prev_liver_R5[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.34))
prev_liver_R5[,3] <- prev_liver_R5[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.43))
prev_liver_R5[,4] <- prev_liver_R5[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.50))
prev_liver_R5[,5] <- prev_liver_R5[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.56))
prev_liver_R5[,6] <- prev_liver_R5[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.61))
prev_liver_R5[,7] <- prev_liver_R5[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.65))
prev_liver_R5[,8] <- prev_liver_R5[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.69))
prev_liver_R5[,9] <- prev_liver_R5[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.73))
prev_liver_R5[,10] <- prev_liver_R5[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.76))
prev_liver_R5[,11] <- prev_liver_R5[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.79))
prev_liver_R5[,12] <- prev_liver_R5[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.82))
prev_liver_R5[,13] <- prev_liver_R5[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.85))
prev_liver_R5[,14] <- prev_liver_R5[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.88))
prev_liver_R5[,15] <- prev_liver_R5[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.90))
prev_liver_R5[,16] <- prev_liver_R5[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.92))
prev_liver_R5[,17] <- prev_liver_R5[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.94))
prev_liver_R5[,18] <- prev_liver_R5[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.96))
prev_liver_R5[,19] <- prev_liver_R5[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.98))
prev_liver_R5[,20] <- prev_liver_R5[,20] * probs$prob_liver * probs$pif_liver_R5 # by the 20th year
# in the model the full benefit is realised

names(prev_liver_R5)
namesVec <- names(prev_liver_R5)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_R5) <- namesVec


# with breast cancer the first 10 years are unaffected by the policy, 
# thereafter three is a 10% increase until it reaches full effect at year 20
prev_bcancer_R5 <- R5_pop
prev_bcancer_R5[,1:10] <- prev_bcancer_R5[,1:10] * probs$prob_bcancer
prev_bcancer_R5[,11] <- prev_bcancer_R5[,11] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.1))
prev_bcancer_R5[,12] <- prev_bcancer_R5[,12] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.2))
prev_bcancer_R5[,13] <- prev_bcancer_R5[,13] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.3))
prev_bcancer_R5[,14] <- prev_bcancer_R5[,14] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.4))
prev_bcancer_R5[,15] <- prev_bcancer_R5[,15] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.5))
prev_bcancer_R5[,16] <- prev_bcancer_R5[,16] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.6))
prev_bcancer_R5[,17] <- prev_bcancer_R5[,17] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.7))
prev_bcancer_R5[,18] <- prev_bcancer_R5[,18] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.8))
prev_bcancer_R5[,19] <- prev_bcancer_R5[,19] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R5) * 0.9))
prev_bcancer_R5[,20] <- prev_bcancer_R5[,20] * probs$prob_bcancer * probs$pif_bcancer_R5 

names(prev_bcancer_R5)
namesVec <- names(prev_bcancer_R5)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_R5) <- namesVec


# joining them altogether into one dataframe
prev_R5_f <- bind_cols(prev_hiv_R5, prev_ii_R5, prev_road_R5, prev_liver_R5, prev_bcancer_R5)

prev_R5_f <- rename(prev_R5_f, age = "age...113")
prev_R5_f <- rename(prev_R5_f, wealth = "wealth...114")
prev_R5_f <- rename(prev_R5_f, heavy_binge_mod = "heavy_binge_mod...115")

prev_R5_f <- prev_R5_f %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)


#### R10 ####

R10_pop <- results_R10_f %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_R10 <- R10_pop
prev_hiv_R10[1:5, 1:5] # just seeing what it looks like
prev_hiv_R10[,1:20] <- prev_hiv_R10[,1:20] * probs$prob_hiv * probs$pif_hiv_R10
prev_hiv_R10[1:5, 1:5] #  checking they look about right
names(prev_hiv_R10)
namesVec <- names(prev_hiv_R10)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_R10) <- namesVec


prev_ii_R10 <- R10_pop
prev_ii_R10[,1:20] <- prev_ii_R10[,1:20] * probs$prob_ii * probs$pif_ii_R10
names(prev_ii_R10)
namesVec <- names(prev_ii_R10)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_R10) <- namesVec


prev_road_R10 <- R10_pop
prev_road_R10[,1:20] <- prev_road_R10[,1:20] * probs$prob_road * probs$pif_road_R10
names(prev_road_R10)
namesVec <- names(prev_road_R10)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_R10) <- namesVec


prev_liver_R10 <- R10_pop
prev_liver_R10[,1] <- prev_liver_R10[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.21))
prev_liver_R10[,2] <- prev_liver_R10[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.34))
prev_liver_R10[,3] <- prev_liver_R10[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.43))
prev_liver_R10[,4] <- prev_liver_R10[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.50))
prev_liver_R10[,5] <- prev_liver_R10[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.56))
prev_liver_R10[,6] <- prev_liver_R10[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.61))
prev_liver_R10[,7] <- prev_liver_R10[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.65))
prev_liver_R10[,8] <- prev_liver_R10[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.69))
prev_liver_R10[,9] <- prev_liver_R10[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.73))
prev_liver_R10[,10] <- prev_liver_R10[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.76))
prev_liver_R10[,11] <- prev_liver_R10[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.79))
prev_liver_R10[,12] <- prev_liver_R10[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.82))
prev_liver_R10[,13] <- prev_liver_R10[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.85))
prev_liver_R10[,14] <- prev_liver_R10[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.88))
prev_liver_R10[,15] <- prev_liver_R10[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.90))
prev_liver_R10[,16] <- prev_liver_R10[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.92))
prev_liver_R10[,17] <- prev_liver_R10[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.94))
prev_liver_R10[,18] <- prev_liver_R10[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.96))
prev_liver_R10[,19] <- prev_liver_R10[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.98))
prev_liver_R10[,20] <- prev_liver_R10[,20] * probs$prob_liver * probs$pif_liver_R10 # by the 20th year
# in the model the full benefit is realised

names(prev_liver_R10)
namesVec <- names(prev_liver_R10)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_R10) <- namesVec


# with breast cancer the first 10 years are unaffected by the policy, 
# thereafter three is a 10% increase until it reaches full effect at year 20
prev_bcancer_R10 <- R10_pop
prev_bcancer_R10[,1:10] <- prev_bcancer_R10[,1:10] * probs$prob_bcancer
prev_bcancer_R10[,11] <- prev_bcancer_R10[,11] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.1))
prev_bcancer_R10[,12] <- prev_bcancer_R10[,12] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.2))
prev_bcancer_R10[,13] <- prev_bcancer_R10[,13] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.3))
prev_bcancer_R10[,14] <- prev_bcancer_R10[,14] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.4))
prev_bcancer_R10[,15] <- prev_bcancer_R10[,15] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.5))
prev_bcancer_R10[,16] <- prev_bcancer_R10[,16] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.6))
prev_bcancer_R10[,17] <- prev_bcancer_R10[,17] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.7))
prev_bcancer_R10[,18] <- prev_bcancer_R10[,18] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.8))
prev_bcancer_R10[,19] <- prev_bcancer_R10[,19] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R10) * 0.9))
prev_bcancer_R10[,20] <- prev_bcancer_R10[,20] * probs$prob_bcancer * probs$pif_bcancer_R10 

names(prev_bcancer_R10)
namesVec <- names(prev_bcancer_R10)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_R10) <- namesVec

prev_R10_f <- bind_cols(prev_hiv_R10, prev_ii_R10, prev_road_R10, prev_liver_R10, prev_bcancer_R10)
prev_R10_f <- rename(prev_R10_f, age = "age...113")
prev_R10_f <- rename(prev_R10_f, wealth = "wealth...114")
prev_R10_f <- rename(prev_R10_f, heavy_binge_mod = "heavy_binge_mod...115")


prev_R10_f <- prev_R10_f %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)


#### R15 ####

R15_pop <- results_R15_f %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_R15 <- R15_pop
prev_hiv_R15[1:5, 1:5] # just seeing what it looks like
prev_hiv_R15[,1:20] <- prev_hiv_R15[,1:20] * probs$prob_hiv * probs$pif_hiv_R15
prev_hiv_R15[1:5, 1:5] #  checking they look about right
names(prev_hiv_R15)
namesVec <- names(prev_hiv_R15)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_R15) <- namesVec


prev_ii_R15 <- R15_pop
prev_ii_R15[,1:20] <- prev_ii_R15[,1:20] * probs$prob_ii * probs$pif_ii_R15
names(prev_ii_R15)
namesVec <- names(prev_ii_R15)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_R15) <- namesVec


prev_road_R15 <- R15_pop
prev_road_R15[,1:20] <- prev_road_R15[,1:20] * probs$prob_road * probs$pif_road_R15
names(prev_road_R15)
namesVec <- names(prev_road_R15)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_R15) <- namesVec


prev_liver_R15 <- R15_pop
prev_liver_R15[,1] <- prev_liver_R15[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.21))
prev_liver_R15[,2] <- prev_liver_R15[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.34))
prev_liver_R15[,3] <- prev_liver_R15[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.43))
prev_liver_R15[,4] <- prev_liver_R15[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.50))
prev_liver_R15[,5] <- prev_liver_R15[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.56))
prev_liver_R15[,6] <- prev_liver_R15[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.61))
prev_liver_R15[,7] <- prev_liver_R15[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.65))
prev_liver_R15[,8] <- prev_liver_R15[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.69))
prev_liver_R15[,9] <- prev_liver_R15[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.73))
prev_liver_R15[,10] <- prev_liver_R15[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.76))
prev_liver_R15[,11] <- prev_liver_R15[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.79))
prev_liver_R15[,12] <- prev_liver_R15[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.82))
prev_liver_R15[,13] <- prev_liver_R15[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.85))
prev_liver_R15[,14] <- prev_liver_R15[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.88))
prev_liver_R15[,15] <- prev_liver_R15[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.90))
prev_liver_R15[,16] <- prev_liver_R15[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.92))
prev_liver_R15[,17] <- prev_liver_R15[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.94))
prev_liver_R15[,18] <- prev_liver_R15[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.96))
prev_liver_R15[,19] <- prev_liver_R15[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.98))
prev_liver_R15[,20] <- prev_liver_R15[,20] * probs$prob_liver * probs$pif_liver_R15 # by the 20th year
# in the model the full benefit is realised

names(prev_liver_R15)
namesVec <- names(prev_liver_R15)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_R15) <- namesVec


# with breast cancer the first 10 years are unaffected by the policy, 
# thereafter three is a 10% increase until it reaches full effect at year 20
prev_bcancer_R15 <- R15_pop
prev_bcancer_R15[,1:10] <- prev_bcancer_R15[,1:10] * probs$prob_bcancer
prev_bcancer_R15[,11] <- prev_bcancer_R15[,11] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.1))
prev_bcancer_R15[,12] <- prev_bcancer_R15[,12] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.2))
prev_bcancer_R15[,13] <- prev_bcancer_R15[,13] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.3))
prev_bcancer_R15[,14] <- prev_bcancer_R15[,14] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.4))
prev_bcancer_R15[,15] <- prev_bcancer_R15[,15] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.5))
prev_bcancer_R15[,16] <- prev_bcancer_R15[,16] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.6))
prev_bcancer_R15[,17] <- prev_bcancer_R15[,17] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.7))
prev_bcancer_R15[,18] <- prev_bcancer_R15[,18] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.8))
prev_bcancer_R15[,19] <- prev_bcancer_R15[,19] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_R15) * 0.9))
prev_bcancer_R15[,20] <- prev_bcancer_R15[,20] * probs$prob_bcancer * probs$pif_bcancer_R15 

names(prev_bcancer_R15)
namesVec <- names(prev_bcancer_R15)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_R15) <- namesVec

prev_R15_f <- bind_cols(prev_hiv_R15, prev_ii_R15, prev_road_R15, prev_liver_R15, prev_bcancer_R15)
prev_R15_f <- rename(prev_R15_f, age = "age...113")
prev_R15_f <- rename(prev_R15_f, wealth = "wealth...114")
prev_R15_f <- rename(prev_R15_f, heavy_binge_mod = "heavy_binge_mod...115")

prev_R15_f <- prev_R15_f %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)


#### extreme ####

extreme_pop <- results_extreme_f %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_extreme <- extreme_pop
prev_hiv_extreme[1:5, 1:5] # just seeing what it looks like
prev_hiv_extreme[,1:20] <- prev_hiv_extreme[,1:20] * probs$prob_hiv * probs$pif_hiv_extreme
prev_hiv_extreme[1:5, 1:5] #  checking they look about right
names(prev_hiv_extreme)
namesVec <- names(prev_hiv_extreme)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_extreme) <- namesVec


prev_ii_extreme <- extreme_pop
prev_ii_extreme[,1:20] <- prev_ii_extreme[,1:20] * probs$prob_ii * probs$pif_ii_extreme
names(prev_ii_extreme)
namesVec <- names(prev_ii_extreme)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_extreme) <- namesVec


prev_road_extreme <- extreme_pop
prev_road_extreme[,1:20] <- prev_road_extreme[,1:20] * probs$prob_road * probs$pif_road_extreme
names(prev_road_extreme)
namesVec <- names(prev_road_extreme)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_extreme) <- namesVec


prev_liver_extreme <- extreme_pop
prev_liver_extreme[,1] <- prev_liver_extreme[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.21))
prev_liver_extreme[,2] <- prev_liver_extreme[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.34))
prev_liver_extreme[,3] <- prev_liver_extreme[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.43))
prev_liver_extreme[,4] <- prev_liver_extreme[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.50))
prev_liver_extreme[,5] <- prev_liver_extreme[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.56))
prev_liver_extreme[,6] <- prev_liver_extreme[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.61))
prev_liver_extreme[,7] <- prev_liver_extreme[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.65))
prev_liver_extreme[,8] <- prev_liver_extreme[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.69))
prev_liver_extreme[,9] <- prev_liver_extreme[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.73))
prev_liver_extreme[,10] <- prev_liver_extreme[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.76))
prev_liver_extreme[,11] <- prev_liver_extreme[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.79))
prev_liver_extreme[,12] <- prev_liver_extreme[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.82))
prev_liver_extreme[,13] <- prev_liver_extreme[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.85))
prev_liver_extreme[,14] <- prev_liver_extreme[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.88))
prev_liver_extreme[,15] <- prev_liver_extreme[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.90))
prev_liver_extreme[,16] <- prev_liver_extreme[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.92))
prev_liver_extreme[,17] <- prev_liver_extreme[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.94))
prev_liver_extreme[,18] <- prev_liver_extreme[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.96))
prev_liver_extreme[,19] <- prev_liver_extreme[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.98))
prev_liver_extreme[,20] <- prev_liver_extreme[,20] * probs$prob_liver * probs$pif_liver_extreme # by the 20th year
# in the model the full benefit is realised

names(prev_liver_extreme)
namesVec <- names(prev_liver_extreme)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_extreme) <- namesVec


# with breast cancer the first 10 years are unaffected by the policy, 
# thereafter three is a 10% increase until it reaches full effect at year 20
prev_bcancer_extreme <- extreme_pop
prev_bcancer_extreme[,1:10] <- prev_bcancer_extreme[,1:10] * probs$prob_bcancer
prev_bcancer_extreme[,11] <- prev_bcancer_extreme[,11] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.1))
prev_bcancer_extreme[,12] <- prev_bcancer_extreme[,12] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.2))
prev_bcancer_extreme[,13] <- prev_bcancer_extreme[,13] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.3))
prev_bcancer_extreme[,14] <- prev_bcancer_extreme[,14] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.4))
prev_bcancer_extreme[,15] <- prev_bcancer_extreme[,15] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.5))
prev_bcancer_extreme[,16] <- prev_bcancer_extreme[,16] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.6))
prev_bcancer_extreme[,17] <- prev_bcancer_extreme[,17] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.7))
prev_bcancer_extreme[,18] <- prev_bcancer_extreme[,18] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.8))
prev_bcancer_extreme[,19] <- prev_bcancer_extreme[,19] * probs$prob_bcancer * (1 - ((1 - probs$pif_bcancer_extreme) * 0.9))
prev_bcancer_extreme[,20] <- prev_bcancer_extreme[,20] * probs$prob_bcancer * probs$pif_bcancer_extreme 


names(prev_bcancer_extreme)
namesVec <- names(prev_bcancer_extreme)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_extreme) <- namesVec

prev_extreme_f <- bind_cols(prev_hiv_extreme, prev_ii_extreme, prev_road_extreme, prev_liver_extreme, prev_bcancer_extreme)
prev_extreme_f <- rename(prev_extreme_f, age = "age...113")
prev_extreme_f <- rename(prev_extreme_f, wealth = "wealth...114")
prev_extreme_f <- rename(prev_extreme_f, heavy_binge_mod = "heavy_binge_mod...115")


prev_extreme_f <- prev_extreme_f %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)


#################################################################################
#### males prevelance
#################################################################################
# now thinking about morbidity the same pifs are relevant
# selecting the probabilities for each of the five conditions
# REMINDER males do not have a rr for alcohol and breast cancer

# I am just selecting the variables I need. i also don't want anyone aged -5 to 14
probs <- male_population %>% select(starts_with("prob_hiv"), starts_with("prob_ii"), 
                                      starts_with("prob_road"), starts_with("prob_liver"), 
                                      starts_with("prob_bcancer"), starts_with("pif_"), age, wealth, heavy_binge_mod) %>% filter(age>14)

#### base ####
# create a population table for each scenario
base_pop <- results_base_m %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_base <- base_pop
prev_hiv_base[1:5, 1:5] # just seeing what it looks like

prev_hiv_base[,1:20] <- prev_hiv_base[,1:20] * probs$prob_hiv # multiplying the population columns
# from time t0 (baseline) to t20 by baseline probability of hiv
prev_hiv_base[1:5, 1:5] #  checking they look about right
names(prev_hiv_base)
namesVec <- names(prev_hiv_base)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_base) <- namesVec


prev_ii_base <- base_pop
prev_ii_base[,1:20] <- prev_ii_base[,1:20] * probs$prob_ii
names(prev_ii_base)
namesVec <- names(prev_ii_base)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_base) <- namesVec


prev_road_base <- base_pop
prev_road_base[,1:20] <- prev_road_base[,1:20] * probs$prob_road
names(prev_road_base)
namesVec <- names(prev_road_base)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_base) <- namesVec


prev_liver_base <- base_pop
prev_liver_base[,1:20] <- prev_liver_base[,1:20] * probs$prob_liver
names(prev_liver_base)
namesVec <- names(prev_liver_base)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_base) <- namesVec


prev_bcancer_base <- base_pop
prev_bcancer_base[,1:20] <- prev_bcancer_base[,1:20] * probs$prob_bcancer
names(prev_bcancer_base)
namesVec <- names(prev_bcancer_base)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_base) <- namesVec


# joining them altogether into one dataframe
prev_base_m <- bind_cols(prev_hiv_base, prev_ii_base, prev_road_base, prev_liver_base, prev_bcancer_base)
prev_base_m <- rename(prev_base_m, age = "age...113")
prev_base_m <- rename(prev_base_m, wealth = "wealth...114")
prev_base_m <- rename(prev_base_m, heavy_binge_mod = "heavy_binge_mod...115")


prev_base_m <- prev_base_m %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)

# sense check
# it is worth pausing at this point to compare prevelance at base in year one with
# the prevelance data we used as an input
# male: hiv 2,535,616.299, ii: 963,483, road: 663,137, liver: 55,511, bcancer: 793
sum(prev_base_m$prev_hiv_t1)
sum(prev_base_m$prev_ii_t1)
sum(prev_base_m$prev_road_t1)
sum(prev_base_m$prev_liver_t1)
sum(prev_base_m$prev_bcancer_t1)


#### R5 ####

R5_pop <- results_R5_m %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_R5 <- R5_pop
prev_hiv_R5[1:5, 1:5] # just seeing what it looks like
prev_hiv_R5[,1:20] <- prev_hiv_R5[,1:20] * probs$prob_hiv * probs$pif_hiv_R5
prev_hiv_R5[1:5, 1:5] #  checking they look about right
names(prev_hiv_R5)
namesVec <- names(prev_hiv_R5)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_R5) <- namesVec


prev_ii_R5 <- R5_pop
prev_ii_R5[,1:20] <- prev_ii_R5[,1:20] * probs$prob_ii * probs$pif_ii_R5
names(prev_ii_R5)
namesVec <- names(prev_ii_R5)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_R5) <- namesVec


prev_road_R5 <- R5_pop
prev_road_R5[,1:20] <- prev_road_R5[,1:20] * probs$prob_road * probs$pif_road_R5
names(prev_road_R5)
namesVec <- names(prev_road_R5)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_R5) <- namesVec

prev_liver_R5 <- R5_pop
prev_liver_R5[,1] <- prev_liver_R5[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.21))
prev_liver_R5[,2] <- prev_liver_R5[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.34))
prev_liver_R5[,3] <- prev_liver_R5[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.43))
prev_liver_R5[,4] <- prev_liver_R5[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.50))
prev_liver_R5[,5] <- prev_liver_R5[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.56))
prev_liver_R5[,6] <- prev_liver_R5[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.61))
prev_liver_R5[,7] <- prev_liver_R5[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.65))
prev_liver_R5[,8] <- prev_liver_R5[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.69))
prev_liver_R5[,9] <- prev_liver_R5[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.73))
prev_liver_R5[,10] <- prev_liver_R5[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.76))
prev_liver_R5[,11] <- prev_liver_R5[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.79))
prev_liver_R5[,12] <- prev_liver_R5[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.82))
prev_liver_R5[,13] <- prev_liver_R5[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.85))
prev_liver_R5[,14] <- prev_liver_R5[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.88))
prev_liver_R5[,15] <- prev_liver_R5[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.90))
prev_liver_R5[,16] <- prev_liver_R5[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.92))
prev_liver_R5[,17] <- prev_liver_R5[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.94))
prev_liver_R5[,18] <- prev_liver_R5[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.96))
prev_liver_R5[,19] <- prev_liver_R5[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R5) * 0.98))
prev_liver_R5[,20] <- prev_liver_R5[,20] * probs$prob_liver * probs$pif_liver_R5 # by the 20th year
# in the model the full benefit is realised

names(prev_liver_R5)
namesVec <- names(prev_liver_R5)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_R5) <- namesVec


# with breast cancer males do not have a risk relationship with alcohol
prev_bcancer_R5 <- R5_pop
prev_bcancer_R5[,1:20] <- prev_bcancer_R5[,1:20] * probs$prob_bcancer
names(prev_bcancer_R5)
namesVec <- names(prev_bcancer_R5)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_R5) <- namesVec


# joining them altogether into one dataframe
prev_R5_m <- bind_cols(prev_hiv_R5, prev_ii_R5, prev_road_R5, prev_liver_R5, prev_bcancer_R5)
prev_R5_m <- rename(prev_R5_m, age = "age...113")
prev_R5_m <- rename(prev_R5_m, wealth = "wealth...114")
prev_R5_m <- rename(prev_R5_m, heavy_binge_mod = "heavy_binge_mod...115")

prev_R5_m <- prev_R5_m %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)


#### R10 ####

R10_pop <- results_R10_m %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_R10 <- R10_pop
prev_hiv_R10[1:5, 1:5] # just seeing what it looks like
prev_hiv_R10[,1:20] <- prev_hiv_R10[,1:20] * probs$prob_hiv * probs$pif_hiv_R10
prev_hiv_R10[1:5, 1:5] #  checking they look about right
names(prev_hiv_R10)
namesVec <- names(prev_hiv_R10)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_R10) <- namesVec


prev_ii_R10 <- R10_pop
prev_ii_R10[,1:20] <- prev_ii_R10[,1:20] * probs$prob_ii * probs$pif_ii_R10
names(prev_ii_R10)
namesVec <- names(prev_ii_R10)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_R10) <- namesVec


prev_road_R10 <- R10_pop
prev_road_R10[,1:20] <- prev_road_R10[,1:20] * probs$prob_road * probs$pif_road_R10
names(prev_road_R10)
namesVec <- names(prev_road_R10)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_R10) <- namesVec

prev_liver_R10 <- R10_pop
prev_liver_R10[,1] <- prev_liver_R10[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.21))
prev_liver_R10[,2] <- prev_liver_R10[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.34))
prev_liver_R10[,3] <- prev_liver_R10[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.43))
prev_liver_R10[,4] <- prev_liver_R10[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.50))
prev_liver_R10[,5] <- prev_liver_R10[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.56))
prev_liver_R10[,6] <- prev_liver_R10[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.61))
prev_liver_R10[,7] <- prev_liver_R10[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.65))
prev_liver_R10[,8] <- prev_liver_R10[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.69))
prev_liver_R10[,9] <- prev_liver_R10[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.73))
prev_liver_R10[,10] <- prev_liver_R10[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.76))
prev_liver_R10[,11] <- prev_liver_R10[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.79))
prev_liver_R10[,12] <- prev_liver_R10[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.82))
prev_liver_R10[,13] <- prev_liver_R10[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.85))
prev_liver_R10[,14] <- prev_liver_R10[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.88))
prev_liver_R10[,15] <- prev_liver_R10[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.90))
prev_liver_R10[,16] <- prev_liver_R10[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.92))
prev_liver_R10[,17] <- prev_liver_R10[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.94))
prev_liver_R10[,18] <- prev_liver_R10[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.96))
prev_liver_R10[,19] <- prev_liver_R10[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R10) * 0.98))
prev_liver_R10[,20] <- prev_liver_R10[,20] * probs$prob_liver * probs$pif_liver_R10 # by the 20th year
# in the model the full benefit is realised

names(prev_liver_R10)
namesVec <- names(prev_liver_R10)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_R10) <- namesVec


# with breast cancer males do not have a risk relationship with alcohol
prev_bcancer_R10 <- R10_pop
prev_bcancer_R10[,1:20] <- prev_bcancer_R10[,1:20] * probs$prob_bcancer
names(prev_bcancer_R10)
namesVec <- names(prev_bcancer_R10)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_R10) <- namesVec

prev_R10_m <- bind_cols(prev_hiv_R10, prev_ii_R10, prev_road_R10, prev_liver_R10, prev_bcancer_R10)
prev_R10_m <- rename(prev_R10_m, age = "age...113")
prev_R10_m <- rename(prev_R10_m, wealth = "wealth...114")
prev_R10_m <- rename(prev_R10_m, heavy_binge_mod = "heavy_binge_mod...115")

prev_R10_m <- prev_R10_m %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)



#### R15 ####

R15_pop <- results_R15_m %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_R15 <- R15_pop
prev_hiv_R15[1:5, 1:5] # just seeing what it looks like
prev_hiv_R15[,1:20] <- prev_hiv_R15[,1:20] * probs$prob_hiv * probs$pif_hiv_R15
prev_hiv_R15[1:5, 1:5] #  checking they look about right
names(prev_hiv_R15)
namesVec <- names(prev_hiv_R15)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_R15) <- namesVec


prev_ii_R15 <- R15_pop
prev_ii_R15[,1:20] <- prev_ii_R15[,1:20] * probs$prob_ii * probs$pif_ii_R15
names(prev_ii_R15)
namesVec <- names(prev_ii_R15)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_R15) <- namesVec


prev_road_R15 <- R15_pop
prev_road_R15[,1:20] <- prev_road_R15[,1:20] * probs$prob_road * probs$pif_road_R15
names(prev_road_R15)
namesVec <- names(prev_road_R15)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_R15) <- namesVec


prev_liver_R15 <- R15_pop
prev_liver_R15[,1] <- prev_liver_R15[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.21))
prev_liver_R15[,2] <- prev_liver_R15[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.34))
prev_liver_R15[,3] <- prev_liver_R15[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.43))
prev_liver_R15[,4] <- prev_liver_R15[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.50))
prev_liver_R15[,5] <- prev_liver_R15[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.56))
prev_liver_R15[,6] <- prev_liver_R15[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.61))
prev_liver_R15[,7] <- prev_liver_R15[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.65))
prev_liver_R15[,8] <- prev_liver_R15[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.69))
prev_liver_R15[,9] <- prev_liver_R15[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.73))
prev_liver_R15[,10] <- prev_liver_R15[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.76))
prev_liver_R15[,11] <- prev_liver_R15[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.79))
prev_liver_R15[,12] <- prev_liver_R15[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.82))
prev_liver_R15[,13] <- prev_liver_R15[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.85))
prev_liver_R15[,14] <- prev_liver_R15[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.88))
prev_liver_R15[,15] <- prev_liver_R15[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.90))
prev_liver_R15[,16] <- prev_liver_R15[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.92))
prev_liver_R15[,17] <- prev_liver_R15[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.94))
prev_liver_R15[,18] <- prev_liver_R15[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.96))
prev_liver_R15[,19] <- prev_liver_R15[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_R15) * 0.98))
prev_liver_R15[,20] <- prev_liver_R15[,20] * probs$prob_liver * probs$pif_liver_R15 # by the 20th year
# in the model the full benefit is realised

names(prev_liver_R15)
namesVec <- names(prev_liver_R15)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_R15) <- namesVec


# with breast cancer males do not have a risk relationship with alcohol
prev_bcancer_R15 <- R15_pop
prev_bcancer_R15[,1:20] <- prev_bcancer_R15[,1:20] * probs$prob_bcancer
names(prev_bcancer_R15)
namesVec <- names(prev_bcancer_R15)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_R15) <- namesVec

prev_R15_m <- bind_cols(prev_hiv_R15, prev_ii_R15, prev_road_R15, prev_liver_R15, prev_bcancer_R15)
prev_R15_m <- rename(prev_R15_m, age = "age...113")
prev_R15_m <- rename(prev_R15_m, wealth = "wealth...114")
prev_R15_m <- rename(prev_R15_m, heavy_binge_mod = "heavy_binge_mod...115")

prev_R15_m <- prev_R15_m %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)



#### extreme ####

extreme_pop <- results_extreme_m %>% select(starts_with("pop_"), age, wealth, heavy_binge_mod, -pop_t0)

prev_hiv_extreme <- extreme_pop
prev_hiv_extreme[1:5, 1:5] # just seeing what it looks like
prev_hiv_extreme[,1:20] <- prev_hiv_extreme[,1:20] * probs$prob_hiv * probs$pif_hiv_extreme
prev_hiv_extreme[1:5, 1:5] #  checking they look about right
names(prev_hiv_extreme)
namesVec <- names(prev_hiv_extreme)
namesVec <- gsub("pop", "prev_hiv", namesVec)
names(prev_hiv_extreme) <- namesVec


prev_ii_extreme <- extreme_pop
prev_ii_extreme[,1:20] <- prev_ii_extreme[,1:20] * probs$prob_ii * probs$pif_ii_extreme
names(prev_ii_extreme)
namesVec <- names(prev_ii_extreme)
namesVec <- gsub("pop", "prev_ii", namesVec)
names(prev_ii_extreme) <- namesVec


prev_road_extreme <- extreme_pop
prev_road_extreme[,1:20] <- prev_road_extreme[,1:20] * probs$prob_road * probs$pif_road_extreme
names(prev_road_extreme)
namesVec <- names(prev_road_extreme)
namesVec <- gsub("pop", "prev_road", namesVec)
names(prev_road_extreme) <- namesVec


prev_liver_extreme <- extreme_pop
prev_liver_extreme[,1] <- prev_liver_extreme[,1] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.21))
prev_liver_extreme[,2] <- prev_liver_extreme[,2] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.34))
prev_liver_extreme[,3] <- prev_liver_extreme[,3] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.43))
prev_liver_extreme[,4] <- prev_liver_extreme[,4] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.50))
prev_liver_extreme[,5] <- prev_liver_extreme[,5] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.56))
prev_liver_extreme[,6] <- prev_liver_extreme[,6] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.61))
prev_liver_extreme[,7] <- prev_liver_extreme[,7] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.65))
prev_liver_extreme[,8] <- prev_liver_extreme[,8] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.69))
prev_liver_extreme[,9] <- prev_liver_extreme[,9] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.73))
prev_liver_extreme[,10] <- prev_liver_extreme[,10] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.76))
prev_liver_extreme[,11] <- prev_liver_extreme[,11] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.79))
prev_liver_extreme[,12] <- prev_liver_extreme[,12] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.82))
prev_liver_extreme[,13] <- prev_liver_extreme[,13] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.85))
prev_liver_extreme[,14] <- prev_liver_extreme[,14] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.88))
prev_liver_extreme[,15] <- prev_liver_extreme[,15] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.90))
prev_liver_extreme[,16] <- prev_liver_extreme[,16] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.92))
prev_liver_extreme[,17] <- prev_liver_extreme[,17] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.94))
prev_liver_extreme[,18] <- prev_liver_extreme[,18] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.96))
prev_liver_extreme[,19] <- prev_liver_extreme[,19] * probs$prob_liver * (1 - ((1 - probs$pif_liver_extreme) * 0.98))
prev_liver_extreme[,20] <- prev_liver_extreme[,20] * probs$prob_liver * probs$pif_liver_extreme # by the 20th year
# in the model the full benefit is realised

names(prev_liver_extreme)
namesVec <- names(prev_liver_extreme)
namesVec <- gsub("pop", "prev_liver", namesVec)
names(prev_liver_extreme) <- namesVec


# with breast cancer males do not have a risk relationship with alcohol
prev_bcancer_extreme <- extreme_pop
prev_bcancer_extreme[,1:20] <- prev_bcancer_extreme[,1:20] * probs$prob_bcancer
names(prev_bcancer_extreme)
namesVec <- names(prev_bcancer_extreme)
namesVec <- gsub("pop", "prev_bcancer", namesVec)
names(prev_bcancer_extreme) <- namesVec

prev_extreme_m <- bind_cols(prev_hiv_extreme, prev_ii_extreme, prev_road_extreme, prev_liver_extreme, prev_bcancer_extreme)
prev_extreme_m <- rename(prev_extreme_m, age = "age...113")
prev_extreme_m <- rename(prev_extreme_m, wealth = "wealth...114")
prev_extreme_m <- rename(prev_extreme_m, heavy_binge_mod = "heavy_binge_mod...115")

prev_extreme_m <- prev_extreme_m %>% select(starts_with("prev_"), age, wealth, heavy_binge_mod)

##################################################################################
#### function for estimating cases under each scenario
##################################################################################

# creating a function which gives cases averted by taking 
# the difference between the policy and the baseline
cases_averted <- function(policy_results, policy, sex){
  
  policy_results$policy <- paste0("", policy, "")
  policy_results$sex <- paste0("", sex, "")
  policy_results$cases_ii <- rowSums(policy_results[,grep('prev_ii', names(policy_results))])
  policy_results$cases_hiv <- rowSums(policy_results[,grep('prev_hiv', names(policy_results))])
  policy_results$cases_road <- rowSums(policy_results[,grep('prev_road', names(policy_results))])
  policy_results$cases_liver <- rowSums(policy_results[,grep('prev_liver', names(policy_results))])
  policy_results$cases_bcancer <- rowSums(policy_results[,grep('prev_bcancer', names(policy_results))])
  policy_results <- dplyr::select(policy_results, 'wealth', 'heavy_binge_mod', 'age', 'cases_ii', 'cases_hiv', 
                                  'cases_road', 'cases_liver', 'cases_bcancer', 'policy', 'sex')
}

##################################################################################
#### running the function to calculate cases for each policy scenario                                                       
##################################################################################

cases_female_base <- cases_averted(prev_base_f, "base", "female")
cases_female_R5 <- cases_averted(prev_R5_f, "R5", "female")
cases_female_R10 <- cases_averted(prev_R10_f, "R10", "female")
cases_female_R15 <- cases_averted(prev_R15_f, "R15", "female")
cases_female_extreme <- cases_averted(prev_extreme_f, "extreme", "female")

cases_male_base <- cases_averted(prev_base_m, "base", "male")
cases_male_R5 <- cases_averted(prev_R5_m, "R5", "male")
cases_male_R10 <- cases_averted(prev_R10_m, "R10", "male")
cases_male_R15 <- cases_averted(prev_R15_m, "R15", "male")
cases_male_extreme <- cases_averted(prev_extreme_m, "extreme", "male")


##################################################################################
#### joining all the prevelance results together                                                       
##################################################################################

results_prevelance <- bind_rows(cases_male_base, cases_male_R5, cases_male_R10, cases_male_R15, 
                                cases_male_extreme, cases_female_base, cases_female_R5, cases_female_R10, 
                                cases_female_R15, cases_female_extreme)


##################################################################################
#### saving the results                                                       
##################################################################################
# these need to be saved so I can take them forward and calculate costs
save(prev_base_f, file = "intermediate/prev_base_f.Rda")
save(prev_R5_f, file = "intermediate/prev_R5_f.Rda")
save(prev_R10_f, file = "intermediate/prev_R10_f.Rda")
save(prev_R15_f, file = "intermediate/prev_R15_f.Rda")
save(prev_extreme_f, file = "intermediate/prev_extreme_f.Rda")

save(prev_base_m, file = "intermediate/prev_base_m.Rda")
save(prev_R5_m, file = "intermediate/prev_R5_m.Rda")
save(prev_R10_m, file = "intermediate/prev_R10_m.Rda")
save(prev_R15_m, file = "intermediate/prev_R15_m.Rda")
save(prev_extreme_m, file = "intermediate/prev_extreme_m.Rda")



# these need to be saved in order to visualise the results
save(results_mortality, file = "outputs/results_mortality.Rda")
save(results_prevelance, file = "outputs/results_prevelance.Rda")


