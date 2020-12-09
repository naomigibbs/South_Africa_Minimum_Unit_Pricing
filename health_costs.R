# This script file calculates the hospital/healthcare costs as a result in the 
# change in prevelance

##########################
#### Packages         ####
##########################
library(tidyverse)

##########################
#### Data             ####
##########################
load("intermediate/prev_base_f.Rda")
load("intermediate/prev_R5_f.Rda")
load("intermediate/prev_R10_f.Rda")
load("intermediate/prev_R15_f.Rda")
load("intermediate/prev_extreme_f.Rda")

load("intermediate/prev_base_m.Rda")
load("intermediate/prev_R5_m.Rda")
load("intermediate/prev_R10_m.Rda")
load("intermediate/prev_R15_m.Rda")
load("intermediate/prev_extreme_m.Rda")


#################################################
#### creating dataframes with net cases in   ####
#################################################
# first of all I will take the base case away from the results
# this will give negative numbers as there are less cases with 
# the policy than in the base scenario
 
# need to keep all the years seperate rather than sum all 20 years as 
# we are discounting costs at 5%

prev_R5_f_net <- prev_R5_f
prev_R5_f_net[,1:100] <-  prev_R5_f_net[,1:100] - prev_base_f[,1:100]

prev_R10_f_net <- prev_R10_f
prev_R10_f_net[,1:100] <-  prev_R10_f_net[,1:100] - prev_base_f[,1:100]

prev_R15_f_net <- prev_R15_f
prev_R15_f_net[,1:100] <-  prev_R15_f_net[,1:100] - prev_base_f[,1:100]

prev_extreme_f_net <- prev_extreme_f
prev_extreme_f_net[,1:100] <-  prev_extreme_f_net[,1:100] - prev_base_f[,1:100]


prev_R5_m_net <- prev_R5_m
prev_R5_m_net[,1:100] <-  prev_R5_m_net[,1:100] - prev_base_m[,1:100]

prev_R10_m_net <- prev_R10_m
prev_R10_m_net[,1:100] <-  prev_R10_m_net[,1:100] - prev_base_m[,1:100]

prev_R15_m_net <- prev_R15_m
prev_R15_m_net[,1:100] <-  prev_R15_m_net[,1:100] - prev_base_m[,1:100]

prev_extreme_m_net <- prev_extreme_m
prev_extreme_m_net[,1:100] <-  prev_extreme_m_net[,1:100] - prev_base_m[,1:100]


#################################################
#### creating a function to calculate costs  ####
#################################################
# note the costs for each disease injury will also take
# account of the multiplier

# also note some of the costs need to be increased to account for inflation
# the inflation rates for south africa are as follows
# http://www.statssa.gov.za/publications/P0141/CPIHistory.pdf?
# 2013: 5.76%, 2014: 6.09%, 2015: 4.58%, 2016: 6.34%, 2017: 5.27%, 2018: 4.62%
# 2018 is when we want to callibrate the numbers to

# hiv (cost is 2017/18): cost of first line ART = R3318.62 * 0.62 (multiplier) = R2057.42
# ii (cost is for 2013): R58,928 * 0.41 (multiplier) * 1.0609 * 1.0458 * 1.0634 * 1.0527 * 1.0462 = R31,393.85
# road (cost is for 2012): R56,592.17 * 0.19 (multiplier) * 1.0576 * 1.0609 * 1.0458 * 1.0634 * 1.0527 * 1.0462 = R14776.46
# liver (cost is for 2018): R2967 * 0.5 (multiplier) = R1483.5
# bcancer (cost is for 2015): R15,774 * 0.5 (multiplier) * 1.0634 * 1.0527 * 1.0462 = R9236.93

# also note the discount rate is 5%

calculate_costs <- function(data, policy){
  
data$policy <- paste0("", policy, "")
  
data$cost_hiv_t1 <- data$prev_hiv_t1*2057.42*(1/1.05^1)
data$cost_hiv_t2 <- data$prev_hiv_t2*2057.42*(1/1.05^2)
data$cost_hiv_t3 <- data$prev_hiv_t3*2057.42*(1/1.05^3)
data$cost_hiv_t4 <- data$prev_hiv_t4*2057.42*(1/1.05^4)
data$cost_hiv_t5 <- data$prev_hiv_t5*2057.42*(1/1.05^5)
data$cost_hiv_t6 <- data$prev_hiv_t6*2057.42*(1/1.05^6)
data$cost_hiv_t7 <- data$prev_hiv_t7*2057.42*(1/1.05^7)
data$cost_hiv_t8 <- data$prev_hiv_t8*2057.42*(1/1.05^8)
data$cost_hiv_t9 <- data$prev_hiv_t9*2057.42*(1/1.05^9)
data$cost_hiv_t10 <- data$prev_hiv_t10*2057.42*(1/1.05^10)
data$cost_hiv_t11 <- data$prev_hiv_t11*2057.42*(1/1.05^11)
data$cost_hiv_t12 <- data$prev_hiv_t12*2057.42*(1/1.05^12)
data$cost_hiv_t13 <- data$prev_hiv_t13*2057.42*(1/1.05^13)
data$cost_hiv_t14 <- data$prev_hiv_t14*2057.42*(1/1.05^14)
data$cost_hiv_t15 <- data$prev_hiv_t15*2057.42*(1/1.05^15)
data$cost_hiv_t16 <- data$prev_hiv_t16*2057.42*(1/1.05^16)
data$cost_hiv_t17 <- data$prev_hiv_t17*2057.42*(1/1.05^17)
data$cost_hiv_t18 <- data$prev_hiv_t18*2057.42*(1/1.05^18)
data$cost_hiv_t19 <- data$prev_hiv_t19*2057.42*(1/1.05^19)
data$cost_hiv_t20 <- data$prev_hiv_t20*2057.42*(1/1.05^20)

data$cost_ii_t1 <- data$prev_ii_t1*31393.85*(1/1.05^1)
data$cost_ii_t2 <- data$prev_ii_t2*31393.85*(1/1.05^2)
data$cost_ii_t3 <- data$prev_ii_t3*31393.85*(1/1.05^3)
data$cost_ii_t4 <- data$prev_ii_t4*31393.85*(1/1.05^4)
data$cost_ii_t5 <- data$prev_ii_t5*31393.85*(1/1.05^5)
data$cost_ii_t6 <- data$prev_ii_t6*31393.85*(1/1.05^6)
data$cost_ii_t7 <- data$prev_ii_t7*31393.85*(1/1.05^7)
data$cost_ii_t8 <- data$prev_ii_t8*31393.85*(1/1.05^8)
data$cost_ii_t9 <- data$prev_ii_t9*31393.85*(1/1.05^9)
data$cost_ii_t10 <- data$prev_ii_t10*31393.85*(1/1.05^10)
data$cost_ii_t11 <- data$prev_ii_t11*31393.85*(1/1.05^11)
data$cost_ii_t12 <- data$prev_ii_t12*31393.85*(1/1.05^12)
data$cost_ii_t13 <- data$prev_ii_t13*31393.85*(1/1.05^13)
data$cost_ii_t14 <- data$prev_ii_t14*31393.85*(1/1.05^14)
data$cost_ii_t15 <- data$prev_ii_t15*31393.85*(1/1.05^15)
data$cost_ii_t16 <- data$prev_ii_t16*31393.85*(1/1.05^16)
data$cost_ii_t17 <- data$prev_ii_t17*31393.85*(1/1.05^17)
data$cost_ii_t18 <- data$prev_ii_t18*31393.85*(1/1.05^18)
data$cost_ii_t19 <- data$prev_ii_t19*31393.85*(1/1.05^19)
data$cost_ii_t20 <- data$prev_ii_t20*31393.85*(1/1.05^20)

data$cost_road_t1 <- data$prev_road_t1*14776.46*(1/1.05^1)
data$cost_road_t2 <- data$prev_road_t2*14776.46*(1/1.05^2)
data$cost_road_t3 <- data$prev_road_t3*14776.46*(1/1.05^3)
data$cost_road_t4 <- data$prev_road_t4*14776.46*(1/1.05^4)
data$cost_road_t5 <- data$prev_road_t5*14776.46*(1/1.05^5)
data$cost_road_t6 <- data$prev_road_t6*14776.46*(1/1.05^6)
data$cost_road_t7 <- data$prev_road_t7*14776.46*(1/1.05^7)
data$cost_road_t8 <- data$prev_road_t8*14776.46*(1/1.05^8)
data$cost_road_t9 <- data$prev_road_t9*14776.46*(1/1.05^9)
data$cost_road_t10 <- data$prev_road_t10*14776.46*(1/1.05^10)
data$cost_road_t11 <- data$prev_road_t11*14776.46*(1/1.05^11)
data$cost_road_t12 <- data$prev_road_t12*14776.46*(1/1.05^12)
data$cost_road_t13 <- data$prev_road_t13*14776.46*(1/1.05^13)
data$cost_road_t14 <- data$prev_road_t14*14776.46*(1/1.05^14)
data$cost_road_t15 <- data$prev_road_t15*14776.46*(1/1.05^15)
data$cost_road_t16 <- data$prev_road_t16*14776.46*(1/1.05^16)
data$cost_road_t17 <- data$prev_road_t17*14776.46*(1/1.05^17)
data$cost_road_t18 <- data$prev_road_t18*14776.46*(1/1.05^18)
data$cost_road_t19 <- data$prev_road_t19*14776.46*(1/1.05^19)
data$cost_road_t20 <- data$prev_road_t20*14776.46*(1/1.05^20)

data$cost_liver_t1 <- data$prev_liver_t1*1483.5*(1/1.05^1)
data$cost_liver_t2 <- data$prev_liver_t2*1483.5*(1/1.05^2)
data$cost_liver_t3 <- data$prev_liver_t3*1483.5*(1/1.05^3)
data$cost_liver_t4 <- data$prev_liver_t4*1483.5*(1/1.05^4)
data$cost_liver_t5 <- data$prev_liver_t5*1483.5*(1/1.05^5)
data$cost_liver_t6 <- data$prev_liver_t6*1483.5*(1/1.05^6)
data$cost_liver_t7 <- data$prev_liver_t7*1483.5*(1/1.05^7)
data$cost_liver_t8 <- data$prev_liver_t8*1483.5*(1/1.05^8)
data$cost_liver_t9 <- data$prev_liver_t9*1483.5*(1/1.05^9)
data$cost_liver_t10 <- data$prev_liver_t10*1483.5*(1/1.05^10)
data$cost_liver_t11 <- data$prev_liver_t11*1483.5*(1/1.05^11)
data$cost_liver_t12 <- data$prev_liver_t12*1483.5*(1/1.05^12)
data$cost_liver_t13 <- data$prev_liver_t13*1483.5*(1/1.05^13)
data$cost_liver_t14 <- data$prev_liver_t14*1483.5*(1/1.05^14)
data$cost_liver_t15 <- data$prev_liver_t15*1483.5*(1/1.05^15)
data$cost_liver_t16 <- data$prev_liver_t16*1483.5*(1/1.05^16)
data$cost_liver_t17 <- data$prev_liver_t17*1483.5*(1/1.05^17)
data$cost_liver_t18 <- data$prev_liver_t18*1483.5*(1/1.05^18)
data$cost_liver_t19 <- data$prev_liver_t19*1483.5*(1/1.05^19)
data$cost_liver_t20 <- data$prev_liver_t20*1483.5*(1/1.05^20)

data$cost_bcancer_t1 <- data$prev_bcancer_t1*9236.93*(1/1.05^1)
data$cost_bcancer_t2 <- data$prev_bcancer_t2*9236.93*(1/1.05^2)
data$cost_bcancer_t3 <- data$prev_bcancer_t3*9236.93*(1/1.05^3)
data$cost_bcancer_t4 <- data$prev_bcancer_t4*9236.93*(1/1.05^4)
data$cost_bcancer_t5 <- data$prev_bcancer_t5*9236.93*(1/1.05^5)
data$cost_bcancer_t6 <- data$prev_bcancer_t6*9236.93*(1/1.05^6)
data$cost_bcancer_t7 <- data$prev_bcancer_t7*9236.93*(1/1.05^7)
data$cost_bcancer_t8 <- data$prev_bcancer_t8*9236.93*(1/1.05^8)
data$cost_bcancer_t9 <- data$prev_bcancer_t9*9236.93*(1/1.05^9)
data$cost_bcancer_t10 <- data$prev_bcancer_t10*9236.93*(1/1.05^10)
data$cost_bcancer_t11 <- data$prev_bcancer_t11*9236.93*(1/1.05^11)
data$cost_bcancer_t12 <- data$prev_bcancer_t12*9236.93*(1/1.05^12)
data$cost_bcancer_t13 <- data$prev_bcancer_t13*9236.93*(1/1.05^13)
data$cost_bcancer_t14 <- data$prev_bcancer_t14*9236.93*(1/1.05^14)
data$cost_bcancer_t15 <- data$prev_bcancer_t15*9236.93*(1/1.05^15)
data$cost_bcancer_t16 <- data$prev_bcancer_t16*9236.93*(1/1.05^16)
data$cost_bcancer_t17 <- data$prev_bcancer_t17*9236.93*(1/1.05^17)
data$cost_bcancer_t18 <- data$prev_bcancer_t18*9236.93*(1/1.05^18)
data$cost_bcancer_t19 <- data$prev_bcancer_t19*9236.93*(1/1.05^19)
data$cost_bcancer_t20 <- data$prev_bcancer_t20*9236.93*(1/1.05^20)

data
}


#################################################
#### calculating cost dataframes             ####
#################################################

female_R5_costs <- calculate_costs(prev_R5_f_net, "R5")
female_R10_costs <- calculate_costs(prev_R10_f_net, "R10")
female_R15_costs <- calculate_costs(prev_R15_f_net, "R15")
female_extreme_costs <- calculate_costs(prev_extreme_f_net, "extreme")

male_R5_costs <- calculate_costs(prev_R5_m_net, "R5")
male_R10_costs <- calculate_costs(prev_R10_m_net, "R10")
male_R15_costs <- calculate_costs(prev_R15_m_net, "R15")
male_extreme_costs <- calculate_costs(prev_extreme_m_net, "extreme")

# all the costs are negative because they are cost saving

#################################################
#### function to sum costs for the 20 years  ####
#################################################

aggregate_costs <- function(data, sex){
  
  
  data$sex <- paste0("", sex, "")
  data$prev_ii <- rowSums(data[,grep('prev_ii', names(data))])
  data$prev_hiv <- rowSums(data[,grep('prev_hiv', names(data))])
  data$prev_road <- rowSums(data[,grep('prev_road', names(data))])
  data$prev_liver <- rowSums(data[,grep('prev_liver', names(data))])
  data$prev_bcancer <- rowSums(data[,grep('prev_bcancer', names(data))])
  
  data$cost_ii <- rowSums(data[,grep('cost_ii', names(data))])
  data$cost_hiv <- rowSums(data[,grep('cost_hiv', names(data))])
  data$cost_road <- rowSums(data[,grep('cost_road', names(data))])
  data$cost_liver <- rowSums(data[,grep('cost_liver', names(data))])
  data$cost_bcancer <- rowSums(data[,grep('cost_bcancer', names(data))])
  data <- dplyr::select(data, 'wealth', 'heavy_binge_mod', 'age', 'cost_ii', 'cost_hiv', 
                                  'cost_road', 'cost_liver', 'cost_bcancer', 'policy', 'sex')
}

#################################################
#### summing costs                           ####
#################################################


aggregate_cost_R5_f <- aggregate_costs(female_R5_costs, "female")
aggregate_cost_R10_f <- aggregate_costs(female_R10_costs, "female")
aggregate_cost_R15_f <- aggregate_costs(female_R15_costs, "female")
aggregate_cost_extreme_f <- aggregate_costs(female_extreme_costs, "female")

aggregate_cost_R5_m <- aggregate_costs(male_R5_costs, "male")
aggregate_cost_R10_m <- aggregate_costs(male_R10_costs, "male")
aggregate_cost_R15_m <- aggregate_costs(male_R15_costs, "male")
aggregate_cost_extreme_m <- aggregate_costs(male_extreme_costs, "male")


results_costs <- bind_rows(aggregate_cost_R5_f, aggregate_cost_R10_f, aggregate_cost_R15_f, 
                           aggregate_cost_extreme_f, aggregate_cost_R5_m, aggregate_cost_R10_m, 
                           aggregate_cost_R15_m, aggregate_cost_extreme_m)
# order the ordinal variables
results_costs$policy <- ordered(results_costs$policy, c("base", "R5", "R10", "R15", "extreme"))
results_costs$wealth <- ordered(results_costs$wealth, c("poorest", "poorer", "middle", "richer", "richest"))

#################################################
#### Visualising the results                 ####
#################################################
# first a simple table
results_costs %>% group_by(policy) %>% summarise(sum(cost_hiv), sum(cost_ii), sum(cost_road), sum(cost_liver), sum(cost_bcancer))

results_costs %>% group_by(policy) %>% summarise(sum(cost_hiv) + sum(cost_ii) + sum(cost_road) + sum(cost_liver) + sum(cost_bcancer))

# remember with bar chart you have to sum all the columns first
hiv <- results_costs %>% group_by(policy, sex, wealth) %>% mutate(costs_saved = sum(cost_hiv, na.rm = TRUE))

ggplot(data = subset(hiv, policy != "extreme"), aes(x = wealth, y = costs_saved, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~sex)+
  theme_bw() +
labs(x = "Wealth quintiles", y = "R saved",
     title = "HIV cost saving") 

ii <- results_costs %>% group_by(policy, sex, wealth) %>% mutate(costs_saved = sum(cost_ii, na.rm = TRUE))

ggplot(data = subset(ii, policy != "extreme"), aes(x = wealth, y = costs_saved, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~sex)+
  theme_bw()+
  labs(x = "Wealth quintiles", y = "R saved",
       title = "II: cost saving") 

road <- results_costs %>% group_by(policy, sex, wealth) %>% mutate(costs_saved = sum(cost_road, na.rm = TRUE))

ggplot(data = subset(road, policy != "extreme"), aes(x = wealth, y = costs_saved, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~sex)+
  theme_bw() +
  labs(x = "Wealth quintiles", y = "R saved",
       title = "Road injury: cost saving") 

liver <- results_costs %>% group_by(policy, sex, wealth) %>% mutate(costs_saved = sum(cost_liver, na.rm = TRUE))

ggplot(data = subset(liver, policy != "extreme"), aes(x = wealth, y = costs_saved, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~sex)+
  theme_bw() +
  labs(x = "Wealth quintiles", y = "R saved",
       title = "Liver Cirrhosis: cost saving") 

bcancer <- results_costs %>% group_by(policy, sex, wealth) %>% mutate(costs_saved = sum(cost_bcancer, na.rm = TRUE))

ggplot(data = subset(bcancer), aes(x = wealth, y = costs_saved, fill = policy)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~sex)+
  theme_bw() +
  labs(x = "Wealth quintiles", y = "R saved",
       title = "Breast Cancer: cost saving") 
