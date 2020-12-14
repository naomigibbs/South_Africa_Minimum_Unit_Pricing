# South_Africa_Minimum_Unit_Pricing
This is the code for the MUP model for South Africa provided here for transparency.
It is best read in combination with the published paper and in particular the 
supplementary appendix which describes the methods used in more detail.
If anything is unclear or if you have suggestions for improvement please do get in touch.

The model is split into two distinct sections. Price to consumption, and consumption to harm.

Price to consumption

This consists of one R script file entitled [1_model_pc](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/1_model_pc.R). It requires data some of which is not publically available. 
Data sources include: South African Demographic and Health Survey 2016, International Alcohol Control Study and Euromonitor.

Consumption to harm

This part of the model consists of a number of script files which have to be run in the following order: [2_rr_and_pifs](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/2_rr_and_pifs.R), [3_allocating_wealth_quintile](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/3_allocating_wealth_quintile.R), [4_baseline_probabilities](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/4_baseline_probabilities.R), [5_policy_scenario_probabilities](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/5_policy_scenario_probabilities.R), [6_model_ch](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/6_model_ch.R), [7_health_costs](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/7_health_costs.R). The [8_model_ch](https://github.com/naomigibbs/South_Africa_Minimum_Unit_Pricing/blob/main/8_model_ch.R) is the file that generates the results, all the other script files consititute preparatory stages. It uses the consumption scenarios generated above and some additional datasources. Data sources include: Statistics South Africa population estimates, iHME data on deaths and prevelance of disease and injury conditions, General Household Survey data, relative risks drawn from the literature, costs drawn from the literature.
