# South_Africa_Minimum_Unit_Pricing
The code for the MUP model for South Africa.
Code is provided here for transparency. It is best read in combination with the published paper and in particular the 
supplementary appendix which describes the methods used in more detail.
If anything is unclear or if you have suggestions for improvement please do get in touch.

The model is split into two distinct sections. Price to consumption, and consumption to harm.

Price to consumption

This consists of one R script file entitled "model_pc"
It requires data some of which is not publically available. 
Data sources include: South African Demographic and Health Survey 2016, International Alcohol Control Study and Euromonitor.

Consumption to harm

This part of the model consists of a number of script files which have to be run in the following order:
"rr_and_pifs"
"allocating_wealth_quintile"
"baseline_probabilities"
"policy_scenario_probabilities"
"model_ch"
"health_costs"
The "model_ch" is the file that generates the results, all the other script files consititute preparitry stages so if you are interested
in the overarching way the model works we recommend you start with this file.
It uses the consumption scenarios generated above and some additional datasources.
Data sources include: Statistics South Africa population estimates, iHME data on deaths and prevelance of disease and injury conditions,
General Household Survey data, relative risks drawn from the literature.
