# moorea_lagoon_coral_mortality_2019
moorea_lagoon_coral_mortality_2019

This project analyzes patterns of coral mortality prevalence and severity for Pocillopora and Acropora corals in Moorea followning a marine heatwave in 2019. We evaluate relationships betweeen nitrogen enrichment, heats stress, and coral mortality prevalence and severity. 

Scripts:
*  **0_download_data.R**: downloads data from repository at the Environmental Data Initiative. Creates necessary directories to store data and model outputs in subsequent stripts. Summarizes data at the site level to make map in figure 7.
*  **1_temperature_fill.R**: Temperature loggers at two of our sites failed during marine heatwave in 2019. This script uses time series data going back to 2005 and linear models to impute the missing data from 2019. We also evaluate the effectiveness of this method by predicting data during a marine heatwave for which data are not missing. It is not necessary to run this script to run subsequent scriptst
*  **2a_run_prev_model_all.R**: Runs Bayesian hierarchical model with JAGS for mortality prevalence for all corals.
*  **2a_run_prev_model.R**: Runs Bayesian hierarchical model with JAGS for mortality prevalence. Runs model separately for each size class of corals. 
*  **2b_run_sev_model_all.R**: Runs Bayesian hierarchical model with JAGS for mortality severityfor all corals.
*  **3_crunch_model.R**: Summarizes model outputs, writes outputs to .csv files.
*  **4_mod_checks.R**: Performs posterior predictive checks. Plots obvserved data vs posterior predicted distribution using bayesplot package.
*  **5_plot_coefficients.R**: Makes plots of model coefficients 
*  **6_plot_interactions.R**: Makes plots of significant interactions 
*  **7_map_fig.R**: Plots map of island with sites colored by mortality prevalence and severity. Plots nutrient enrichment data. 
*  **binom_hierarchical.jags**: Bayesian hierarchical model evaluating the effects of nitrogen enrichment and heat stress on mortality prevalence and severity with a binomial response.

