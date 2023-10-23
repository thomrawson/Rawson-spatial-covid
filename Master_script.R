#This script walks through the steps necessary to run the analysis pipeline.

#This is an orderly store, and requires orderly to be installed to run:
install.packages("orderly")

###################################################################
#We run task 01, which loads in multiple different data sources, and outputs a complete data frame and some additional files.
Task01 <- orderly::orderly_run("01_Prepare_Data")
#Commit this task
orderly::orderly_commit(Task01)

###################################################################
#Now fit the model with task 02.
#There is one "main analysis" labelled as Model A
#There are 11 additional sensitivity analyses, labelled B-L
#Here we launch just the main analysis, but sensitivities can be launched in the same manner.

#The model results as presented can be launched like so:
Task02 <- orderly::orderly_run("02_model_fit_A",
                               parameters = list(n_chains = 16))
orderly::orderly_commit(Task02)
#However, in practice this will not be able to run on a standard home computer.
#This should instead be launched via a HPC.
#In the meantime, an illustrative version is presented below, but will display poor convergence:

Task02_illustrative <- orderly::orderly_run("02_model_fit_A",
                                            parameters = list(tree_depth = 13,
                                                              total_iterations = 600,
                                                              output_loo = FALSE))
orderly::orderly_commit(Task02_illustrative)

###################################################################
#Now run task 03, which will output some model summaries.

Task03 <- orderly::orderly_run("03_model_summaries_A",
                               parameters = list(n_chains = 16))
orderly::orderly_commit(Task03)

#Illustrative version:
Task03_illustrative <- orderly::orderly_run("03_model_summaries_A",
                                            parameters = list(tree_depth = 13,
                                                              total_iterations = 600))
orderly::orderly_commit(Task03_illustrative)

##################################################################
#Before outputting the manuscript figures, we perform a computationally intensive task separately.
#Task 04 calculates the additional uncertainty in the model fit estimates due to the Negative Binomial distribution
#This uncertainty will eventually be plotted in the following task
#This should again be run via a HPC if available, though an illustrative version is again supplied as well.

Task04 <- orderly::orderly_run("04_nb_CI_output",
                               parameters = list(n_chains = 16))
orderly::orderly_commit(Task04)

#Illustrative version:
Task04_illustrative <- orderly::orderly_run("04_nb_CI_output",
                                            parameters = list(tree_depth = 13,
                                                              total_iterations = 600))
orderly::orderly_commit(Task04_illustrative)

##################################################################
#Task 05 then produces all four figures from the main manuscript

Task05 <- orderly::orderly_run("05_manuscript_figures",
                               parameters = list(n_chains = 16))
orderly::orderly_commit(Task05)

#Illustrative version:
Task05_illustrative <- orderly::orderly_run("05_manuscript_figures",
                                            parameters = list(tree_depth = 13,
                                                              total_iterations = 600))
orderly::orderly_commit(Task05_illustrative)

##################################################################
#One many similarly run task 06 for additional figures from the SI, however this will also require running
#task 02_model_fit_univariate, for each of the sixteen covariates separately
#It will also require:
#running task 02_model_fit_A with parameter "use_SGTF_data = FALSE"
#running task 02_model_fit_A with parameter "cases_type = "Dashboard" "
#running task 02_model_fit_A with parameters "cases_type = "Dashboard" AND "use_SGTF_data = FALSE"

covariates <- c("prop_asian", "prop_black_afr_car", "prop_other",
                "IMD_Average_score", "prop_o65", "Pop_per_km2",
                "Median_annual_income", "workplaces_percent_change_from_baseline",
                "residential_percent_change_from_baseline",
                "transit_stations_percent_change_from_baseline",
                "s_Alpha_prop", "s_Delta_prop", "s_Omicron_prop",
                "unringfenced", "contain_outbreak_management",
                "ASC_infection_control_fund")

for(i in covariates){
  orderly::orderly_run("02_model_fit_univariate",
                       parameters = list(n_chains = 16,
                                         covariate = covariates[i]))
}

orderly::orderly_run("02_model_fit_A",
                     parameters = list(n_chains = 16,
                                       cases_type = "Dashboard"))
orderly::orderly_run("02_model_fit_A",
                     parameters = list(n_chains = 16,
                                       use_SGTF_data = FALSE))
orderly::orderly_run("02_model_fit_A",
                     parameters = list(n_chains = 16,
                                       use_SGTF_data = FALSE,
                                       cases_type = "Dashboard"))

Task06 <- orderly::orderly_run("06_SI_figs",
                               parameters = list(n_chains = 16))
orderly::orderly_commit(Task06)

