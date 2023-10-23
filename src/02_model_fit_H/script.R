###########################################################################
#Load the cleaned and prepared case/covariate data
load('Cases_Data.RData')
#Load the neigbors matrix
load('W.RData')

#if you are using rstan locally on a multicore machine and have plenty of RAM to
#estimate your model in parallel, at this point execute
options(mc.cores = parallel::detectCores())
#and
rstan_options(auto_write = TRUE)
#which allows you to automatically save a bare version of a compiled Stan program 
#to the hard disk so that it does not need to be recompiled (unless you change it). 
#You will need to run these commands each time you load the rstan library.

###################
#Let's print an output .txt of the parameters used:
param_string <- sprintf("tree_depth: %s \n
chains: %s \n
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  rw_penalty: %s \n
  random_walk_prior_scale: %s ", tree_depth, n_chains,
                        scale_by_susceptible_pool, cases_type,
                        use_SGTF_data, final_week, rw_penalty,
                        random_walk_prior_scale)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)
################################################################################
################################################################################


Case_Rates_Data <- Case_Rates_Data[,c("areaCode", "Week", "areaName",
                                      "Population", "INDEX",
                                      "Pop_per_km2",
                                      "date_begin"                                   ,
                                      "Week_Cases"                                   ,
                                      "previous_week_cases"                          ,
                                      "next_week_cases"                              ,
                                      "Linelist_P2_PCR_Week_Cases",
                                      "Linelist_P2_PCR_Previous_Week_Cases",
                                      "Linelist_P2_PCR_Next_Week_Cases",
                                      "First_Episodes_Total",
                                      "prop_white_british"                           ,
                                      "prop_all_other_white",
                                      "prop_asian"                                   ,
                                      "prop_black_afr_car"                           ,
                                      "prop_mixed_multiple",
                                      "prop_other",
                                      "IMD_Average_score"                            ,
                                      "mean_age"                                     ,
                                      "prop_o65"                                     ,
                                      "Median_annual_income"                         ,
                                      "transit_stations_percent_change_from_baseline",
                                      "workplaces_percent_change_from_baseline"      ,
                                      "residential_percent_change_from_baseline"     ,
                                      "Alpha_proportion"                             ,
                                      "Delta_proportion"                             ,
                                      "Delta_AY_4_2_proportion"                      ,
                                      "Omicron_BA_1_proportion"                      ,
                                      "Omicron_BA_2_proportion"                      ,
                                      "Other_proportion"                             ,
                                      "Omicron_BA_4_proportion"                      ,
                                      "Omicron_BA_5_proportion"                      ,
                                      "s_Wild_prop"                                  ,
                                      "s_Alpha_prop"                                 ,
                                      "s_Delta_prop"                                 ,
                                      "s_Omicron_prop"                               ,
                                      "CCG_2019_Name"                                ,
                                      "NHS_registered_population"                    ,
                                      "Core_services_funding_by_weighted"            ,
                                      "Primary_care_funding_by_weighted"             ,
                                      "Specialised_services_by_weighted"             ,
                                      "unringfenced"                                 ,
                                      "contain_outbreak_management"                  ,
                                      "ASC_infection_control_fund"                   ,
                                      "ASC_workforce_capacity"                       ,
                                      "ASC_rapid_testing"                            ,
                                      "centroid_x"                                   ,
                                      "centroid_y")] 


 
  Case_Rates_Data <- filter(Case_Rates_Data, Week < final_week+1)
  #Missing Scotland and Wales in the variant proportions, so start by filtering out those:
  Case_Rates_Data <- Case_Rates_Data[which(!is.na(Case_Rates_Data$Alpha_proportion)),]
 
###################
#Begin STAN fit
#Define model

#Annoyingly, we need to have two cases of the script, for if we scale or not
if(scale_by_susceptible_pool){
  Stan_model_string_neighbours <- "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population

  real random_walk_prior; //What prior value to use in the prior distribution for the random walk sd
  real penalty_term; // Penalty applied to the random_walk magnitude

}
transformed data {
}
parameters {
  real<lower = 0, upper = 1> zetas;       //spatial kernel, but this can't be less than 0
  
  vector[T] beta_random_walk_steps; //We add in a random walk error term
  real<lower=0> sqrtQ; //Standard deviation of random walk

  real<lower = 0> phi; //Overdispersion parameter for the neg_binom_2
  
  real<lower  = 0, upper = 1> susc_scaling; //parameter for scaling the number of first episodes so far for aqcquired immunity
}
transformed parameters {
vector[T] beta_random_walk  = cumulative_sum(beta_random_walk_steps);
}
model {

y[,1] ~ neg_binomial_2( ((susc_scaling*susceptible_proxy[,1]).*(E[,1] + (zetas*E_neighbours[,1])))*exp( (beta_random_walk[1])), phi);  // extra noise removed removed: + theta[,i]
target += -penalty_term*fabs(beta_random_walk[1]);
for(i in 2:T){
  y[,i] ~ neg_binomial_2( ((susc_scaling*susceptible_proxy[,i]).*(E[,i] + (zetas*E_neighbours[,i])))*exp( (beta_random_walk[i])), phi);  // extra noise removed removed: + theta[,i]
  target += -penalty_term*fabs(beta_random_walk[i]);
}

  zetas ~ beta(1,1);
  phi ~ gamma(2,1);
  sqrtQ ~ gamma(1,random_walk_prior);
  susc_scaling ~ beta(1,1);

  for(i in 1:T){
  beta_random_walk_steps[i] ~ normal(0, sqrtQ);
  }

}
generated quantities {
real log_lik[N, T]; // Log-likelihood for each data point

  for (n in 1:N) {
    for (t in 1:T) {
      log_lik[n, t] = neg_binomial_2_lpmf(y[n, t] | ((susc_scaling*susceptible_proxy[n,t]) * (E[n,t]+ (zetas*E_neighbours[n,t])))*exp(beta_random_walk[t]), phi);
    }                                               
  }
}
"
} else {
  Stan_model_string_neighbours <- "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population

  real random_walk_prior; //What prior value to use in the prior distribution for the random walk sd
  real penalty_term; // Penalty applied to the random_walk magnitude
}
transformed data {
}
parameters {
  real<lower = 0, upper = 1> zetas;       //spatial kernel, but this can't be less than 0
  
  vector[T] beta_random_walk_steps; //We add in a random walk error term
  real<lower=0> sqrtQ; //Standard deviation of random walk

  real<lower = 0> phi; //Overdispersion parameter for the neg_binom_2
  real<lower  = 0, upper = 1> susc_scaling; //parameter for scaling the number of first episodes so far for aqcquired immunity
}
transformed parameters {
vector[T] beta_random_walk  = cumulative_sum(beta_random_walk_steps);
}
model {

y[,1] ~ neg_binomial_2( ((susceptible_proxy[,1]).*(E[,1] + (zetas*E_neighbours[,1])))*exp((beta_random_walk[1])), phi);  // extra noise removed removed: + theta[,i]
target += -penalty_term*fabs(beta_random_walk[1]);
for(i in 2:T){
  y[,i] ~ neg_binomial_2( ((susceptible_proxy[,i]).*(E[,i] + (zetas*E_neighbours[,i])))*exp((beta_random_walk[i]) ), phi);  // extra noise removed removed: + theta[,i]
  target += -penalty_term*fabs(beta_random_walk[i]);
}

  zetas ~ beta(1,1);
  phi ~ gamma(2,1);
  sqrtQ ~ gamma(1,random_walk_prior);

  for(i in 1:T){
  beta_random_walk_steps[i] ~ normal(0, sqrtQ);
  }

}
generated quantities {
real log_lik[N, T]; // Log-likelihood for each data point

  for (n in 1:N) {
    for (t in 1:T) {
      log_lik[n, t] = neg_binomial_2_lpmf(y[n, t] | ((susceptible_proxy[n,t]) * (E[n,t]+ (zetas*E_neighbours[n,t])))*exp(beta_random_walk[t]), phi);
    }                                               
  }
}
"
}

#######################################################################################################################
######################################################################################################################


T <- final_week-1;
N <- 306;
y <- array(0, dim = c(T,N))
E <- array(0, dim = c(T,N))
scale_by_recent_cases <- array(0, dim = c(T,N))

for(i in 2:final_week){
  j <- i-1
  
  Reduced_Data <- filter(Case_Rates_Data, Week == i)
  Reduced_Data <- Reduced_Data[order(Reduced_Data$INDEX),]
  
  if(cases_type == "Dashboard"){
    y[j,] = Reduced_Data$next_week_cases;
    E[j,] = pmax(Reduced_Data$Week_Cases, 0.000001);
  } else if(cases_type == "Linelist"){
    y[j,] = Reduced_Data$Linelist_P2_PCR_Next_Week_Cases;
    E[j,] = pmax(Reduced_Data$Linelist_P2_PCR_Week_Cases, 0.000001);
  } else{
    stop("Unrecognised cases data type, must be Dashboard or Linelist")
  }
  
  scale_by_recent_cases[j,] = Reduced_Data$First_Episodes_Total/Reduced_Data$Population
  
 }
  ###########


W_reduced <- W[Reduced_Data$INDEX, Reduced_Data$INDEX]

#Calculate E_neighbours
E_neighbours <- W_reduced%*%t(E)
susceptible_proportion_estimate <- 1 - scale_by_recent_cases #This is currently #of first episodes / population, so 1 - this is a rough proxy of S/N

if(scale_by_susceptible_pool){
  susceptible_proxy <- t(susceptible_proportion_estimate)
}else if(!scale_by_susceptible_pool){
  susceptible_proxy <- t(array(1, dim = c(T,N)))
}else{
  stop("Incompatible scale_by_susceptible_pool. Expected TRUE or FALSE")
}

#################################################################################

#################################################################################


stanfit = stan(model_code = Stan_model_string_neighbours,
               data=list(N=N,T=T,
                         y=t(y),
                         E=t(E),
                         E_neighbours = E_neighbours,
                         susceptible_proxy = susceptible_proxy,
                         random_walk_prior = random_walk_prior_scale,
                         penalty_term = rw_penalty),
               #warmup=warmup_iterations, 
               iter=total_iterations,
               chains =n_chains,
               control = list(max_treedepth = tree_depth));

#Make folder for outputs
dir.create("Outputs")

save(stanfit, file = 'Outputs/stanfit.RData')
save(N,T,y,E,E_neighbours, susceptible_proxy, 
       Case_Rates_Data, W_reduced, file = "Outputs/model_data.RData")
save(Stan_model_string_neighbours, file = "Outputs/stan_model_code.RData")


#Plot LOO-CV criterions
loo_stanfit <- rstan::loo(stanfit, moment_match = TRUE, cores = 1)
save(loo_stanfit, file = 'Outputs/loo_mm_stanfit.RData')

hold <- loo::pareto_k_table(loo_stanfit)

tg = gridExtra::tableGrob(hold)
h = grid::convertHeight(sum(tg$heights), "in", TRUE)
w = grid::convertWidth(sum(tg$widths), "in", TRUE)
ggplot2::ggsave("lareto_k_table_mm.png", tg, width=w, height=h)

#loo::pareto_k_values(loo_stanfit)
#loo::psis_n_eff_values(loo_stanfit)
png(file="loo_mm_PSIS.png",
    width=1440, height=1080, res = 150)
plot(loo_stanfit)
dev.off()

hold <- loo_stanfit$estimates
tg = gridExtra::tableGrob(hold)
h = grid::convertHeight(sum(tg$heights), "in", TRUE)
w = grid::convertWidth(sum(tg$widths), "in", TRUE)
ggplot2::ggsave("loo_mm_estimates.png", tg, width=w, height=h)

graphics.off()
