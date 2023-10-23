load("stanfit.RData")
dir.create("Outputs")

################################################################################
################################################################################
T <- final_week - 1

#Print an output .txt of the parameters used:
param_string <- sprintf("tree_depth: %s \n
n_chains: %s \n
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  random_walk_prior_scale: %s \n
  rw_penalty: %s \n
  covariate: %s  ", tree_depth, n_chains, 
                        scale_by_susceptible_pool, cases_type,
                         use_SGTF_data, final_week,
                        random_walk_prior_scale, rw_penalty, covariate)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)

#Output summaries of the mixed chains for all parameters
# This lists mixed posterior, effective sample size, and Rhat (psrf) for every parameter
if(scale_by_susceptible_pool){
  main_summaries <- summary(stanfit, pars = c('sqrtQ', 'susc_scaling', 'betas', 'beta_random_walk_steps', 'zetas', 'phi', 'lp__'))
  write.csv(main_summaries$summary,"Outputs/main_summaries.csv", row.names = TRUE)

}else{
main_summaries <- summary(stanfit, pars = c('sqrtQ', 'betas', 'beta_random_walk_steps', 'zetas', 'phi', 'lp__'))
write.csv(main_summaries$summary,"Outputs/main_summaries.csv", row.names = TRUE)
}


#Theta summaries
theta_summaries <- summary(stanfit, pars = c('theta'))
write.csv(theta_summaries$summary,"Outputs/theta_summaries.csv", row.names = TRUE)


#######################################
#Plot posterior ranges for the beta coefficient parameter


beta_summaries <- main_summaries$summary
#Next we have a look at how good our fit is
betas_1_10 <- plot(stanfit, pars = 'betas')
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_1_10 <- betas_1_10 + scale_y_continuous(breaks = c(1),
                                labels = covariate) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Covariate coefficients")

png(file="Outputs\\betas_1_10.png",
    width=1440, height=1080, res = 150)
plot(betas_1_10)
dev.off()

#Here we plot some additional model convergence / analysis measures
sampler_params <- get_sampler_params(stanfit, inc_warmup = FALSE)
mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
write.table(as.character(mean_accept_stat_by_chain), file = "Outputs\\mean_accept_stat_by_chain.txt", sep = "\t",
            row.names = FALSE)

rhat_plot <- stan_rhat(stanfit)+ ggtitle(sprintf("%s parameters - %s have Rhat over 1.1", nrow(main_summaries$summary), sum(main_summaries$summary[, "Rhat"] > 1.1)))
ggsave(rhat_plot,
       filename = "Outputs\\stan_rhat.png")
rm(rhat_plot)



#Investigate how good the model fit is against real data
load("model_data.RData")
#y is the REAL "next week cases". T (time) x 306 (LTLAs)
#E is the "current" weeks cases.  i.e. E[2] = y[1]

  dir.create("Outputs//Goodness_of_fit")
  dir.create("Outputs//Goodness_of_fit//trajectories")


  ##This block rebuilds the model mean approximation
  model_betas <- as.numeric(get_posterior_mean(stanfit, pars = 'betas')[,(n_chains+1)])
  model_beta_random_walk <- as.numeric(get_posterior_mean(stanfit, pars = 'beta_random_walk')[,(n_chains+1)])
  model_beta_random_walk_steps <- as.numeric(get_posterior_mean(stanfit, pars = 'beta_random_walk_steps')[,(n_chains+1)])
  model_zetas <- as.numeric(get_posterior_mean(stanfit, pars = 'zetas')[,(n_chains+1)])

  model_theta <- as.numeric(get_posterior_mean(stanfit, pars = 'theta')[,(n_chains+1)])

  if(scale_by_susceptible_pool){
    model_susc_scale <- as.numeric(get_posterior_mean(stanfit, pars = 'susc_scaling')[,(n_chains+1)])
  }

  model_approx_y <- array(0, dim = c(T,306)) #


  for(i in 1:T){
    if(scale_by_susceptible_pool){
      model_approx_y[i,] <- as.numeric(((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (model_zetas *E_neighbours[,i]))))*exp(x[i,,]*model_betas + (model_beta_random_walk[i]) + model_theta)

    }else{
  model_approx_y[i,] <- as.numeric((susceptible_proxy[,i]*(E[i,] + (model_zetas *E_neighbours[,i]))))*exp(x[i,,]*model_betas + (model_beta_random_walk[i]) + model_theta)
    }
  }

  REAL_week_difference <- abs(y - E)
  MODEL_week_difference <- abs(model_approx_y - E)
  #############################################################################
  png(file="Outputs//Goodness_of_fit//log_week_to_week_difference.png",
      width=1440, height=1080, res = 150)
  plot(log(REAL_week_difference), log(MODEL_week_difference))
  abline(a=0, b=1, col = "red")
  dev.off()
  
  png(file="Outputs//Goodness_of_fit//week_to_week_difference.png",
      width=1440, height=1080, res = 150)
  plot(REAL_week_difference, MODEL_week_difference)
  abline(a=0, b=1, col = "red")
  dev.off()
  #############################################################################

  #Let's plot the random walks too.
  dates_hold <- unique(Case_Rates_Data[,c("Week","date_begin")])
  #Order it by week
  dates_hold <- dates_hold[order(dates_hold$Week,decreasing=FALSE),]
  random_walk_plot <- data.frame(date = as.Date(dates_hold$date_begin), random_walk_value = model_beta_random_walk)
  random_walk_steps_plot <- data.frame(date = as.Date(dates_hold$date_begin), random_walk_steps_value = model_beta_random_walk_steps)

  ggplot(data = random_walk_plot) +
    geom_line(aes(x = date, y = random_walk_value)) ->random_walk_p1

  png(file="Outputs//random_walk_values.png",
      width=1440, height=1080, res = 150)
  plot(random_walk_p1)
  dev.off()

  ggplot(data = random_walk_steps_plot) +
    geom_line(aes(x = date, y = random_walk_steps_value)) ->random_walk_steps_p1

  png(file="Outputs//random_walk_steps_values.png",
      width=1440, height=1080, res = 150)
  plot(random_walk_steps_p1)
  dev.off()


  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  AllDates <- sort(unique(Case_Rates_Data$date_begin))

###########################################################
  #Here we plot the fit to data in each LTLA
  #########################################################
  for(i in 1:(306)){

    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]

    dataHold <- data.frame(Date = rep(AllDates[2:T],2),
                           areaCode = rep(areaCode_plot, (T-1)*2),
                           areaName = rep(areaName_plot, (T-1)*2),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1))),
                           Cases = c(y[1:(T-1),j],model_approx_y[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2))
    )
    dataHold$date <- as.Date(dataHold$Date)

    ggplot(dataHold) +
      geom_line(aes(x = Date, y = Cases, color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold




    png(file=sprintf("Outputs//Goodness_of_fit//trajectories//trajectories_%s_%s.png", i, areaName_plot),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()

  }

  #############################################################################
# Make a plot showing the average error across all LTLAs for each week
  our_model_error <- abs(y - model_approx_y)

  Error_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                           Pop_Adj_Error = rep(0,(306*T)))
  for(i in 1:T){
    Error_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
    Error_data$Pop_Adj_Error[(((i-1)*306)+1):((i)*306)] <- our_model_error[i,]/LTLAs_by_Index$Population
  }

  total_error <- sum(Error_data$Pop_Adj_Error)

  ggplot(Error_data, aes(x=Time, y=Pop_Adj_Error, group = Time)) +
    geom_boxplot(alpha = 0.2) + ylab("Population Adjusted Error across all LTLAs") +
    ylim(c(0,plyr::round_any(max(Error_data$Pop_Adj_Error), 0.02, f=ceiling))) + ggtitle(sprintf("Total Error: %s",total_error)) + theme_bw() -> error_box

  png(file="Outputs//Goodness_of_fit/Boxplot_error.png",
      width=1440, height=1080, res = 150)
  plot(error_box)
  dev.off()

  #while (!is.null(dev.list()))  dev.off()

##############################################################################

  #Add a phi posterior plot
  phi_plot <- plot(stanfit, pars = 'phi')
  #ci_level: 0.8 (80% intervals)
  #outer_level: 0.95 (95% intervals)

  png(file="Outputs\\phi_posterior.png",
      width=1440, height=1080, res = 150)
  plot(phi_plot)
  dev.off()


  if(scale_by_susceptible_pool){
    susc_scaling <- plot(stanfit, pars = 'susc_scaling')
    #ci_level: 0.8 (80% intervals)
    #outer_level: 0.95 (95% intervals)

    png(file="Outputs\\susc_scale_posterior.png",
        width=1440, height=1080, res = 150)
    plot(susc_scaling)
    dev.off()

  }

  #Plot Random Walk with CrI
  random_walk_summaries <- summary(stanfit, pars = c('beta_random_walk'))
  random_walk_summaries <- random_walk_summaries$summary

  All_dates <- sort(unique(Case_Rates_Data$date_begin))
  rw_data <- data.frame( Week = All_dates, mean = random_walk_summaries[,1],
                         lower = random_walk_summaries[,4],
                         upper = random_walk_summaries[,8])

  ggplot(rw_data) +
    geom_line(aes(x = Week, y = mean)) +
    geom_ribbon(aes(x = Week, ymin = lower, ymax = upper), alpha = 0.4) +
    ylab("Random Walk term") + theme_classic() -> rw_plot

  png(file="Outputs\\random_walk_CrI.png",
      width=1440, height=1080, res = 150)
  plot(rw_plot)
  dev.off()

# ####################################################################
# ####################################################################

  #Some additional posterior plots for the SI:
  #Output ALL traceplots
  dir.create("Outputs\\Posterior_plots")


  #Save a plot of the beta trajectories
  zeta_trajectories1 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',1:30)), nrow = 5)
  ggsave(filename = "zeta1_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories1,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories2 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',31:60)), nrow = 5)
  ggsave(filename = "zeta2_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories2,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',61:90)), nrow = 5)
  ggsave(filename = "zeta3_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories3,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories4 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',91:120)), nrow = 5)
  ggsave(filename = "zeta4_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories4,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories5 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',121:150)), nrow = 5)
  ggsave(filename = "zeta5_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories5,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories6 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',151:180)), nrow = 5)
  ggsave(filename = "zeta6_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories6,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories7 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',181:210)), nrow = 5)
  ggsave(filename = "zeta7_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories7,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories8 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',211:240)), nrow = 5)
  ggsave(filename = "zeta8_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories8,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories9 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',241:270)), nrow = 5)
  ggsave(filename = "zeta9_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories9,
         dpi=300, height=8, width=11, units="in")

  zeta_trajectories10 <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',271:300)), nrow = 5)
  ggsave(filename = "zeta10_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = zeta_trajectories10,
         dpi=300, height=8, width=11, units="in")

  #################
  if(scale_by_susceptible_pool){
  other_trajectories <- rstan::traceplot(stanfit, pars=c('phi', 'susc_scaling',
                                                         'sqrtQ', 'betas'), nrow = 2)
  new_names <- c('phi', 'lambda', 'Q', 'betas')

  levels(other_trajectories$data$parameter) <- new_names # change all names, if new_names is character vector of correct length

  ggsave(filename = "other_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = other_trajectories,
         dpi=300, height=8, width=11, units="in")
  } else{
    other_trajectories <- rstan::traceplot(stanfit, pars=c('phi', 
                                                           'sqrtQ', 'betas'), nrow = 2)
    new_names <- c('phi', 'Q', 'betas')
    
    levels(other_trajectories$data$parameter) <- new_names # change all names, if new_names is character vector of correct length
    
    ggsave(filename = "other_trajectories.png",
           path = 'Outputs\\Posterior_plots', plot = other_trajectories,
           dpi=300, height=8, width=11, units="in")
}
  ###############
  #theta
  theta_trajectories1 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',1:30)), nrow = 5)
  ggsave(filename = "theta1_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories1,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories2 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',31:60)), nrow = 5)
  ggsave(filename = "theta2_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories2,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',61:90)), nrow = 5)
  ggsave(filename = "theta3_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories3,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories4 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',91:120)), nrow = 5)
  ggsave(filename = "theta4_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories4,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories5 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',121:150)), nrow = 5)
  ggsave(filename = "theta5_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories5,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories6 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',151:180)), nrow = 5)
  ggsave(filename = "theta6_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories6,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories7 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',181:210)), nrow = 5)
  ggsave(filename = "theta7_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories7,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories8 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',211:240)), nrow = 5)
  ggsave(filename = "theta8_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories8,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories9 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',241:270)), nrow = 5)
  ggsave(filename = "theta9_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories9,
         dpi=300, height=8, width=11, units="in")

  theta_trajectories10 <- rstan::traceplot(stanfit, pars=c(sprintf('theta[%s]',271:300)), nrow = 5)
  ggsave(filename = "theta10_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = theta_trajectories10,
         dpi=300, height=8, width=11, units="in")



########################
  #Random walk
  rw_trajectories1 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',1:30)), nrow = 5)
  new_names <- c(sprintf('X[%s]',1:30))
  levels(rw_trajectories1$data$parameter) <- new_names
  ggsave(filename = "rw1_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = rw_trajectories1,
         dpi=300, height=8, width=11, units="in")

  rw_trajectories2 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',31:60)), nrow = 5)
  new_names <- c(sprintf('X[%s]',31:60))
  levels(rw_trajectories2$data$parameter) <- new_names
  ggsave(filename = "rw2_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = rw_trajectories2,
         dpi=300, height=8, width=11, units="in")

  rw_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',61:90)), nrow = 5)
  new_names <- c(sprintf('X[%s]',61:90))
  levels(rw_trajectories3$data$parameter) <- new_names
  ggsave(filename = "rw3_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = rw_trajectories3,
         dpi=300, height=8, width=11, units="in")

  ######################
  #Additionals, 6 + 6 + 5 = 17

  add_traject <- rstan::traceplot(stanfit, pars=c(sprintf('zetas[%s]',301:306),
                                                  sprintf('theta[%s]',301:306),
                                                  sprintf('beta_random_walk_steps[%s]',91:95)), nrow = 3)
  new_names <- c(sprintf('zetas[%s]',301:306),
                 sprintf('theta[%s]',301:306),
                 sprintf('X[%s]',91:95))
  levels(add_traject$data$parameter) <- new_names
  ggsave(filename = "additional_trajectories.png",
         path = 'Outputs\\Posterior_plots', plot = add_traject,
         dpi=300, height=8, width=11, units="in")

  #############################
  #We also output all the posterior estimates in a format that can be pasted straight into
  #the SI table in LaTeX format
  #LaTeX Format "Parameter - LTLA name & 0.45 (0.33 - 0.67) \\ \n"
  
  zeta_summaries <- summary(stanfit, pars = c('zetas'))
  zeta_summaries <- zeta_summaries$summary
  #zeta_summaries[,"2.5%"]
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  
  vector_col <- rep("", 306)  # Create a vector filled with NA
  vector_col[seq(2, 306, by = 2)] <- "\\rowcolor[gray]{.9}"  # Set even-indexed elements
  
  zeta_text <- sprintf(c("%s $\\zeta_{%s}$ - %s & %s (%s - %s) \\\\ "), vector_col, 1:306, LTLAs_by_Index$areaName, signif(zeta_summaries[,"mean"],3), signif(zeta_summaries[,"2.5%"],3), signif(zeta_summaries[,"97.5%"],3))
  fileConn<-file("zeta_posterior.txt")
  writeLines(zeta_text, fileConn)
  close(fileConn)
  
  theta_summaries <- summary(stanfit, pars = c('theta'))
  theta_summaries <- theta_summaries$summary
  
  theta_text <- sprintf(c("%s $\\theta_{%s}$ - %s & %s (%s - %s) \\\\ "), vector_col, 1:306, LTLAs_by_Index$areaName, signif(theta_summaries[,"mean"],3), signif(theta_summaries[,"2.5%"],3), signif(theta_summaries[,"97.5%"],3))
  fileConn<-file("theta_posterior.txt")
  writeLines(theta_text, fileConn)
  close(fileConn)
  
  rw_summaries <- summary(stanfit, pars = c('beta_random_walk_steps'))
  rw_summaries <- rw_summaries$summary
  
  vector_col <- rep("", 95)  # Create a vector filled with NA
  vector_col[seq(2, 95, by = 2)] <- "\\rowcolor[gray]{.9}"  # Set even-indexed elements
  
  rw_text <- sprintf(c("%s $X_{%s}$  & %s (%s - %s) \\\\ "), vector_col, 1:95,  signif(rw_summaries[,"mean"],3), signif(rw_summaries[,"2.5%"],3), signif(rw_summaries[,"97.5%"],3))
  fileConn<-file("rw_posterior.txt")
  writeLines(rw_text, fileConn)
  close(fileConn)
  
  ################# PLOTTING THE VARIATION IN THETA AND ZETA WITH BOXPLOTS
  zeta_means <- zeta_summaries[,"mean"]
  theta_means <- theta_summaries[,"mean"]

  
  png(file="Outputs\\boxplot_theta_zeta.png",
      width=1280, height=960, res = 150)
  boxplot(list(zeta = zeta_means, theta = theta_means), 
          main = "Mean values for zeta and theta across 306 LTLAs",
          ylab = "Values",
          col = "#ffc914",  # Fill color of the boxes
          border = "#e4572e",    # Border color of the boxes
          #notch = TRUE,       # Add notches
          horizontal = FALSE  # Vertical boxplot
  ) 
  dev.off()
  
  ###############
  graphics.off()