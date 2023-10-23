#This script plots the figures used in the main manuscript
load("stanfit.RData")
load("model_data.RData")
dir.create("Case_Outputs")
dir.create("Case_Outputs\\LTLA_model_fits")
dir.create("Case_Outputs\\Final_Figures")

################################################################################
################################################################################
T <- final_week - 1

#Let's print an output .txt of the parameters used:
param_string <- sprintf("tree_depth: %s \n
n_chains: %s \n
total_iterations: %s \n
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  random_walk_prior_scale: %s \n
  rw_penalty: %s \n
  print_extra_gof:  %s ", tree_depth, n_chains, total_iterations,
                        scale_by_susceptible_pool, cases_type,
                         use_SGTF_data, final_week,
                        random_walk_prior_scale, rw_penalty, print_extra_gof)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)

############################################################################
#FIGURE 1
############################################################################
#This figure will show regional variation in certain covariates
#IMD and proportion white british

################################################################################
#Calculate the quintiles
Case_Rates_Data <- Case_Rates_Data %>%
  mutate(IMD_quintile = ntile(IMD_Average_score, 5))


sort(unique(Case_Rates_Data$IMD_Average_score))
#26.79 - 45 is the highest (5)
# 21.06 - 26.79 (4)
#16.27 - 21.06 (3)
#12.31 - 16.27 (2)
#5.54 - 12.31 is the lowest IMD (1)
################################################################
#Make a Case rates column:
Case_Rates_Data$Case_Rates <- Case_Rates_Data$Week_Cases/Case_Rates_Data$Population

quintile_data <- Case_Rates_Data %>%
  dplyr::group_by(IMD_quintile, date_begin) %>%
  dplyr::summarise(
    mean_Case_Rates = mean(Case_Rates),
    lower_Case_Rates = quantile(Case_Rates, 0.025),
    upper_Case_Rates = quantile(Case_Rates, 0.975)
  ) %>%
  ungroup()

quintile_data$IMD_quintile <- as.factor(quintile_data$IMD_quintile)

custom_palette <- c("#17bebb", "#8BC061", "#FFC107", "#F28220", "#e54339")

ggplot(quintile_data, aes(x = date_begin, y = mean_Case_Rates, group = IMD_quintile, fill = IMD_quintile)) +
  geom_line(aes(color = IMD_quintile), size = 1) +
  geom_ribbon(aes(ymin = lower_Case_Rates, ymax = upper_Case_Rates, fill = IMD_quintile), alpha = 0.09) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-03-01'))) +
  labs(x = "Date", y = "Average Case Rates", color = "IMD Quintile") +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Average COVID-19 Weekly Case Rates in UK LTLAs \nby IMD Quintile ") +
  scale_color_manual(name = "IMD Quintile", values = custom_palette,
                     labels = c("1 - Lowest \nvalues",
                                "2", "3", "4", "5 - Highest \nvalues")) + 
  scale_fill_manual(name = "IMD Quintile", values = custom_palette,
                    labels = c("1 - Lowest \nvalues",
                               "2", "3", "4", "5 - Highest \nvalues")) -> quintile_plot

#Now do the same for prop. white british
################################################################################
Case_Rates_Data <- Case_Rates_Data %>%
  mutate(prop_white_quintile = ntile(prop_white_british, 5))

sort(unique(Case_Rates_Data$prop_white_british))
#96 - 100 is the highest (5)
# 92.2 - 96 (4)
#86.6- 92.2 (3)
#75.9 - 86.6 (2)
#15.4 - 75.9 is the lowest IMD (1)
################################################################

prop_white_data <- Case_Rates_Data %>%
  dplyr::group_by(prop_white_quintile, date_begin) %>%
  dplyr::summarise(
    mean_Case_Rates = mean(Case_Rates),
    lower_Case_Rates = quantile(Case_Rates, 0.025),
    upper_Case_Rates = quantile(Case_Rates, 0.975)
  ) %>%
  ungroup()

prop_white_data$prop_white_quintile <- as.factor(prop_white_data$prop_white_quintile)

ggplot(prop_white_data, aes(x = date_begin, y = mean_Case_Rates, group = prop_white_quintile, fill = prop_white_quintile)) +
  geom_line(aes(color = prop_white_quintile), size = 1) +
  geom_ribbon(aes(ymin = lower_Case_Rates, ymax = upper_Case_Rates, fill = prop_white_quintile), alpha = 0.09) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-03-01'))) +
  labs(x = "Date", y = "Average Case Rates", color = "IMD Quintile") +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Average COVID-19 Weekly Case Rates in UK LTLAs \nby Proportion White British Quintile ") +
  scale_color_manual(name = "Prop. White \nBritish Quintile", values = custom_palette) + 
  scale_fill_manual(name = "Prop. White \nBritish Quintile", values = custom_palette) -> prop_white_quintile_plot



#Then I want the regional shading plots
############################################################################
####################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

#IMD average score is the same for every date
Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, IMD_Average_score, prop_white_british) -> fig1_data

fig1_data <- fig1_data[order(fig1_data$INDEX),]

Boundaries_reduced$IMD_Average_score <- as.numeric(fig1_data$IMD_Average_score) 
Boundaries_reduced$prop_white_british <- as.numeric(fig1_data$prop_white_british) 

#PLOT 
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = IMD_Average_score)) +
  scale_fill_gradientn(
    colours = c("#17bebb", "#ffc914", "#e4572e"),
    values = scales::rescale(c(min(Boundaries_reduced$IMD_Average_score), median(Boundaries_reduced$IMD_Average_score), max(Boundaries_reduced$IMD_Average_score)))
  ) +
  ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Average \nIMD Score") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm")) -> england_IMD

#And London
ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = IMD_Average_score), show.legend = FALSE) +
  scale_fill_gradientn(
    colours = c("#17bebb", "#ffc914", "#e4572e"),
    values = scales::rescale(c(min(Boundaries_reduced$IMD_Average_score), median(Boundaries_reduced$IMD_Average_score), max(Boundaries_reduced$IMD_Average_score)))
  ) +
  labs(fill = "Average \nIMD Score") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> london_IMD



ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_white_british)) +
  scale_fill_gradientn(
    colours = c("#17bebb", "#ffc914", "#e4572e"),
    values = scales::rescale(c(min(Boundaries_reduced$prop_white_british), median(Boundaries_reduced$prop_white_british), max(Boundaries_reduced$prop_white_british)))
  ) +
  ggtitle("White British Proportion of LTLA Population") +
  labs(fill = "Proportion \nWhite British") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm")) -> england_prop_white  


# Fig1_plot <- plot_grid(england_IMD, england_prop_white, 
#                        nrow = 1, labels = c("A", "B"), align = "v", axis = "bt") +
#   theme(plot.background = element_rect(fill = "white"))

# ggsave(filename = "spatial_covariates.png",
#        path = 'Case_Outputs', plot = Fig1_plot,
#        dpi=300, height=6.31, width=11, units="in")
# 
# ggsave(filename = "spatial_covariates.tiff",
#        path = 'Case_Outputs', plot = Fig1_plot,
#        dpi=300, height=6.31, width=11, units="in")
# 
# ggsave(filename = "spatial_covariates.pdf",
#        path = 'Case_Outputs', plot = Fig1_plot,
#        dpi=300, height=6.31, width=11, units="in")


Fig1_plot <- plot_grid(england_IMD, england_prop_white, 
                       ncol = 1, labels = c("A", "C"), align = "v", axis = "bt") 

combined_quintile <- plot_grid(quintile_plot+xlab(""), prop_white_quintile_plot,
                               ncol = 1, labels = c("B","D"),
                               align = "v", axis = "tb")

Total_Fig_plot <- plot_grid(Fig1_plot, combined_quintile,
                            nrow = 1,
                            #labels = c("A","B",""),
                            rel_widths = c(1,0.8))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Variation in socio-demographic factors by LTLA and the respective stratification of COVID-19 case rates",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 20
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, Total_Fig_plot,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.06, 1)
) +
  theme(plot.background = element_rect(fill = "white", color = "white")) -> Total_Fig1_plot

ggsave(filename = "Final_Fig1.png",
       path = 'Case_Outputs\\Final_Figures', plot = Total_Fig1_plot,
       dpi=300, height=15, width=14, units="in")

ggsave(filename = "Final_Fig1.tiff",
       path = 'Case_Outputs\\Final_Figures', plot = Total_Fig1_plot,
       dpi=300, height=15, width=14, units="in")

ggsave(filename = "Final_Fig1.pdf",
       path = 'Case_Outputs\\Final_Figures', plot = Total_Fig1_plot,
       dpi=300, height=15, width=14, units="in")

#########################################
#FIGURE 2
#This figure shows model fit to data
#####################################
#We've already prepared the dataframe needed in task 04
load("CI_data_outputs.RData")

##################################

grey_lines <- c(
  "2020-06-01", ## End of first stay-at-home-order
  "2020-11-05", ## Start of second stay at home order
  "2020-12-02", ## End of second stay-at-home order 
  "2021-01-06", ## Start of third stay-at-home order
  "2021-03-08", ## End of third stay-at-home order (step 1)
  "2021-07-19" ## End of steps, no more legal restrictions
)

variant_dates <- c( #Dates that variants first appear in our dataset
  "2020-07-26", #ALPHA
  "2021-03-28", #DELTA
  "2021-09-12"  #OMICRON
)

lockdown_shades <- data.frame(date_start = as.Date(c("2020-04-01", grey_lines)),
                              date_end = as.Date(c(grey_lines, '2022-04-01' )),
                              rect_bottom = rep(775, 7),
                              rect_top = rep(800, 7),
                              rect_col = c("red", "orange", "red", "orange", "red", "orange", "green"))

######################################

ggplot() +
  geom_line(data = england_daily_total, aes(x = date, y = Model_mean / 1000,
                                            color = "Model Fit"), size = 1) +
  geom_ribbon(data = england_daily_total,
              aes(x = date, ymin = Model_lower / 1000, ymax = Model_upper / 1000,
                  fill = "Model Fit"), alpha = 0.3, show.legend = TRUE) +
  theme_classic() + ylab('Weekly New Cases (thousands)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = max(england_daily_total$Model_upper/1000)-30,
                                        ymax = max(england_daily_total$Model_upper/1000)),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  
  geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Weekly total Pillar 2 and PCR cases reported in England ") +
  geom_point(data = england_daily_total, aes(x = date, y = Real_Cases / 1000,
                                             color = "Data"),
             alpha = 0.7, shape = 18) +
  scale_color_manual(name = "", values = c("Data" = "black", "Model Fit" = "#17bebb")) +
  scale_fill_manual(name = "", values = c("Data" = "white", "Model Fit" = "#17bebb")) +   # Adjust fill for the ribbon legend
  #scale_linetype_manual(name = "", values = c("Data" = 0, "Model Fit" = 1)) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(18, NA),  # Remove point from Model Fit
        linetype = c(0, 1)  # Remove line from Data
      )
    )
  ) -> england_plot

######################################


png(file="england_fit.png",
    width=1440, height=1080, res = 150)
plot(england_plot)
dev.off()

LTLA_names <- unique(Model_fit_data$areaName)

for(i in 1:length(LTLA_names)){
  
  areaName_hold <- LTLA_names[i]
  
  ggplot(data = filter(Model_fit_data, areaName == areaName_hold), aes(x= date, y = Model_mean )) +
    geom_line(size = 1, color = "#17bebb") + 
    geom_ribbon(aes(ymin = Model_lower, ymax = Model_upper), alpha = 0.3, fill = "#17bebb", show.legend = FALSE) + 
    theme_classic() + ylab('Weekly Cases') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    coord_cartesian(xlim = c(as.Date('2020-05-01'),as.Date('2022-04-01'))) +
    #labs(color = "Strategy") +
    geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.9, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab(NULL) +
    theme(axis.text=element_text(size=rel(1.2)),
          axis.title=element_text(size=rel(1.3)),
          legend.text = element_text(size=rel(1.2)),
          legend.title = element_text(size=rel(1.3))) +
    ggtitle(sprintf("Weekly cases reported in %s", areaName_hold)) +
    geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases), alpha = 0.7, shape = 18,
               color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\LTLA_model_fits\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}

dir.create("Case_Outputs\\LTLA_infection_rates")

for(i in 1:length(LTLA_names)){
  
  areaName_hold <- LTLA_names[i]
  
  #Let's just do a one off, for example, of Bolton
  ggplot(data = filter(Model_fit_data, areaName == areaName_hold), aes(x= date, y = Model_mean/Population )) +
    geom_line(size = 1, color = "#17bebb") + 
    geom_ribbon(aes(ymin = Model_lower/Population, ymax = Model_upper/Population), alpha = 0.3, fill = "#17bebb", show.legend = FALSE) + 
    theme_classic() + ylab('Weekly Case rates') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    coord_cartesian(xlim = c(as.Date('2020-05-01'),as.Date('2022-04-01'))) +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab(NULL) +
    theme(axis.text=element_text(size=rel(1.2)),
          axis.title=element_text(size=rel(1.3)),
          legend.text = element_text(size=rel(1.2)),
          legend.title = element_text(size=rel(1.3))) +
    ggtitle(sprintf("Weekly case rates reported in %s", areaName_hold)) +
    geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases/Population), alpha = 0.7, shape = 18,
               color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\LTLA_infection_rates\\case_rate_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}


####################################################
#Output plots for LTLA_A, and LTLA_B to compare
LTLA_A_index <- 267
LTLA_B_index <- 67
LTLA_A_name <- LTLA_names[LTLA_A_index]
LTLA_B_name <- LTLA_names[LTLA_B_index]
LTLA_A_data <- filter(Model_fit_data, areaName == LTLA_A_name)
LTLA_B_data <- filter(Model_fit_data, areaName == LTLA_B_name)

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.029, ymax = 0.03),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  geom_line(data = LTLA_A_data, aes(x = date, y = Model_mean / Population,
                                            color = "Model Fit"), size = 1, show.legend = FALSE) +
  geom_ribbon(data = LTLA_A_data,
              aes(x = date, ymin = Model_lower / Population, ymax = Model_upper / Population,
                  fill = "Model Fit"), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('Weekly Case Rates') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  ylim(c(0,0.03)) +
  #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text.y = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3))) +
  ggtitle(sprintf("%s ", LTLA_A_name)) +
  geom_point(data = LTLA_A_data, aes(x = date, y = Real_Cases / Population,
                                             color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#17bebb")) +
  scale_fill_manual(values = c("Model Fit" = "#17bebb")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_A_plot

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.029, ymax = 0.03),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  geom_line(data = LTLA_B_data, aes(x = date, y = Model_mean / Population,
                                    color = "Model Fit"), size = 1, show.legend = FALSE) +
  geom_ribbon(data = LTLA_B_data,
              aes(x = date, ymin = Model_lower / Population, ymax = Model_upper / Population,
                  fill = "Model Fit"), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  ylim(c(0,0.03)) +
  #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text.y = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3))) +
  ggtitle(sprintf("%s ", LTLA_B_name)) +
  geom_point(data = LTLA_B_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#17bebb")) +
  scale_fill_manual(values = c("Model Fit" = "#17bebb")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_B_plot


##########################

#Now stick it all together in a cowplot.

LTLAs_plot <- plot_grid(LTLA_A_plot, LTLA_B_plot, labels = c('B', 'C'))
Fig2_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "Final_Fig2.png",
       path = 'Case_Outputs\\Final_Figures', plot = Fig2_plot,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "Final_Fig2.tiff",
       path = 'Case_Outputs\\Final_Figures', plot = Fig2_plot,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "Final_Fig2.pdf",
       path = 'Case_Outputs\\Final_Figures', plot = Fig2_plot,
       dpi=300, height=11, width=11, units="in")

################################################################################
#FIGURE 3
################################################################################
#This figure shows the covariate, beta, coefficient CrIs. 

labels_hold <- c(
  "1) Proportion Asian", "2) Proportion Black Afr/Car",
  "3) Proportion Other Ethnicity", "4) IMD Average Score",
  "5) Proportion Over Age 65", "6) Population per km^2",
  "7) Median Annual Income", "8) Visits To Workplace",
  "9) Time At Home", "10) Visits To Transit Stations")


#Next we have a look at how good our fit is
betas_1_10 <- stan_plot(stanfit, pars = sprintf('betas[%s]',1:10),
                        fill_color = "#2a2a2a",
                        show_density = FALSE,
                        point_est = "mean",
                        ci_level = 0.95)
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
  betas_1_10 + scale_y_continuous(breaks = c(10:1),
                                              labels =labels_hold) +
  geom_vline(xintercept = 0, color = "darkgrey", lty = 5, size = 1) +
    ggtitle("Population Coefficients") +
    xlim(c(-0.4, 0.7)) +
  #theme(axis.text.y = element_text(angle = 35, hjust = 1, vjust = 0)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      #axis.text.y = element_markdown(size = rel(1.2)),
      axis.text.y = element_text(size = rel(1.2)),
      axis.title.y = element_blank(),
      legend.text = element_text(size = rel(1.2)),
      legend.title = element_text(size = rel(1.3)),
      axis.text.x = element_blank()
    ) -> betas_1_10

  betas_11_13 <- stan_plot(stanfit, pars = sprintf('betas[%s]',11:13),
                           fill_color = "#2a2a2a",
                           show_density = FALSE,
                           point_est = "mean",
                           ci_level = 0.95)
  #ci_level: 0.8 (80% intervals)
  #outer_level: 0.95 (95% intervals)
  betas_11_13 + scale_y_continuous(breaks = c(3:1),
                                  labels = c("11) Alpha Proportion", "12) Delta Proportion", "13) Omicron Proportion")) +
    geom_vline(xintercept = 0, color = "darkgrey", lty = 5, size = 1) +
    ggtitle("SARS-CoV-2 Variant Coefficients") +
    xlim(c(-0.4, 0.7)) +
    #theme(axis.text.y = element_text(angle = 35, hjust = 1, vjust = 0)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      axis.text.y = element_text(size = rel(1.2)),
      axis.title.y = element_blank(),
      axis.text.x = element_blank()
    )  -> betas_11_13
  
  betas_14_16 <- stan_plot(stanfit, pars = sprintf('betas[%s]',14:16),
                           fill_color = "#2a2a2a",
                           show_density = FALSE,
                           point_est = "mean",
                           ci_level = 0.95)
  #ci_level: 0.8 (80% intervals)
  #outer_level: 0.95 (95% intervals)
  betas_14_16 + scale_y_continuous(breaks = c(3:1),
                                   labels = c("14) Unringfenced",
                                              "15) Outbreak Management", "16) ASC infection control")) +
    geom_vline(xintercept = 0, color = "darkgrey", lty = 5, size = 1) +
    ggtitle("Funding Coefficients") +
    xlim(c(-0.4, 0.7)) +
    #theme(axis.text.y = element_text(angle = 35, hjust = 1, vjust = 0)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      axis.text.y = element_text(size = rel(1.2)),
      axis.text.x = element_text(size = rel(1.4)),
      axis.title.y = element_blank()
    )  -> betas_14_16
  
  
  Fig3_plot <- plot_grid(betas_1_10, betas_11_13, betas_14_16,
                         nrow = 3, rel_heights = c(10,3,3), align = "v")

     
  ggsave(filename = "Final_Fig3.png",
         path = 'Case_Outputs\\Final_Figures', plot = Fig3_plot,
         dpi=300, height=10, width=9, units="in")
  
  ggsave(filename = "Final_Fig3.tiff",
         path = 'Case_Outputs\\Final_Figures', plot = Fig3_plot,
         dpi=300, height=10, width=9, units="in")
  
  ggsave(filename = "Final_Fig3.pdf",
         path = 'Case_Outputs\\Final_Figures', plot = Fig3_plot,
         dpi=300, height=10, width=9, units="in")
  ########################################
#FIGURE 4
  #This figure plots the remaining model terms, zeta, theta, and the random walk
  ########################################
  
  load("Boundaries_Data.RData")
  #Remove to just the indices I've modeled:
  areaCodes_used <- unique(Case_Rates_Data$areaCode)
  Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)
  
  #Extract zeta:
  zetas_mean <- get_posterior_mean(stanfit, pars = 'zetas')
  zetas_mean <- as.data.frame(zetas_mean[,(n_chains+1)])
  
  Boundaries_reduced$zetas <- as.numeric(zetas_mean[,1]) 

  #PLOT
  ggplot(Boundaries_reduced) +
    geom_sf(aes(fill = zetas)) +
    scale_fill_gradientn(
      colours =  c("#28A1D7", "#FDFD96", "#FF6347"),  # Blue, yellow, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$zetas), median(Boundaries_reduced$zetas), max(Boundaries_reduced$zetas)))
    ) +
    ggtitle(expression(paste("Nearest-neighbour spatial kernel value,", zeta, ", by LTLA"))) +
    labs(fill = "Zeta") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm"))  -> england_zetas
  
  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = zetas), show.legend = FALSE) +
    scale_fill_gradientn(
      colours =  c("#28A1D7", "#FDFD96", "#FF6347"),  # Blue, white, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$zetas), median(Boundaries_reduced$zetas), max(Boundaries_reduced$zetas)))
    ) +
    labs(fill = "Average zeta Score") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  -> london_zetas
  
  ##########################################################
  #And now we do the same thing for Theta!
  ##########################################################
  
  #Extract
  theta_mean <- get_posterior_mean(stanfit, pars = 'theta')
  theta_mean <- as.data.frame(theta_mean[,(n_chains+1)])
  
  Boundaries_reduced$thetas <- as.numeric(theta_mean[,1]) 

  ggplot(Boundaries_reduced) +
    geom_sf(aes(fill = thetas)) +
    scale_fill_gradientn(
      colours =  c("#39C687", "white", "#C63978"),  
      values = scales::rescale(c(min(Boundaries_reduced$thetas), median(Boundaries_reduced$thetas), max(Boundaries_reduced$thetas)))
    ) +
    ggtitle(expression(paste("Spatial error term, ", theta, ", by LTLA"))) +
    labs(fill = "Theta") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm"))  -> england_theta
  
  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = thetas), show.legend = FALSE) +
    scale_fill_gradientn(
      colours =  c("#39C687", "white", "#C63978"),  
      values = scales::rescale(c(min(Boundaries_reduced$thetas), median(Boundaries_reduced$thetas), max(Boundaries_reduced$thetas)))
    ) +
    labs(fill = "Average theta Score") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  -> london_theta
  
  
  
  
  
  Fig_AB_plot <- plot_grid(england_zetas, england_theta, 
                           nrow = 1, labels = c("A", "B"), align = "v", axis = "bt") #+
  #theme(plot.background = element_rect(fill = "white"))
  
  # ggsave(filename = "ab_fig.png",
  #        path = 'Case_Outputs\\', plot = Fig_AB_plot,
  #        dpi=300, height=6.31, width=11, units="in")
  # 
  # ggsave(filename = "ab_fig.tiff",
  #        path = 'Case_Outputs\\', plot = Fig_AB_plot,
  #        dpi=300, height=6.31, width=11, units="in")
  # 
  # ggsave(filename = "ab_fig.pdf",
  #        path = 'Case_Outputs\\', plot = Fig_AB_plot,
  #        dpi=300, height=6.31, width=11, units="in")
  
  #########################################
  #FIGURE C
  #Now the random walk plot.
  #Plot Random Walk but with CrI too
  grey_lines <- c(
    "2020-06-01", ## End of first stay-at-home-order
    "2020-11-05", ## Start of second stay at home order
    "2020-12-02", ## End of second stay-at-home order 
    "2021-01-06", ## Start of third stay-at-home order
    "2021-03-08", ## End of third stay-at-home order (step 1)
    "2021-07-19" ## End of steps, no more legal restrictions
  )
  
  variant_dates <- c( #Dates that variants first appear in our dataset
    "2020-07-26", #ALPHA
    "2021-03-28", #DELTA
    "2021-09-12"  #OMICRON
  )
  
  lockdown_shades <- data.frame(date_start = as.Date(c("2020-04-01", grey_lines)),
                                date_end = as.Date(c(grey_lines, '2022-04-01' )),
                                rect_bottom = rep(775, 7),
                                rect_top = rep(800, 7),
                                rect_col = c("red", "orange", "red", "orange", "red", "orange", "green"))
  
  random_walk_summaries <- summary(stanfit, pars = c('beta_random_walk'))
  random_walk_summaries <- random_walk_summaries$summary
  
  All_dates <- sort(unique(Case_Rates_Data$date_begin))
  rw_data <- data.frame( Week = All_dates, mean = random_walk_summaries[,1],
                         lower = random_walk_summaries[,4],
                         upper = random_walk_summaries[,8])
  
  ggplot(rw_data) +
    geom_line(aes(x = Week, y = mean), color = "#39C687") +
    geom_ribbon(aes(x = Week, ymin = lower, ymax = upper), fill = "#39C687", alpha = 0.4) +
    ylab(bquote(z[t] * " value")) + theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
    geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                          ymin = max(rw_data$upper)-0.08,
                                          ymax = max(rw_data$upper)),
              fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
              show.legend = FALSE) +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab("Date") +
    ggtitle("Random Walk term trajectory") +
    # annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = -1.3, colour = "black", size = 4, angle = 90) +
    # annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = -1.3, colour = "black", size = 4, angle = 90) +
    # annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 0.3, colour = "black", size = 4, angle = 90) +
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.3)),
          legend.text = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.3))) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %y") -> rw_plot
  

  Fig_plot <- plot_grid(Fig_AB_plot, rw_plot, 
                        nrow = 2, labels = c("", "C"), rel_heights = c(0.6,0.4),
                        align = "v", axis = "bt") +
    theme(plot.background = element_rect(fill = "white"))
  
  
  ggsave(filename = "Final_Fig4.png",
         path = 'Case_Outputs\\Final_Figures', plot = Fig_plot,
         dpi=300, height=10, width=11, units="in")
  
  ggsave(filename = "Final_Fig4.tiff",
         path = 'Case_Outputs\\Final_Figures', plot = Fig_plot,
         dpi=300, height=10, width=11, units="in")
  
  ggsave(filename = "Final_Fig4.pdf",
         path = 'Case_Outputs\\Final_Figures', plot = Fig_plot,
         dpi=300, height=10, width=11, units="in")

  
  
  