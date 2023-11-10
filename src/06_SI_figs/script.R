load("model_data.RData")
dir.create("Covariate_Outputs")


################################################################################
#This script makes additional plots for the SI
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
# First, plots that show the variation in ALL our covariates 
############################################################################

colours_list <- c("#17bebb", "#ffc914", "#e4572e")

############################################################################
# 1) prop_asian
############################################################################




load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, prop_asian) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_asian <- as.numeric(fig_data$prop_asian) 

variable_vector <- Boundaries_reduced$prop_asian

#PLOT 
  ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_asian), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Asian proportion of the population by LTLA") +
    labs(fill = "Proportion Asian") +
  theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_asian

  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = prop_asian), show.legend = TRUE) +

    scale_fill_gradientn(
      colours =  colours_list,  
      values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
      limits = c(min(variable_vector),max(variable_vector))
    ) +
    labs(fill = "Proportion Asian") +
    theme_void() +
    theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_asian


  combined_plot <- plot_grid(england_asian, london_asian, nrow = 1, rel_widths = c(2.5,1)) +
    theme(plot.background = element_rect(fill = "white", colour = "white"))
  

ggsave(filename = "prop_asian.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


#########################################
############################################################################
# 2) prop_black
############################################################################




load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, prop_black_afr_car) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_black_afr_car <- as.numeric(fig_data$prop_black_afr_car) 

variable_vector <- Boundaries_reduced$prop_black_afr_car

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_black_afr_car), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Black African / Caribbean proportion of the population by LTLA") +
  labs(fill = "Proportion Black") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_black

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_black_afr_car), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Proportion Black") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_black


combined_plot <- plot_grid(england_black, london_black, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_black_afr_car.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# 3) prop_mixed_multiple + prop_other
############################################################################


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, prop_other, prop_mixed_multiple) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_other <- as.numeric(fig_data$prop_mixed_multiple) + as.numeric(fig_data$prop_other)

variable_vector <- Boundaries_reduced$prop_other

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_other), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle(" All other ethnicity proportion of the population by LTLA") +
  labs(fill = "Proportion Other \nEthnicity") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_other

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_other), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Proportion Other \nEthnicity") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_other


combined_plot <- plot_grid(england_other, london_other, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_other.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# 4) prop_white_british + prop_all_other_white
############################################################################


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, prop_white_british, prop_all_other_white) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_white <- as.numeric(fig_data$prop_white_british) + as.numeric(fig_data$prop_all_other_white)

variable_vector <- Boundaries_reduced$prop_white

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_white), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("White proportion of the population by LTLA") +
  labs(fill = "Proportion White") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_white


ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_white), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Proportion White") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_white

combined_plot <- plot_grid(england_white, london_white, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_white.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

############################################################################
# 5) IMD_Average_score
############################################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, IMD_Average_score) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$IMD_Average_score <- as.numeric(fig_data$IMD_Average_score) 

variable_vector <- Boundaries_reduced$IMD_Average_score

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = IMD_Average_score), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "IMD") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_IMD

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = IMD_Average_score), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "IMD") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_IMD

combined_plot <- plot_grid(england_IMD, london_IMD, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "IMD_Average_score.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

############################################################################
# 6) prop_o65
############################################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, prop_o65) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_o65 <- as.numeric(fig_data$prop_o65) 

variable_vector <- Boundaries_reduced$prop_o65

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_o65), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Proportion of population over the age of 65 by LTLA") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_o65

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_o65), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_o65


combined_plot <- plot_grid(england_o65, london_o65, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_o65.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


############################################################################
# Pop_per_km2
############################################################################
load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, Pop_per_km2) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$Pop_per_km2 <- as.numeric(fig_data$Pop_per_km2) 

variable_vector <- Boundaries_reduced$Pop_per_km2

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = Pop_per_km2), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Population density by LTLA (Population per square kilometer)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_popden

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = Pop_per_km2), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Population Density") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_popden

combined_plot <- plot_grid(england_popden, london_popden, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "Pop_per_km2.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

############################################################################
# Median_annual_income 2020/21
############################################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, Median_annual_income) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$Median_annual_income <- as.numeric(fig_data$Median_annual_income)/1000

variable_vector <- Boundaries_reduced$Median_annual_income

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = Median_annual_income), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Median annual income (\u00a3 thousand) by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_income

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = Median_annual_income), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Median Income") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_income

combined_plot <- plot_grid(england_income, london_income, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "Median_annual_income_2020_21.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

############################################################################
# Median_annual_income 2021/22
############################################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, Median_annual_income) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$Median_annual_income <- as.numeric(fig_data$Median_annual_income)/1000

variable_vector <- Boundaries_reduced$Median_annual_income

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = Median_annual_income), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Median annual income (\u00a3 thousand) by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_income

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = Median_annual_income), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Median Income") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_income

combined_plot <- plot_grid(england_income, london_income, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "Median_annual_income_2021_22.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


############################################################################
# unringfenced 2020/21
############################################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, unringfenced, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$unringfenced <- (as.numeric(fig_data$unringfenced)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$unringfenced

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = unringfenced), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Unringfenced COVID-19 funding (\u00a3 thousand) per thousand people by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_unringfenced

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = unringfenced), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Unringfenced \nFunding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_unringfenced


combined_plot <- plot_grid(england_unringfenced, london_unringfenced, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "unringfenced_2020_21.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


############################################################################
# unringfenced 2021/22
############################################################################


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, unringfenced, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$unringfenced <- (as.numeric(fig_data$unringfenced)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$unringfenced

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = unringfenced), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Unringfenced COVID-19 funding (\u00a3 thousand) per thousand people by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_unringfenced


ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = unringfenced), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Unringfenced \nFunding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_unringfenced

combined_plot <- plot_grid(england_unringfenced, london_unringfenced, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "unringfenced_2021_22.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


############################################################################
# contain_outbreak_management2020/21
############################################################################

load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, contain_outbreak_management, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$contain_outbreak_management <- (as.numeric(fig_data$contain_outbreak_management)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$contain_outbreak_management

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Contain Outbreak Management Fund (COMF) funding (\u00a3 thousand) per thousand people by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_comf


ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "COMF Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_comf


combined_plot <- plot_grid(england_comf, london_comf, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "contain_outbreak_management_2020_21.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

############################################################################
# contain_outbreak_management2021/22
############################################################################


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, contain_outbreak_management, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]
Boundaries_reduced$contain_outbreak_management <- (as.numeric(fig_data$contain_outbreak_management)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$contain_outbreak_management

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Contain Outbreak Management Fund (COMF) funding (\u00a3 thousand) per thousand people by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_comf

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "COMF Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_comf

combined_plot <- plot_grid(england_comf, london_comf, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "contain_outbreak_management_2021_22.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


############################################################################
# ASC_infection_control_fund 2020/21
############################################################################




load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, ASC_infection_control_fund, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$ASC_infection_control_fund <- (as.numeric(fig_data$ASC_infection_control_fund)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$ASC_infection_control_fund

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Adult Social Care (ASC) infection control funding (\u00a3 thousand) per thousand people by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_asc

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "ASC Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_asc


combined_plot <- plot_grid(england_asc, london_asc, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "ASC_infection_control_fund_2020_21.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


############################################################################
# ASC_infection_control_fund 2021/22
############################################################################




load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, ASC_infection_control_fund, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$ASC_infection_control_fund <- (as.numeric(fig_data$ASC_infection_control_fund)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$ASC_infection_control_fund

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = FALSE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Adult Social Care (ASC) infection control funding (\u00a3 thousand) per thousand people by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_asc


ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = TRUE) +
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "ASC Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_asc

combined_plot <- plot_grid(england_asc, london_asc, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "ASC_infection_control_fund_2021_22.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")



#########################################

#For these future plots, they change weekly, so we're going to plot weekly box-plots I think is the best way.
AllDates <- sort(unique(Case_Rates_Data$date_begin))
T <- length(AllDates)

###########################################
##########################################
#  Start with Time at Workplace 
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                         time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$workplaces_percent_change_from_baseline
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Percent change in people visiting workplaces compared to pre-pandemic over all LTLAs") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "workplace_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, workplaces_percent_change_from_baseline) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(mean_workplaces_percent_change = mean(workplaces_percent_change_from_baseline))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$workplaces <- as.numeric(fig_data$mean_workplaces_percent_change)
                                  
variable_vector <- Boundaries_reduced$workplaces

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = workplaces), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean percent difference in people visiting workplaces compared to pre-pandemic (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_workplaces

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = workplaces), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Workplace \nVisits") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_workplaces



combined_plot <- plot_grid(england_workplaces, london_workplaces, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "workplaces.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

###########################################
##########################################
# transit stations
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$transit_stations_percent_change_from_baseline
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Percent change in people visiting transit stations compared to pre-pandemic over all LTLAs") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "transit_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, transit_stations_percent_change_from_baseline) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(transit_stations_percent_change_from_baseline = mean(transit_stations_percent_change_from_baseline))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$transit <- as.numeric(fig_data$transit_stations_percent_change_from_baseline)

variable_vector <- Boundaries_reduced$transit

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = transit), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean percent difference in people visiting transit stations compared to pre-pandemic (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_transit

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = transit), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Transit \nStation \nVisits") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_transit


combined_plot <- plot_grid(england_transit, london_transit, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "transit.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

###########################################
##########################################
# residential
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$residential_percent_change_from_baseline
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Percent change in hours spent in residential areas compared to pre-pandemic over all LTLAs") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "residential_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, residential_percent_change_from_baseline) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(residential_percent_change_from_baseline = mean(residential_percent_change_from_baseline))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$residential <- as.numeric(fig_data$residential_percent_change_from_baseline)

variable_vector <- Boundaries_reduced$residential

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = residential), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean percent difference in hours spent in residential areas compared to pre-pandemic (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_residential

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = residential), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Residential \nVisits") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_residential


combined_plot <- plot_grid(england_residential, london_residential, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "residential.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


#########################################

#AND NOW VARIANTS!

###########################################
##########################################
# Wild_type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Wild_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are wild-type variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_wildtype_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Wild_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Wild_prop = mean(s_Wild_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Wild_prop <- as.numeric(fig_data$s_Wild_prop)

variable_vector <- Boundaries_reduced$s_Wild_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Wild_prop), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were wild-type variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Wild_prop

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Wild_prop), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  labs(fill = "Mean \nWild-type \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Wild_prop


combined_plot <- plot_grid(england_s_Wild_prop, london_s_Wild_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Wild_prop.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


###########################################
##########################################
# Alpha type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Alpha_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are Alpha variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_alpha_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Alpha_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Alpha_prop = mean(s_Alpha_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Alpha_prop <- as.numeric(fig_data$s_Alpha_prop)

variable_vector <- Boundaries_reduced$s_Alpha_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Alpha_prop), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were Alpha variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Alpha_prop

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Alpha_prop), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nAlpha \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Alpha_prop


combined_plot <- plot_grid(england_s_Alpha_prop, london_s_Alpha_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Alpha_prop.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")


###########################################
##########################################
# Delta type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Delta_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are Delta variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_delta_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Delta_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Delta_prop = mean(s_Delta_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Delta_prop <- as.numeric(fig_data$s_Delta_prop)

variable_vector <- Boundaries_reduced$s_Delta_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Delta_prop), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were Delta variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Delta_prop

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Delta_prop), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nDelta \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Delta_prop



combined_plot <- plot_grid(england_s_Delta_prop, london_s_Delta_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Delta_prop.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

###########################################
##########################################
# Omicron type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Omicron_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are Omicron variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_omicron_boxplot.png",
       path = 'Covariate_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

Case_Rates_Data %>%
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Omicron_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Omicron_prop = mean(s_Omicron_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Omicron_prop <- as.numeric(fig_data$s_Omicron_prop)

variable_vector <- Boundaries_reduced$s_Omicron_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Omicron_prop), show.legend = FALSE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were Omicron variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Omicron_prop


ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Omicron_prop), show.legend = TRUE) +
  
  scale_fill_gradientn(
    colours =  colours_list,  
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nOmicron \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Omicron_prop



combined_plot <- plot_grid(england_s_Omicron_prop, london_s_Omicron_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Omicron_prop.png",
       path = 'Covariate_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")



#############################################################################
#############################################################################
#And now, some sensitivity plots
#############################################################################
#############################################################################

dir.create("Sensitivity_Outputs")


################################################################################
################################################################################
#load in our main analysis
df_hold <- read.csv("main_summaries_ALL.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")


#Format it into the way we want for the eventual plot
Plot_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Multivariate", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "1) Proportion Asian"

df_hold <- read.csv("main_summaries_01_prop_asian.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "2) Proportion Black Afr/Car"

df_hold <- read.csv("main_summaries_02_prop_black_afr_car.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "3) Proportion Other Ethnicity"

df_hold <- read.csv("main_summaries_03_prop_other.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "4) IMD Average Score"

df_hold <- read.csv("main_summaries_04_IMD.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "5) Proportion Over Age 65"

df_hold <- read.csv("main_summaries_05_prop_o65.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "6) Population per km^2"

df_hold <- read.csv("main_summaries_06_Pop_per_km2.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "7) Median Annual Income"

df_hold <- read.csv("main_summaries_07_Median_annual_income.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "8) Time At Workplace"

df_hold <- read.csv("main_summaries_08_workplaces.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "9) Time At Home"

df_hold <- read.csv("main_summaries_09_residential.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "10) Time At Transit Stations"

df_hold <- read.csv("main_summaries_10_transit.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "11) Alpha Proportion"

df_hold <- read.csv("main_summaries_11_Alpha.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "12) Delta Proportion"

df_hold <- read.csv("main_summaries_12_Delta.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "13) Omicron Proportion"

df_hold <- read.csv("main_summaries_13_Omicron.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "14) Unringfenced"

df_hold <- read.csv("main_summaries_14_unringfenced.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "15) Outbreak Management"

df_hold <- read.csv("main_summaries_15_COMF.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "16) ASC infection control"

df_hold <- read.csv("main_summaries_16_ASC.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################

#Now, let's ignore the variants because, of course, they're a bit different.
Plot_data <- filter(Plot_data, !(Variable %in% c("11) Alpha Proportion", "12) Delta Proportion", 
                                                 "13) Omicron Proportion")))

Plot_data$Variable <- factor(Plot_data$Variable, levels = rev(unique(Plot_data$Variable)))


ggplot(Plot_data) +
  geom_vline(xintercept = 0, color = "grey", lty = 5, size = 1) +
  geom_errorbar(aes(x = 0, xmin = lower, xmax = upper, y = Variable, color = model_type), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(aes(x = mean, y = Variable, color = model_type), size = 2.5, position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Comparing multivariate and univariate model output") +
  xlab("Beta coefficient value") +
  labs(color = "Model type") +
  scale_color_manual(values=c( "#17bebb", "#e4572e")) -> fig_1

# ggsave(filename = "multi_uni_comp1.png",
#        path = 'Sensitivity_Outputs', plot = fig_1,
#        dpi=300, height=6, width=9, units="in")

#Arguably best to leave out funding if you're not also including variant

Plot_data <- filter(Plot_data, !(Variable %in% c("11) Alpha Proportion", "12) Delta Proportion", 
                                                 "13) Omicron Proportion", "14) Unringfenced",
                                                 "15) Outbreak Management", "16) ASC infection control")))

Plot_data$Variable <- factor(Plot_data$Variable, levels = rev(unique(Plot_data$Variable)))


ggplot(Plot_data) +
  geom_vline(xintercept = 0, color = "grey", lty = 5, size = 1) +
  geom_errorbar(aes(x = 0, xmin = lower, xmax = upper, y = Variable, color = model_type), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(aes(x = mean, y = Variable, color = model_type), size = 2.5, position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Comparing multivariate and univariate model output") +
  xlab("Beta coefficient value") +
  labs(color = "Model type") +
  scale_color_manual(values=c( "#17bebb", "#e4572e")) -> fig_2

ggsave(filename = "multi_uni_comp.png",
       path = 'Sensitivity_Outputs', plot = fig_2,
       dpi=300, height=6, width=9, units="in")


#####################################################################
#####################################################################

#Now we compare different data streams;
#swapping the cases from PCR only linelist to pillar 1 + pillar 2 and LFD
#Changing SGTF defined variant to dashboard VAM data
#Or both!
#load in our main analysis
df_hold <- read.csv("main_summaries_ALL.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")


#Format it into the way we want for the eventual plot
Plot_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Main Model", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

#Load in the different cases
df_hold <- read.csv("main_summaries_dashboard_cases.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")
Hold_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Dashboard Cases", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

Plot_data <- rbind(Plot_data, Hold_data)

#Load in the different variants
df_hold <- read.csv("main_summaries_dashboard_variant.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")
Hold_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Dashboard Variants", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

Plot_data <- rbind(Plot_data, Hold_data)

#Load in the different variants
df_hold <- read.csv("main_summaries_dashboard_cases_AND_variant.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")
Hold_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Dashboard Cases \nand Variants", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

Plot_data <- rbind(Plot_data, Hold_data)

Plot_data$Variable <- factor(Plot_data$Variable, levels = rev(unique(Plot_data$Variable)))


ggplot(Plot_data) +
  geom_vline(xintercept = 0, color = "grey", lty = 5, size = 1) +
  geom_errorbar(aes(x = 0, xmin = lower, xmax = upper, y = Variable, color = model_type), size = 1, position = position_dodge(width = 0.6)) +
  geom_point(aes(x = mean, y = Variable, color = model_type), size = 2.5, position = position_dodge(width = 0.6)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3)),
        legend.spacing.y = unit(0.3, 'cm')) +
  ggtitle("Comparing model output for different data streams") +
  xlab("Beta coefficient value") +
  labs(color = "Data sources") +
  scale_color_manual(breaks = c("Main Model",
                                "Dashboard Cases",
                                "Dashboard Variants",
                                "Dashboard Cases \nand Variants"), 
                     values=c( "#17bebb", "#e4572e", "#00C853", "#FFB300")) +
  #theme(legend.spacing.y = unit(1.0, 'cm')) +
  guides(color= guide_legend(byrow = TRUE)) -> data_streams_plot

ggsave(filename = "data_streams_plot.png",
       path = 'Sensitivity_Outputs', plot = data_streams_plot,
       dpi=300, height=9, width=10, units="in")

