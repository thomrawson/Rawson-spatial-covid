#This task loads in separate data files, and combines them for the final data frame to use for model fitting.

#Make folder for outputs
dir.create("Outputs")

#Start by loading information on UK LTLA region boundaries from shape file
Boundaries <- st_read("Data/district_borough_unitary_region.shp")
#SOURCE: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2021-boundaries-gb-bfc/explore?location=52.029728%2C-2.585724%2C7.03


#Now load daily new COVID cases data at LTLA level
#Originally this was pulled from the government portal API:
#Case_Rates_Data<- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv')
#But in the interest of re-producibility, we use the hard-saved pull from this API
#Current source pulled June 22nd 2023
Case_Rates_Data <- read.csv('Data/ltla_cases_2023_06_22.csv')

#Ensure date is being read as a date
Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)

#Clip off everything after Nov 10th 2022
Case_Rates_Data <- filter(Case_Rates_Data, date < as.Date("2022-11-10"))

#############################################################################
#############################################################################

#Attach cumulative first episodes:
FirstEpisodes_Data <- read.csv("Data/ltla_first_episodes_2023_06_22.csv")
FirstEpisodes_Data$date <- as.Date(FirstEpisodes_Data$date)
FirstEpisodes_Data <- filter(FirstEpisodes_Data, date < as.Date("2022-11-10"))

CaseRates_LTLA_codes <- unique(Case_Rates_Data$areaCode)
CaseRates_LTLA_names <- unique(Case_Rates_Data$areaName)


#Stick together:
Case_Rates_Data <- full_join(Case_Rates_Data, FirstEpisodes_Data, by=c("areaCode", "areaName", "areaType", "date"))

#############################################################################
#############################################################################
#Next, we're going to attach a different measure of cases:
#Only the pillar 2, and PCR_only cases from the direct England line list
Linelist_Data <- readRDS("Data/reduced_linelist.rds")
Linelist_Data <- filter(Linelist_Data, specimen_date < as.Date("2022-11-10"))

new_LTLA_codes <- unique(Linelist_Data$ltla_code)
new_LTLA_names <- unique(Linelist_Data$ltla_name)
CaseRates_LTLA_names[which(!(CaseRates_LTLA_codes %in% new_LTLA_codes))]
CaseRates_LTLA_codes[which(!(CaseRates_LTLA_codes %in% new_LTLA_codes))]

#The linelist does not have data for 4 of the English LTLAs in the dashboard
#(It also doesn't have any of the 65 devolved nations LTLAs info):
#E07000006 - South Bucks
#E07000004 - Aylesbury Vale
#E07000005 - Chiltern
#E07000007 - Wycombe

new_LTLA_names[which(!(new_LTLA_codes %in% CaseRates_LTLA_codes))]
new_LTLA_codes[which(!(new_LTLA_codes %in% CaseRates_LTLA_codes))]

#Likewise, the dashboard data does not have info on three of the linelist LTLAs:
#E06000053 - Isles of Scilly
#E06000060 - Buckinghamshire
#E09000001 - City of London

#Firstly, in the dashboard data, it combines Hackney AND City of London under (E09000012)
#Let's go ahead and combine these in the line list data now:
Linelist_Data$ltla_code[which(Linelist_Data$ltla_code == "E09000001")] <- "E09000012"
Linelist_Data$ltla_name[which(Linelist_Data$ltla_code == "E09000012")] <- "Hackney and City of London"

#Similarly, in the dashboard data, Isles of Scilly are rolled in with Cornwall under E06000052 (just Cornwall for linelist)
Linelist_Data$ltla_code[which(Linelist_Data$ltla_code == "E06000053")] <- "E06000052"
Linelist_Data$ltla_name[which(Linelist_Data$ltla_code == "E06000052")] <- "Cornwall and Isles of Scilly"

#Now we aggregate these by date and code:
Linelist_Data <- aggregate(Linelist_Data$cases,
                                 by=list(date=Linelist_Data$specimen_date, 
                                         areaCode = Linelist_Data$ltla_code, 
                                         areaName = Linelist_Data$ltla_name),
                                 FUN=sum)

#############################################################################
#############################################################################



#Load vaccine uptake proportions
#These are the live API links used
#First_Vaccine_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&format=csv')
#Second_Vaccine_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&format=csv')
#Third_Vaccine_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage&format=csv')

#Similarly to above though, we use the data accessed on November 10th 2022
First_Vaccine_Data <- read.csv('Data/ltla_cum_1dose_vaccine_percentage_10_11_2022.csv')
Second_Vaccine_Data <- read.csv('Data/ltla_cum_2dose_vaccine_percentage_10_11_2022.csv')
Third_Vaccine_Data <- read.csv('Data/ltla_cum_3dose_vaccine_percentage_10_11_2022.csv')

#Assign a sensible variable name
names(First_Vaccine_Data) <- c('areaCode', 'areaName', 'areaType', 'date', 'cumVaccPercentage_FirstDose')
names(Second_Vaccine_Data) <- c('areaCode', 'areaName', 'areaType', 'date', 'cumVaccPercentage_SecondDose')
names(Third_Vaccine_Data) <- c('areaCode', 'areaName', 'areaType', 'date', 'cumVaccPercentage_ThirdDose')

#Ensure date is date-type
First_Vaccine_Data$date <- as.Date(First_Vaccine_Data$date)
Second_Vaccine_Data$date <- as.Date(Second_Vaccine_Data$date)
Third_Vaccine_Data$date <- as.Date(Third_Vaccine_Data$date)

#Merge the Case rates data with the vaccinations data
Case_Rates_Data <- merge(Case_Rates_Data, First_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(7,8)]

Case_Rates_Data <- merge(Case_Rates_Data, Second_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(8,9)]

Case_Rates_Data <- merge(Case_Rates_Data, Third_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(9,10)]

#Remove the Northern Ireland data:
Case_Rates_Data <- filter(Case_Rates_Data, !grepl('N09', areaCode))

#Now we need to deal with some discrepancies between the areaCodes in our case data, and those in our Boundaries data

#All area codes in data: 369
Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
#All polygon codes: 363
Boundary_Codes <- unique(Boundaries$CODE)

sum(Boundary_Codes %in% Case_Data_Codes)
#5 of the boundary codes are not in Case data - Let's list which:

index <- 1
Not_in_Case_Rates1 <- rep(NA,5)
Not_in_Case_Rates2 <- rep(NA,5)
for( i in 1:length(Boundary_Codes)){
  if(Boundary_Codes[i] %in% Case_Data_Codes){
    
  } else {
    Not_in_Case_Rates1[index] <- Boundaries$NAME[i]
    Not_in_Case_Rates2[index] <- Boundary_Codes[i]
    index <- index + 1
  }
}

Not_in_Case_Rates1
Not_in_Case_Rates2
#Case rates does not have;
#West Northamptonshire
#Isles of Scilly
#North Northamptonshire
#Buckinghamshire
#City and County of the City of London

sum(Case_Data_Codes %in% Boundary_Codes)
#And 11 in cases codes are not in boundary codes

index <- 1
Not_in_Boundaries1 <- rep(NA,11)
Not_in_Boundaries2 <- rep(NA,11)
for( i in 1:length(Case_Data_Codes)){
  if(Case_Data_Codes[i] %in% Boundary_Codes){
    
  } else {
    #Not_in_Boundaries1[index] <- Boundaries$NAME[i]
    Not_in_Boundaries2[index] <- Case_Data_Codes[i]
    index <- index + 1
  }
}

for(i in 1:length(Not_in_Boundaries2)){
  Not_in_Boundaries1[i] <- Case_Rates_Data$areaName[which(Case_Rates_Data$areaCode == Not_in_Boundaries2[i])[1]]
}

Not_in_Boundaries1
Not_in_Boundaries2

#We first combine Hackney and City of London boundaries, as is done for case data


#This will change the polygon for Hackney to be a polygon for hackney AND city of london
Boundaries$geometry[320] <- st_cast(
  st_union(Boundaries[c(which(grepl('Hackney', Boundaries$NAME)), which(grepl('City of London', Boundaries$NAME))),]) 
  , "MULTIPOLYGON")

#Now let's remove City of London from Boundaries
Boundaries <- Boundaries[-324,]


#Now we do this for Scilly too
#We want to add Scilly boundaries to the Cornwall boundaries.
Boundaries$geometry[120] <- st_cast(
  st_union(Boundaries[c(which(grepl('Cornwall', Boundaries$NAME)), which(grepl('Scilly', Boundaries$NAME))),]) 
  , "MULTIPOLYGON")

#Now remove Isles of Scilly 
Boundaries <- Boundaries[-121,]

#Let's see what's left
#All area codes in data: 369
Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
#All polygon codes: 361
Boundary_Codes <- unique(Boundaries$CODE)

sum(Boundary_Codes %in% Case_Data_Codes)

index <- 1
Not_in_Case_Rates1 <- rep(NA,3)
Not_in_Case_Rates2 <- rep(NA,3)
for( i in 1:length(Boundary_Codes)){
  if(Boundary_Codes[i] %in% Case_Data_Codes){
    
  } else {
    Not_in_Case_Rates1[index] <- Boundaries$NAME[i]
    Not_in_Case_Rates2[index] <- Boundary_Codes[i]
    index <- index + 1
  }
}

Not_in_Case_Rates1
Not_in_Case_Rates2

Not_in_Boundaries1
Not_in_Boundaries2
#So, there are eleven regions which the Case rates data list, which are actually made up of
#three regions in the Boundaries data:
#Buckinghamshire = South Bucks, Aylesbury Vale, Chiltern, Wycombe, 
#West Northamptonshire = Northampton , South Northamptonshire, Daventry
#North Northamptonshire = East Northamptonshire (weirdly, I think), Wellingborough, Corby, Kettering

#So we collapse  those case locations into the broader boundaries.

areaChanges <- data.frame(from_code = Not_in_Boundaries2, from_name = Not_in_Boundaries1,
                          to_name = c('Buckinghamshire', 'Buckinghamshire', 'Buckinghamshire',
                                      "Buckinghamshire", "North Northamptonshire", "West Northamptonshire",
                                      "North Northamptonshire", "North Northamptonshire", "West Northamptonshire",
                                      'West Northamptonshire', "North Northamptonshire"),
                          to_code = c("E06000060", "E06000060", "E06000060",
                                      "E06000060", "E06000061", "E06000062",
                                      "E06000061", "E06000061", "E06000062",
                                      "E06000062", "E06000061"))

#Now change the areaCode and areaName:
for(i in 1:length(Case_Rates_Data[,1])){
  if(Case_Rates_Data$areaCode[i] %in% Not_in_Boundaries2){
    index <- which(areaChanges$from_code == Case_Rates_Data$areaCode[i])
    Case_Rates_Data$areaCode[i] <- areaChanges$to_code[index]
    Case_Rates_Data$areaName[i] <- areaChanges$to_name[index]
  }
}

New_Cases <- filter(Case_Rates_Data, areaCode %in% Not_in_Case_Rates2)
Case_Rates_Data <- Case_Rates_Data[,-c(3,4)]

#Now we aggregate these by date and code:
Agg_Case_Rates_Data <- aggregate(Case_Rates_Data$newCasesBySpecimenDate,
                                 by=list(date=Case_Rates_Data$date, 
                                         areaCode = Case_Rates_Data$areaCode, 
                                         areaName = Case_Rates_Data$areaName),
                                 FUN=sum)

Agg_First_Episodes_Data <- aggregate(Case_Rates_Data$cumFirstEpisodesBySpecimenDate,
                                 by=list(date=Case_Rates_Data$date, 
                                         areaCode = Case_Rates_Data$areaCode, 
                                         areaName = Case_Rates_Data$areaName),
                                 FUN=sum)

#We need to calculate the "mean" vaccination rate across these three agglomerated regions
First_Vacc_Mean_Data <- aggregate(Case_Rates_Data$cumVaccPercentage_FirstDose,
                            by=list(date=Case_Rates_Data$date,
                                    areaCode = Case_Rates_Data$areaCode,
                                    areaName = Case_Rates_Data$areaName),
                            FUN=mean)

Second_Vacc_Mean_Data <- aggregate(Case_Rates_Data$cumVaccPercentage_SecondDose,
                                  by=list(date=Case_Rates_Data$date,
                                          areaCode = Case_Rates_Data$areaCode,
                                          areaName = Case_Rates_Data$areaName),
                                  FUN=mean)

Third_Vacc_Mean_Data <- aggregate(Case_Rates_Data$cumVaccPercentage_ThirdDose,
                                  by=list(date=Case_Rates_Data$date,
                                          areaCode = Case_Rates_Data$areaCode,
                                          areaName = Case_Rates_Data$areaName),
                                  FUN=mean)

New_Cases2 <- filter(Agg_Case_Rates_Data, areaCode %in% Not_in_Case_Rates2)

#Now we no longer have any overlap



Agg_Case_Rates_Data$previous_day_cases <- NA
colnames(Agg_Case_Rates_Data) <- c('date', 'areaCode', 'areaName','Cases', 'previous_day_cases')
colnames(Agg_First_Episodes_Data) <- c('date', 'areaCode', 'areaName','First_Episodes_Total')
colnames(First_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_FirstDose')
colnames(Second_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_SecondDose')
colnames(Third_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_ThirdDose')

Case_Rates_Data <- merge(Agg_Case_Rates_Data, Agg_First_Episodes_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, First_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, Second_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, Third_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))

Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)

#########################
#Now we repeat all these steps with the linelist data, collapsing two of the region sets
#Now change the areaCode and areaName:
for(i in 1:length(Linelist_Data[,1])){
  if(Linelist_Data$areaCode[i] %in% Not_in_Boundaries2){
    index <- which(areaChanges$from_code == Linelist_Data$areaCode[i])
    Linelist_Data$areaCode[i] <- areaChanges$to_code[index]
    Linelist_Data$areaName[i] <- areaChanges$to_name[index]
  }
}

New_Cases <- filter(Linelist_Data, areaCode %in% Not_in_Case_Rates2)

colnames(Linelist_Data) <- c("date", "areaCode", "areaName", "Linelist_P2_PCR_cases")
#Now we aggregate these by date and code:
Agg_Linelist_Data <- aggregate(Linelist_Data$Linelist_P2_PCR_cases,
                                 by=list(date=Linelist_Data$date, 
                                         areaCode = Linelist_Data$areaCode, 
                                         areaName = Linelist_Data$areaName),
                                 FUN=sum)

colnames(Agg_Linelist_Data) <- c("date", "areaCode", "areaName", "Linelist_P2_PCR_cases")

#We now have no mismatch in LTLAs between our two case sets
Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
Linelist_Codes <- unique(Agg_Linelist_Data$areaCode)



#Tidy
Agg_Linelist_Data <- Agg_Linelist_Data[,-3]
#########################
#Now merge our two data sets
Case_Rates_Data <- merge(Case_Rates_Data, Agg_Linelist_Data, all = TRUE, by = c('areaCode', 'date'))

#Replace the NAs in the Linelist cases column with 0s:
Case_Rates_Data$Linelist_P2_PCR_cases[is.na(Case_Rates_Data$Linelist_P2_PCR_cases)] <- 0

#Cut the junk
rm(areaChanges, New_Cases, New_Cases2,
   Not_in_Boundaries1, Not_in_Boundaries2,
   Not_in_Case_Rates1, Not_in_Case_Rates2, Agg_First_Episodes_Data,
   Agg_Linelist_Data, Case_Data_Codes, Compare_Names,
   Linelist_Codes, Linelist_Data, FirstEpisodes_Data)


#We choose to cut everything before April 30th 2020, as case data was weaker before this date
minimum_date <- "2020-04-30"
Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)
Case_Rates_Data <- filter(Case_Rates_Data, date > as.Date(minimum_date))

#Load LTLA population data.
LTLA_Demog <- read.csv('Data/LTLA_demog.csv')
#This contains, for each LTLA, the population, area, pop. density, and median age

#Attach this demographic data to the Case_Rates data frame

Case_Rates_Data <- merge(Case_Rates_Data, LTLA_Demog[,c(1,5,6,7)], by = 'areaCode', all.x = TRUE)

#Remove the unnecessary vaccine dataframes
rm(Agg_Case_Rates_Data, First_Vacc_Mean_Data, First_Vaccine_Data,
   Second_Vacc_Mean_Data, Second_Vaccine_Data, 
   Third_Vacc_Mean_Data, Third_Vaccine_Data, LTLA_Demog)

#And remove some extra rubbish
rm(Boundary_Codes, CaseRates_LTLA_codes, CaseRates_LTLA_names,
   i, index, new_LTLA_codes, new_LTLA_names)

#We remove geographies which have no land boundaries with other LTLAs:
#102 - isle of wight
#103 - isle of anglesey
#112 - Orkney islands
#119 - Shetland Islands
#124 - Na h-Eileanan an Iar
Error_indices <- c(102, 103, 112, 119, 124)
Error_codes <- Boundaries$CODE[Error_indices]

#Remove from Boundaries and from case data
Boundaries <- filter(Boundaries, !(CODE %in% Error_codes))
Case_Rates_Data <- filter(Case_Rates_Data, !(areaCode %in% Error_codes))

#Define a column that will say which element of the spatial matrix each row relates to
Case_Rates_Data$INDEX <- NA
for(i in 1:length(Case_Rates_Data$date)){
  hold_code <- Case_Rates_Data$areaCode[i]
  Case_Rates_Data$INDEX[i] <- which(Boundaries$CODE == hold_code)
}

#Build weight matrix - defining if two LTLAs are connected or not

col_sp <- as(Boundaries, "Spatial")
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure

W <- W * 1L
#Define D as the total number of neighbours for each LTLA
D <- diag(rowSums(W))



##################################################################
##      WEEKLY
##################################################################
#We want to convert to using the weekly sum of cases

#Start by summing together the weekly cases
#Start by adding a new variable, week number:

#FIRST DATE: Sunday 3rd May 2020
Case_Rates_Data$Week <- plyr::round_any(as.numeric(Case_Rates_Data$date - as.Date('2020-05-03') + 0.5), 7, f = ceiling)/7

#Filter out the week 0 cases
Case_Rates_Data <- filter(Case_Rates_Data, Week > 0)

#Now sum by, everything, but not vaccination yet
Case_Rates_Data_hold <- aggregate(Case_Rates_Data$Cases, by=list(Week=Case_Rates_Data$Week, areaCode = Case_Rates_Data$areaCode,
                                                                 areaName = Case_Rates_Data$areaName, Population = Case_Rates_Data$Population,
                                                                 Pop_per_km2 = Case_Rates_Data$Pop_per_km2,
                                                                 Median_age = Case_Rates_Data$Median_age,
                                                                 INDEX = Case_Rates_Data$INDEX), FUN=sum)

#Now need to add a "previous week cases" column, and a "date_begin" column:
colnames(Case_Rates_Data_hold) <- c('Week', 'areaCode', 'areaName', 'Population',
                                    'Pop_per_km2', 'Median_age', 'INDEX', 'Week_Cases')

Case_Rates_Data_hold$date_begin <- as.Date('2020-05-03') + ((Case_Rates_Data_hold$Week - 1)*7) 

#Previous weeks column
Case_Rates_Data_hold$previous_week_cases <- NA
Case_Rates_Data_hold$next_week_cases <- NA

for(i in 1: length(Case_Rates_Data_hold$Week)){
  areaName_hold <- Case_Rates_Data_hold$areaName[i]
  date_hold <- Case_Rates_Data_hold$Week[i]
  
  if(date_hold == 1){
    next_cases <- filter(Case_Rates_Data_hold, ((areaName == areaName_hold)&(Week == date_hold + 1)))$Week_Cases
    Case_Rates_Data_hold$next_week_cases[i] <- next_cases
  }else if(date_hold == 132){
    prev_cases <- filter(Case_Rates_Data_hold, ((areaName == areaName_hold)&(Week == date_hold - 1)))$Week_Cases
    Case_Rates_Data_hold$previous_week_cases[i] <- prev_cases
  }
  else{
    prev_cases <- filter(Case_Rates_Data_hold, ((areaName == areaName_hold)&(Week == date_hold - 1)))$Week_Cases
    next_cases <- filter(Case_Rates_Data_hold, ((areaName == areaName_hold)&(Week == date_hold + 1)))$Week_Cases
    Case_Rates_Data_hold$previous_week_cases[i] <- prev_cases
    Case_Rates_Data_hold$next_week_cases[i] <- next_cases
  }
  
}

#Drop "week one", as we don't have the previous week cases.
Case_Rates_Data_hold <- drop_na(Case_Rates_Data_hold)

rm(data_hold, areaName_hold, date_hold, i)

#Add back the vaccination states, and first episodes for that day too. 
#We currently use the percentage on the last day of the week 
Case_Rates_Data_hold$First_Episodes_Total <- NA
Case_Rates_Data_hold$cumVaccPercentage_FirstDose <- NA
Case_Rates_Data_hold$cumVaccPercentage_SecondDose <- NA
Case_Rates_Data_hold$cumVaccPercentage_ThirdDose <- NA
Case_Rates_Data_hold$date_begin <- as.Date(Case_Rates_Data_hold$date_begin)

for(i in 1:length(Case_Rates_Data_hold$Week)){
  date_hold <- Case_Rates_Data_hold$date_begin[i] + 6
  areaCode_hold <- Case_Rates_Data_hold$areaCode[i]
  
  data_filter <- filter(Case_Rates_Data, date == date_hold)
  data_filter <- filter(data_filter, areaCode == areaCode_hold)
  
  Case_Rates_Data_hold$First_Episodes_Total[i] <- data_filter$First_Episodes_Total[1]
  
  Case_Rates_Data_hold$cumVaccPercentage_FirstDose[i] <- data_filter$cumVaccPercentage_FirstDose[1]
  Case_Rates_Data_hold$cumVaccPercentage_SecondDose[i] <- data_filter$cumVaccPercentage_SecondDose[1]
  Case_Rates_Data_hold$cumVaccPercentage_ThirdDose[i] <- data_filter$cumVaccPercentage_ThirdDose[1]
}

rm(data_filter, date_hold, areaCode_hold)


#Now we do the same steps but for the line list cases, and then attach the two together
##############################################################

#Now sum by, everything, but not vaccination yet
Linelist_Data_hold <- aggregate(Case_Rates_Data$Linelist_P2_PCR_cases, by=list(Week=Case_Rates_Data$Week, areaCode = Case_Rates_Data$areaCode), FUN=sum)

#Now need to add a "previous week cases" column, and a "date_begin" column:
colnames(Linelist_Data_hold) <- c('Week', 'areaCode', 'Linelist_P2_PCR_Week_Cases')

#Previous weeks column
Linelist_Data_hold$Linelist_P2_PCR_Previous_Week_Cases <- NA
Linelist_Data_hold$Linelist_P2_PCR_Next_Week_Cases <- NA

for(i in 1: length(Linelist_Data_hold$Week)){
  areaCode_hold <- Linelist_Data_hold$areaCode[i]
  date_hold <- Linelist_Data_hold$Week[i]
  
  if(date_hold == 1){
    next_cases <- filter(Linelist_Data_hold, ((areaCode == areaCode_hold)&(Week == date_hold + 1)))$Linelist_P2_PCR_Week_Cases
    Linelist_Data_hold$Linelist_P2_PCR_Next_Week_Cases[i] <- next_cases
  }else if(date_hold == 132){
    prev_cases <- filter(Linelist_Data_hold, ((areaCode == areaCode_hold)&(Week == date_hold - 1)))$Linelist_P2_PCR_Week_Cases
    Linelist_Data_hold$Linelist_P2_PCR_Previous_Week_Cases[i] <- prev_cases
  }
  else{
    prev_cases <- filter(Linelist_Data_hold, ((areaCode == areaCode_hold)&(Week == date_hold - 1)))$Linelist_P2_PCR_Week_Cases
    next_cases <- filter(Linelist_Data_hold, ((areaCode == areaCode_hold)&(Week == date_hold + 1)))$Linelist_P2_PCR_Week_Cases
    Linelist_Data_hold$Linelist_P2_PCR_Previous_Week_Cases[i] <- prev_cases
    Linelist_Data_hold$Linelist_P2_PCR_Next_Week_Cases[i] <- next_cases
  }
  
}

#Drop "week one", as we don't have the previous week cases.
Linelist_Data_hold <- drop_na(Linelist_Data_hold)

rm(data_hold, areaCode_hold, date_hold, i, data_hold2)

#Now re-join the data
Case_Rates_Data <- merge(Case_Rates_Data_hold, Linelist_Data_hold, by = c("areaCode", "Week"))

rm(Case_Rates_Data_hold, Linelist_Data_hold)
##############################################################

#We need to replace the NA's for vaccination:
NA_vacc_data <- filter(Case_Rates_Data, is.na(Case_Rates_Data$cumVaccPercentage_FirstDose))

#VACCINATION ASSUMPTIONs
#Step 1
#If NA and not in Wales, then set to 0

for(i in 1:length(Case_Rates_Data$Week)){
  if(!(grepl('W', Case_Rates_Data$areaCode[i]))){
    if(is.na(Case_Rates_Data$cumVaccPercentage_FirstDose[i])){
      Case_Rates_Data$cumVaccPercentage_FirstDose[i] <- 0
    }
    if(is.na(Case_Rates_Data$cumVaccPercentage_SecondDose[i])){
      Case_Rates_Data$cumVaccPercentage_SecondDose[i] <- 0
    }
    if(is.na(Case_Rates_Data$cumVaccPercentage_ThirdDose[i])){
      Case_Rates_Data$cumVaccPercentage_ThirdDose[i] <- 0
    }
  }
}

#Step 2
#Set all Welsh figures equal to the Wales national average for that day
#We use the data downloaded from API on November 11th 2022
#Wales_First_Vaccination_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&format=csv')
Wales_First_Vaccination_Data <- read.csv('Data/Wales_cum_first_vacc.csv')

#Wales_Second_Vaccination_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumVaccinationSecondDoseUptakeByPublishDatePercentage&format=csv')
Wales_Second_Vaccination_Data <- read.csv('Data/Wales_cum_second_vacc.csv')

#Wales_Third_Vaccination_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumVaccinationThirdInjectionUptakeByPublishDatePercentage&format=csv')
Wales_Third_Vaccination_Data <- read.csv('Data/Wales_cum_third_vacc.csv')

for(i in 1:length(Case_Rates_Data$Week)){
  if((grepl('W', Case_Rates_Data$areaCode[i]))){
    if(is.na(Case_Rates_Data$cumVaccPercentage_FirstDose[i])){
        vacc_hold <- filter(Wales_First_Vaccination_Data, Wales_First_Vaccination_Data$date == Case_Rates_Data$date_begin[i])
        if(length(vacc_hold)>0){
        Case_Rates_Data$cumVaccPercentage_FirstDose[i] <- vacc_hold$cumVaccinationFirstDoseUptakeByPublishDatePercentage[1]
        }
    }
    
    if(is.na(Case_Rates_Data$cumVaccPercentage_SecondDose[i])){
      vacc_hold <- filter(Wales_Second_Vaccination_Data, Wales_Second_Vaccination_Data$date == Case_Rates_Data$date_begin[i])
      if(length(vacc_hold)>0){
        Case_Rates_Data$cumVaccPercentage_SecondDose[i] <- vacc_hold$cumVaccinationSecondDoseUptakeByPublishDatePercentage[1]
      }
    }
    
    if(is.na(Case_Rates_Data$cumVaccPercentage_ThirdDose[i])){
      vacc_hold <- filter(Wales_Third_Vaccination_Data, Wales_Third_Vaccination_Data$date == Case_Rates_Data$date_begin[i])
      if(length(vacc_hold)>0){
        Case_Rates_Data$cumVaccPercentage_ThirdDose[i] <- vacc_hold$cumVaccinationThirdInjectionUptakeByPublishDatePercentage[1]
      }
    }
    
  }
}

#Step 3 
#Everything left as NA is pre-vaccination Wales, so set to 0
for(i in 1:length(Case_Rates_Data$Week)){
  if(is.na(Case_Rates_Data$cumVaccPercentage_FirstDose[i])){
    Case_Rates_Data$cumVaccPercentage_FirstDose[i] <- 0
  } 
  if(is.na(Case_Rates_Data$cumVaccPercentage_SecondDose[i])){
    Case_Rates_Data$cumVaccPercentage_SecondDose[i] <- 0
  } 
  if(is.na(Case_Rates_Data$cumVaccPercentage_ThirdDose[i])){
    Case_Rates_Data$cumVaccPercentage_ThirdDose[i] <- 0
  } 
}

rm(NA_vacc_data, vacc_hold, Wales_First_Vaccination_Data,
   Wales_Second_Vaccination_Data, Wales_Third_Vaccination_Data)

#######################
#Order the data by index
Case_Rates_Data <- Case_Rates_Data[order(Case_Rates_Data$INDEX),]

###################################################
# Affix more covariates
###################################################
Covar_Data <- read.csv('Data/combined_covariates.csv')

#For now we're picking out just these covariates:
Covars_keep <- c('LTLA19CD',
                 'prop_white_british',
                 'prop_all_other_white',
                 'prop_mixed_multiple',
                 'prop_asian',
                 'prop_black_afr_car',
                 'prop_other',
                 'IMD_Average_score',
                 'IMD_Rank_of_average_score',
                 'prop_travelling_to_work',
                 'resident_earnings',
                 'mean_age',
                 'mean_popden',
                 'prop_o65',
                 'prop_not_in_work')

#Filter down just to the above columns
Covar_Data <- Covar_Data[ , which(names(Covar_Data) %in% Covars_keep)]

#Rename areaCode
colnames(Covar_Data)[colnames(Covar_Data) == 'LTLA19CD'] <- 'areaCode'

#(remove City of London because it's data is incompatible)
Covar_Data <- filter(Covar_Data, areaCode != 'E09000001')
Covar_Data$resident_earnings <- as.numeric(Covar_Data$resident_earnings)


#We are missing info for: 
#E06000060 - Buckinghamshire
#E06000061 - North Northamptonshire
#E06000062 - West Northamptonshire

#This is because, to match the case counts, we had to stick together:
#Buckinghamshire = South Bucks ("E07000006"), Aylesbury Vale ("E07000004"), Chiltern ("E07000005"), Wycombe ("E07000007"), 
#West Northamptonshire = Northampton ("E07000154") , South Northamptonshire ("E07000155"), Daventry ("E07000151")
#North Northamptonshire = East Northamptonshire ("E07000152"), Wellingborough ("E07000156"), Corby ("E07000150"), Kettering ("E07000153")

#Make this change manually
Buck_codes <- c("E07000004", "E07000005", "E07000006", "E07000007")
North_North_codes <- c("E07000153", "E07000150", "E07000156", "E07000152")
West_North_codes <- c("E07000151", "E07000155", "E07000154")

Covar_Data_full <- read.csv('Data/combined_covariates.csv')
colnames(Covar_Data_full)[colnames(Covar_Data_full) == 'LTLA19CD'] <- 'areaCode'
Covar_Data_full <- filter(Covar_Data_full, areaCode != 'E09000001')
Covar_Data_full$resident_earnings <-as.numeric(Covar_Data_full$resident_earnings)

Buck_data <- filter(Covar_Data_full, areaCode %in% Buck_codes)
North_North_data <- filter(Covar_Data_full, areaCode %in% North_North_codes)
West_North_data <- filter(Covar_Data_full, areaCode %in% West_North_codes)


Covar_Extra <- data.frame(areaCode = c("E06000060", "E06000061", "E06000062"),
                          prop_travelling_to_work = c((sum(Buck_data$nomis_total_people*Buck_data$prop_travelling_to_work)/sum(Buck_data$nomis_total_people)),
                                                      (sum(North_North_data$nomis_total_people*North_North_data$prop_travelling_to_work)/sum(North_North_data$nomis_total_people)),
                                                      (sum(West_North_data$nomis_total_people*West_North_data$prop_travelling_to_work)/sum(West_North_data$nomis_total_people))),
                          
                          prop_not_in_work = c((sum(Buck_data$nomis_total_people*Buck_data$prop_not_in_work)/sum(Buck_data$nomis_total_people)),
                                               (sum(North_North_data$nomis_total_people*North_North_data$prop_not_in_work)/sum(North_North_data$nomis_total_people)),
                                               (sum(West_North_data$nomis_total_people*West_North_data$prop_not_in_work)/sum(West_North_data$nomis_total_people))),
                          
                          prop_white_british = c((sum(Buck_data$total_people_eth*Buck_data$prop_white_british)/sum(Buck_data$total_people_eth)),
                                                 (sum(North_North_data$total_people_eth*North_North_data$prop_white_british)/sum(North_North_data$total_people_eth)),
                                                 (sum(West_North_data$total_people_eth*West_North_data$prop_white_british)/sum(West_North_data$total_people_eth))),
                          
                          prop_all_other_white = c((sum(Buck_data$total_people_eth*Buck_data$prop_all_other_white)/sum(Buck_data$total_people_eth)),
                                                   (sum(North_North_data$total_people_eth*North_North_data$prop_all_other_white)/sum(North_North_data$total_people_eth)),
                                                   (sum(West_North_data$total_people_eth*West_North_data$prop_all_other_white)/sum(West_North_data$total_people_eth))),
                          
                          prop_mixed_multiple = c((sum(Buck_data$total_people_eth*Buck_data$prop_mixed_multiple)/sum(Buck_data$total_people_eth)),
                                                  (sum(North_North_data$total_people_eth*North_North_data$prop_mixed_multiple)/sum(North_North_data$total_people_eth)),
                                                  (sum(West_North_data$total_people_eth*West_North_data$prop_mixed_multiple)/sum(West_North_data$total_people_eth))),
                          
                          prop_asian = c((sum(Buck_data$total_people_eth*Buck_data$prop_asian)/sum(Buck_data$total_people_eth)),
                                         (sum(North_North_data$total_people_eth*North_North_data$prop_asian)/sum(North_North_data$total_people_eth)),
                                         (sum(West_North_data$total_people_eth*West_North_data$prop_asian)/sum(West_North_data$total_people_eth))),
                          
                          prop_black_afr_car = c((sum(Buck_data$total_people_eth*Buck_data$prop_black_afr_car)/sum(Buck_data$total_people_eth)),
                                                 (sum(North_North_data$total_people_eth*North_North_data$prop_black_afr_car)/sum(North_North_data$total_people_eth)),
                                                 (sum(West_North_data$total_people_eth*West_North_data$prop_black_afr_car)/sum(West_North_data$total_people_eth))),
                          
                          prop_other = c((sum(Buck_data$total_people_eth*Buck_data$prop_other)/sum(Buck_data$total_people_eth)),
                                         (sum(North_North_data$total_people_eth*North_North_data$prop_other)/sum(North_North_data$total_people_eth)),
                                         (sum(West_North_data$total_people_eth*West_North_data$prop_other)/sum(West_North_data$total_people_eth))),
                          
                          IMD_Average_score = c(sum(Buck_data$IMD_Average_score*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                                sum(North_North_data$IMD_Average_score*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                                sum(West_North_data$IMD_Average_score*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          #An estimation
                          IMD_Rank_of_average_score = c(sum(Buck_data$IMD_Rank_of_average_score*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                                        sum(North_North_data$IMD_Rank_of_average_score*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                                        sum(West_North_data$IMD_Rank_of_average_score*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          
                          resident_earnings = c(sum(Buck_data$resident_earnings*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                                sum(North_North_data$resident_earnings*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                                sum(West_North_data$resident_earnings*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          
                          mean_age = c(sum(Buck_data$mean_age*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                       sum(North_North_data$mean_age*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                       sum(West_North_data$mean_age*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          
                          prop_o65 = c(sum(Buck_data$prop_o65*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                       sum(North_North_data$prop_o65*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                       sum(West_North_data$prop_o65*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          #Estimating this just from the population 2019 and sqrt data
                          mean_popden = c(543973/1563,
                                          (94527+79707+72218+101776)/(510+163+80+233),
                                          (753278+94490+85950)/(2364+634+663))
)

#stick the two together
Covar_Data <- rbind(Covar_Data, Covar_Extra)

rm(Covar_Data_full, Buck_data, North_North_data, West_North_data, Buck_codes, North_North_codes, West_North_codes, Covar_Extra, i, Covars_keep)

#Now merge with Case_Rates_Data
Case_Rates_Data <- merge(Case_Rates_Data, Covar_Data, by = 'areaCode', all.x = TRUE)

################
#Add earnings data
################
#This is data taken from here: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8

Earnings_Data <- read.csv('Data/Annual_median_income.csv')



#Fortunately, all Earnings areaCodes are already in the case rate data codes
#We have two "years" split by the tax year change point of 6th April 2021
#"Annual estimates are provided for the tax year that ended on 5th April in the reference year"
Earnings_Data_2021 <- filter(Earnings_Data, year == 2021)
Earnings_Data_2022 <- filter(Earnings_Data, year == 2022)

#Note, we have some "NA" in the earnings data which is marked with an "x"
#if it's missing, we'll use the value from the other year
for(i in 1:length(Earnings_Data_2021$areaCode)){
  year_hold <- Earnings_Data_2021$year[i]
  code_hold <- Earnings_Data_2021$areaCode[i]
  earnings_hold <- Earnings_Data_2021$Median_annual_income[i]
  jobs_hold <- Earnings_Data_2021$no_jobs[i]
  
  if(earnings_hold == "x"){
    pull_earnings <- filter(Earnings_Data_2022, areaCode == code_hold)
      Earnings_Data_2021$Median_annual_income[i] <- pull_earnings$Median_annual_income[1]
  }
  
  if(jobs_hold == "x"){
    pull_jobs <- filter(Earnings_Data_2022, areaCode == code_hold)
    Earnings_Data_2021$no_jobs[i] <- pull_earnings$no_jobs[1]
  }
}

for(i in 1:length(Earnings_Data_2022$areaCode)){
  year_hold <- Earnings_Data_2022$year[i]
  code_hold <- Earnings_Data_2022$areaCode[i]
  earnings_hold <- Earnings_Data_2022$Median_annual_income[i]
  jobs_hold <- Earnings_Data_2022$no_jobs[i]
  
  if(earnings_hold == "x"){
    pull_earnings <- filter(Earnings_Data_2021, areaCode == code_hold)
    Earnings_Data_2022$Median_annual_income[i] <- pull_earnings$Median_annual_income[1]
  }
  
  if(jobs_hold == "x"){
    pull_jobs <- filter(Earnings_Data_2021, areaCode == code_hold)
    Earnings_Data_2022$no_jobs[i] <- pull_earnings$no_jobs[1]
  }
}




#Make new cols
Case_Rates_Data$Median_annual_income <- NA
Case_Rates_Data$no_jobs <- NA

for(i in 1:length(Case_Rates_Data$areaCode)){
  date_hold <- Case_Rates_Data$date_begin[i]
  area_hold <- Case_Rates_Data$areaCode[i]
  
  if(date_hold > as.Date("2021-04-06")){
    earnings_hold <- filter(Earnings_Data_2022, areaCode == area_hold )
    Case_Rates_Data$Median_annual_income[i] <- earnings_hold$Median_annual_income[1]
    Case_Rates_Data$no_jobs[i] <- earnings_hold$no_jobs[1]
  } else if(date_hold <= as.Date("2021-04-06")){
    earnings_hold <- filter(Earnings_Data_2021, areaCode == area_hold )
    Case_Rates_Data$Median_annual_income[i] <- earnings_hold$Median_annual_income[1]
    Case_Rates_Data$no_jobs[i] <- earnings_hold$no_jobs[1]
  } else{
    print("Incompatible date for earnings data")
  }
  
}

#remove held data
rm(earnings_hold, area_hold, date_hold, Earnings_Data_2021,
   Earnings_Data_2022, Earnings_Data, year_hold, code_hold, jobs_hold,
   pull_earnings, pull_jobs)


#Need to convert these earnings/jobs data to numeric
Case_Rates_Data$Median_annual_income <- as.numeric(Case_Rates_Data$Median_annual_income)
Case_Rates_Data$no_jobs <- as.numeric(Case_Rates_Data$no_jobs)

################
#Add mobility data
################

#data from https://www.google.com/covid19/mobility/

#The data shows how visitors to (or time spent in) categorized places change compared to 
#our baseline days. A baseline day represents a normal value for that day of the week. 
#The baseline day is the median value from the 5‑week period Jan 3 – Feb 6, 2020.

#For each region-category, the baseline isn’t a single value—it’s 7 individual values.
#The same number of visitors on 2 different days of the week, result in different
#percentage changes. So, we recommend the following:
#Don’t infer that larger changes mean more visitors or smaller changes mean less visitors.
#Avoid comparing day-to-day changes. Especially weekends with weekdays.

#The data shows how visits to places, such as corner shops and parks, are changing 
#in each geographic region.

#The Community Mobility Reports are no longer being updated as of 2022-10-15. 
#All historical data will remain publicly available for research purposes.
Google_2020_Data <- read.csv('Data/2020_GB_Region_Mobility_Report.csv')
Google_2021_Data <- read.csv('Data/2021_GB_Region_Mobility_Report.csv')
Google_2022_Data <- read.csv('Data/2022_GB_Region_Mobility_Report.csv')
#combine them:
Google_Total_Data <- rbind(Google_2020_Data, Google_2021_Data, Google_2022_Data)
rm(Google_2020_Data, Google_2021_Data, Google_2022_Data)

#We filter to one day out to observe different areas:
One_day_google <- filter(Google_Total_Data, date == "2021-03-30")

One_code_google <- Google_Total_Data[!duplicated(Google_Total_Data$place_id),]


#Google has an odd place name level structure.
#We extract the name of "region 2" and see if it exists in our data frame currently
#If not, we then also search it's "region 1" name.
Reduced_Data <- filter(Case_Rates_Data, Week == 100)

One_code_google$areaCode <- NA

for(i in 1:length(Reduced_Data$areaCode)){
  search_string <- Reduced_Data$areaName[i]
  
  if( sum( grepl( search_string, One_code_google$sub_region_2 , fixed = TRUE)   ) > 0  ){
    #search string was in region 2
    
    Reduced_google <- filter(One_code_google, grepl( search_string, One_code_google$sub_region_2 , fixed = TRUE) )
    
    if ( nrow(Reduced_google) == 1){
      #Just one match with the area name:
      insert_index <- which(grepl( search_string, One_code_google$sub_region_2 , fixed = TRUE))
      
      One_code_google$areaCode[insert_index] <- Reduced_Data$areaCode[i]
      
    }else{
      print(sprintf('Multiple region 2 locations found for %s', search_string))
      print(Reduced_Data$areaCode[i])
      print(i)
    }
    
  } else{
    #Couldn't find search string in region 2. So look for a unique region 1
    
    #filter out everything with a region 2 specifier
    Reduced_google <- filter(One_code_google, sub_region_2 == "")
    
    #Now see if the search string exists in region 1:
    if( sum( grepl( search_string, Reduced_google$sub_region_1 , fixed = TRUE)   ) > 0  ){
      #search string was in region 1
      Reduced_google <- filter(Reduced_google, grepl( search_string, Reduced_google$sub_region_1 , fixed = TRUE) )
      
      if ( nrow(Reduced_google) == 1){
        #Just one match with the area name:
        Google_code_match <- Reduced_google$place_id[1]
        insert_index <- which(One_code_google$place_id == Google_code_match)
        
        One_code_google$areaCode[insert_index] <- Reduced_Data$areaCode[i]
        
      }else{
        print(sprintf('Multiple region 1 locations found for %s', search_string))
        print(Reduced_Data$areaCode[i])
        print(i)
      }
      
      
    } else {
      print(sprintf('Name %s not found in region 1 or 2', search_string))
      print(Reduced_Data$areaCode[i])
      print(i)
    }
    
  }
  
  
  
}

Google_conversion <- One_code_google[,-c(6,7,9,10,11,12,13,14,15)]
#For those not found, we manually assign
#26 LTLA codes not assigned:
# "E06000010" "E06000014" "E06000015" "E06000016" "E06000018"
# "E06000019" "E06000023" "E06000052" "E06000055" "E06000061"
# "E06000062" "E07000008" "E07000064" "E07000112" "E07000128"
# "E07000178" "E07000197" "E07000222" "E07000240" "E07000244"
# "E07000246" "E08000013" "E09000005" "E09000012" "S12000036"
# "S12000038"

# "E06000010" - Kingston upon Hull, City of
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Kingston upon Hull")] <- "E06000010"
#"E06000014" - York
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "York")] <- "E06000014"
#"E06000015" - Derby
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Derby")] <- "E06000015"
#"E06000016" - Leicester
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Leicester")] <- "E06000016"
#"E06000018" - Nottingham
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Nottingham")] <- "E06000018"
#"E06000019" - Herefordshire
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Herefordshire")] <- "E06000019"
#"E06000023" - Bristol City
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Bristol City")] <- "E06000023"
#"E06000052" - Cornwall (two listed, one is Penryn which we ignore)
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Cornwall")[1]] <- "E06000052"
#"E06000055" - Bedford
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Bedford")] <- "E06000055"
#"E06000061" - North Northamptonshire
#This is one of the problematic ones, recall that:
#Buckinghamshire = South Bucks ("E07000006"), Aylesbury Vale ("E07000004"), Chiltern ("E07000005"), Wycombe ("E07000007"), 
#West Northamptonshire = Northampton ("E07000154") , South Northamptonshire ("E07000155"), Daventry ("E07000151")
#North Northamptonshire = East Northamptonshire ("E07000152"), Wellingborough ("E07000156"), Corby ("E07000150"), Kettering ("E07000153")
#so we assign this code to 4 different areas:
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "East Northamptonshire")] <- "E06000061"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Borough of Wellingborough")] <- "E06000061"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Corby District")] <- "E06000061"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Borough of Kettering")] <- "E06000061"
#"E06000062" - West Northamptonshire
#we assign this code to 3 different areas
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Northampton District")] <- "E06000062"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "South Northamptonshire District")] <- "E06000062"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Daventry District")] <- "E06000062"
#"E07000008" - Cambridge
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Cambridge")] <- "E07000008"
#"E07000064" - Rother District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Rother District")] <- "E07000064"
#"E07000112" - Folkestone & Hythe District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Folkestone & Hythe District")] <- "E07000112"
#"E07000128" - Wyre District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Wyre District")] <- "E07000128"
#"E07000178" - Oxford
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Oxford")] <- "E07000178"
#"E07000197" - Stafford District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Stafford District")] <- "E07000197"
#"E07000222" - Warwick
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Warwick")] <- "E07000222"
#"E07000240" - Saint Albans District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Saint Albans District")] <- "E07000240"
#"E07000244" - East Suffolk
#This is actually made up of two areas: Suffolk Coastal District, Waveney District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Suffolk Coastal District")] <- "E07000244"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Waveney District")] <- "E07000244"
#"E07000246" - Somerset West and Taunton
#Also made up of two: West Somerset District, Taunton Deane
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "West Somerset District")] <- "E07000246"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Taunton Deane")] <- "E07000246"
#"E08000013" - Metropolitan Borough of St Helens
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Metropolitan Borough of St Helens")] <- "E08000013"
#"E09000005" - London Borough of Brent
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "London Borough of Brent")] <- "E09000005"
#"E09000012" - London Borough of Hackney
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "London Borough of Hackney")] <- "E09000012"
#"S12000036" - Edinburgh
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Edinburgh")] <- "S12000036"
#"S12000038" - Renfrewshire
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Renfrewshire")] <- "S12000038"

#All areaCodes from the case rates data are now in this "Google_conversion" sheet.
#We save this for reference
write.csv(Google_conversion, file = 'Outputs/Google_codes_LTLA_codes.csv')


Google_conversion <- filter(Google_conversion, !is.na(areaCode))

#Now assign an area code to everything in Google_Total_Data

Google_Total_Data <- merge(x = Google_Total_Data, y = Google_conversion[ , c("place_id", "areaCode")], by = "place_id", all.x=TRUE)

Google_Total_Data <- filter(Google_Total_Data, !is.na(areaCode))

Google_Total_Data$date <- as.Date(Google_Total_Data$date)

#Now average together the areas which have multiple locations under the code:
Trouble_Codes <- c('E06000061', #North Northamptonshire
                   'E06000062', #West Northamptonshire
                   'E09000012', #Hackney and City of London     #NOTE, CITY OF LONDON HAS NO RESIDENTIAL DATA, so I've not affixed the code to the Google entries for "City of London"
                   'E07000244', #East Suffolk
                   'E07000246' #Taunton and West Somerset
)


#We average the columns by areaCode AND date
#We use the formula method of aggregate. 
#The variables on the 'rhs' of ~ are the grouping variables while 
#the . represents all other variables in the 'df1' 
#(from the example, we assume that we need the mean for all the 
#columns except the grouping)
#aggregate(.~id1+id2, df1, mean)

Google_Total_Data <- stats::aggregate(cbind(retail_and_recreation_percent_change_from_baseline,
                                  grocery_and_pharmacy_percent_change_from_baseline,
                                  parks_percent_change_from_baseline,
                                  transit_stations_percent_change_from_baseline,
                                  workplaces_percent_change_from_baseline,
                                  residential_percent_change_from_baseline
                                )~areaCode+date, Google_Total_Data, FUN=mean, na.rm =TRUE,  na.action=na.pass)

#The NAs have changed to NaN, so change them back
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Google_Total_Data[is.nan(Google_Total_Data)] <- NA


Google_Mobility_Data <- Google_Total_Data

write.csv(Google_Mobility_Data, file = 'Outputs/Google_Mobility_Data.csv')


#affix the mobility data to the case_rates data
#We use the mean of the week
Case_Rates_Data$retail_and_recreation_percent_change_from_baseline <- NA
Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline <- NA
Case_Rates_Data$parks_percent_change_from_baseline <- NA
Case_Rates_Data$transit_stations_percent_change_from_baseline <- NA
Case_Rates_Data$workplaces_percent_change_from_baseline <- NA
Case_Rates_Data$residential_percent_change_from_baseline <- NA

for(i in 1:nrow(Case_Rates_Data)){
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  date_hold <- Case_Rates_Data$date_begin[i] + 0:6
  
  Reduced_Mobility <- filter(Google_Mobility_Data, areaCode == areaCode_hold)
  Reduced_Mobility <- filter(Reduced_Mobility, date %in% date_hold)
  mobility_mean <- mean(Reduced_Mobility$Residential_mobility_percentage_change, na.rm = TRUE)
  
  Case_Rates_Data$retail_and_recreation_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$parks_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$transit_stations_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$workplaces_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$residential_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE))
  
}

#To replace NAs, we'll use the average value between weeks surrounding an NA:
for(i in 1:nrow(Case_Rates_Data)){
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  week_hold <- Case_Rates_Data$Week[i]
  week_hold <- week_hold + c(-1,0,1)
  
  Reduced_Mobility <- filter(Case_Rates_Data, areaCode == areaCode_hold)
  Reduced_Mobility <- filter(Reduced_Mobility, Week %in% week_hold )
  
  Case_Rates_Data$retail_and_recreation_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$parks_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$transit_stations_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$workplaces_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$residential_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE))

}

###############
#We now iron out a few remaining issues with NAs for mobility
#Where do we still have missing date for residential?
missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$residential_percent_change_from_baseline)),]
unique(missing_data$areaCode)

#Devolved nations we ignore, as these will later be removed anyway
missing_data <- filter(missing_data, grepl('E', areaCode))

#Majority of these are Rutland due to its very small population

#For Rutland, we're going to take the average of it's neighbors mobility score for these days:
#Rutland is index 97
which(W[97,] == 1)
#Has four neighboring regions, 117, 189, 190, 195
Rutland_neighbors <- c(117, 189, 190, 195)
for(i in 1:length(missing_data$areaCode)){
  if(missing_data$areaName[i] == "Rutland"){
    Week_hold <- missing_data$Week[i]
    Neighbor_hold <- filter(Case_Rates_Data, Week == Week_hold)
    Neighbor_hold <- filter(Neighbor_hold, INDEX %in% Rutland_neighbors)
    residential_mean_hold <- mean(Neighbor_hold$residential_percent_change_from_baseline)
    
    Case_Rates_Data$residential_percent_change_from_baseline[which((Case_Rates_Data$areaName == "Rutland")&(Case_Rates_Data$Week == Week_hold))] <- residential_mean_hold
    
  }
}

#See what is still missing:
missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$residential_percent_change_from_baseline)),]
missing_data <- filter(missing_data, grepl('E', areaCode))
#Most of these are week 130/131, which we will not be considering, so can ignore
missing_data <- filter(missing_data, Week < 100)

#For these remaining, all that are week 2 and 3, I'm going to use the mobility scores for
#week 4 in those areas
#These dates are within the first lockdown so we don't envision much difference
for( i in 1:length(missing_data$areaCode)){
  Week_hold <- missing_data$Week[i]
  areaCode_hold <- missing_data$areaCode[i]
  
  Neighbor_hold <- filter(Case_Rates_Data, areaCode == areaCode_hold)
  Neighbor_hold <- filter(Neighbor_hold, Week == 4)
  residential_mean_hold <- Neighbor_hold$residential_percent_change_from_baseline[1]
  
  Case_Rates_Data$residential_percent_change_from_baseline[which((Case_Rates_Data$areaCode == areaCode_hold)&(Case_Rates_Data$Week == Week_hold))] <- residential_mean_hold
  
}

missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$residential_percent_change_from_baseline)),]

#Same issue now for the transit station visits


missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$transit_stations_percent_change_from_baseline)),]
missing_data <- filter(missing_data, grepl('E', areaCode))
#Most of these are week 130/131, which we will not be considering, so can ignore
missing_data <- filter(missing_data, Week < 100)
#Every single one is Rutland, so we again use the mean of its neighbours scores

for(i in 1:length(missing_data$areaCode)){
  Week_hold <- missing_data$Week[i]
  Neighbor_hold <- filter(Case_Rates_Data, Week == Week_hold)
  Neighbor_hold <- filter(Neighbor_hold, INDEX %in% Rutland_neighbors)
  transit_mean_hold <- mean(Neighbor_hold$transit_stations_percent_change_from_baseline)
  
  Case_Rates_Data$transit_stations_percent_change_from_baseline[which((Case_Rates_Data$areaName == "Rutland")&(Case_Rates_Data$Week == Week_hold))] <- transit_mean_hold
  
}



#Variant Proportion
######################
#Here we record the proportion of cases by variant as a covariate
#This is too sparse at LTLA level, so we provide aggregates at
#NHS region instead using dashboard data

#We use this file to help us link LTLAs to NHS regions
LTLA_to_region <- read.csv('Data/LTLA_to_Region.csv')

#We have two sources of data
#The first is the one taken from the UK COVID portal, there is a live API:
#Portal_Variant_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=variants&format=csv')
#But for consistency we use the hard-saved version downloaded on Nov 28th 2022:
Portal_Variant_Data <- read.csv("Data/Portal_variant_data.csv")
Portal_Variant_Data <- Portal_Variant_Data[,-c(1,3)]
Portal_Variant_Data$date <- as.Date(Portal_Variant_Data$date)

#We rename the regions
Portal_Variant_Data %>%
  mutate(across('areaName', str_replace, 'North East', 'north_east_and_yorkshire')) %>%
  mutate(across('areaName', str_replace, 'North West', 'north_west')) %>%
  mutate(across('areaName', str_replace, 'Yorkshire and The Humber', 'north_east_and_yorkshire')) %>%
  mutate(across('areaName', str_replace, 'East Midlands', 'midlands')) %>%
  mutate(across('areaName', str_replace, 'West Midlands', 'midlands')) %>%
  mutate(across('areaName', str_replace, 'East of England', 'east_of_england')) %>%
  mutate(across('areaName', str_replace, 'London', 'london')) %>%
  mutate(across('areaName', str_replace, 'South East', 'south_east')) %>%
  mutate(across('areaName', str_replace, 'South West', 'south_west')) -> Portal_Variant_Data

#Multiply out the number of cases for Portal_Variant_Data
#To then aggregate back into a 7-region format
Portal_Variant_Data$cases <- round(Portal_Variant_Data$cumWeeklySequenced*(Portal_Variant_Data$newWeeklyPercentage/100))
Portal_Variant_Data <- Portal_Variant_Data[,-c(4,5)]

#Aggregate
Portal_Variant_Data <- aggregate(Portal_Variant_Data$cases,
                              by=list(date=Portal_Variant_Data$date, 
                                      nhs_region = Portal_Variant_Data$areaName, 
                                      variant = Portal_Variant_Data$variant),
                              FUN=sum)

colnames(Portal_Variant_Data) <- c("date", "nhs_region",
                                   "variant", "cases")


group_by(Portal_Variant_Data, date, nhs_region) %>% 
  mutate(newWeeklyPercentage = 100*(cases/sum(cases))) -> Portal_Variant_Data


#We also have another source, aggregated from the VAM line list.
#This has the benefit of covering the Alpha window. The above
#data only covers Feb-21 onwards.
VAM_Variant_Data <- read.csv("Data/VAM_variant_data.csv")

VAM_Variant_Data %>%
  mutate(across('nhs_region', str_replace, 'North East and Yorkshire', 'north_east_and_yorkshire')) %>%
  mutate(across('nhs_region', str_replace, 'North West', 'north_west')) %>%
  mutate(across('nhs_region', str_replace, 'Midlands', 'midlands')) %>%
  mutate(across('nhs_region', str_replace, 'East of England', 'east_of_england')) %>%
  mutate(across('nhs_region', str_replace, 'London', 'london')) %>%
  mutate(across('nhs_region', str_replace, 'South East', 'south_east')) %>%
  mutate(across('nhs_region', str_replace, 'South West', 'south_west')) -> VAM_Variant_Data

VAM_Variant_Data$date <- as.Date(VAM_Variant_Data$date)

#Now ensure they have the same variant names
#in the VAM data, undetermined means we don't know what it is
#Unclassified means we do, but it's not a significant variant.
#Filter out all the undetermined :
VAM_Variant_Data <- filter(VAM_Variant_Data, variant != "Undetermined")

VAM_Variant_Data %>%
  mutate(across('variant', str_replace, 'Unclassified', 'Other')) %>%
  mutate(across('variant', str_replace, 'VUI-22JAN-01', 'Omicron_BA_2')) %>%
  mutate(across('variant', str_replace, 'VOC-20DEC-01', 'Alpha')) %>%
  mutate(across('variant', str_replace, 'VOC-21APR-02', 'Delta')) %>%
  mutate(across('variant', str_replace, 'VOC-21NOV-01', 'Omicron_BA_1')) %>%
  mutate(across('variant', str_replace, 'VUI-21OCT-01', 'Delta_AY_4_2')) %>%
  mutate(across('variant', str_replace, 'V-22APR-04', 'Omicron_BA_5')) %>%
  mutate(across('variant', str_replace, 'V-22APR-03', 'Omicron_BA_4')) %>%
  mutate(across('variant', str_replace, 'V-22OCT-01', 'Omicron_BQ_1')) -> VAM_Variant_Data

Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, " ", "_")
Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, "\\.", "_")
Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, "\\(", "")
Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, "\\)", "")


Portal_Variant_Data %>%
  mutate(across('variant', str_replace, 'VOC-22JAN-01_Omicron_BA_2', 'Omicron_BA_2')) %>%
  mutate(across('variant', str_replace, 'V-20DEC-01_Alpha', 'Alpha')) %>%
  mutate(across('variant', str_replace, 'V-21APR-02_Delta_B_1_617_2', 'Delta')) %>%
  mutate(across('variant', str_replace, 'VOC-21NOV-01_Omicron_BA_1', 'Omicron_BA_1')) %>%
  mutate(across('variant', str_replace, 'V-21OCT-01_Delta_AY_4_2', 'Delta_AY_4_2')) %>%
  mutate(across('variant', str_replace, 'VOC-22APR-04_Omicron_BA_5', 'Omicron_BA_5')) %>%
  mutate(across('variant', str_replace, 'VOC-22APR-03_Omicron_BA_4', 'Omicron_BA_4')) %>%
  mutate(across('variant', str_replace, 'V-22OCT-01_Omicron_BQ_1', 'Omicron_BQ_1')) -> Portal_Variant_Data
  
#Cut the earlier dates
VAM_Variant_Data <- filter(VAM_Variant_Data, date > minimum_date)

#Aggregate the VAM data into weeks
VAM_Variant_Data$date <- cut.Date(VAM_Variant_Data$date, 
                                  breaks = "week", start.on.monday = FALSE)

VAM_Variant_Data <- aggregate(VAM_Variant_Data$cases,
                              by=list(date=VAM_Variant_Data$date, 
                                      nhs_region = VAM_Variant_Data$nhs_region, 
                                      variant = VAM_Variant_Data$variant),
                              FUN=sum)


colnames(VAM_Variant_Data) <- c("date", "nhs_region", "variant", "cases")

VAM_Variant_Data$date <- as.Date(VAM_Variant_Data$date)
#Calculate the percentages for the VAM data:
group_by(VAM_Variant_Data, date, nhs_region) %>% 
  mutate(newWeeklyPercentage = 100*(cases/sum(cases))) -> VAM_Variant_Data

#plot the two datasets together to see how they vary 

VAM_Variant_Data$data_source <- "VAM"
Portal_Variant_Data$data_source <- "Portal"

#We will use the Portal data but append the earlier VAM data to the list.

#So cut the later VAM data:
VAM_Variant_Data <- filter(VAM_Variant_Data, date < as.Date("2021-02-14"))

#Stitch the two together
Variant_Data <- rbind(VAM_Variant_Data, Portal_Variant_Data)

ggplot(Variant_Data) +
  geom_line(aes(x = date, y = newWeeklyPercentage, color = variant,
                lty = data_source)) +
  facet_wrap(~ nhs_region) -> variant_plot

ggsave(filename = "Variants.png",
       path = 'Outputs/', plot = variant_plot,
       dpi=300, 
       height=20, width=35, units="cm")


write.csv(Variant_Data, file = "Outputs/Variants_data.csv")


#Now add this to Case_Rates_Data
#We want a separate column for each variant
Variant_Data <- Variant_Data[,-c(4,6)]

Variant_Data %>%
  pivot_wider(names_from = variant, 
              values_from = newWeeklyPercentage) -> Variant_Data

#Set all the NAs to 0s
Variant_Data[is.na(Variant_Data)] <- 0

Case_Rates_Data$Alpha_proportion <- NA
Case_Rates_Data$Delta_proportion <- NA
Case_Rates_Data$Delta_AY_4_2_proportion <- NA
Case_Rates_Data$Omicron_BA_1_proportion <- NA
Case_Rates_Data$Omicron_BA_2_proportion <- NA
Case_Rates_Data$Other_proportion <- NA
Case_Rates_Data$Omicron_BQ_1_proportion <- NA
Case_Rates_Data$Omicron_BA_4_proportion <- NA
Case_Rates_Data$Omicron_BA_5_proportion <- NA

Available_codes <- unique(LTLA_to_region$LAD21CD)

for(i in 1:nrow(Case_Rates_Data)){
  #read date
  date_hold <- Case_Rates_Data$date_begin[i]
  LTLA_hold <- Case_Rates_Data$areaCode[i]
  #Check we have data available
  
  
  if(!(LTLA_hold %in% Available_codes)){
    print(sprintf("Warning, unrecognised LTLA code in row %s", i)) #Note, this will happen for Scotland, Wales, NI
  }else{
    region_hold <- LTLA_to_region$RGN21NM[which(LTLA_to_region$LAD21CD == LTLA_hold)]
    
    variant_reduced <- filter(Variant_Data, nhs_region == region_hold)
    variant_reduced <- filter(variant_reduced, date == date_hold)
    
    Case_Rates_Data$Alpha_proportion[i] <- variant_reduced$Alpha[1]
    Case_Rates_Data$Delta_proportion[i] <- variant_reduced$Delta[1]
    Case_Rates_Data$Delta_AY_4_2_proportion[i] <- variant_reduced$Delta_AY_4_2[1]
    Case_Rates_Data$Omicron_BA_1_proportion[i] <- variant_reduced$Omicron_BA_1[1]
    Case_Rates_Data$Omicron_BA_2_proportion[i] <- variant_reduced$Omicron_BA_2[1]
    Case_Rates_Data$Other_proportion[i] <- variant_reduced$Other[1]
    Case_Rates_Data$Omicron_BQ_1_proportion[i] <- variant_reduced$Omicron_BQ_1[1]
    Case_Rates_Data$Omicron_BA_4_proportion[i] <- variant_reduced$Omicron_BA_4[1]
    Case_Rates_Data$Omicron_BA_5_proportion[i] <- variant_reduced$Omicron_BA_5[1]
    
  }
  
}

##################
# The downside to the VAM data is that it's only at the NHS region level
# the s-gene target failure (SGTF) data meanwhile is at the LTLA level, and can be used
# as a proxy to signal variant, if we pick certain dates at which to interpret s_pos
# and s_neg as particular variants. We build these columns below:

#Load in the SGTF data
SGTF_Data <- read.csv("Data/s_by_ltla_pillar2_pcr.csv")

#First, collapse this to be weekly sums as opposed to daily.
SGTF_Data$specimen_date <- as.Date(SGTF_Data$specimen_date)

SGTF_Data$date <- cut.Date(SGTF_Data$specimen_date, 
                                  breaks = "week", start.on.monday = FALSE)

#We're going to alter the troublesome LTLA codes now before we aggregate;
#Three england LTLAs not covered: "E06000060" "E06000061" "E06000062"
#E06000060 - Buckinghamshire
#E06000061 - North Northamptonshire
#E06000062 - West Northamptonshire

#This is because, to match the case counts we had to stick together:
#Buckinghamshire = South Bucks ("E07000006"), Aylesbury Vale ("E07000004"), Chiltern ("E07000005"), Wycombe ("E07000007"), 
#West Northamptonshire = Northampton ("E07000154") , South Northamptonshire ("E07000155"), Daventry ("E07000151")
#North Northamptonshire = East Northamptonshire ("E07000152"), Wellingborough ("E07000156"), Corby ("E07000150"), Kettering ("E07000153")

Buck_codes <- c("E07000004", "E07000005", "E07000006", "E07000007")
North_North_codes <- c("E07000153", "E07000150", "E07000156", "E07000152")
West_North_codes <- c("E07000151", "E07000155", "E07000154")

SGTF_Data$ltla_code[which(SGTF_Data$ltla_code %in% Buck_codes)] <- "E06000060"
SGTF_Data$ltla_code[which(SGTF_Data$ltla_code %in% North_North_codes)] <- "E06000061"
SGTF_Data$ltla_code[which(SGTF_Data$ltla_code %in% West_North_codes)] <- "E06000062"

SGTF_Data <- aggregate(cbind(SGTF_Data$s_positive, SGTF_Data$s_negative),
                              by=list(date=SGTF_Data$date, 
                                      ltla_code = SGTF_Data$ltla_code),
                              FUN=sum)


colnames(SGTF_Data) <- c("date", "ltla_code", "s_positive", "s_negative")

SGTF_Data$date <- as.Date(SGTF_Data$date)

#double check that all LTLAs in the data exist in the Case_Rates_Data and vice versa
Case_Data_areaCodes <- unique(Case_Rates_Data$areaCode)
SGTF_areaCodes <- unique(SGTF_Data$ltla_code)
#Note that we don't have SGTF for the devolved nations
Case_Data_areaCodes[which(!(Case_Data_areaCodes %in% SGTF_areaCodes))]
SGTF_areaCodes[which(!(SGTF_areaCodes %in% Case_Data_areaCodes))]
#Only ones we don't use are Isle of Wight, Isles of Scilly, and City of London, which is what we expected


#Next, we need to define how we interpret s_pos and s_neg at certain time-points
#Key Dates:
#START - 2021-03-21 : For these weeks, s_positive is wild type, and s-negative is Alpha
#2021-03-28 - 2021-09-05 : For these weeks, s_positive is Delta, and s-negative is Alpha
#2021-09-12 - 2021-12-26: For these weeks, s_positive is Delta, and s-negative is Omicron 
#2022-01-02 - From this point on, we say it's all Omicron, as BA.2 is s-positive, hard to capture the blur out of delta

#Note, SGTF data ends at May 29th 2022, after which we call it all Omicron
Case_Rates_Data$s_Wild_prop <- NA
Case_Rates_Data$s_Alpha_prop <- NA
Case_Rates_Data$s_Delta_prop <- NA
Case_Rates_Data$s_Omicron_prop <- NA

for(i in 1:length(Case_Rates_Data$Week)){
  
  date_hold <- Case_Rates_Data$date_begin[i]
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  
  s_pos_hold <- SGTF_Data$s_positive[which((SGTF_Data$date == date_hold)&(SGTF_Data$ltla_code == areaCode_hold))]
  s_neg_hold <- SGTF_Data$s_negative[which((SGTF_Data$date == date_hold)&(SGTF_Data$ltla_code == areaCode_hold))]
  s_total_hold <- s_pos_hold + s_neg_hold
  if(length(s_total_hold) == 0){
    s_total_hold <- 0
  }

  if(date_hold < as.Date("2021-03-28")){
    Case_Rates_Data$s_Delta_prop[i] <- 0
    Case_Rates_Data$s_Omicron_prop[i] <- 0
    if(s_total_hold == 0){
      Case_Rates_Data$s_Wild_prop[i] <- 1
      Case_Rates_Data$s_Alpha_prop[i] <- 0
    } else{
      Case_Rates_Data$s_Wild_prop[i] <- s_pos_hold/s_total_hold
      Case_Rates_Data$s_Alpha_prop[i] <- s_neg_hold/s_total_hold
    }
  }else if(date_hold < as.Date("2021-09-12")){
    Case_Rates_Data$s_Omicron_prop[i] <- 0
    Case_Rates_Data$s_Wild_prop[i] <- 0
    if(s_total_hold == 0){
      Case_Rates_Data$s_Delta_prop[i] <- 1
      Case_Rates_Data$s_Alpha_prop[i] <- 0
    }else{
      Case_Rates_Data$s_Delta_prop[i] <- s_pos_hold/s_total_hold
      Case_Rates_Data$s_Alpha_prop[i] <- s_neg_hold/s_total_hold
    }
  }else if(date_hold < as.Date("2022-01-12")){
    Case_Rates_Data$s_Wild_prop[i] <- 0
    Case_Rates_Data$s_Alpha_prop[i] <- 0
    if(s_total_hold == 0){
      Case_Rates_Data$s_Delta_prop[i] <- 0
      Case_Rates_Data$s_Omicron_prop[i] <- 1
    } else{
      Case_Rates_Data$s_Delta_prop[i] <- s_pos_hold/s_total_hold
      Case_Rates_Data$s_Omicron_prop[i] <- s_neg_hold/s_total_hold
    }
      
  } else{
    Case_Rates_Data$s_Wild_prop[i] <- 0
    Case_Rates_Data$s_Alpha_prop[i] <- 0
      Case_Rates_Data$s_Delta_prop[i] <- 0
      Case_Rates_Data$s_Omicron_prop[i] <- 1
  }
  
  }

#Now add funding data.
#Start with NHS Funding data, as taken from the allocation doc found here: 
#https://www.england.nhs.uk/allocations/previous/allocations-for-2019-20-to-2023-24/

CCG_LTLA_lookup <- read.csv("Data/LTLA_to_CCG_codes.csv")
NHS_funding_data <- read.csv("Data/NHS_funding_allocations.csv")

Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
Lookup_codes <- unique(CCG_LTLA_lookup[,1])

cols_hold <- colnames(CCG_LTLA_lookup)
cols_hold[1] <- "areaCode"
colnames(CCG_LTLA_lookup) <- cols_hold

CCG_LTLA_lookup <- CCG_LTLA_lookup[,c(1,5,6)]

Case_Rates_Data <- left_join(Case_Rates_Data, CCG_LTLA_lookup, by = 'areaCode')

#Now add the funding data
cols_hold <- colnames(NHS_funding_data)
cols_hold[1] <- "CCG_code"
colnames(NHS_funding_data) <- cols_hold

CCG_Case_codes <- unique(Case_Rates_Data$CCG_2019_Code)
Funding_CCG_codes <- unique(NHS_funding_data$CCG_code)
CCG_Case_codes %in% Funding_CCG_codes
#the NHS Funding data stream here is using two different CCGs for Devon (99P and 99Q)
#This hasn't been correctly changed to 15N, so change all appearances of 15N in Case rates to "QJK" which is the Devon STP
Case_Rates_Data$CCG_2019_Code[which(Case_Rates_Data$CCG_2019_Code == "15N")] <- "QJK"

Case_Rates_Data$NHS_registered_population <- NA

Case_Rates_Data$Core_services_funding <- NA
Case_Rates_Data$Primary_care_funding <- NA
Case_Rates_Data$Specialised_services <- NA

#The allocated funds are designed via a algorithm that calculates a weighted population need
#This column is the above total funding but divided by that weighted population
Case_Rates_Data$Core_services_funding_by_weighted <- NA
Case_Rates_Data$Primary_care_funding_by_weighted <- NA
Case_Rates_Data$Specialised_services_by_weighted <- NA

for(i in 1:length(Case_Rates_Data$areaCode)){
  if(Case_Rates_Data$CCG_2019_Code[i] != ""){
date_hold <- Case_Rates_Data$date_begin[i]
CCG_hold <- Case_Rates_Data$CCG_2019_Code[i]

if(date_hold < as.Date("2021-04-06")){
  funding_hold <- filter(NHS_funding_data, tax_year_start == 2020)
  funding_hold <- filter(funding_hold, CCG_code == CCG_hold)
  funding_hold_core <- filter(funding_hold, funding_type == "core_services")
  funding_hold_primary <- filter(funding_hold, funding_type == "primary_care")
  funding_hold_specialised <- filter(funding_hold, funding_type == "specialised_services")
  
  Case_Rates_Data$NHS_registered_population[i] <- funding_hold$registered_population[1]
  
  Case_Rates_Data$Core_services_funding[i] <- funding_hold_core$funding_allocation[1]
  Case_Rates_Data$Primary_care_funding[i] <- funding_hold_primary$funding_allocation[1]
  Case_Rates_Data$Specialised_services[i] <- funding_hold_specialised$funding_allocation[1]
  
  Case_Rates_Data$Core_services_funding_by_weighted[i] <- funding_hold_core$funding_allocation[1] / funding_hold_core$weighted_population[1]
  Case_Rates_Data$Primary_care_funding_by_weighted[i] <- funding_hold_primary$funding_allocation[1] / funding_hold_primary$weighted_population[1]
  Case_Rates_Data$Specialised_services_by_weighted[i] <- funding_hold_specialised$funding_allocation[1] / funding_hold_specialised$weighted_population[1]
  
} else if(date_hold < as.Date("2022-04-06")){
  funding_hold <- filter(NHS_funding_data, tax_year_start == 2021)
  funding_hold <- filter(funding_hold, CCG_code == CCG_hold)
  funding_hold_core <- filter(funding_hold, funding_type == "core_services")
  funding_hold_primary <- filter(funding_hold, funding_type == "primary_care")
  funding_hold_specialised <- filter(funding_hold, funding_type == "specialised_services")
  
  Case_Rates_Data$NHS_registered_population[i] <- funding_hold$registered_population[1]
  
  Case_Rates_Data$Core_services_funding[i] <- funding_hold_core$funding_allocation[1]
  Case_Rates_Data$Primary_care_funding[i] <- funding_hold_primary$funding_allocation[1]
  Case_Rates_Data$Specialised_services[i] <- funding_hold_specialised$funding_allocation[1]
  
  Case_Rates_Data$Core_services_funding_by_weighted[i] <- funding_hold_core$funding_allocation[1] / funding_hold_core$weighted_population[1]
  Case_Rates_Data$Primary_care_funding_by_weighted[i] <- funding_hold_primary$funding_allocation[1] / funding_hold_primary$weighted_population[1]
  Case_Rates_Data$Specialised_services_by_weighted[i] <- funding_hold_specialised$funding_allocation[1] / funding_hold_specialised$weighted_population[1]
  
} else{
  funding_hold <- filter(NHS_funding_data, tax_year_start == 2022)
  funding_hold <- filter(funding_hold, CCG_code == CCG_hold)
  funding_hold_core <- filter(funding_hold, funding_type == "core_services")
  funding_hold_primary <- filter(funding_hold, funding_type == "primary_care")
  funding_hold_specialised <- filter(funding_hold, funding_type == "specialised_services")
  
  Case_Rates_Data$NHS_registered_population[i] <- funding_hold$registered_population[1]
  
  Case_Rates_Data$Core_services_funding[i] <- funding_hold_core$funding_allocation[1]
  Case_Rates_Data$Primary_care_funding[i] <- funding_hold_primary$funding_allocation[1]
  Case_Rates_Data$Specialised_services[i] <- funding_hold_specialised$funding_allocation[1]
  
  Case_Rates_Data$Core_services_funding_by_weighted[i] <- funding_hold_core$funding_allocation[1] / funding_hold_core$weighted_population[1]
  Case_Rates_Data$Primary_care_funding_by_weighted[i] <- funding_hold_primary$funding_allocation[1] / funding_hold_primary$weighted_population[1]
  Case_Rates_Data$Specialised_services_by_weighted[i] <- funding_hold_specialised$funding_allocation[1] / funding_hold_specialised$weighted_population[1]
  
}

}
}


#Now add specific COVID support funds provided, as taken from here:
#https://www.gov.uk/government/publications/covid-19-emergency-funding-for-local-government
#Outbreak management details here:
#https://www.gov.uk/government/publications/contain-outbreak-management-fund-2021-to-2022/contain-outbreak-management-fund-guidance-financial-year-2021-to-2022
  
COVID_funding <- read.csv("Data/LTLA_Covid_Extra_Funding.csv")
funding_types <- unique(COVID_funding$fund)
ONS_codes <- unique(COVID_funding$ONS_code)
#All the England codes are captured in this funding block
#Turn funding type into a variable
COVID_funding <- pivot_wider(COVID_funding, names_from = fund, values_from = value)

Case_Rates_Data[funding_types] <- NA

#Some funding was given to LTLAs (included), while 
#other funding was given to UTLAs. We estimate by dividing up this UTLA funding amongst the LTLAs
#We do this by LTLA population,

#First, split the data into those that we currently track (LTLAs) and those we don't
COVID_funding_LTLA <- filter(COVID_funding, ONS_code %in% Case_Data_Codes)
COVID_funding_other <- filter(COVID_funding, !(ONS_code %in% Case_Data_Codes))

#We have 30 fire services. I'm going to remove these
COVID_funding_other <- filter(COVID_funding_other, !grepl('E31', ONS_code))

LTLA_to_UTLA <- read.csv("Data/LTLA_to_UTLA_codes.csv")
UTLA_codes <- unique(LTLA_to_UTLA$UTLA21CD)
sum(UTLA_codes %in% COVID_funding_other$ONS_code) #There's 27 codes we can divide up further
LTLA_Populations <- data.frame(areaCode = Case_Rates_Data$areaCode,
                               Population = Case_Rates_Data$Population)
LTLA_Populations <- distinct(LTLA_Populations)

COVID_funding_UTLA <- filter(COVID_funding_other, ONS_code %in% UTLA_codes)
COVID_funding_other <- filter(COVID_funding_other, !(ONS_code %in% UTLA_codes))

#First divide up the UTLAs funding into the respective LTLAs
for(i in 1:length(COVID_funding_UTLA$ONS_code)){
  UTLA_code_hold <- COVID_funding_UTLA$ONS_code[i]
  tax_year_hold <- COVID_funding_UTLA$tax_year_start[i]
  
  #extract which LTLAs this will be divided by:
  LTLAs_hold <- filter(LTLA_to_UTLA, UTLA21CD == UTLA_code_hold)
  LTLA_codes_hold <- unique(LTLAs_hold$LTLA21CD)
  Populations_hold <- filter(LTLA_Populations, areaCode %in% LTLA_codes_hold)
  Population_sum <- sum(Populations_hold$Population)
  
  for(j in 1:length(LTLAs_hold$LTLA21CD)){
    LTLA_code_hold <- LTLAs_hold$LTLA21CD[j]
    
    #There are three specialist cases: Isle of Wight, Isles of Scilly, City of London
    if(LTLA_code_hold == "E06000046"){} else #Isle of Wight
      if(LTLA_code_hold == "E06000053"){
        LTLA_code_hold <- "E06000052" #If Scilly, change to Cornwall
        funding_vector <- COVID_funding_UTLA[i,4:25]

        #Now add this vector to the LTLA version
        if(!(LTLA_code_hold %in% unique(COVID_funding_LTLA$ONS_code))){
          sprintf("Error: We don't have an LTLA code (%s) to give this to.",LTLA_code_hold)
        }
        COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
        
      } else 
      if(LTLA_code_hold == "E09000001"){
        LTLA_code_hold <- "E09000012" #If City of London, change to Hackney
        funding_vector <- COVID_funding_UTLA[i,4:25]
        
        #Now add this vector to the LTLA version
        if(!(LTLA_code_hold %in% unique(COVID_funding_LTLA$ONS_code))){
          sprintf("Error: We don't have an LTLA code (%s) to give this to.",LTLA_code_hold)
        }
        COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
        
      } else{
    
    specific_population_hold <- Populations_hold$Population[which(Populations_hold$areaCode == LTLA_code_hold)]
    funding_vector <- COVID_funding_UTLA[i,4:25]
    funding_vector <- funding_vector*(specific_population_hold/Population_sum)
    
    #Now add this vector to the LTLA version
    if(!(LTLA_code_hold %in% unique(COVID_funding_LTLA$ONS_code))){
      sprintf("Error: We don't have an LTLA code (%s) to give this to.",LTLA_code_hold)
    }
    COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
    
    
  }
  }
}


#Next, we need to divide up this Greater London extra funding across the 32 london districts we include
COVID_funding_london <- filter(COVID_funding_other, ONS_code == "-")
COVID_funding_other <- filter(COVID_funding_other, !(ONS_code == "-"))
London_populations <- filter(LTLA_Populations, grepl('E09', areaCode))
London_sum_populations <- sum(London_populations$Population)
funding_vector <- COVID_funding_london[1,4:25]
for(i in length(London_populations$areaCode)){
  LTLA_code_hold <- London_populations$areaCode[i]
  funding_vector <- funding_vector*(London_populations$Population[i]/London_sum_populations)
  COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
}
funding_vector <- COVID_funding_london[2,4:25]
for(i in length(London_populations$areaCode)){
  LTLA_code_hold <- London_populations$areaCode[i]
  funding_vector <- funding_vector*(London_populations$Population[i]/London_sum_populations)
  COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
}

#Lastly, we need to sort out the northamptonshire issue that's been coming up constantly
#We need to divide the northamptonshire funding across the seven sub-regions, then split that into the separate two councils
northamptonshire_codes <- unique(COVID_funding_other$ONS_code)
#Cut the master UTLA
northamptonshire_codes <- northamptonshire_codes[-6]

west_north_fund_2020 <- filter(COVID_funding_other, tax_year_start == 2020)
west_north_fund_2020 <- filter(west_north_fund_2020, 
                               ONS_code %in% c("E07000154", "E07000155", "E07000151"))
west_north_fund_2020 %>%
  adorn_totals("row") -> west_north_fund_2020
west_north_fund_2020$ONS_code[4] <- "E06000062"
west_north_fund_2020$ONS_name[4] <- "West Northamptonshire"
west_north_fund_2020 <- west_north_fund_2020[4,]
west_north_fund_2020$tax_year_start <- 2020
#We then take west northamptonshire's proportional amount from the total "northamptonshire" allocation
#West Northamptonshire population = 406733
#North Northamptonshire population = 350448
west_north_fund_2020[1,4:25] <- west_north_fund_2020[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2020)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(406733/(406733+350448)))

west_north_fund_2021 <- filter(COVID_funding_other, tax_year_start == 2021)
west_north_fund_2021 <- filter(west_north_fund_2021, 
                               ONS_code %in% c("E07000154", "E07000155", "E07000151"))
west_north_fund_2021 %>%
  adorn_totals("row") -> west_north_fund_2021
west_north_fund_2021$ONS_code[4] <- "E06000062"
west_north_fund_2021$ONS_name[4] <- "West Northamptonshire"
west_north_fund_2021 <- west_north_fund_2021[4,]
west_north_fund_2021$tax_year_start <- 2021
west_north_fund_2021[1,4:25] <- west_north_fund_2021[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2021)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(406733/(406733+350448)))


north_north_fund_2020 <- filter(COVID_funding_other, tax_year_start == 2020)
north_north_fund_2020 <- filter(north_north_fund_2020, 
                                ONS_code %in% c("E07000152", "E07000156", "E07000150", "E07000153"))
north_north_fund_2020 %>%
  adorn_totals("row") -> north_north_fund_2020
north_north_fund_2020$ONS_code[5] <- "E06000061"
north_north_fund_2020$ONS_name[5] <- "North Northamptonshire"
north_north_fund_2020 <- north_north_fund_2020[5,]
north_north_fund_2020$tax_year_start <- 2020
north_north_fund_2020[1,4:25] <- north_north_fund_2020[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2020)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(350448/(406733+350448)))


north_north_fund_2021 <- filter(COVID_funding_other, tax_year_start == 2021)
north_north_fund_2021 <- filter(north_north_fund_2021, 
                                ONS_code %in% c("E07000152", "E07000156", "E07000150", "E07000153"))
north_north_fund_2021 %>%
  adorn_totals("row") -> north_north_fund_2021
north_north_fund_2021$ONS_code[5] <- "E06000061"
north_north_fund_2021$ONS_name[5] <- "North Northamptonshire"
north_north_fund_2021 <- north_north_fund_2021[5,]
north_north_fund_2021$tax_year_start <- 2021
north_north_fund_2021[1,4:25] <- north_north_fund_2021[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2021)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(350448/(406733+350448)))

COVID_funding_Northamptonshire <- rbind(west_north_fund_2020, west_north_fund_2021, north_north_fund_2020, north_north_fund_2021)

COVID_funding_LTLA <- rbind(COVID_funding_LTLA, COVID_funding_Northamptonshire)

#There are some existing payments to North northamptonshire, add the two together
COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire4" )),4:25]
COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire4" )),4:25]

COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire5" )),4:25]
COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire5" )),4:25]

#Remove the old ones
COVID_funding_LTLA <- filter(COVID_funding_LTLA, !(ONS_name %in% c("North Northamptonshire4", "West Northamptonshire5") ))

#Now, assign those LTLA funding values to the Case Rates dataframe

case_rates_cols <- ncol(Case_Rates_Data)

for(i in 1:length(Case_Rates_Data$areaCode)){
  date_hold <- Case_Rates_Data$date_begin[i]
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  
  if(areaCode_hold %in% ONS_codes){
    
    if(date_hold < as.Date("2021-04-06")){
      
      extra_funding_hold <- filter(COVID_funding_LTLA, tax_year_start == 2020)
      extra_funding_hold <- filter(extra_funding_hold, ONS_code == areaCode_hold)
      
      Case_Rates_Data[i,(case_rates_cols-21):case_rates_cols] <- extra_funding_hold[4:25]
      
    } else{
      extra_funding_hold <- filter(COVID_funding_LTLA, tax_year_start == 2021)
      extra_funding_hold <- filter(extra_funding_hold, ONS_code == areaCode_hold)
      
      Case_Rates_Data[i,(case_rates_cols-21):case_rates_cols] <- extra_funding_hold[4:25]
    }
    
  }
}

#Lastly, we want to export some midpoint coordinates for more spatially-specific spatial kernels
sf_cent <- st_centroid(Boundaries)
sf_cent_geom <- sf_cent$geometry

LTLA_centroids <- data.frame(areaCode = sf_cent$CODE, centroid_x = NA, centroid_y = NA)
for(i in 1:356){
  LTLA_centroids$centroid_x[i] <- as.numeric(sf_cent$geometry[[i]])[1]
  LTLA_centroids$centroid_y[i] <- as.numeric(sf_cent$geometry[[i]])[2]
}

Case_Rates_Data <- merge(Case_Rates_Data, LTLA_centroids, by = c('areaCode'), all.x = TRUE)

#export the data

write.csv(Case_Rates_Data, file = 'Outputs/Cases_Data.csv')
save(Case_Rates_Data, file = 'Outputs/Cases_Data.RData')
save(W, file = 'Outputs/W.RData')
save(Boundaries, file = 'Outputs/Boundaries_Data.RData')

