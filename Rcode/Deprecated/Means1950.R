library("readxl")
library("tidyr")
library("xlsx")

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

#######  read country-region list here ####################################
####### 'df1' is the master data file ######################
dfCountry <- read_excel("Data/00C2_country_region.xlsx",
                        sheet="_00C2_country_region")


#######  read data here ####################################
####### 'df1' is the master data file ######################
df1 <- read_excel("Data/00C1_Export_all_sorted.xlsx",
                  sheet="_00C1_Export_all_sorted")
df1 <- df1 %>% rename( year = Year)
df1$RNI <- df1$CBR - df1$CDR 


Mean1950 <- df1 %>% group_by(RegionName, SubRegionName ) %>% 
  subset( year==1950) %>% 
  summarise(
    nrows = n(),
    meanCBR = mean(CBR, na.rm = TRUE),
    meanCDR = mean(CDR, na.rm = TRUE),
    meanRNI = mean(RNI, na.rm = TRUE),
    n_CDR =sum(!is.na(CDR)),
    n_CBR =sum(!is.na(CBR)),
    n_RNI =sum(!is.na(RNI))
  )

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")
write.xlsx(as.data.frame(Mean1950), file="Output/Means1950.xlsx")
