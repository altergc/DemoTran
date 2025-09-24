########### compute means by subregions 1950 & 2020 #############

library("readxl")
library("tidyr")
library("xlsx")
library(dplyr)

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

###### delete df Means 1950 if it exists ##########
if(exists("Means1950") && is.data.frame(get('Means1950'))) rm(Means1950)


####### Print country names by subregion ##########

dft3 <- df1 %>% filter(year==1950 ) %>%
  select( c(RegionCode, SubRegionCode, 
            RegionName, SubRegionName, Country  )) %>% 
  group_by( RegionName, SubRegionName ) %>%
  summarise(Countries = paste(Country, collapse = ", ")) %>%
  as.data.frame()

write.xlsx(dft3, file="Output/Means1950.xlsx", sheetName="Names", append=FALSE, showNA = FALSE)



###### loop on selected years ##################
for (yy in c( seq(1950, 2020, by=10) ) ) {

MeansTemp <- df1 %>% group_by(RegionName, SubRegionName ) %>% 
  subset( year==yy) %>% 
  summarise(
     nrows  = n(),
     meanCDR  = mean(CDR, na.rm = TRUE),
     meanCBR  = mean(CBR, na.rm = TRUE),
     meanRNI  = mean(RNI, na.rm = TRUE),
     n_CDR  =sum(!is.na(CDR)),
     n_CBR  =sum(!is.na(CBR)),
     n_RNI  =sum(!is.na(RNI))
  )

   for (ii in 3:length(MeansTemp) ) {
     colnames(MeansTemp)[ii] <- paste(colnames(MeansTemp)[ii], yy, sep="")
   }
   
    if(exists("Means1950") && is.data.frame(get('Means1950')))
      Means1950 <- cbind(Means1950, MeansTemp)
    else
      Means1950 <- as.data.frame(MeansTemp)   

}



setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")
write.xlsx(as.data.frame(Means1950), file="Output/Means1950.xlsx", sheetName="Means", append=TRUE, showNA = FALSE)
