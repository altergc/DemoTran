#####  This program creates a dataframe dfBD with one row per country
#####  and summary measures of CDR and CBR

library(dplyr)
library(tibble)
library("xlsx")
####### create dataframes for CBR and CDR  ################
####### rename variables with prefixes for rate ###########
dfCBR <- dfOut %>% filter(demoVar=="CBR")   %>%
  rename_with(~ paste0("CBR_",. ), -c(Country, RegionCode, RegionName, SubRegionCode, SubRegionName, 
                      maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear) )

dfCDR <- dfOut %>% filter(demoVar=="CDR")   %>%
     select( -c(RegionCode, RegionName, SubRegionCode, SubRegionName , 
          maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear))   %>%
         rename_with(~ paste0("CDR_",. ), -c(Country) )

####### Merge dataframes by rate into a single dataframe #######
dfBD <- merge(dfCBR, dfCDR, by="Country")

###########################################################
options(digits = 2)
##### Means by SubRegion ##################################
#regionMean <- regionMean[0,]

regionMean <- dfBD %>% group_by(RegionName ) %>% 
  subset(CBR_startCBRmav>25) %>% 
  summarise( 
            nrows = n(),
            meanMaxRNI = mean(maxRNI, na.rm = TRUE),
            meanMinRNI = mean(minRNI, na.rm = TRUE),
            meanCDR_YrRate35=mean(CDR_YrRate35, na.rm = TRUE), 
            meanCDR_YrRate25=mean(CDR_YrRate25, na.rm = TRUE), 
            meanCDR_YrRate15=mean(CDR_YrRate15, na.rm = TRUE), 
            meanCBR_YrRate35=mean(CBR_YrRate35, na.rm = TRUE), 
            meanCBR_YrRate25=mean(CBR_YrRate25, na.rm = TRUE), 
            meanCBR_YrRate15=mean(CBR_YrRate15, na.rm = TRUE), 
            meanCDR_maxSlope=mean(CDR_maxSlope, na.rm = TRUE), 
            meanCBR_maxSlope=mean(CBR_maxSlope, na.rm = TRUE), 
            DtoBlag35 = mean( (CBR_YrRate35 - CDR_YrRate35), na.rm = TRUE),
            DtoBlag25 = mean( (CBR_YrRate25 - CDR_YrRate25), na.rm = TRUE),
            
            n_CDR_YrRate35=sum(!is.na(CDR_YrRate35)),
            n_CDR_YrRate25=sum(!is.na(CDR_YrRate25)),
            n_CDR_YrRate15=sum(!is.na(CDR_YrRate15)),
            n_CBR_YrRate35=sum(!is.na(CBR_YrRate35)),
            n_CBR_YrRate25=sum(!is.na(CBR_YrRate25)),
            n_CBR_YrRate15=sum(!is.na(CBR_YrRate15)),
            n_CDR_maxSlope=sum(!is.na(CDR_maxSlope)),
            n_CBR_maxSlope=sum(!is.na(CBR_maxSlope)),
            n_DtoBlag35 = sum(!is.na(CDR_YrRate35) & !is.na(CBR_YrRate35)),
            n_DtoBlag25 = sum(!is.na(CDR_YrRate25) & !is.na(CBR_YrRate25))
                      )



setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")
write.xlsx(regionMean, file="Output/regionMean.xlsx")


cat("\n\n================Group means ================\n")
print(arrange(regionMean[,c(1:4, 13, 14)], regionMean$meanMaxRNI ), n=nrow(regionMean))

print(arrange(regionMean[,c(1,5:7, 12)], regionMean$meanMaxRNI ), n=nrow(regionMean))
print(arrange(regionMean[,c(1,8:11, 13)], regionMean$meanMaxRNI ), n=nrow(regionMean))
##################################################################

######### results for a region ##########
options(digits = 2)
tdf <- dfBD %>% subset( ( (dfBD$RegionName=="Africa")  ) )  %>% tibble()

##### print column names and numbers ##########
##print(as.data.frame(colnames(tdf)))


print(tdf[, c(1, 89:90, 87:88 )])            ### start and end
print(tdf[, c(1,   92:94)])            ### milestones
print(tdf[, c(1,   41:43)])            ### milestones

print(tdf[, c(1,64:68 )])  ### coefficients
print(tdf[, c(1,  14:17)])  ### coefficients

print(tdf[, c(1,  56, 96)])  ### max slopes