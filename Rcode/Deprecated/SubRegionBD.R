#######  This program creates summary measures by sub-region
####### 

library(dplyr)
library(tibble)
library("xlsx")

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

######  This program displays stats from 'dfOut' by region #############
load("Data/dfOut_2.rdata")


####### create dataframes for CBR and CDR  ################
####### rename variables with prefixes for rate ###########
dfCBR <- dfOut %>% filter(demoVar=="CBR" & !is.na(YrRate35)   & !is.na(dfOut$YrRate25)  & !is.na(dfOut$YrRate15) )   %>%
  rename_with(~ paste0("CBR_",. ), -c(Country, RegionCode, RegionName, SubRegionCode, SubRegionName, 
                      maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear) )

dfCDR <- dfOut %>% filter(demoVar=="CDR")   %>%
  select( -c(RegionCode, RegionName, SubRegionCode, SubRegionName , 
          maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear))   %>%
         rename_with(~ paste0("CDR_",. ), -c(Country) )

####### Merge dataframes by rate into a single dataframe #######
dfBD <- merge(dfCBR, dfCDR, by="Country")

write.xlsx(dfBD, file="Output/CountryStats.xlsx", sheetName="CBR35-15")
###########################################################
options(digits = 2)
##### Means by SubRegion ##################################
subRegionMean <- subRegionMean[0,]

subRegionMean <- dfBD %>% group_by(SubRegionName ) %>% 
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
write.xlsx(subRegionMean, file="Output/subRegionMean.xlsx", sheetName="means_CBR35-15")

# ########################################################
# ########### Output countries in a sub-region  #############
# 
# subSelect <- "Southern Cone"
# subRegionCountries <- dfBD[ dfBD$SubRegionName %in% subSelect, ] %>% 
#   arrange(RegionName, SubRegionName , Country) %>%
#   select( c(RegionName, SubRegionName , Country, 
#             CDR_YrRate35, CDR_YrRate25, CDR_YrRate15, 
#             CBR_YrRate35, CBR_YrRate25, CBR_YrRate15 ))  
# 
# setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")
# ##write.xlsx(subRegionCountries, file="Output/subRegionMean.xlsx", sheetName=SubRegionName)
# ########################################################
# 
# 
# cat("\n\n================Group means ================\n")
# print(arrange(subRegionMean[,c(1:4, 13, 14)], subRegionMean$meanMaxRNI ), n=nrow(subRegionMean))
# 
# print(arrange(subRegionMean[,c(1,5:7, 12)], subRegionMean$meanMaxRNI ), n=nrow(subRegionMean))
# print(arrange(subRegionMean[,c(1,8:11, 13)], subRegionMean$meanMaxRNI ), n=nrow(subRegionMean))
##################################################################

# ######### results for a region ##########
# options(digits = 2)
# tdf <- dfBD %>% subset( ( (dfBD$SubRegionName=="Eastern Asia")  ) )  %>% tibble()
# 
# ##### print column names and numbers ##########
# ##print(as.data.frame(colnames(tdf)))
# 
# 
# print(tdf[, c(1, 89:90, 87:88 )])            ### start and end
# print(tdf[, c(1,   92:94)])            ### milestones
# print(tdf[, c(1,   41:43)])            ### milestones
# 
# print(tdf[, c(1,64:68 )])  ### coefficients
# print(tdf[, c(1,  14:17)])  ### coefficients
# 
# print(tdf[, c(1,  56, 96)])  ### max slopes