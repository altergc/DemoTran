library("dplyr")
library("tidyr")


setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")


######  This program displays stats from 'dfOut' by region #############
load("Data/dfOut_2.rdata")

#######  time to transitions between milestones #############
dfOut$tran50to40 <- dfOut$YrRate40-dfOut$YrRate50
dfOut$tran40to30 <- dfOut$YrRate30-dfOut$YrRate40
dfOut$tran30to20 <- dfOut$YrRate20-dfOut$YrRate30
dfOut$tran20to10 <- dfOut$YrRate10-dfOut$YrRate20

#### print column numbers and names 
##print(as.data.frame(colnames(dfOut)))

###### 'rVar' is rate to display -- CBR or CDR ###########
rVAR = "CBR"

#####  Means by UN subregions ######################
group_mean <- dfOut %>% group_by(SubRegionName, SubRegionCode) %>% 
  filter(demoVar==rVAR)   %>% 
  summarise_at(vars("startCBRmav", "endCBRmav", "tran50to40", "tran40to30", "tran30to20", "tran20to10"),
               mean, na.rm = TRUE) 

group_mean$t40_20 <- (group_mean$tran40to30 + group_mean$tran30to20)

format(group_mean,2)

print(arrange(group_mean, group_mean$t40_20))


#### select subregion by code numbers ############
subCode <- 419

######### results for a region ##########
tdf <- dfOut %>% subset( ( (dfOut$SubRegionCode==subCode) & (dfOut$demoVar=="CBR") ) ) 

##### print column names and numbers ##########
##print(as.data.frame(colnames(tdf)))

for (ii in c(seq(8,12), seq(14,49)) ) {
  tdf[ii] <- format(round(tdf[[ii]],2),nsmall=2)
}
print(tdf[, c(1, 45:48)])
print(tdf[, c(1, 6, 7, 13, 14:17)])
print(tdf[, c(1, 14:17 , 24:26)])
print(tdf[, c(1, 40:44)])
print(tdf[, c(1, 36:39)])

######### results by slopes of line segments (coefficients)  ##########
tdf <- dfOut %>% subset( ( (dfOut$coeff1>-.1) & (dfOut$coeff2<0) )  & 
                           (dfOut$coeff3>0)& (dfOut$demoVar=="CBR") &
                           (dfOut$startCBRmav>35 ) & (dfOut$endCBRmav<20)) 
print(as.data.frame(colnames(tdf)))

for (ii in c(seq(8,12), seq(14,49)) ) {
  tdf[ii] <- format(round(tdf[[ii]],2),nsmall=2)
}
print(tdf[, c(1, 45:48)])
print(tdf[, c(1, 6, 7, 13, 14:17)])
print(tdf[, c(1, 14:17 , 24:26)])
print(tdf[, c(1, 40:44)])
print(tdf[, c(1, 36:39)])

