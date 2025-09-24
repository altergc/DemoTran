library("dplyr")
library("tidyr")
library(ggplot2, lib.loc = "C:/Program Files/R/R-4.4.1/library")

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")


######  This program displays stats from 'dfOut' by region #############
load("Data/dfOut_2.rdata")


#######  time to transitions between milestones #############
dfOut$tran45to35 <- ifelse(dfOut$YrRate45>dfOut$YrRate35, (dfOut$YrRate35-dfOut$YrRate45), NA)
dfOut$tran35to25 <- ifelse(dfOut$YrRate25>dfOut$YrRate35, (dfOut$YrRate25-dfOut$YrRate35), NA)
dfOut$tran25to15 <- ifelse(dfOut$YrRate15>dfOut$YrRate25, (dfOut$YrRate15-dfOut$YrRate25), NA)
dfOut$tran15to05 <- ifelse(dfOut$YrRate05>dfOut$YrRate15, (dfOut$YrRate05-dfOut$YrRate15), NA)

######## compute slopes of segments ########################
dfOut$slope1 <- dfOut$coeff1
dfOut$slope2 <- dfOut$coeff1 + dfOut$coeff2
dfOut$slope3 <- dfOut$slope2 + dfOut$coeff3
dfOut$slope4 <- dfOut$slope3 + dfOut$coeff4


#### print column numbers and names 
##print(as.data.frame(colnames(dfOut)))

###########################################################
#####  Means by UN subregions ######################

###### 'rVar' is rate to display -- CBR or CDR ###########
rVAR <- "CBR"
group_mean <- dfOut %>% group_by(SubRegionName, SubRegionCode) %>% 
  filter(demoVar==rVAR)   %>% 
  summarise_at(vars("startCBRmav", "endCBRmav", "tran35to25", "tran25to15" ),
               mean, na.rm = TRUE) 

group_mean$t35_15 <- (group_mean$tran35to25 + group_mean$tran25to15)

format(group_mean,2)

cat("\n\nGroup means for CBR ================\n")
print(arrange(group_mean, group_mean$t35_15))

############################################################
####### Graph time between milestones by Region and SubRegion
dfOut %>% subset( (dfOut$RegionName=="Americas" &  dfOut$demoVar=="CBR" & !is.na(dfOut$tran35to25) ) )  %>% 
  ggplot() +
  aes(x = YrRate35, y=tran35to25, color=SubRegionName  ) +
  scale_y_continuous(limits=c(0,50)) + 
  scale_x_continuous(limits=c(1900,2000)) + 
  geom_point() +
  geom_text(aes(label=Country), size=3)
  

############################################################
#### select subregion by code numbers ############
subCode <- 419

######### results for a region ##########
tdf <- dfOut %>% subset( ( (dfOut$SubRegionCode==subCode) & (dfOut$demoVar=="CBR") ) ) 

##### print column names and numbers ##########
##print(as.data.frame(colnames(tdf)))

for (ii in c(seq(8,12), seq(14,53)) ) {
  tdf[ii] <- format(round(tdf[[ii]],2),nsmall=2)
}
print(tdf[, c(1, 45:48)])            ### transition durations
print(tdf[, c(1, 40:44)])            ### years for milestones

print(tdf[, c(1, 50:53)])            ### slopes
print(tdf[, c(1, 6, 7, 13, 14:17)])  ### coefficients
print(tdf[, c(1, 14:17 , 24:26)])    ### coefficients and breaks
print(tdf[, c(1, 36:39)])            ### start/end CBR and CDR

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

#########################################
####### pattern of decrease by segment ##
dfOut %>% subset( (dfOut$demoVar=="CBR") )  %>% 
    ggplot( aes(x= coeff1 ) ) +
         geom_histogram(  ) +
        xlim(-2,2)
       
dfOut %>% subset( (dfOut$demoVar=="CBR") )  %>% 
  ggplot( aes(x= coeff1+coeff2 ) ) +
  geom_histogram(  ) +
  xlim(-2,2)

dfOut %>% subset( (dfOut$demoVar=="CBR") )  %>% 
  ggplot( aes(x= coeff1+coeff2 + coeff3 ) ) +
  geom_histogram(  ) +
  xlim(-2,2)

dfOut %>% subset( (dfOut$demoVar=="CBR") )  %>% 
  ggplot( aes(x= coeff1+coeff2 + coeff3+coeff4 ) ) +
  geom_histogram(  ) +
  xlim(-2,2)



        