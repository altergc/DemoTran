#######  This program creates summary measures by sub-region
####### 

library(dplyr)
library(tibble)
library("xlsx")

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

######  This program displays stats from 'dfOut' by subregion #############
load("Data/dfOut_2.rdata")


####### create dataframes for CBR and CDR  ################

###### Select countries with milestones CBR= 35,25, 15 ############

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

write.xlsx(dfBD, file="Output/CBRstats.xlsx", sheetName="CBR35-15")

####### Print country names by subregion ##########

dft3 <- dfBD %>% 
  select( c(RegionCode, SubRegionCode, 
            RegionName, SubRegionName, Country  )) %>% 
      group_by( RegionName, SubRegionName ) %>%
      summarise(Countries = paste(Country, collapse = ", ")) %>%
      as.data.frame()

write.xlsx(dft3, file="Output/CBRstats.xlsx", sheetName="CountryNames", append=TRUE)

###########################################################
options(digits = 2)
##### Means by SubRegion ##################################
#subRegionMean <- subRegionMean[0,]

subRegionMean <- dfBD %>% group_by(SubRegionName ) %>% 
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



write.xlsx(subRegionMean, file="Output/CBRstats.xlsx", sheetName="means_CBR35-15", append=TRUE)

#########  graph CBR subregions ##############

library(ggplot2)

########### requires df subRegionMean

dfMiles <- data.frame(GroupName = character(),
                      year =double(),
                      cRate=double() )
dfMt <- data.frame(GroupName = character(),
                   year =double(),
                   cRate=double() )

for(vv in c("CBR"))   {
  if ( nrow(dfMiles)>0) {dfMiles <- dfMiles[0,]}
  
  for( rr in c(15, 25, 35) )   {
    vname <- paste("mean",vv, "_YrRate", rr, sep="")
    dfMt <- subRegionMean %>% subset( !is.na(vname) ) %>%
      select( c(SubRegionName, vname)) %>% 
      rename(year = vname, GroupName = SubRegionName) %>%
      subset( !is.na(year) )
    
    cat(vv, rr, "rows" , nrow(dfMt), "\n")    
    if (nrow(dfMt)>0)  {
      dfMt$cRate <-rr
      dfMiles <- rbind(dfMiles , dfMt)}
  }
  
  
  selTitle <- vv
  
  subSelect <- list(   "Northern Europe",  "US & Canada", "Western Europe", 
                       "Southern Cone", "Eastern Asia", "Andean & Amazonian" , 
                       "Eastern Europe", "Northern Africa", 
                       "Southern Africa", "Southern Asia"  )
  
  dfMiles <- dfMiles[ dfMiles$GroupName %in% subSelect, ]
  
  
  ##### save plot to png  ############
  pngFile <- paste("Output/CBRpng/milestonesSub", vv, ".png", sep="")
  png(pngFile)
  
  #### make list of groups for assigning symbols #########
  groupNum <- seq(1:length(unique(dfMiles$GroupName)))
  names(groupNum) <- unique(dfMiles$GroupName)
  
  xmin <- 1850
  xmax <- 2025
  xbrks <- 25
  
  print(
    ggplot(dfMiles, aes(x=year, y=cRate, color=GroupName, group=GroupName, shape=GroupName, linetype = GroupName )) +
      geom_line(size=1.5) + 
      geom_point(size=2.5, stroke=1.5) +
      scale_shape_manual( values=groupNum) +
      scale_x_continuous(name="Year",  breaks = c(seq(xmin, xmax, xbrks)),
                         limits=(c(xmin, xmax)))+
      scale_y_continuous(name="Events per thousand population",  limit=c( 5, 40), breaks = c( 5,  15 , 25,   35,   40 ) ) +
      ggtitle(selTitle) + 
      theme(legend.position="bottom", legend.title=element_blank())
  )
}

dev.off()
dev.off()
