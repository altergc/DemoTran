#######  This program creates summary measures by sub-region
####### 

library(dplyr)
library(tibble)
library("xlsx")
library(ggplot2)
library(RColorBrewer, lib.loc = "C:/Program Files/R/R-4.4.1/library")
library(ggrepel)

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

######  This program displays stats from 'dfOut' by subregion #############
load("Data/dfOut_3.rdata")

##### Initialize spreadsheet ###############
write.xlsx(Sys.time(), file="Output/SelCountries/SelectStats.xlsx", sheetName="RunTime")

for (runNm in c("All")) {

  ####### create dataframes for CBR and CDR  ################
  ####### rename variables with prefixes for rate ###########
  dfCBR <- dfOut %>% filter(demoVar=="CBR" )   %>%
    rename_with(~ paste0("CBR_",. ), -c(Country, RegionCode, RegionName, SubRegionCode, SubRegionName, 
                        maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear) )
  
  dfCDR <- dfOut %>% filter(demoVar=="CDR")   %>%
    select( -c(RegionCode, RegionName, SubRegionCode, SubRegionName , 
            maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear))   %>%
           rename_with(~ paste0("CDR_",. ), -c(Country) )
  
 
  
  ####### Merge dataframes by rate into a single dataframe #######
  dfBD <- merge(dfCBR, dfCDR, by="Country")
  
  ######## Select countries ##########################
  countrySelect <- c("Taiwan", "Jamaica",  "Sweden", "France", 
                     "Argentina", "Venezuela", "Italy", 
                     "Germany")
  
  ##### "United States","England and Wales" , "Russia", "Mauritius", 
  
  dfBD <- dfBD %>% filter( Country %in% countrySelect )
  
  sheetNm = paste(runNm, "byCountry", sep="", collapse="")
  write.xlsx(dfBD, file="Output/SelCountries/SelectStats.xlsx", sheetName=sheetNm, append=TRUE, showNA = FALSE)
  
  ####### Print country names by subregion ##########
  
  dft3 <- dfBD %>% 
    select( c(RegionCode, SubRegionCode, 
              RegionName, SubRegionName, Country  )) %>% 
        group_by( RegionName, SubRegionName ) %>%
        summarise(Countries = paste(Country, collapse = ", ")) %>%
        as.data.frame()
  
  sheetNm = paste(runNm, "CountryNames", sep="", collapse="")
  write.xlsx(dft3, file="Output/SelCountries/SelectStats.xlsx", sheetName=sheetNm, append=TRUE, showNA = FALSE)
  
  ###########################################################
  options(digits = 2)
  
  #########  graph CDR subregions ##############
  

  
  ########### dfMiles is df of milestones ################
  
  dfMiles <- data.frame(GroupName = character(),
                        year =double(),
                        cRate=double() )
  dfMt <- data.frame(GroupName = character(),
                     year =double(),
                     cRate=double() )
  
  for(vv in c("CDR", "CBR"))   {
    if ( nrow(dfMiles)>0) {dfMiles <- dfMiles[0,]}
    
    for( rr in c("05", "10", "15", "20", "25", "30", "35") )   {
      vname <- paste( vv, "_YrRate", rr, sep="")
      dfMt <- dfBD %>% subset( !is.na(vname) ) %>%
        select( c(Country, all_of(vname) ) )  %>% 
        rename(year = vname, GroupName = Country) %>%
        subset( !is.na(year) )
      
      cat(runNm, vv, rr, "rows" , nrow(dfMt), "\n")    
      if (nrow(dfMt)>0)  {
        dfMt$cRate <-  as.numeric(rr)
        dfMiles <- rbind(dfMiles , dfMt)}
    }
    
    
    ######### select subregions to display in graph #######################
    subSelect <- unique(dfMiles$GroupName)
 
    dfMiles <- dfMiles[ dfMiles$GroupName %in% subSelect, ]
    
    
    ##### save plot to png  ############
    fileNm = paste(vv, runNm, sep="", collapse="")
    pngFile <- paste("Output/SelCountries/milestonesSub", fileNm, ".png", sep="")
    png(pngFile, width = 600, height = 600, units = "px")
    
    #### make list of groups for assigning symbols #########
    groupNum <- seq(1:length(unique(dfMiles$GroupName)))
    names(groupNum) <- sort(unique(dfMiles$GroupName))
    
    xmin <- 1750
    ###trunc((min(dfMiles$year)/25))*25
    xmax <- 2025
    xbrks <- 25
    
#    if (vv=="CBR") xmin <- 1900

    # if (vv=="CDR") xmin <- 1825
    # if (vv=="CDR") xmax <-1975

    ###### label  first and last values #############
    dfMiles$yrLab <- NA
    for (cc in unique(dfMiles$GroupName)) {
      d5 <- dfMiles %>% filter(GroupName==cc & year>xmin )
      tmin <- min(d5$year)
      tmax <- max(d5$year)
      dfMiles <- dfMiles %>% mutate(yrLab = ifelse(GroupName==cc & year==tmin, cc, yrLab) )
      dfMiles <- dfMiles %>% mutate(yrLab = ifelse(GroupName==cc & year==tmax, cc, yrLab) )
    }
    
        
    ####### colors ################
    mypalette <- brewer.pal(12,"Set3")
    
    
    selTitle <- vv
    
    if (vv=="CDR") {yTitle <- "Deaths per thousand population"}
          else {yTitle <- "Births per thousand population"}
      
    
    
    print(
      ggplot(dfMiles, aes(x=year, y=cRate, color=GroupName, group=GroupName, shape=GroupName, linetype = GroupName )) +
        geom_line(linewidth=1.0) + 
        geom_point(size=1.5, stroke=1.5) +
        scale_shape_manual( values=groupNum) +
        scale_x_continuous(name="Year",  breaks = c(seq(xmin, xmax, xbrks)),
                           limits=(c(xmin, xmax)))+
        scale_y_continuous(name=yTitle,  limit=c( 5, 40), breaks = c( 5,  10 , 20,   30,   40 ) ) +
        theme(legend.position="bottom", legend.title=element_blank()) +
        geom_label_repel(aes(label = yrLab), show.legend = FALSE , force=9) 
    )
  }
  dev.off()

}
graphics.off()
