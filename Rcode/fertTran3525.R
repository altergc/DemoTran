####### 

library(dplyr)
library(tibble)
library("xlsx")
library(ggplot2)

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

######  This program displays stats from 'dfOut' by subregion #############
load("Data/dfOut_3.rdata")

####### create dataframes for CBR and CDR  ################
####### rename variables with prefixes for rate ###########
dfCBR <- dfOut %>% filter(demoVar=="CBR" & !is.null(YrRate35) & !is.null(YrRate25))   %>%
  rename_with(~ paste0("CBR_",. ), -c(Country, RegionCode, RegionName, SubRegionCode, SubRegionName, 
                                      maxRNI, maxRNIyear, minRNI, minRNIyear,RNIgt30, RNI20, RNI10, RNI00, RNIlt0, startYear, endYear) )

dfCBR <- dfCBR %>% mutate( ferTran = case_when(
  CBR_YrRate25<1950 ~ "Early"  ,
  CBR_YrRate25>=1950 & CBR_YrRate25<2000 ~ "Mid" ,
  CBR_YrRate25>2000 ~ "Late"
      ),
  years3525 = CBR_YrRate25 - CBR_YrRate35,
  dateCBR35 = 25*trunc(CBR_YrRate35/25)
  )

dft2 <- dfCBR %>% group_by(dateCBR35 ) %>% 
  summarise( nrows = n(),
             meanYrs3525  = mean(years3525, na.rm = TRUE) )

dft3 <- dfCBR %>% 
  select( c(dateCBR35, Country  )) %>% 
  group_by( dateCBR35 ) %>%
  summarise(Countries = paste(Country, collapse = ", ")) %>%
  as.data.frame()

########## group by dateCBR35  == year CBR reached 35 ##########
dfMeans <- merge(dft2, dft3, by="dateCBR35", all.x=TRUE, all.y=TRUE)

#sheetNm = paste(runNm, "CountryNames", sep="", collapse="")

###################################################################
########## group by "SubRegionName", "dateCBR35"

dft2 <- dfCBR %>% group_by(SubRegionName, dateCBR35  ) %>% 
  filter( !is.na(years3525) )%>% 
  summarise( nrows = n(),
             meanYrs3525  = mean(years3525, na.rm = TRUE),
             sdYrs3525  = sd(years3525, na.rm = TRUE))

dft3 <- dfCBR %>% 
  select( c(SubRegionName, dateCBR35, Country  )) %>% 
  group_by( SubRegionName, dateCBR35 ) %>%
  summarise(Countries = paste(Country, collapse = ", ")) %>%
  as.data.frame()

dfMeans <- merge(dft2, dft3, by=c("SubRegionName", "dateCBR35"  ), all.x=TRUE, all.y=TRUE)


##########################################
groupNum <- seq(1:length(unique(dfCBR$RegionName)))
names(groupNum) <- sort(unique(dfCBR$RegionName))

ggplot(dfCBR, 
        aes(y=years3525, x=dateCBR35, group=RegionName, color=RegionName, shape=RegionName) ) +
         geom_point(size=1.5, stroke=1.5) +
         scale_shape_manual( values=groupNum)

