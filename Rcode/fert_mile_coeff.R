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
  select( Country, RegionCode, RegionName, SubRegionCode, SubRegionName, 
          coeff0, coeff1, coeff2, coeff3, coeff4,
          break1, break2, break3, nbreaks,
          YrRate45, YrRate35, YrRate30, YrRate25, YrRate20, YrRate15,
          startYear, endYear, maxSlope)
 
dfCBR <- dfCBR  %>% 
  mutate( slope1 = coeff1,
          slope2 = slope1 + coeff2,
          slope3 = slope2 + coeff3,
          slope4 = slope3 + coeff4)
          


dfCBR <- dfCBR  %>% filter( !is.null(YrRate35))  %>% 
  mutate( yr35cat = 25*trunc(YrRate35/25 ),
          yr35slope = case_when(
            YrRate35<break1  & slope1<0 ~ slope1,
            nbreaks>1 & YrRate35>=break1 & YrRate35<break2 & slope2<1 ~ slope2,
            nbreaks>2 & YrRate35>=break2 & YrRate35<break3 & slope3<2 ~ slope3,
            nbreaks==3 & YrRate35>=break3 &  slope4<3 ~ slope4       ),
          yr30cat = 25*trunc(YrRate30/25 ),
          yr30slope = case_when(
            YrRate30<break1  & slope1<0 ~ slope1,
            nbreaks>1 & YrRate30>=break1 & YrRate30<break2 & slope2<1 ~ slope2,
            nbreaks>2 & YrRate30>=break2 & YrRate30<break3 & slope3<2 ~ slope3,
            nbreaks==3 & YrRate30>=break3 &  slope4<3 ~ slope4       ),
          yr25cat = 25*trunc(YrRate25/25 ),
          yr25slope = case_when(
            YrRate25<break1  & slope1<0 ~ slope1,
            nbreaks>1 & YrRate25>=break1 & YrRate25<break2 & slope2<1 ~ slope2,
            nbreaks>2 & YrRate25>=break2 & YrRate25<break3 & slope3<2 ~ slope3,
            nbreaks==3 & YrRate25>=break3 &  slope4<3 ~ slope4       ),
          yr20cat = 25*trunc(YrRate20/25 ),
          yr20slope = case_when(
            YrRate20<break1  & slope1<0 ~ slope1,
            nbreaks>1 & YrRate20>=break1 & YrRate20<break2 & slope2<1 ~ slope2,
            nbreaks>2 & YrRate20>=break2 & YrRate20<break3 & slope3<2 ~ slope3,
            nbreaks==3 & YrRate20>=break3 &  slope4<3 ~ slope4  ),
          yr15cat = 25*trunc(YrRate15/25 ),
          yr15slope = case_when(
              YrRate15<break1  & slope1<0 ~ slope1,
              nbreaks>1 & YrRate15>=break1 & YrRate15<break2 & slope2<1 ~ slope2,
              nbreaks>2 & YrRate15>=break2 & YrRate15<break3 & slope3<2 ~ slope3,
              nbreaks==3 & YrRate15>=break3 &  slope4<3 ~ slope4       )       
          
          )

dft5 <- dfCBR %>%
    select(Country, yr35cat, YrRate35, yr35slope,
           yr30cat, YrRate30, yr30slope,
           yr25cat, YrRate25, yr25slope,
           yr20cat, YrRate20, yr20slope,
           yr15cat, YrRate15, yr15slope
    )

dft35 <-  dfCBR %>% filter(!is.null(yr35slope))  %>% 
  group_by(yr35cat ) %>% 
  summarize(milestone = 35,
            nrows = n(),
            meanSlope = mean(yr35slope, na.rm = TRUE), 
            Countries = paste(Country, collapse = ", "))  %>% 
    rename( yrCat = yr35cat)

dft30 <-  dfCBR %>% filter(!is.null(yr30slope))  %>% 
  group_by(yr30cat ) %>% 
  summarize(milestone = 30,
            nrows = n(),
            meanSlope = mean(yr30slope, na.rm = TRUE), 
            Countries = paste(Country, collapse = ", "))  %>% 
  rename( yrCat = yr30cat)

dft25 <-  dfCBR %>% filter(!is.null(yr25slope))  %>% 
  group_by(yr25cat ) %>% 
  summarize(milestone = 25,
            nrows = n(),
            meanSlope = mean(yr25slope, na.rm = TRUE), 
            Countries = paste(Country, collapse = ", "))  %>% 
  rename( yrCat = yr25cat)


dft20 <-  dfCBR %>% filter(!is.null(yr20slope))  %>% 
  group_by(yr20cat ) %>% 
  summarize(milestone = 20,
            nrows = n(),
            meanSlope = mean(yr20slope, na.rm = TRUE), 
            Countries = paste(Country, collapse = ", "))  %>% 
  rename( yrCat = yr20cat)


dft15 <-  dfCBR %>% filter(!is.null(yr15slope))  %>% 
  group_by(yr15cat ) %>% 
  summarize(milestone = 15,
            nrows = n(),
            meanSlope = mean(yr15slope, na.rm = TRUE), 
            Countries = paste(Country, collapse = ", "))  %>% 
  rename( yrCat = yr15cat)

dfMileSlope <- rbind(dft35, dft30)
dfMileSlope <- rbind(dfMileSlope, dft25)
dfMileSlope <- rbind(dfMileSlope, dft20)
dfMileSlope <- rbind(dfMileSlope, dft15)

write.xlsx(dfMileSlope, file="Output/CBR/MileSlope.xlsx", sheetName="slopes", append=TRUE, showNA = FALSE)
