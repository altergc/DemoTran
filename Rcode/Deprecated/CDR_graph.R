##### Combined CDR graphs ############

library("readxl")
library("tidyr")
library("xlsx")
library(tidyverse)


setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

######  Load stats from 'dfOut'  #############
load("Data/dfOut_2.rdata")

######  Load predicted segments from 'dfPred' #############
load("Data/dfPred.rdata")

# ##### make list of non-European countries with data <1940  #########
# dfC4 <- dfOut %>%
#   subset(  (startYear<1940) | (Country=="India" | Country=="China") ) %>%
#   subset((RegionName=="Asia") & (Country !="Cyprus") ) %>%
#   select(Country)  %>%
#   unique()

####### create lists of countries #########
cl1 = c("China", "India", "Japan", "Sri Lanka", "Taiwan")
cl2 = c("Colombia", "Ecuador", "Venezuela", "Argentina", "Chile", "Uruguay")
cl3 <- c("Barbados", "Jamaica", "Trinidad and Tobago", "Panama")
cl4 <- c("Bulgaria", "Czechoslovakia", "Hungary", "Poland", "Romania", "Russia") 
cl5 <- c( "Denmark", "England and Wales", "Finland", "Ireland", "Norway", "Scotland", "Sweden") 
cl6 <- c( "Greece", "Italy", "Portugal", "Serbia", "Spain", "Yugoslavia")
cl7 <- c( "Austria", "Belgium", "France", "Germany", "Netherlands", "Switzerland")
cl8 <- c( "Canada", "United States", "Australia", "New Zealand")
clist <- list(cl1, cl2, cl3, cl4, cl5, cl6,  cl7, cl8 )

cnames <- c("Asia", "South_America", "Caribbean", "Eastern_Europe", 
            "Northern_Europe", "Southern_Europe", "Western_Europe", "Europe_offshoots")

for( cc in 1:8) {

      ###### select countries from predicted ##########
      dfP4 <- df3 %>%
            subset(  Country %in%  clist[[cc]]) 
      
      #### make list of countries for assigning symbols #########
      groupNum <- seq(1:length(unique(dfP4$Country)))
      names(groupNum) <- unique(dfP4$Country)
      
      pngFile <- paste("Output/CDRpng/", cnames[cc], "_CDR.png", sep="")
      png(pngFile)
      
      print (
      ggplot(dfP4, aes(x=year, y=CDRpred, color=Country, group=Country, 
                       linetype=Country )) +
        geom_line(linewidth=1.5) + 
        scale_x_continuous(name="Year",  breaks = c(seq(1750, 2025, 25)),
                           limits=(c(1750,2025)))+
        scale_y_continuous(name="Deaths per thousand population",  
                  limit=c( 5, 40), breaks = c( 5,  15 , 25,   35,   40 ) ) +
        theme(legend.position="bottom", legend.title=element_blank())
        )
      dev.off()  
      
  }    
dev.off()

#####################################
######## Maximum slope by subregion #

clall = c("China", "India", "Japan", "Sri Lanka", "Taiwan",
          "Colombia", "Ecuador", "Venezuela", "Argentina", "Chile", "Uruguay",
          "Barbados", "Jamaica", "Trinidad and Tobago", "Panama",
          "Bulgaria", "Czechoslovakia", "Hungary", "Poland", "Romania", "Russia",
          "Denmark", "England and Wales", "Finland", "Ireland", "Norway", "Scotland", "Sweden",
          "Greece", "Italy", "Portugal", "Serbia", "Spain", "Yugoslavia",
          "Austria", "Belgium", "France", "Germany", "Netherlands", "Switzerland",
          "Canada", "United States", "Australia", "New Zealand")

mxs1 <- dfOut %>% subset(demoVar=="CDR") %>% 
  subset(Country %in% clall) %>% 
  group_by(RegionName, SubRegionName ) %>% 
  summarise(mxSlope = mean( maxSlope) ) %>%
  as.data.frame()

mxs2 <- dfOut %>% subset(demoVar=="CDR") %>% 
  subset(Country %in% clall) %>% 
  select(RegionName, SubRegionName, Country) %>%
  as.data.frame()

write.xlsx(mxs1, file="Output/CDRpng/CDRmaxSlope.xlsx", sheetName="mxSlope")

write.xlsx(mxs2, file="Output/CDRpng/CDRmaxSlope.xlsx", sheetName="coNames", append=TRUE)

########### Asian countries details ##########
mxs3 <- dfOut %>% 
  subset(Country %in% c("China", "India", "Japan", "Sri Lanka", "Taiwan") ) %>% 
  subset(demoVar == "CDR" ) %>%
  select(Country, maxSlope, break1, break2, break3, break4, coeff1, 
         coeff2, coeff3, coeff4)
write.xlsx(mxs3, file="Output/CDRpng/CDRmaxSlope.xlsx", sheetName="Asia", append=TRUE)
