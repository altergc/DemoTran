library("readxl")
library("tidyr")
library("xlsx")

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

###### call runSegs.R to set up function that computes segments and stats #########
source("Rcode/runSegs.R")


#######  read country-region list here ####################################
####### 'df1' is the master data file ######################
dfCountry <- read_excel("Data/00C2_country_region.xlsx",
                  sheet="_00C2_country_region")


#######  read data here ####################################
####### 'df1' is the master data file ######################
df1 <- read_excel("Data/00C1_Export_all_sorted.xlsx",
                  sheet="_00C1_Export_all_sorted")

###### keep rows with both CBR and CDR values ###############
df1 <- subset(df1, (!is.na(df1$CBR) & !is.na(df1$CDR)) )


df1 <- df1 %>% rename( year = Year)


##########----------------------------------------#############
##########   loop on countries                    #############
for (selCountry in c("Sweden","Taiwan") ) {
  
  ######### output location #################
#  fileNm <- "SwedenPlots3"
  pngFile <- paste("Output/", selCountry, "Plots.png", sep="")
  png(pngFile, width = 600, height = 600, units = "px")
  
  

#####for (selCountry in unique(dfCountry$Country) ) {

selRegion <-  dfCountry[dfCountry$Country==selCountry, 4 ] 
selSubRegion <-  dfCountry[dfCountry$Country==selCountry, 6 ] 

selTitle <- paste(selRegion, selSubRegion, selCountry, sep=" - ")
cat(selTitle, "%%%%%%%%%%")
    
  ########## 'df2' holds one country at a time ##########
  
  ########## RNI and moving averages added  ###################
  df2 <- df1[df1$Country==selCountry ,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ) ] %>%  
    mutate(RNI = CBR - CDR,
           RNI_11 = rollmean(RNI, k=11, fill=NA, align='center'),
           CBR_MAV = rollmean(CBR, k=11, fill=NA, align='center'),
           CDR_MAV = rollmean(CDR, k=11, fill=NA, align='center')
    ) %>%  arrange(year)
  

  ########  call runSegs function ##############################
  runSegs(1725, 2025)   
  
   }

####### end selCountry loop ################

####### close PDF file
dev.off()

#####  save dfOut dataframe of statistics to disk  ##########
save(dfOut, file='Data/dfOut_3.rdata')


#####  save df3 dataframe of observed and predicted to disk  ##########
save(df3, file='Data/dfPred3.rdata')

graphics.off()
