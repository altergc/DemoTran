library("readxl")
library("tidyr")
library("xlsx")
library(dplyr)
library(zoo)

library(ggplot2)
library(RColorBrewer, lib.loc = "C:/Program Files/R/R-4.4.1/library")
library(ggrepel)

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

df3 <- data.frame(
  Country=character(),
  year =integer(),
  RegionCode=integer(),
  RegionName=character(),
  SubRegionCode=integer(),
  SubRegionName=character(),
  CBR= double(),
  CDR= double(),
  RNI= double(),
  RNI_11= double(),
  CBR_MAV= double(),
  CDR_MAV= double(),
  CBRpred= double(),
  CDRpred= double()
  )

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


#######  open pdf file for graphs ###################### 
pdf("Output/SegPlots3.pdf")


##########----------------------------------------#############
##########   loop on countries                    #############
### test loop ####for (selCountry in c("Czechoslovakia") ) {

for (selCountry in unique(dfCountry$Country) ) {
  
  selRegion <-  dfCountry[dfCountry$Country==selCountry, 4 ] 
  selSubRegion <-  dfCountry[dfCountry$Country==selCountry, 6 ] 
  
  selTitle <- paste(selCountry, selRegion, selSubRegion, sep=" - ")
#  cat(selTitle, "%%%%%%%%%%")
  
  ########## 'df2' holds one country at a time ##########
  
  ########## RNI and moving averages added  ###################
  df2 <- df1[df1$Country==selCountry ,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ) ] %>%  
    mutate(RNI = CBR - CDR,
           RNI_11 = rollmean(RNI, k=11, fill=NA, align='center'),
           CBR_MAV = rollmean(CBR, k=11, fill=NA, align='center'),
           CDR_MAV = rollmean(CDR, k=11, fill=NA, align='center')
    ) %>%  arrange(year)
  
  df3 <- rbind(df3, df2)

}

####### end selCountry loop ################

####################################
######## Select countries ##########################
countrySelect <- c("Taiwan",  "Argentina", "Venezuela", "France", "Germany", "Sweden")

#countrySelect <- c("Jamaica", "France", "Germany")

#, "Sweden", "Italy"

df4 <- df3 %>% filter( Country %in% countrySelect )

##### save plot to png  ############
fileNm = "RNI_graph"
pngFile <- paste("Output/SelCountries/", fileNm, ".png", sep="")
png(pngFile, width = 480, height = 480, units = "px")

#### make list of groups for assigning symbols #########
groupNum <- seq(1:length(unique(df4$Country)))
names(groupNum) <- sort(unique(df4$Country))

########  



xmin <- 1750
#    if (vv=="CBR") xmin <- 1900
###trunc((min(df4$year)/25))*25
xmax <- 2025
#    if (vv=="CDR") xmax <-1975
xbrks <- 25

########  fileter by xmin -- required for labelling  #########
df4 <- df4 %>% filter(year>= xmin)

###### label  first and last values #############
df4$yrLab <- NA
for (cc in unique(df4$Country)) {
  d5 <- df4 %>% filter(Country==cc & !is.na(RNI_11)  )
  tmin <-  min(d5$year)
  tmax <- max(d5$year)-5
  df4 <- df4 %>% mutate(yrLab = ifelse(Country==cc & year==tmin, cc, yrLab) )
  df4 <- df4 %>% mutate(yrLab = ifelse(Country==cc & year==tmax, cc, yrLab) )
}


####### colors ################
mypalette <- brewer.pal(12,"Set3")


yTitle <- "Rate of Natural Increase"


print(
  ggplot(df4, aes(x=year, y=RNI_11, color=Country, group=Country, 
                  shape=Country, linetype = Country )) +
    geom_line(linewidth=1.0) + 
    scale_x_continuous(name="Year",  breaks = c(seq(xmin, xmax, xbrks)),
                       limits=(c(xmin, xmax)))+
    scale_y_continuous(name=yTitle ) +
    theme(legend.position="bottom", legend.title=element_blank()) +
    geom_label_repel(aes(label = yrLab), show.legend = FALSE ) 

  
    )

dev.off()
dev.off()
