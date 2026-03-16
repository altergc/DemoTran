########### Preston Curves for RNI #############

library("readxl")
library("tidyr")
library("xlsx")
library(dplyr)
library(tidyverse)

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

#######  read country-region list here ####################################
dfCountry <- read_excel("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran/Data/00C2_country_region.xlsx",
                        sheet="_00C2_country_region")


#######  read crude rates  here ####################################
dfDemog <- read_excel("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran/Data/00C1_Export_all_sorted.xlsx",
                  sheet="_00C1_Export_all_sorted")
dfDemog <- dfDemog %>% rename( year = Year)
dfDemog$RNI <- dfDemog$CBR - dfDemog$CDR 

dfDemog <- dfDemog[dfDemog$RNI>-30,]

#######  read GDPpc data here ####################################
dfEcon <- read_excel("C:/Users/altergc/Documents/DATA/Country_level/AHDI_countries_1870-2020-1.xlsx",
                     sheet="Table 1 Per Capita GDP $1990", skip=1)
dfEcon <- dfEcon %>% rename( Country = Y1990)
dfEcon <- dfEcon[!is.na(dfEcon[[txtYear]]), ]




yearList <- list(1950, 1970, 1990, 2010)

dfPredE0 <- data.frame(year = numeric(0), PredE = numeric(0), GDPpc = numeric(0) )
dfNewRows  <- data.frame(year = numeric(0), PredE = numeric(0), GDPpc = numeric(0) )



for (numYear in yearList) {
    
    txtYear <- as.character(numYear)
    
    #####################################
    ######## merge demog, econ, life exp #######
    dfDemogYr <- dfDemog[dfDemog$year==numYear, ]
    
    dfEconYr <- dfEcon[, c('Country', txtYear)]
    dfEconYr <- dfEconYr %>% rename( GDPpc = txtYear)
    
    dfMerge <- merge(
      x = dfDemogYr,
      y = dfEconYr,
      by = "Country",
      all = FALSE 
    )
    
    #####################
    dfMerge <- dfMerge %>% filter(!is.na(dfMerge$'GDPpc') & !is.na(dfMerge$RNI) )
    
    ##################################
    selTitle <- paste("Income by Rate of Natural Increase", txtYear, sep=" ")
    xmin <- 0
    xmax <- 20000
    
    # print(
    #   ggplot(dfMerge, mapping=aes(x=GDPpc )) +
    #     ggtitle(selTitle) +
    #     geom_point( aes(y=RNI ) ) +
    #     scale_x_continuous(name="GDPpc", limit=c(xmin, xmax)  )+
    #     scale_y_continuous(name="Rate of Natural Increase" )
    # )
    
    ###### OLS
    dfMerge$lExp <- log( (200/(150-dfMerge$RNI)) - 1)

    dfMerge$GDPscal <- log( (dfMerge$GDPpc - 40)/(10000 - 40) )

    ols_model <- lm(lExp ~ GDPscal , data = dfMerge)
    print(summary(ols_model))
    predicted_values_fitted <- predict(ols_model)
    dfMerge$Predicted_Y <- predicted_values_fitted

    dfMerge$PredE <- -(200 - 150*(1 + exp(dfMerge$Predicted_Y)))/(1 + exp(dfMerge$Predicted_Y))

    dfNewRows <- dfMerge %>%
            select(year = year, PredE = PredE, GDPpc = GDPpc)

    dfPredE0 <- rbind(dfPredE0, dfNewRows)

    dfNewRows <- dfNewRows[0, ]

    print(
      ggplot(dfMerge, mapping=aes(x=GDPpc )) +
        ggtitle(selTitle) +
        geom_point( aes(y=RNI ) ) +
        geom_line( aes(y=PredE)) +
        scale_x_continuous(name="GDPpc", limit=c(xmin, xmax)  )+
        scale_y_continuous(name="Rate of Natural Increase" )
    )
    }

dfPredE0$year <- as.character(dfPredE0$year)


ggplot(dfPredE0, mapping=aes(x=GDPpc , y=PredE , group=year, color=year )) +
  ggtitle("Income by Rate of Natural Increase") +
  geom_line( linewidth= 1) +
  scale_x_continuous(name="GDPpc", limit=c(xmin, xmax)  )+
  scale_y_continuous(name="Rate of Natural Increase" )

