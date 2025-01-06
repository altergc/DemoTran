library("readxl")
library("tidyr")
library("xlsx")

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran")

###### call runSegs.R to set up function that computes segments and stats #########
source("Rcode/runSegs.R")

#######  read data here ####################################
####### 'df1' is the master data file ######################
df1 <- read_excel("Data/Combine_UN_IDB_Palg.xlsx",
                  sheet="_49_combine_Un_IDB_Palg")

###### keep rows with both CBR and CDR values ###############
df1 <- subset(df1, (!is.na(df1$CBR) & !is.na(df1$CDR)) )

### fix for Russia
df1 <- subset(df1, !(df1$Country=="Russia" & df1$year<1928))



#######  open pdf file for graphs ###################### 
pdf("Output/SegPlots.pdf")


##########----------------------------------------#############
##########   loop on countries                    #############
##########   selCountry is name of country in loop
for (selCountry in unique(df1$Country) ) {

########## 'df2' holds one country at a time ##########
########## RNI and moving averages added  ###################
df2 <- df1[df1$Country==selCountry ,c(1, 2, 3, 4, 5, 6, 7, 8, 9 ) ] %>%  
  mutate(RNI = CBR - CDR,
         RNI_11 = rollmean(RNI, k=11, fill=NA, align='center'),
         CBR_MAV = rollmean(CBR, k=11, fill=NA, align='center'),
         CDR_MAV = rollmean(CDR, k=11, fill=NA, align='center')
  ) %>%  arrange(year)


startYear <- min(df2$year)
endYear <- max(df2$year)
maxRNI <- max(df2$RNI)



####### loop for CBR and CDR  #####################
for (demoVar in c("CBR" , "CDR" )  ) {
  
  ##### demoVar is "CBR" or "CD"  
  ##### colVar is the symbol that can be used as a column name
  ##### specVar is the regression model 
##  colVar = ensym(demoVar)
  specVar <- paste(demoVar, " ~ year")
  
  ########  call runSegs function ##############################
  runSegs( specVar, demoVar)   
  
   }
   #### end demoVar loop  #################

###### combined graph  ##################
xmax <- max(df2$year)
xmin <- min(df2$year)
xbrks <- round((xmax -xmin)/10, 0)
print(
    ggplot(df2, mapping=aes(x=year )) +
      ggtitle(selCountry) +
      geom_point( aes(y=CDR, color="CDR" , group=1) ) +
      geom_point( aes(y=CBR, color="CBR" , group=1 ) ) +
      geom_line( aes(y=CDRpred, color="CDRpred" , group=1) ) +
      geom_line( aes(y=CBRpred, color="CBRpred" , group=1 ) ) +
      scale_x_continuous(name="Years",  breaks = c(seq(xmin, xmax, xbrks)) )+
      scale_y_continuous(name="Rate",  limit=c( 0, 50), breaks = c(   0,   10 ,   20,   30,   40, 50) ) +
      scale_color_manual(values=c("blue", "blue", "red", "red" ))
     )



}
####### end selCountry loop ################

####### close PDF file
dev.off()

#####  save dfOut dataframe of statistics to disk  ##########
save(dfOut, file='Data/dfOut_2.rdata')
