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

#### countries in Palgrave that don't match UN projections 1950-59 ###
df1 <- subset(df1, !( (df1$Country %in% c("China", "Colombia", "Egypt", "Fiji", "Mauritius", "Philippines", "Russia", "Singapore", "Venezuela"))
                       & df1$year<1950)  )


#######  open pdf file for graphs ###################### 
pdf("Output/SegPlots.pdf")


##########----------------------------------------#############
##########   loop on countries                    #############
### test loop ####for (selCountry in c("France","Germany") ) {

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
  

  ########  call runSegs function ##############################
  runSegs()   
  
   }

####### end selCountry loop ################

####### close PDF file
dev.off()

#####  save dfOut dataframe of statistics to disk  ##########
save(dfOut, file='Data/dfOut_2.rdata')


#####  save df3 dataframe of observed and predicted to disk  ##########
save(df3, file='Data/dfPred.rdata')

