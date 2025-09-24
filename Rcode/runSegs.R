library(segmented)
library(crosstable)
library(googlesheets4)
library(tidyverse)
library(zoo)
library(gmodels)
library(flextable)
library(rlang)
library(foreach)

############# setup output 'dfOut' dataframe for country stats  #################
dfOut <- data.frame(Country=character(),
                    RegionCode=character(),
                    RegionName=character(),
                    SubRegionCode=character(),
                    SubRegionName=character(),
                    demoVar=character(),
                    npsi=integer(),
                    BIC0=double(),
                    BIC1=double(),
                    BIC2=double(),
                    BIC3=double(),
                    BIC4=double(),
                    nbreaks=integer(),
                    coeff0=double(),
                    coeff1=double(),
                    coeff2=double(),
                    coeff3=double(),
                    coeff4=double(),
                    coeff0SE=double(),
                    coeff1SE=double(),
                    coeff2SE=double(),
                    coeff3SE=double(),
                    coeff4SE=double(),
                    break1=double(),
                    break2=double(),
                    break3=double(),
                    break4=double(),
                    break1SE=double(),
                    break2SE=double(),
                    break3SE=double(),
                    break4SE=double(),
                    obsB1=double(),
                    obsB2=double(),
                    obsB3=double(),
                    obsB4=double(),
                    startCBRmav=double(), 
                    endCBRmav=double(), 
                    startCDRmav=double(), 
                    endCDRmav=double(),
                    YrRate45=double(),
                    YrRate35=double(),
                    YrRate30=double(),
                    YrRate25=double(),
                    YrRate20=double(),
                    YrRate15=double(),
                    YrRate10=double(),
                    YrRate05=double(),
                    maxRNI=double(),
                    maxRNIyear=integer(),
                    minRNI=double(),
                    minRNIyear=integer(),
                    RNIgt30 =integer(),
                    RNI20 =integer(),
                    RNI10 =integer(),
                    RNI00 =integer(),
                    RNIlt0 =integer(),
                    startYear=integer(),
                    endYear=integer(),
                    maxSlope=double()
                    )

###### 'coeffs' collects coefficients and SEs from the segmented regression
coeffs <- list(
                coeff0=double(),
                coeff1=double(),
                coeff2=double(),
                coeff3=double(),
                coeff4=double(),
                coeff0SE=double(),
                coeff1SE=double(),
                coeff2SE=double(),
                coeff3SE=double(),
                coeff4SE=double())

###### 'breaks' collects break points (i.e., transition years) and SEs 
######    from segmented regression
breaks <- list(break1=double(),
               break2=double(),
               break3=double(),
               break4=double(),
               break1SE=double(),
               break2SE=double(),
               break3SE=double(),
               break4SE=double(),
               obsB1=double(),
               obsB2=double(),
               obsB3=double(),
               obsB4=double())
#######################################################################

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

###print("mark 1")     ####### debugging checkpoint 



##########  run segments function ############################

runSegs <- function(yrMin, yrMax) {  

print("mark 2-")     ####### debugging checkpoint
  
    cat("\n\n*********  ", selCountry, "*********")
 
    startYear <- min(df2$year)
    endYear <- max(df2$year)
    maxRNI <- max(df2$RNI)
    minRNI <- min(df2$RNI)
    
    ###### if multiple max or min of RNI   ########
    ######     maxRNI is earlier date      ########
    ######     minRNI is later date        ########
    maxRNIyear <- min(df2[which(df2$RNI==maxRNI), "year" ])
    minRNIyear <- max(df2[which(df2$RNI==minRNI), "year" ])
    
    ### Number of years RNI was in a range of values ####
    rniCounts <- summarise(df2, RNIgt30 = sum(RNI >= 30),
                                     RNI20 = sum(RNI >=20 & RNI<30),
                                     RNI10 = sum(RNI >=10 & RNI<20),
                                     RNI00 = sum(RNI >= 0 & RNI<10),
                                     RNIlt0 = sum(RNI<0)
                                     )
  
  
  ####### 'dVar' loop for CBR and CDR  #####################
  for (dVar in c("CDR", "CBR"  )  ) {
 
    ##### spec is the regression model 
      spec <- paste(dVar, " ~ year") 

          ##### fit a linear model 
      fit.lm <- lm(spec, data=df2 )
      
      #####  selgmented selects the best fit segmented model 
      #####   Kmax= is the maximum number of breakpoints to consider
      osel <- selgmented(fit.lm, Kmax=3, type="bic", check.dslope = TRUE)
      
      #####  'npsi' is the number of breakpoints selected by selsegmented
      #####  'nbreaks' adds 1 to 'npsi' so there is at least one breakpoint
      nbreaks <- osel$selection.psi$npsi
      if (nbreaks==0) {nbreaks=1}
      cat("\nusing", nbreaks, "breakpoints")
      
      #### segmented runs the segmented model with 'nbreaks' breakpoints 
      fit.seg <- segmented(fit.lm, seg.Z=~year, npsi=nbreaks)
      
      cat("\n\nModel summary:")
      print(summary(fit.seg))
      
      #### pr1 is vector of predicted values in segmented model
      pr1 <<- predict(fit.seg)

cat("mark 2+  -- length(pr1)", length(pr1))     ####### debugging checkpoint 

      ####%%%%% pad vector of predicted values when dependent variable 11-pt moving average %%%%%%%%%%
      ##if (length(pr1)<length(df2$year) ) {pr1 <<- c(NA, NA, NA, NA, NA, pr1, NA, NA, NA, NA, NA)}
      
      
      
      #### add predicted values to dataframe 'df2'
      #### 'predVar' is the column name of the predicted values CBRpred & CDRpred
      predVar <- paste(dVar,"pred", sep="")
      df2[,predVar] <<- pr1

cat("mark 2a -- nrow(df2)", nrow(df2))     ####### debugging checkpoint 

      #### print summary statistics     
      cat("\n\nSummary Observed and Predicted:\n")
      ss0 <- df2[c( dVar, predVar ) ] %>% summary()
      print(ss0)
      
      ps2 <- paste("ps", dVar, sep="_"  )
      cat("\npredicted variable name:",ps2)
      cat("\npassed variable name:",dVar, "\n\n")
      
      ################################################################
      ####### years when Rate (CBR or CDR) passed milestones #########
      #######    milestones are rates of 45, 35, 30, 25, 20, 15, 10, 5  #########
      xmile <- c(45, 35, 30, 25, 20, 15, 10, 5)
      #####  'xval' is vector of observed years in which milestones were reached
      #####  If milestone is crossed more than once, result is average of years
      xval <- approx(x = pr1 , y=df2$year , xout = xmile )$y
      
      cat("\n", "xval=", xval, "\n")
      
#       ###### check that MAV of rate was above milestone 2 years earlier ########
#       ###### compares milestone to value of moving average in observed  ########
#       ######    series 2 years earlier                                  ########
#       ######    if observed series was lower, milestone set to NA       ########
#       for(ii in seq(1:8) ) {
#         ckYear <- trunc(xval[ii]-2)
# cat(ii,"ckYear--***--", ckYear, "******\n")
#         indxYear <- ckYear - min(df2$year)
# cat(ii,"indxYear--***--", indxYear, "%%%%%\n")
#         if (dVar=="CDR" & !is.na(xval[ii]) & !is.na(df2$CDR_MAV[indxYear]) ) {
#           cat(dVar, xmile[ii], xval[ii], ckYear, indxYear, df2$CDR_MAV[indxYear], "\n")   #### precheck
#           if (df2$CDR_MAV[indxYear]< xmile[ii]) 
#             {xval[ii] <- NA
#             cat("D value nulled \n")
#             cat(dVar, xmile[ii], xval[ii], ckYear, indxYear, df2$CDR_MAV[indxYear], "\n")   #### postcheck  
#             }
#           } 
#         if (dVar=="CBR" & !is.na(xval[ii]) & !is.na(df2$CBR_MAV[indxYear]) ) {
#           cat(dVar, xmile[ii], xval[ii], ckYear, indxYear, df2$CBR_MAV[indxYear], "\n")   #### precheck
#           if (df2$CBR_MAV[indxYear]< xmile[ii]) 
#             {xval[ii] <- NA
#             cat("B value nulled \n")
#             cat(dVar, xmile[ii], xval[ii], ckYear, indxYear, df2$CBR_MAV[indxYear], "\n")   #### postcheck
#             }
#         } 
#         
#        }
      
      
      #####  'extYears' is a sequence of years from 1700 to 2100
      # extYears <- seq(1700, 2100)
      #####  'prWide' is predicted value of the segmented model for 'extYears'
      # prWide <- predict(fit.seg, newdata = data.frame(year = extYears) )
      # #####  'xvalwide' is vector of milestone years extrapolated 
      # xvalWide <- approx(x = prWide , y=extYears , xout = c(45, 35, 25, 15, 5))$y
      # 
      # #####  use 'xvalwide' when 'xval' is not available
      # for (yy in c(1,2,3,4,5)) {
      #   if (is.na(xval[yy])) {xval[yy]<-xvalWide[yy]}  }
      
      
#######################################################################
######### capture results  ############################################

####### collect coefficients and SEs  in 'coeffs' #####################
    for (cc in 1:5) {
      coeffs[cc] <<- NA
      coeffs[cc+5] <<- NA
      if (cc<=nbreaks+2) {
        coeffs[cc] <<- coef(summary(fit.seg))[cc,1]
        coeffs[cc+5] <<- coef(summary(fit.seg))[cc,2]
      }
    }

maxSlope <- coeffs$coeff1
if ( !is.na(coeffs$coeff2) & maxSlope > coeffs$coeff1 + coeffs$coeff2 ) 
    {maxSlope <- coeffs$coeff1 + coeffs$coeff2}
if ( !is.na(coeffs$coeff3) & maxSlope > coeffs$coeff1 + coeffs$coeff2 + coeffs$coeff3 ) 
    {maxSlope <- coeffs$coeff1 + coeffs$coeff2 + coeffs$coeff3}
if ( !is.na(coeffs$coeff4) & maxSlope > coeffs$coeff1 + coeffs$coeff2 + coeffs$coeff3 + coeffs$coeff4 ) 
    {maxSlope <- coeffs$coeff1 + coeffs$coeff2 + coeffs$coeff3 + coeffs$coeff4}
if (maxSlope>0) {maxSlope <- NA}

print("mark 2b")     ####### debugging checkpoint 

####### collect breakpoints and SEs in 'breaks' ##################################
#######  breaks[bb] is breakpoint in decimal year
#######  breaks[bb+4] is SE of breakpoint
#######  breaks[bb+8] is observed value of rounded breakpoint year
    for (bb in 1:4) {
      breaks[bb] <<- NA
      breaks[bb+4] <<- NA
      breaks[bb+8] <<- NA
      if (bb<=nbreaks) {
        breaks[bb] <<- fit.seg$psi[bb,2]
        breaks[bb+4] <<- fit.seg$psi[bb,3]
        
        #### check that rate is observed in break year #########
        if (nrow(df2 %>% subset( year==round(breaks[[bb]])) ==1 ))  {
          breaks[bb+8] <<- df2 %>% subset( year==round(breaks[[bb]])) %>% 
            dplyr::select( dVar )  
          }
      }
    }
      
print("mark 2c\n")     ####### debugging checkpoint 
cat("Breaks:\n", str(breaks), "\n*******\n")
####  collect stats in vector 'dtemp'  #########################
      dtemp <-  data.frame( list(selCountry,
                                 df2$RegionCode[1],
                                 df2$RegionName[1],
                                 df2$SubRegionCode[1],
                                 df2$SubRegionName[1],
                                 dVar,
                                 osel$selection.psi$npsi,
                                 osel$selection.psi$bic.values[2,1],
                                 osel$selection.psi$bic.values[2,2],
                                 osel$selection.psi$bic.values[2,3],
                                 NA,
                                 NA,
                                 nbreaks,
                                 coeffs$coeff0,
                                 coeffs$coeff1,
                                 coeffs$coeff2,
                                 coeffs$coeff3,
                                 coeffs$coeff4,
                                 coeffs$coeff0SE,
                                 coeffs$coeff1SE,
                                 coeffs$coeff2SE,
                                 coeffs$coeff3SE,
                                 coeffs$coeff4SE ,
                                 breaks$break1,
                                 breaks$break2,
                                 breaks$break3,
                                 breaks$break4,
                                 breaks$break1SE,
                                 breaks$break2SE,
                                 breaks$break3SE,
                                 breaks$break4SE,
                                 breaks$obsB1,
                                 breaks$obsB2,
                                 breaks$obsB3,
                                 breaks$obsB4,
                                 df2$CBR_MAV[6],
                                 df2$CBR_MAV[length(df2$CBR_MAV)-5],
                                 df2$CDR_MAV[6],
                                 df2$CDR_MAV[length(df2$CDR_MAV)-5] ,
                                 xval[1],
                                 xval[2],
                                 xval[3],
                                 xval[4],
                                 xval[5],
                                 xval[6],
                                 xval[7],
                                 xval[8],
                                 maxRNI,
                                 maxRNIyear,
                                 minRNI,
                                 minRNIyear ,
                                 rniCounts[1],
                                 rniCounts[2],
                                 rniCounts[3],
                                 rniCounts[4],
                                 rniCounts[5],
                                 startYear,
                                 endYear,
                                 maxSlope
                                 ) )

cat("mark 2d -- rows in dtemp:", nrow(dtemp), "\n")     ####### debugging checkpoint 

        ####  Add row to output dataframe 'dfout' #########################
        dfOut <<- rbind(dfOut, setNames(dtemp, names(dfOut)))

        ##########################################################
        
        #######################################################
        #######   Plot results                #################
      
        ####### 'vertLines' adds vertical lines at breakpoints #################
        #######  'bkLines' and 'gp' check for the number of breakpoints 
        bkLines <- c(dfOut$break1[length(dfOut$break1)])
        gp <- c(1)

print("mark 2e")     ####### debugging checkpoint         
        
        
        if (dfOut$nbreaks[length(dfOut$nbreaks)]  >1) {
          bkLines <- c(bkLines, dfOut$break2[length(dfOut$break2)])
          gp <- c(gp, 2)
        }

        if (dfOut$nbreaks[length(dfOut$nbreaks)]>2) {
          bkLines <- c(bkLines, dfOut$break3[length(dfOut$break3)])
          gp <- c(gp, 3)
        }
        
        vertLines <- tibble(gp , bkLines ) 
        
print("mark 2f")     ####### debugging checkpoint 
      #   #############  plot is printed to the PDF file ###########
      #   print(
      #     ggplot(df2, mapping=aes(x=year )) +
      #       ggtitle(selCountry) +
      #       geom_point( aes(y = !!sym(dVar)   , color=dVar, group=1  ) ) +
      #       geom_line( aes(y=!!sym(predVar), color="Predicted" , group=1) ) +
      #       labs(color = "Variable") +
      #       scale_y_continuous(name="rate" , limits= c(0,60) ) +
      #       geom_vline(data= vertLines, aes(xintercept = bkLines, group= gp ), linetype = "dashed"  )
      #   )        
        
    }
  #### end dVar loop  #################
  
  ###### combined graph  ##################
  if (yrMax > 0) {xmax <-yrMax} else { xmax <- max(df2$year) }
  if (yrMin > 0) {xmin <-yrMin} else { xmin <- min(df2$year) }
    
    cat("****", yrMax, yrMin, xmax, xmin)

  xbrks <- round((xmax -xmin)/10, 0)
  groupNum <- c(1,3)
  
  print(
    ggplot(df2, mapping=aes(x=year )) +
      ggtitle(selTitle) +
      geom_point( aes(y=CDR, color="CDR" , group=1 , shape="CDR" ) ) +
      geom_point( aes(y=CBR, color="CBR" , group=2 , shape="CBR"   ) ) +
      geom_line( aes(y=CDRpred, color="CDR" , group=3) ) +
      geom_line( aes(y=CBRpred, color="CBR" , group=4 ) ) +
      scale_x_continuous(name="Year", limit=c(xmin, xmax), breaks = c(seq(xmin, xmax, xbrks)) )+
      scale_y_continuous(name="Events per thousand population",  limit=c( 0, 55), breaks = c(   0, 5,  15 ,   25,   35,   45, 55) ) +
      scale_shape_manual(name= "CBR and CDR", values=c(16,3) ) +
      scale_color_manual(name= "CBR and CDR", values=c("blue", "red", "blue", "red" ))
  )

  
  cat("\nmark 3----", selCountry, "\n\n\n")       ####### debugging checkpoint 

  
  #######  add row to df3  ################  
   df3 <<- rbind(df3,  df2  )
  
  
}
########## end runsegs ###########################

