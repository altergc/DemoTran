library(segmented)
library(crosstable)
library(googlesheets4)
library(tidyverse)
library(zoo)
library(gmodels)
library(flextable)
library(rlang)

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
                    YrRate50=double(),
                    YrRate40=double(),
                    YrRate30=double(),
                    YrRate20=double(),
                    YrRate10=double(),
                    maxRNI=double()
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

##########  run segments function ############################

runSegs <- function(spec, dVar ) {  
      ### 'spec' is the regression model
      ### 'dVar' is the variable (CBR or CDR)
  
      cat("\n\n*********  ", selCountry, "*********")
  
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
      
      ####%%%%% pad vector of predicted values when dependent variable 11-pt moving average %%%%%%%%%%
      if (length(pr1)<length(df2$year) ) {pr1 <<- c(NA, NA, NA, NA, NA, pr1, NA, NA, NA, NA, NA)}
      
print("mark 1")     ####### debugging checkpoint 
      
      
      #### add predicted values to dataframe 'df2'
      #### 'predVar' is the column name of the predicted values
      predVar <- paste(dVar,"pred", sep="")
      df2[,predVar] <<- pr1

print("mark 2")     ####### debugging checkpoint 
      
      #### print summary statistics     
      cat("\n\nSummary Observed and Predicted:\n")
      ss0 <- df2 %>% select( c( all_of({{ dVar }} ), predVar ) ) %>% summary()
      print(ss0)
      
      ps2 <- paste("ps", dVar, sep="_"  )
      cat("\npredicted variable name:",ps2)
      cat("\npassed variable name:",dVar, "\n\n")
      
      ################################################################
      ####### years when Rate (CBR or CDR) passed milestones #########
      #######    milestones are rates of 50, 40, 30, 20, 10  #########
      
      #####  'extYears' is a sequence of years from 1700 to 2100
      extYears <- seq(1700, 2100)
      
      #####  'prWide' is predicted value of the segmented model for 'extYears'
      prWide <- predict(fit.seg, newdata = data.frame(year = extYears) )
      
      #####  'xval' is vector of observed years in which milestones were reached
      xval <- approx(x = pr1 , y=df2$year , xout = c(50, 40, 30, 20, 10))$y
      
      #####  'xvalwide' is vector of milestone years extrapolated 
      xvalWide <- approx(x = prWide , y=extYears , xout = c(50, 40, 30, 20, 10))$y
      
      #####  use 'xvalwide' when 'xval' is not available
      for (yy in c(1,2,3,4,5)) {
        if (is.na(xval[yy])) {xval[yy]<-xvalWide[yy]}  }
      

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


####### collect breakpoints and SEs in 'breaks' ##################################
    for (bb in 1:4) {
      breaks[bb] <<- NA
      breaks[bb+4] <<- NA
      breaks[bb+8] <<- NA
      if (bb<=nbreaks) {
        breaks[bb] <<- fit.seg$psi[bb,2]
        breaks[bb+4] <<- fit.seg$psi[bb,3]
        breaks[bb+8] <<- df2 %>% subset( year==round(breaks[[bb]])) %>% select( all_of(sym(dVar) ) )
      }
    }

      
####  collect stats in vector 'dtemp'  #########################
      dtemp <-  data.frame( list(selCountry,
                                 df2$RegionCode[1],
                                 df2$RegionName[1],
                                 df2$SubRegionCode[1],
                                 df2$SubRegionName[1],
                                 demoVar,
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
                                 coeffs$coeff4SE,
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
                                 df2$CDR_MAV[length(df2$CDR_MAV)-5],
                                 xval[1],
                                 xval[2],
                                 xval[3],
                                 xval[4],
                                 xval[5],
                                 maxRNI
                                 ) )
      
      cat("mark 3----", selCountry)       ####### debugging checkpoint 
     
      

      ####  Add row to output dataframe 'dfout' #########################
        dfOut <<- rbind(dfOut, setNames(dtemp, names(dfOut)))
        
        ##########################################################
        
        #######################################################
        #######   Plot results                #################
      
        ####### 'vertLines' adds vertical lines at breakpoints #################
        #######  'bkLines' and 'gp' check for the number of breakpoints 
        bkLines <- c(dfOut$break1[length(dfOut$break1)])
        gp <- c(1)
        
        if (dfOut$nbreaks[length(dfOut$nbreaks)]  >1) {
          bkLines <- c(bkLines, dfOut$break2[length(dfOut$break2)])
          gp <- c(gp, 2)
        }
        if (dfOut$nbreaks[length(dfOut$nbreaks)]>2) {
          bkLines <- c(bkLines, dfOut$break3[length(dfOut$break3)])
          gp <- c(gp, 3)
        }
        
        vertLines <- tibble(gp , bkLines ) 
        
        #############  plot is printed to the PDF file ###########
        print(
          ggplot(df2, mapping=aes(x=year )) +
            ggtitle(selCountry) +
            geom_point( aes(y = !!sym(demoVar)   , color=demoVar, group=1  ) ) +
            geom_line( aes(y=!!sym(predVar), color="Predicted" , group=1) ) +
            labs(color = "Variable") +
            scale_y_continuous(name="rate" , limits= c(0,60) ) +
            geom_vline(data= vertLines, aes(xintercept = bkLines, group= gp ), linetype = "dashed"  )
        )        
             
      }

########## end runsegs ###########################

