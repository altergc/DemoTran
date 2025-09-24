listCountries <- dfOut %>%
         subset(startYear<1940) %>%
         select("Country")  %>%
         unique()

listCountries <- rbind(listCountries, c(Country="China"))
listCountries <- rbind(listCountries, c( Country="India") )

setwd("C:/Users/altergc/Documents/Historical Demography/Handbook/DT_git/DemoTran/Output/CountryPNG")


for( CC in 1:nrow(listCountries) ) {
  
  selCountry <- listCountries[CC,1]
  
  fileTitle <- paste(selCountry, "_1700.png", sep="")
  
  png(fileTitle)

    dfG <- subset(df3, df3$Country == selCountry)
    
    xmax <- max(2025)
    xmin <- min(1725)
    xbrks <- round((xmax -xmin)/10, 0)
    selTitle <- selCountry

    print  (   
    ggplot(dfG, mapping=aes(x=year )) +
      ggtitle(selTitle) +
      geom_point( aes(y=CDR, color="CDR" , group=1) ) +
      geom_point( aes(y=CBR, color="CBR" , group=1 ) ) +
      geom_line( aes(y=CDRpred, color="CDRpred" , group=1) ) +
      geom_line( aes(y=CBRpred, color="CBRpred" , group=1 ) ) +
      scale_x_continuous(name="Year",  breaks = c(seq(xmin, xmax, xbrks)),
                         limits=(c(1725,2025)))+
      scale_y_continuous(name="Events per thousand population",  limit=c( 0, 55), breaks = c(   0, 5,  15 ,   25,   35,   45, 55) ) +
      scale_color_manual(values=c("blue", "blue", "red", "red" ))
    )
    
    ####### close PNG file
    dev.off()    
    }