library(ggplot2)

########### requires df subRegionMean

dfMiles <- data.frame(GroupName = character(),
                      year =double(),
                      cRate=double() )
dfMt <- data.frame(GroupName = character(),
                      year =double(),
                      cRate=double() )

for(vv in c( "CBR"))   {
   if ( nrow(dfMiles)>0) {dfMiles <- dfMiles[0,]}
   
   for( rr in c(15, 25, 35) )   {
     vname <- paste("mean",vv, "_YrRate", rr, sep="")
       dfMt <- subRegionMean %>% subset( !is.na(vname) ) %>%
         select( c(SubRegionName, vname)) %>% 
         rename(year = vname, GroupName = SubRegionName) %>%
         subset( !is.na(year) )
 
     cat(vv, rr, "rows" , nrow(dfMt), "\n")    
     if (nrow(dfMt)>0)  {
       dfMt$cRate <-rr
       dfMiles <- rbind(dfMiles , dfMt)}
     }
   
   
   selTitle <- vv

   subSelect <- list(   "Northern Europe",  "US & Canada", "Western Europe", 
                        "Southern Cone", "Eastern Asia", "Andean & Amazonian" , 
                   "Eastern Europe", "Northern Africa", 
                   "Southern Africa", "Southern Asia"
                    )
   
   dfMiles <- dfMiles[ dfMiles$GroupName %in% subSelect, ]
   
   
   ##### save plot to jpg ############
   pngFile <- paste("Output/CBRpng/milestonesSub", vv, ".png", sep="")
   png(pngFile)
  
   #### make list of groups for assigning symbols #########
   groupNum <- seq(1:length(unique(dfMiles$GroupName)))
   names(groupNum) <- unique(dfMiles$GroupName)
   
   xmin <- 1850
   xmax <- 2025
   xbrks <- 25
   
   print(
   ggplot(dfMiles, aes(x=year, y=cRate, color=GroupName, group=GroupName, shape=GroupName )) +
     geom_line(size=1.5) + 
     geom_point(size=2.5, stroke=1.5) +
     scale_shape_manual( values=groupNum) +
     scale_x_continuous(name="Year",  breaks = c(seq(xmin, xmax, xbrks)),
                        limits=(c(xmin, xmax)))+
     scale_y_continuous(name="Events per thousand population",  limit=c( 5, 40), breaks = c( 5,  15 , 25,   35,   40 ) ) +
         ggtitle(selTitle) + 
     theme(legend.position="bottom", legend.title=element_blank())
   )
}

dev.off()
dev.off()
