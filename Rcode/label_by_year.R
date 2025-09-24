dfMaxMin <- data.frame(
      Country = character(),
      rniMin = integer(),
      rniMax = integer()
      )
d6 <- df4

for (cc in unique(df4$Country)) {
  d5 <- d6 %>% filter(Country==cc & !is.na(RNI_11)  )
  tmin <-  min(d5$year)
  tmax <- max(d5$year)
  d6 <- d6 %>% mutate(yrLab = ifelse(Country==cc & year==tmin, cc, yrLab) )
  d6 <- d6 %>% mutate(yrLab = ifelse(Country==cc & year==tmax, cc, yrLab) )
}

