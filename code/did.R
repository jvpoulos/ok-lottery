###################################
# DiD Estimation for sanity check #
###################################
library(dplr)

## Patents

# Summarize by date/county/state

patents.sum.did <- patents %>%
  group_by(date,county_code,state_code) %>%
  summarise_each(funs(sum),sales,homesteads)

# Category
patents.sum.did$cat <- ifelse(patents.sum.did$state_code=="OK" & patents.sum.did$county_code %in% c(ok.lottery), "Treated", "Control") # compare lottery counties vs. all other

cats.sums.did <- patents.sum.did %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(sum),sales,homesteads) 

# Create var for when treatment started

cats.sums.did$time <- 0
cats.sums.did$time[cats.sums.did$date >= "Jul 1901"] <- 1

cats.sums.did$time.placebo <- 0
cats.sums.did$time.placebo[cats.sums.did$date >= "Jul 1898" & cats.sums.did$date < "Jul 1901"] <- 1 # for placebo

# Sales 
did.sales <- lm(sales ~ cat*time, data = cats.sums.did[cats.sums.did$date >= "Aug 1891",]) # subset to when treated counties exist

summary(did.sales)

confint(did.sales)[4,]

# sales (placebo)
did.sales.placebo <- lm(sales ~ cat*time.placebo, data = cats.sums.did[cats.sums.did$date >= "Aug 1891" & cats.sums.did$date < "Jul 1901",]) # subset fake pre & post-period

summary(did.sales.placebo)

confint(did.sales.placebo)[4,]

# Homesteads 
did.homesteads <- lm(homesteads ~ cat*time, data = cats.sums.did[cats.sums.did$date >= "Aug 1891",]) # subset to when treated counties exist

summary(did.homesteads)

confint(did.homesteads)[4,]

# Homesteads (placebo)
did.homesteads.placebo <- lm(homesteads ~ cat*time.placebo, data = cats.sums.did[cats.sums.did$date >= "Aug 1891" & cats.sums.did$date < "Jul 1901",]) # subset fake pre & post-period

summary(did.homesteads.placebo)

confint(did.homesteads.placebo)[4,]

## Inequality/tenancy

c.county <- RbindMatchColumns(df1, df8)[c("year","state", "county", "G", "aG", "tenancy")] #1890-1950 # Gini & Tenancy

# Year to time

c.county$date <- as.yearmon(paste0("12/01/",c.county$year), "%m/%d/%Y",tz="UTC") # convert to monthly data

# Create county category var 

c.county$cat <- ifelse(c.county$state==53 & c.county$county %in% c(ok.lottery), "Treated", "Control") # compare lottery counties vs. all other

cats.sums.did <- c.county %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),G, aG, tenancy) 

# Create var for when treatment started

cats.sums.did$time <- 0
cats.sums.did$time[cats.sums.did$date > "Dec 1900"] <- 1

cats.sums.did$time.placebo <- 0
cats.sums.did$time.placebo[cats.sums.did$date == "Dec 1900" ] <- 1

# gini 
did.gini <- lm(G ~ cat*time, data = cats.sums.did) 

summary(did.gini)

confint(did.gini)[4,]

# adjusted gini 
did.agini <- lm(aG ~ cat*time, data = cats.sums.did) 

summary(did.agini)

confint(did.agini)[4,]

# tenancy 
did.tenancy <- lm(tenancy ~ cat*time, data = cats.sums.did) 

summary(did.tenancy)

confint(did.tenancy)[4,]

# adjusted gini (placebo)
did.agini.placebo <- lm(aG ~ cat*time.placebo, data = cats.sums.did[cats.sums.did$date <= "Dec 1910",]) # subset to fake pre- and post-period

summary(did.agini.placebo)

confint(did.agini.placebo)[4,]

# gini (placebo)
did.gini.placebo <- lm(G ~ cat*time.placebo, data =  cats.sums.did[cats.sums.did$date <= "Dec 1910",]) # subset to fake pre- and post-period

summary(did.gini.placebo)

confint(did.gini.placebo)[4,]

# tenancy.placebo (placebo)
did.tenancy.placebo <- lm(tenancy ~ cat*time.placebo, data = cats.sums.did[cats.sums.did$date <= "Dec 1910",]) # subset to fake pre- and post-period

summary(did.tenancy.placebo)

confint(did.tenancy.placebo)[4,]