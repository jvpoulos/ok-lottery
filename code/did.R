###################################
# DiD Estimation for sanity check #
###################################

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

# Sales 
did.sales <- lm(sales ~ cat*time, data = cats.sums.did[cats.sums.did$date >= "Aug 1891",]) # subset to when treated counties exist

summary(did.sales)

confint(did.sales)[4,]

# Homesteads 
did.homesteads <- lm(homesteads ~ cat*time, data = cats.sums.did[cats.sums.did$date >= "Aug 1891",]) # subset to when treated counties exist

summary(did.homesteads)

confint(did.homesteads)[4,]

## Census

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

# gini 
did.gini <- lm(G ~ cat*time, data = cats.sums.did) # subset to when treated counties exist

summary(did.gini)

confint(did.gini)[4,]

# adjusted gini 
did.agini <- lm(aG ~ cat*time, data = cats.sums.did) # subset to when treated counties exist

summary(did.agini)

confint(did.agini)[4,]


# tenancy 
did.tenancy <- lm(tenancy ~ cat*time, data = cats.sums.did) # subset to when treated counties exist

summary(did.tenancy)

confint(did.tenancy)[4,]