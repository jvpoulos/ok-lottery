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

# Create var for when treatment started

patents.sum.did$time <- 0
patents.sum.did$time[patents.sum.did$date >= "Jul 1901"] <- 1

did.sales <- lm(sales ~ cat*time, data = patents.sum.did)

summary(did.sales)

confint(did.sales)