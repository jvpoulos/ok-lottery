# Plot time-series and estimate causal impacts

# Import test results

sales.test.pred <- read.csv("~/Dropbox/github/drnns-prediction/results/ok-pred/sales-test-pred-weights-3.421.hdf5.csv", header=FALSE, col.names = "sales.pred")

patents.test <- cbind(data.frame(patents.test), sales.test.pred)

# Import training fit

sales.train.pred <- read.csv("~/Dropbox/github/drnns-prediction/results/ok-pred/sales-train-pred-weights-3.421.hdf5.csv", header=FALSE, col.names = "sales.pred")

patents.train <- cbind(data.frame(patents.train), sales.train.pred)

# Import validation fit

sales.val.pred <- read.csv("~/Dropbox/github/drnns-prediction/results/ok-pred/sales-val-pred-weights-3.421.hdf5.csv", header=FALSE, col.names = "sales.pred")

patents.val <- cbind(data.frame(patents.val), sales.val.pred)

# Create time series data

# time.vars <- c("id","date","sales","sales.pred")
# 
# ts.dat <- rbind(county.x.train[time.vars],county.x.test[time.vars])
# 
# cat.ids <- data.frame("id"=as.numeric(interaction(c.county$county, c.county$state)),
#                        "cat"=c.county$cat) # back out categories
# cat.ids <- cat.ids[!duplicated(cat.ids$id),]
# 
# ts.dat <- merge(ts.dat, cat.ids, by="id", all.x=TRUE)
# 
# ts.dat$cat <- ifelse(ts.dat$cat>=2, "Treated", "Control") # compare land reform counties vs. all other

time.vars <- c("date","county_code","state_code","sales","sales.pred")

ts.dat <- rbind(patents.train[time.vars],patents.val[time.vars],patents.test[time.vars])

ts.dat$cat <- ifelse(ts.dat$state_code=="OK" & ts.dat$county_code %in% c(ok.lottery,ok.run,ok.allotment,ok.sealed), "Treated", "Control") # compare land reform counties vs. all other

## Plot time series

ts.means <- ts.dat %>%
  group_by(date,cat) %>% # also: dv
  summarise(obs = mean(sales,na.rm=TRUE),
            pred = mean(sales.pred,na.rm=TRUE)) 

ts.means <- ts.means  %>%
  group_by(date) %>%
  mutate(pointwise = ifelse(cat=='Treated', obs-pred, NA)) %>% # calc. pointwise impact
  group_by(cat) %>%
  mutate(cumulative = cumsum(pointwise))

ts.means <- melt(as.data.frame(ts.means), id.var=c("date","cat"))

#ts.means$date <- as.yearmon(paste0(ts.means$date,"-01"), "%Y-%m",tz="UTC") # convert date to date class
ts.means$date <- as.yearmon(ts.means$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means$date <- round(as.POSIXct(ts.means$date, tz="UTC"), "months")

ts.means$series <- NA
ts.means$series[ts.means$variable=="obs" | ts.means$variable=="pred"] <- "Time-series"
ts.means$series[ts.means$variable=="pointwise"] <- "Pointwise impact"
ts.means$series[ts.means$variable=="cumulative"] <- "Cumulative impact"

ts.means$series<- factor(ts.means$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means$variable) <- c("Observed","Predicted","Pointwise","Cumulative")


# sales
ts.means$value[ts.means$variable=="Observed" & ts.means$value >40] <- NA # for y axis control
TsPlot(ts.means,  main="Causal effect of land reform on land sales")

# gini
TsPlot(ts.means,  main="Causal effect of land reform on land inequality")

