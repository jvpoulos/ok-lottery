# Plot time-series and estimate causal impacts

# Import test results

gini.test.pred <- read_csv("~/Dropbox/github/drnns-prediction/results/ok-pred/gini-test-pred.csv", col_names = FALSE)

gini.test.pred <- rbind(data.frame("X1"=rep(NA,7)), gini.test.pred)

names(gini.test.pred) <- c("gini.pred")

county.x.test <- cbind(county.x.test, gini.test.pred)

# Import training fit

gini.train.pred <- read_csv("~/Dropbox/github/drnns-prediction/results/ok-pred/gini-train-pred.csv", col_names = FALSE)

names(gini.train.pred) <- c("gini.pred")

county.x.train <- cbind(county.x.train, gini.train.pred)

# Create time series data

time.vars <- c("id","year","gini","gini.pred")

ts.dat <- rbind(county.x.train[time.vars],county.x.test[time.vars])

cat.ids <- data.frame("id"=as.numeric(interaction(c.county$county, c.county$state)),
                      "cat"=c.county$cat) # back out categories
cat.ids <- cat.ids[!duplicated(cat.ids$id),]

ts.dat <- merge(ts.dat, cat.ids, by="id", all.x=TRUE)

ts.dat$cat <- ifelse(ts.dat$cat>=2, "Treated", "Control") # compare land reform counties vs. all other

## Plot time series

ts.means <- ts.dat %>%
  group_by(year,cat) %>% # also: dv
  summarise(obs = mean(gini,na.rm=TRUE),
            pred = mean(gini.pred,na.rm=TRUE)) 

ts.means <- ts.means  %>%
  group_by(year) %>%
  mutate(pointwise = ifelse(cat=='Treated', obs-pred, NA)) %>% # calc. pointwise impact
  group_by(cat) %>%
  mutate(cumulative = cumsum(pointwise))


ts.means <- melt(as.data.frame(ts.means), id.var=c("year","cat"))

ts.means$year <- as.yearmon(paste0(ts.means$year,"-01"), "%Y-%m",tz="UTC") # convert year to date class

ts.means$year <- round(as.POSIXct(ts.means$year, tz="UTC"), "months")

ts.means$series <- NA
ts.means$series[ts.means$variable=="obs" | ts.means$variable=="pred"] <- "Time-series"
ts.means$series[ts.means$variable=="pointwise"] <- "Pointwise impact"
ts.means$series[ts.means$variable=="cumulative"] <- "Cumulative impact"

ts.means$series<- factor(ts.means$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means$variable) <- c("Observed","Predicted","Pointwise","Cumulative")

# Gini

TsPlot(ts.means,  main="Causal effect of land reform on land inequality")

