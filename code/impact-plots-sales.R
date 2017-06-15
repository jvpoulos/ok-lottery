# Plot time-series and estimate causal impacts

setwd("~/Dropbox/github/drnns-prediction/results/ok-pred/sales")

# Import test results

test.files <- list.files(pattern = "sales-test*")

sales.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="sales.pred"))
sales.test.pred$sales.sd <- matrixStats::rowSds(as.matrix(sales.test.pred))

# Import training fit

train.files <- list.files(pattern = "sales-train*")

sales.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="sales.pred"))
sales.train.pred$sales.sd <- matrixStats::rowSds(as.matrix(sales.train.pred))

# Import validation fit

val.files <- list.files(pattern = "sales-val*")

sales.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                       col.names="sales.pred"))
sales.val.pred$sales.sd <- matrixStats::rowSds(as.matrix(sales.val.pred))

# Bind to splits
patents.test <- cbind(data.frame(patents.test), sales.test.pred[c(1,ncol(sales.test.pred))]) # first column is best model
patents.train <- cbind(data.frame(patents.train), sales.train.pred[c(1,ncol(sales.test.pred))])
patents.val <- cbind(data.frame(patents.val), sales.val.pred[c(1,ncol(sales.test.pred))])

# Create time series data

time.vars <- c("date","county_code","state_code","sales","sales.pred","sales.sd")

ts.dat <- rbind(patents.train[time.vars],patents.val[time.vars],patents.test[time.vars])

ts.dat$cat <- ifelse(ts.dat$state_code=="OK" & ts.dat$county_code %in% c(ok.lottery,ok.run,ok.allotment,ok.sealed), "Treated", "Nontreated") # compare land reform counties vs. all other

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

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date","cat"))

ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- round(as.POSIXct(ts.means.m$date, tz="UTC"), "months")

# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="obs" | ts.means.m$variable=="pred"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed","Predicted", "Pointwise","Cumulative")

# SD

sds <- ts.dat %>%
  group_by(date,cat) %>%
  summarise(obs = mean(sales,na.rm=TRUE),
            pred = mean(sales.pred,na.rm=TRUE),
            pred.min = pred - mean(sales.sd, na.rm=TRUE),
            pred.max = pred + mean(sales.sd, na.rm=TRUE))

sds <- sds  %>%
  group_by(date) %>%
  mutate(pointwise.min = ifelse(cat=='Treated', obs-pred.min, NA),
         pointwise.max = ifelse(cat=='Treated', obs-pred.max, NA)) %>% 
  group_by(cat) %>%
  mutate(cumulative.min = cumsum(pointwise.min),
         cumulative.max = cumsum(pointwise.max))

pred.vars <- c("pred.min","pred.max","pointwise.min","pointwise.max","cumulative.min","cumulative.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot sales
ts.means.m$value[ts.means.m$variable=="Observed" & ts.means.m$value >200] <- NA # for y axis control
sales.ts.plot <- TsPlot(ts.means.m,  main="Causal impact of land reform on land sales")

ggsave(paste0(data.directory,"plots/sales-ts-plot.png"), sales.ts.plot, width=8.5, height=11)