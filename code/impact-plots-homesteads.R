# Plot time-series and estimate causal impacts

setwd("~/Dropbox/github/drnns-prediction/results/ok-pred/homesteads")

# Import test results

test.files <- list.files(pattern = "homesteads-test*")

homesteads.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="homesteads.pred"))
homesteads.test.pred$homesteads.sd <- matrixStats::rowSds(as.matrix(homesteads.test.pred))

# Import training fit

train.files <- list.files(pattern = "homesteads-train*")

homesteads.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="homesteads.pred"))
homesteads.train.pred$homesteads.sd <- matrixStats::rowSds(as.matrix(homesteads.train.pred))

# Import validation fit

val.files <- list.files(pattern = "homesteads-val*")

homesteads.val.pred <- do.call(cbind,lapply(val.files,read.csv, 
                                        header=FALSE,
                                       col.names="homesteads.pred"))
homesteads.val.pred$homesteads.sd <- matrixStats::rowSds(as.matrix(homesteads.val.pred))

# Bind to splits
patents.test <- cbind(data.frame(patents.test), homesteads.test.pred[c(1,ncol(homesteads.test.pred))]) # first column is best model
patents.train <- cbind(data.frame(patents.train), homesteads.train.pred[c(1,ncol(homesteads.test.pred))])
patents.val <- cbind(data.frame(patents.val), homesteads.val.pred[c(1,ncol(homesteads.test.pred))])

# Create time series data

time.vars <- c("date","county_code","state_code","homesteads","homesteads.pred","homesteads.sd")

ts.dat <- rbind(patents.train[time.vars],patents.val[time.vars],patents.test[time.vars])

ts.dat$cat <- ifelse(ts.dat$state_code=="OK" & ts.dat$county_code %in% c(ok.lottery,ok.run,ok.allotment,ok.sealed), "Treated", "Nontreated") # compare land reform counties vs. all other

## Plot time series 

ts.means <- ts.dat %>%
  group_by(date,cat) %>% # also: dv
  summarise(obs = mean(homesteads,na.rm=TRUE),
            pred = mean(homesteads.pred,na.rm=TRUE)) 

ts.means <- ts.means  %>%
  group_by(date) %>%
  mutate(pointwise = obs-pred) %>% # calc. pointwise impact
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
  summarise(obs = mean(homesteads,na.rm=TRUE),
            pred = mean(homesteads.pred,na.rm=TRUE),
            pred.min = pred - mean(homesteads.sd, na.rm=TRUE),
            pred.max = pred + mean(homesteads.sd, na.rm=TRUE))

sds <- sds  %>%
  group_by(date) %>%
  mutate(pointwise.min = obs-pred.min,
         pointwise.max = obs-pred.max, NA) %>% 
  group_by(cat) %>%
  mutate(cumulative.min = cumsum(pointwise.min),
         cumulative.max = cumsum(pointwise.max))

pred.vars <- c("pred.min","pred.max","pointwise.min","pointwise.max","cumulative.min","cumulative.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot homesteads
#ts.means.m$value[ts.means.m$variable=="Observed" & ts.means.m$value >200] <- NA # for y axis control
homesteads.ts.plot <- TsPlot(ts.means.m,  main="Causal impact of land reform on homesteads")

ggsave(paste0(data.directory,"plots/homesteads-ts-plot.png"), homesteads.ts.plot, width=8.5, height=11)