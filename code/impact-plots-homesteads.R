# Plot time-series and estimate causal impacts

setwd("~/Dropbox/github/drnns-prediction/results/ok-weights/homesteads")

# Import test results

test.files <- list.files(pattern = "*test.csv")

homesteads.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="homesteads.pred"))
homesteads.test.mean <-rowMeans(homesteads.test.pred)
homesteads.test.sd <- matrixStats::rowSds(as.matrix(homesteads.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

homesteads.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="homesteads.pred"))
homesteads.train.mean <- rowMeans(as.matrix(homesteads.train.pred))
homesteads.train.sd <- matrixStats::rowSds(as.matrix(homesteads.train.pred))


# Bind to splits
homesteads.test <- cbind(homesteads.y.test, 
                         "homesteads.mean"= homesteads.test.mean, 
                         "homesteads.sd"= homesteads.test.sd) 
homesteads.train <- cbind(homesteads.y.train, 
                          "homesteads.mean"=homesteads.train.mean, 
                          "homesteads.sd"=homesteads.train.sd) 

# Create time series data

ts.dat <- rbind(homesteads.train,homesteads.test)

## Plot time series 

time.vars <- c("date","homesteads.Treated","homesteads.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise = homesteads.Treated-homesteads.mean,
         cumulative = cumsum(pointwise)) 

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date"))

# Adjust date for plot
ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- as.POSIXct(ts.means.m$date, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="homesteads.Treated" | ts.means.m$variable=="homesteads.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed","Predicted", "Pointwise","Cumulative")


# SDs

sds <- ts.dat  %>%
  mutate(pred.min = homesteads.mean - homesteads.sd,
         pred.max = homesteads.mean + homesteads.sd,
         pointwise.min = homesteads.Treated-pred.min,
         pointwise.max = homesteads.Treated-pred.max,
         cumulative.min = cumsum(pointwise.min),
         cumulative.max = cumsum(pointwise.max))

pred.vars <- c("homesteads.mean", "homesteads.sd", "pred.min", "pred.max", "pointwise.min", "pointwise.max", "cumulative.min", "cumulative.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot homesteads
homesteads.ts.plot <- TsPlot(ts.means.m)

ggsave(paste0(data.directory,"plots/homesteads-ts-plot.png"), homesteads.ts.plot, width=11, height=8.5)