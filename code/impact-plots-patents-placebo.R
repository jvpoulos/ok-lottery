# Plot time-series and estimate causal impacts

## Sales data
setwd("~/Dropbox/github/drnns-prediction/results/ok-weights/sales-placebo")

# Import test results

test.files <- list.files(pattern = "*test.csv")

sales.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="sales.pred"))
sales.test.mean <-rowMeans(sales.test.pred)
sales.test.sd <- matrixStats::rowSds(as.matrix(sales.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

sales.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="sales.pred"))
sales.train.mean <- rowMeans(as.matrix(sales.train.pred))
sales.train.sd <- matrixStats::rowSds(as.matrix(sales.train.pred))


# Bind to splits
sales.test <- cbind(sales.y.test.p, 
                         "sales.mean"= sales.test.mean, 
                         "sales.sd"= sales.test.sd) 
sales.train <- cbind(sales.y.train.p, 
                          "sales.mean"=sales.train.mean, 
                          "sales.sd"=sales.train.sd) 

## Homesteads data 
setwd("~/Dropbox/github/drnns-prediction/results/ok-weights/homesteads-placebo")

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
homesteads.test <- cbind(homesteads.y.test.p, 
                         "homesteads.mean"= homesteads.test.mean, 
                         "homesteads.sd"= homesteads.test.sd) 
homesteads.train <- cbind(homesteads.y.train.p, 
                          "homesteads.mean"=homesteads.train.mean, 
                          "homesteads.sd"=homesteads.train.sd) 

## Create time series data
setwd(code.directory)

ts.dat <- merge(rbind(sales.train,sales.test), rbind(homesteads.train,homesteads.test), by="date")

## Plot time series 

time.vars <- c("date","sales.Treated","sales.mean","homesteads.Treated", "homesteads.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.sales = sales.Treated-sales.mean,
         cumulative.sales = cumsum(pointwise.sales),
         pointwise.homesteads = homesteads.Treated-homesteads.mean,
         cumulative.homesteads = cumsum(pointwise.homesteads)) 

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date"))

# Adjust date for plot
ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- as.POSIXct(ts.means.m$date, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="sales.Treated" | ts.means.m$variable=="sales.mean" | ts.means.m$variable=="homesteads.Treated" | ts.means.m$variable=="homesteads.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.sales" | ts.means.m$variable=="pointwise.homesteads"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.sales" | ts.means.m$variable=="cumulative.homesteads"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed sales","Predicted sales", "Observed homesteads", "Predicted homesteads",
                                 "Pointwise sales", "Cumulative sales", 
                                 "Pointwise homesteads", "Cumulative homesteads")

# SDs

sds <- ts.dat  %>%
  mutate(pred.sales.min = sales.mean - sales.sd,
         pred.sales.max = sales.mean + sales.sd,
         pointwise.sales.min = sales.Treated-pred.sales.min,
         pointwise.sales.max = sales.Treated-pred.sales.max,
         cumulative.sales.min = cumsum(pointwise.sales.min),
         cumulative.sales.max = cumsum(pointwise.sales.max),
         pred.homesteads.min = homesteads.mean - homesteads.sd,
         pred.homesteads.max = homesteads.mean + homesteads.sd,
         pointwise.homesteads.min = homesteads.Treated-pred.homesteads.min,
         pointwise.homesteads.max = homesteads.Treated-pred.homesteads.max,
         cumulative.homesteads.min = cumsum(pointwise.homesteads.min),
         cumulative.homesteads.max = cumsum(pointwise.homesteads.max))

pred.vars <- c("sales.mean", "sales.sd", "pred.sales.min", "pred.sales.max", "pointwise.sales.min", "pointwise.sales.max", "cumulative.sales.min", "cumulative.sales.max",
               "homesteads.mean", "homesteads.sd", "pred.homesteads.min", "pred.homesteads.max", "pointwise.homesteads.min", "pointwise.homesteads.max", "cumulative.homesteads.min", "cumulative.homesteads.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot
ts.plot <- TsPlotPatents(ts.means.m) + geom_vline(xintercept=c(as.numeric(as.POSIXct("1898-07-31 06:00:00",tz="UTC"))), linetype=1) 

data.directory <- "~/Dropbox/github/ok-lottery/data/"
ggsave(paste0(data.directory,"plots/patents-ts-plot-placebo.png"), ts.plot, width=11, height=8.5) 

# Calculate avg. pointwise impact: July 1898-June 1901

# sales
mean(ts.means.m$value[ts.means.m$variable=="Pointwise sales" & (ts.means.m$date>="1898-07-31 19:00:00" & ts.means.m$date<="1901-06-30 19:00:00")])

mean(sds$sales.sd[(sds$date>="Jul 1898" & sds$date<="Jun 1901")])

# homesteads
mean(ts.means.m$value[ts.means.m$variable=="Pointwise homesteads" & (ts.means.m$date>="1898-07-31 19:00:00" & ts.means.m$date<="1901-06-30 19:00:00")])

mean(sds$homesteads.sd[(sds$date>="Jul 1898" & sds$date<="Jun 1901")])

# Calculate cumulative impact: July 1898-June 1901

# sales
abs(ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1898-07-31 19:00:00"] -ts.means.m$value[ts.means.m$variable=="Cumulative sales" & ts.means.m$date=="1901-06-30 19:00:00"])

abs((abs(sds$cumulative.sales.min[(sds$date=="Jul 1898")] -sds$cumulative.sales.max[(sds$date=="Jul 1898")])/2) -(abs(sds$cumulative.sales.min[(sds$date=="Jun 1901")] -sds$cumulative.sales.max[(sds$date=="Jun 1901")])/2))

# homesteads
abs(ts.means.m$value[ts.means.m$variable=="Cumulative homesteads" & ts.means.m$date=="1898-07-31 19:00:00"] -ts.means.m$value[ts.means.m$variable=="Cumulative homesteads" & ts.means.m$date=="1901-06-30 19:00:00"])

abs((abs(sds$cumulative.homesteads.min[(sds$date=="Jul 1898")] -sds$cumulative.homesteads.max[(sds$date=="Jul 1898")])/2) -(abs(sds$cumulative.homesteads.min[(sds$date=="Jun 1901")] -sds$cumulative.homesteads.max[(sds$date=="Jun 1901")])/2))