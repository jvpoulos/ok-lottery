# Plot time-series and estimate causal impacts

setwd("~/Dropbox/github/drnns-prediction/results/ok-weights/sales")

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
sales.test <- cbind(sales.y.test, 
                         "sales.mean"= sales.test.mean, 
                         "sales.sd"= sales.test.sd) 
sales.train <- cbind(sales.y.train, 
                          "sales.mean"=sales.train.mean, 
                          "sales.sd"=sales.train.sd) 

# Create time series data

ts.dat <- rbind(sales.train,sales.test)

## Plot time series 

time.vars <- c("date","sales.Treated","sales.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise = sales.Treated-sales.mean,
         cumulative = cumsum(pointwise)) 

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date"))

# Adjust date for plot
ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- as.POSIXct(ts.means.m$date, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="sales.Treated" | ts.means.m$variable=="sales.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed","Predicted", "Pointwise","Cumulative")


# SDs

sds <- ts.dat  %>%
  mutate(pred.min = sales.mean - sales.sd,
         pred.max = sales.mean + sales.sd,
         pointwise.min = sales.Treated-pred.min,
         pointwise.max = sales.Treated-pred.max,
         cumulative.min = cumsum(pointwise.min),
         cumulative.max = cumsum(pointwise.max))

pred.vars <- c("sales.mean", "sales.sd", "pred.min", "pred.max", "pointwise.min", "pointwise.max", "cumulative.min", "cumulative.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot sales
sales.ts.plot <- TsPlot(ts.means.m)

ggsave(paste0(data.directory,"plots/sales-ts-plot.png"), sales.ts.plot, width=11, height=8.5)