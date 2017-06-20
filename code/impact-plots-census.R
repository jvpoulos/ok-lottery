# Plot time-series and estimate causal impacts

## gini data
setwd("~/Dropbox/github/drnns-prediction/results/ok-weights/gini")

# Import test results

test.files <- list.files(pattern = "*test.csv")

gini.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                header=FALSE,
                                col.names="gini.pred"))
gini.test.mean <-rowMeans(gini.test.pred)
gini.test.sd <- matrixStats::rowSds(as.matrix(gini.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

gini.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                        header=FALSE,
                                        col.names="gini.pred"))
gini.train.mean <- rowMeans(as.matrix(gini.train.pred))
gini.train.sd <- matrixStats::rowSds(as.matrix(gini.train.pred))


# Bind to splits
gini.test <- cbind(gini.y.test, 
                         "gini.mean"= gini.test.mean, 
                         "gini.sd"= gini.test.sd) 
gini.train <- cbind(gini.y.train, 
                          "gini.mean"=gini.train.mean, 
                          "gini.sd"=gini.train.sd) 

## tenancy data 
setwd("~/Dropbox/github/drnns-prediction/results/ok-weights/tenancy")

# Import test results

test.files <- list.files(pattern = "*test.csv")

tenancy.test.pred <- do.call(cbind,lapply(test.files,read.csv, 
                                             header=FALSE,
                                             col.names="tenancy.pred"))
tenancy.test.mean <-rowMeans(tenancy.test.pred)
tenancy.test.sd <- matrixStats::rowSds(as.matrix(tenancy.test.pred))

# Import training fit

train.files <- list.files(pattern = "*train.csv")

tenancy.train.pred <- do.call(cbind,lapply(train.files,read.csv, 
                                              header=FALSE,
                                              col.names="tenancy.pred"))
tenancy.train.mean <- rowMeans(as.matrix(tenancy.train.pred))
tenancy.train.sd <- matrixStats::rowSds(as.matrix(tenancy.train.pred))


# Bind to splits
tenancy.test <- cbind(tenancy.y.test, 
                         "tenancy.mean"= tenancy.test.mean, 
                         "tenancy.sd"= tenancy.test.sd) 
tenancy.train <- cbind(tenancy.y.train, 
                          "tenancy.mean"=tenancy.train.mean, 
                          "tenancy.sd"=tenancy.train.sd) 

## Create time series data
setwd(code.directory)

ts.dat <- merge(rbind(gini.train,gini.test), rbind(tenancy.train,tenancy.test), by="date")

## Plot time series 

time.vars <- c("date","G.Treated","gini.mean","tenancy.Treated", "tenancy.mean")

ts.means <- ts.dat[time.vars]  %>%
  mutate(pointwise.gini = G.Treated-gini.mean,
         cumulative.gini = cumsum(pointwise.gini),
         pointwise.tenancy = tenancy.Treated-tenancy.mean,
         cumulative.tenancy = cumsum(pointwise.tenancy)) 

ts.means.m <- melt(as.data.frame(ts.means), id.var=c("date"))

# Adjust date for plot
ts.means.m$date <- as.yearmon(ts.means.m$date, "%Y-%m",tz="UTC") # convert date to date class

ts.means.m$date <- as.POSIXct(ts.means.m$date, tz="UTC")
                         
# Labels

ts.means.m$series <- NA
ts.means.m$series[ts.means.m$variable=="G.Treated" | ts.means.m$variable=="gini.mean" | ts.means.m$variable=="tenancy.Treated" | ts.means.m$variable=="tenancy.mean"] <- "Time-series"
ts.means.m$series[ts.means.m$variable=="pointwise.gini" | ts.means.m$variable=="pointwise.tenancy"] <- "Pointwise impact"
ts.means.m$series[ts.means.m$variable=="cumulative.gini" | ts.means.m$variable=="cumulative.tenancy"] <- "Cumulative impact"

ts.means.m$series<- factor(ts.means.m$series, levels=c("Time-series","Pointwise impact", "Cumulative impact")) # reverse order

levels(ts.means.m$variable) <- c("Observed gini","Predicted gini", "Observed tenancy", "Predicted tenancy",
                                 "Pointwise gini", "Cumulative gini", 
                                 "Pointwise tenancy", "Cumulative tenancy")

# SDs

sds <- ts.dat  %>%
  mutate(pred.gini.min = gini.mean - gini.sd,
         pred.gini.max = gini.mean + gini.sd,
         pointwise.gini.min = G.Treated-pred.gini.min,
         pointwise.gini.max = G.Treated-pred.gini.max,
         cumulative.gini.min = cumsum(pointwise.gini.min),
         cumulative.gini.max = cumsum(pointwise.gini.max),
         pred.tenancy.min = tenancy.mean - tenancy.sd,
         pred.tenancy.max = tenancy.mean + tenancy.sd,
         pointwise.tenancy.min = tenancy.Treated-pred.tenancy.min,
         pointwise.tenancy.max = tenancy.Treated-pred.tenancy.max,
         cumulative.tenancy.min = cumsum(pointwise.tenancy.min),
         cumulative.tenancy.max = cumsum(pointwise.tenancy.max))

pred.vars <- c("gini.mean", "gini.sd", "pred.gini.min", "pred.gini.max", "pointwise.gini.min", "pointwise.gini.max", "cumulative.gini.min", "cumulative.gini.max",
               "tenancy.mean", "tenancy.sd", "pred.tenancy.min", "pred.tenancy.max", "pointwise.tenancy.min", "pointwise.tenancy.max", "cumulative.tenancy.min", "cumulative.tenancy.max")
ts.means.m <- cbind(ts.means.m, sds[pred.vars])
ts.means.m[pred.vars][ts.means.m$variable=="Observed",] <- NA

# Plot
ts.plot <- TsPlotCensus(ts.means.m)

ggsave(paste0(data.directory,"plots/census-ts-plot.png"), ts.plot, width=11, height=8.5)