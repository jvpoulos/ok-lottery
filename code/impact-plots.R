# Plot time-series and estimate causal impacts

# Import test results

gini.test.pred <- read_csv("~/Dropbox/github/drnns-prediction/results/ok-pred/gini-test-pred.csv", col_names = FALSE)

gini.test.pred <- rbind(data.frame("X1"=rep(NA,7)), gini.test.pred)

names(gini.test.pred) <- c("gini.pred")

county.x.test <- cbind(county.x.test, gini.test.pred)

# Import training and validation fits

gini.train.pred <- read_csv("~/Dropbox/github/drnns-prediction/results/ok-pred/gini-train-pred.csv", col_names = FALSE)

names(gini.train.pred) <- c("gini.pred")

county.x.train <- cbind(county.x.train, gini.train.pred)

gini.val.pred <- read_csv("~/Dropbox/github/drnns-prediction/results/ok-pred/gini-val-pred.csv", col_names = FALSE)

gini.val.pred <- rbind(data.frame("X1"=rep(NA,1)), gini.val.pred)

names(gini.val.pred) <- c("gini.pred")

county.x.val <- cbind(county.x.val, gini.val.pred)

# Create time series data

time.vars <- c("id","year","gini","gini.pred")

ts.dat <- rbind(county.x.train[time.vars],county.x.val[time.vars],county.x.test[time.vars])

cat.ids <- data.frame("id"=as.numeric(interaction(c.county$county, c.county$state)),
                      "cat"=c.county$cat) # back out categories
cat.ids <- cat.ids[!duplicated(cat.ids$id),]

ts.dat <- merge(ts.dat, cat.ids, by="id", all.x=TRUE)

ts.dat$cat <- ifelse((ts.dat$cat==3 | ts.dat$cat==2), "Treated", "Control") # compare lottery counties vs. all other

## Plot time series

ts.means <- ts.dat %>%
  group_by(year,cat) %>%
  summarise(gini.obs = mean(gini,na.rm=TRUE),
            gini.pred = mean(gini.pred,na.rm=TRUE)) 

ts.means <- reshape(data.frame(ts.means), idvar = "year", timevar = "cat", direction = "wide") # reshape long

ts.means$gini.pointwise <- ts.means$gini.obs.Treated-ts.means$gini.pred.Treated # calc. pointwise impact

ts.means$gini.cumulative <- c(rep(0,4),cumsum(ts.means$gini.pointwise[5:8]))

ts.means$year <- as.yearmon(paste0(ts.means$year,"-12"), "%Y-%m",frac=1,tz="UTC") # convert year to date class

ts.means <- xts(ts.means,order.by = ts.means$year)

ts.means <- ts.means[,-1]

storage.mode(ts.means) <- "numeric" # convert to numeric

# Gini

gg.charts.PerformanceSummary(ts.means[,c("gini.obs.Control","gini.pred.Treated","gini.obs.Treated","gini.pointwise")], main="Causal effect of land reform on land inequality")

