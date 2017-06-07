# Plot time-series and estimate causal impacts

# Import test results

gini.test.pred <- read_csv("~/Dropbox/github/drnns-prediction/results/ok-pred/gini-test-pred.csv", col_names = FALSE)

gini.test.pred <- rbind(data.frame("X1"=rep(NA,7)), gini.test.pred)

names(gini.test.pred) <- c("gini.pred")

county.x.test <- cbind(county.x.test, gini.test.pred)

# Create time series data

time.vars <- c("id","year","gini","gini.pred")
county.x.train$gini.pred <- NA
county.x.val$gini.pred <- NA

ts.dat <- rbind(county.x.train[time.vars],county.x.val[time.vars],county.x.test[time.vars])

cat.ids <- data.frame("id"=as.numeric(interaction(c.county$county, c.county$state)),
                      "cat"=c.county$cat) # back out categories
cat.ids <- cat.ids[!duplicated(cat.ids$id),]

ts.dat <- merge(ts.dat, cat.ids, by="id", all.x=TRUE)

# Plot time series

ts.means <- ts.dat %>%
  group_by(year,cat) %>%
  summarise(gini.obs = mean(gini,na.rm=TRUE),
            gini.pred = mean(gini.pred,na.rm=TRUE))

ts.means[is.na(ts.means$gini.pred),]$gini.pred <- ts.means[is.na(ts.means$gini.pred),]$gini.obs # pre-treatment pred is observed

