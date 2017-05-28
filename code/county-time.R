#####################################
### County-level time-series analysis        ###
#####################################

#pre.period <- c(years[1], years[5])
#post.period <- c(years[6], years[10])

#impact <- CausalImpact(cbind(gini,county.x1[!colnames(county.x1) %in% c("id")]), pre.period, post.period)

set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1), time.points)
head(data)

pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)

summary(impact)

summary(impact, "report")