#####################################
### Individual-level analysis (1910 Census Dvs)    ###
#####################################

slides <- TRUE

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

set.seed(42)
RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Est. balancing weights

colnames(census.covars) <- make.names(colnames(census.covars), unique=TRUE) # rm spaces

treatment.fit.census <- SuperLearner(Y=link.1900.1910.subs$draw,
                              X=data.frame(census.covars),
                              SL.library=SL.library.reg,
                              family="gaussian")
#Save pred model
saveRDS(treatment.fit.census, file = paste0(data.directory,"treatment-fit-census.rds"))

# summarize ensemble
print(treatment.fit.census)

# calculate propensity scores
p.scores.census <- predict(treatment.fit.census, type = "response")$pred[,1]

## Estimate dose-response curves

# farm (binary)

farm.fit <- SuperLearner(Y=link.1900.1910.subs$farm,
                         X=data.frame("draw"=link.1900.1910.subs$draw, "p.scores"=p.scores.census),
                         SL.library=SL.library.class,
                         family="binomial")

#Save pred model
saveRDS(farm.fit, file = paste0(data.directory,"farm-fit.rds"))

print(farm.fit)

draw.max <- 6500
farm.preds <- lapply(1:draw.max, function (j) predict(farm.fit, newdata=data.frame("p.scores"=p.scores.census,
                                                                                   "farm"=link.1900.1910.subs$farm,
                                                                                   "draw"=rep(j,times=length(link.1900.1910.subs$draw))), type="response", onlySL = FALSE))
saveRDS(farm.preds, file = paste0(data.directory,"farm-preds.rds"))

farm.curve <- sapply(farm.preds, function(x) mean(x$pred))
farm.curve.upper <- farm.curve + (qnorm(0.975)*sapply(farm.preds, function(x) sd(x$library.predict)))
farm.curve.lower <- farm.curve - (qnorm(0.975)*sapply(farm.preds, function(x) sd(x$library.predict)))

farm.curve.female <- sapply(farm.preds, function(x) mean(x$pred[which(link.1900.1910.subs$female==1)]))
farm.curve.female.upper <- farm.curve.female + (qnorm(0.975)*sapply(farm.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==1)])))
farm.curve.female.lower <- farm.curve.female - (qnorm(0.975)*sapply(farm.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==1)])))

farm.curve.male <- sapply(farm.preds, function(x) mean(x$pred[which(link.1900.1910.subs$female==0)]))
farm.curve.male.upper <- farm.curve.male + (qnorm(0.975)*sapply(farm.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==0)])))
farm.curve.male.lower <- farm.curve.male - (qnorm(0.975)*sapply(farm.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==0)])))

farm.curve.df <- data.frame("fit.male"=farm.curve.male,
                            "lower.male"=farm.curve.male.lower,
                            "upper.male"= farm.curve.male.upper,
                            "fit.female"=farm.curve.female,
                            "lower.female"=farm.curve.female.lower,
                            "upper.female"= farm.curve.female.upper,
                            "draw"=1:draw.max)

farm.plot <- ggplot(farm.curve.df, aes(x = as.numeric(draw))) +
  theme_bw() +
  geom_line(aes(y = fit.male, color="Male")) +
  geom_line(aes(y = fit.female, color="Female")) +
  labs(y="Probability of owning a farm in 1910",
       x="Draw number",
       color="Subgroup:") +
  geom_smooth(aes(y=fit.male, ymin = lower.male, ymax = upper.male, color="Male"), stat = "identity",alpha=0.1) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.3) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/farm-plot.png", farm.plot, scale = 1.25)

if(slides){
  farm.plot <- farm.plot + ggtitle(paste0("Conditional average dose-response function"))
  ggsave("plots/farm-plot-slides.png",farm.plot,scale=1.25)
}

# Mann-Whitney U test for difference

print(wilcox.test(x=farm.curve.male,y=farm.curve.female,alternative="two.sided", exact=FALSE, conf.int = TRUE))

# MTE
lags <-1000
farm.mte <-  sapply((1+lags):length(farm.curve), function(j) farm.curve[j] - farm.curve[j-lags])/lags 
farm.mte.upper <- farm.mte + (farm.curve.upper-farm.curve)[(1+lags):length(farm.curve)]
farm.mte.lower <- farm.mte - (farm.curve.upper-farm.curve)[(1+lags):length(farm.curve)]

farm.mte.female <-  sapply((1+lags):length(farm.curve.female), function(j) farm.curve.female[j] - farm.curve.female[j-lags])/lags 
farm.mte.female.upper <- farm.mte.female + sapply((1+lags):length(farm.curve), function(j) (farm.curve.female.upper-farm.curve.female)[j] - (farm.curve.female.upper-farm.curve.female)[j-lags])/lags 
farm.mte.female.lower <- farm.mte.female - sapply((1+lags):length(farm.curve), function(j) (farm.curve.female.upper-farm.curve.female)[j] - (farm.curve.female.upper-farm.curve.female)[j-lags])/lags 

farm.mte.male <-  sapply((1+lags):length(farm.curve.male), function(j) farm.curve.male[j] - farm.curve.male[j-lags])/lags 
farm.mte.male.upper <- farm.mte.male + sapply((1+lags):length(farm.curve), function(j) (farm.curve.male.upper-farm.curve.male)[j] - (farm.curve.male.upper-farm.curve.male)[j-lags])/lags 
farm.mte.male.lower <- farm.mte.male - sapply((1+lags):length(farm.curve), function(j) (farm.curve.male.upper-farm.curve.male)[j] - (farm.curve.male.upper-farm.curve.male)[j-lags])/lags

farm.mte.df <- data.frame("fit.male"=farm.mte.male,
                          "lower.male"=farm.mte.male.lower,
                          "upper.male"= farm.mte.male.upper,
                          "fit.female"=farm.mte.female,
                          "lower.female"=farm.mte.female.lower,
                          "upper.female"= farm.mte.female.upper,
                          "draw"=(1+lags):length(farm.curve))

farm.mte.plot <- ggplot(farm.mte.df, aes(x = (1+lags):length(farm.curve))) +
  theme_bw() +
  labs(y="Probability of owning a farm in 1910",
       x="Draw number",
       color="Subgroup:") +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  #geom_smooth(aes(y=fit.male, color="Male"), stat = "smooth",method = stats::loess, n = 100, span = 0.5, alpha=0.1) +
  #geom_smooth(aes(y=fit.female, color="Female"), stat = "smooth", method =stats::loess, n = 100, span = 0.5,alpha=0.3) +
  geom_smooth(aes(y=fit.male, ymin = lower.male, ymax = upper.male, color="Male"), stat = "identity",alpha=0.1) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.3) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/farm-mte-plot.png", farm.mte.plot, scale = 1.25)

if(slides){
  farm.mte.plot <- farm.mte.plot + ggtitle(paste0("Conditional marginal treatment effect function"))
  ggsave("plots/farm-mte-plot-slides.png",farm.mte.plot,scale=1.25)
}
# own (binary)

own.fit <- SuperLearner(Y=link.1900.1910.subs$own,
                         X=data.frame("draw"=link.1900.1910.subs$draw, "p.scores"=p.scores.census),
                         SL.library=SL.library.class,
                         family="binomial")

#Save pred model
saveRDS(own.fit, file = paste0(data.directory,"own-fit.rds"))

print(own.fit)

draw.max <- 6500
own.preds <- lapply(1:draw.max, function (j) predict(own.fit, newdata=data.frame("p.scores"=p.scores.census,
                                                                                   "own"=link.1900.1910.subs$own,
                                                                                   "draw"=rep(j,times=length(link.1900.1910.subs$draw))), type="response", onlySL = FALSE))
saveRDS(own.preds, file = paste0(data.directory,"own-preds.rds"))

own.curve <- sapply(own.preds, function(x) mean(x$pred))
own.curve.upper <- own.curve + (qnorm(0.975)*sapply(own.preds, function(x) sd(x$library.predict))) 
own.curve.lower <- own.curve - (qnorm(0.975)*sapply(own.preds, function(x) sd(x$library.predict)))

own.curve.female <- sapply(own.preds, function(x) mean(x$pred[which(link.1900.1910.subs$female==1)]))
own.curve.female.upper <- own.curve.female + (qnorm(0.975)*sapply(own.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==1)])))
own.curve.female.lower <- own.curve.female - (qnorm(0.975)*sapply(own.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==1)])))

own.curve.male <- sapply(own.preds, function(x) mean(x$pred[which(link.1900.1910.subs$female==0)]))
own.curve.male.upper <- own.curve.male + (qnorm(0.975)*sapply(own.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==0)])))
own.curve.male.lower <- own.curve.male - (qnorm(0.975)*sapply(own.preds, function(x) sd(x$library.predict[which(link.1900.1910.subs$female==0)])))

own.curve.df <- data.frame("fit.male"=own.curve.male,
                            "lower.male"=own.curve.male.lower,
                            "upper.male"= own.curve.male.upper,
                            "fit.female"=own.curve.female,
                            "lower.female"=own.curve.female.lower,
                            "upper.female"= own.curve.female.upper,
                            "draw"=1:draw.max)

own.plot <- ggplot(own.curve.df, aes(x = as.numeric(draw))) +
  theme_bw() +
  geom_line(aes(y = fit.male, color="Male")) +
  geom_line(aes(y = fit.female, color="Female")) +
  labs(y="Probability of owning a home in 1910",
       x="Draw number",
       color="Subgroup:") +
  geom_smooth(aes(y=fit.male, ymin = lower.male, ymax = upper.male, color="Male"), stat = "identity",alpha=0.1) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.3) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/own-plot.png", own.plot, scale = 1.25)

if(slides){
  own.plot <- own.plot + ggtitle(paste0("Conditional average dose-response function"))
  ggsave("plots/own-plot-slides.png",own.plot,scale=1.25)
}

# Mann-Whitney U test for difference

print(wilcox.test(x=own.curve.male,y=own.curve.female,alternative="two.sided", exact=FALSE, conf.int = TRUE))

# MTE
lags <-1000
own.mte <-  sapply((1+lags):length(own.curve), function(j) own.curve[j] - own.curve[j-lags])/lags 
own.mte.upper <- own.mte + (own.curve.upper-own.curve)[(1+lags):length(own.curve)]
own.mte.lower <- own.mte - (own.curve.upper-own.curve)[(1+lags):length(own.curve)]

own.mte.female <-  sapply((1+lags):length(own.curve.female), function(j) own.curve.female[j] - own.curve.female[j-lags])/lags 
own.mte.female.upper <- own.mte.female + sapply((1+lags):length(own.curve), function(j) (own.curve.female.upper-own.curve.female)[j] - (own.curve.female.upper-own.curve.female)[j-lags])/lags 
own.mte.female.lower <- own.mte.female - sapply((1+lags):length(own.curve), function(j) (own.curve.female.upper-own.curve.female)[j] - (own.curve.female.upper-own.curve.female)[j-lags])/lags 

own.mte.male <-  sapply((1+lags):length(own.curve.male), function(j) own.curve.male[j] - own.curve.male[j-lags])/lags 
own.mte.male.upper <- own.mte.male + sapply((1+lags):length(own.curve), function(j) (own.curve.male.upper-own.curve.male)[j] - (own.curve.male.upper-own.curve.male)[j-lags])/lags 
own.mte.male.lower <- own.mte.male - sapply((1+lags):length(own.curve), function(j) (own.curve.male.upper-own.curve.male)[j] - (own.curve.male.upper-own.curve.male)[j-lags])/lags

own.mte.df <- data.frame("fit.male"=own.mte.male,
                          "lower.male"=own.mte.male.lower,
                          "upper.male"= own.mte.male.upper,
                          "fit.female"=own.mte.female,
                          "lower.female"=own.mte.female.lower,
                          "upper.female"= own.mte.female.upper,
                          "draw"=(1+lags):length(own.curve))

own.mte.plot <- ggplot(own.mte.df, aes(x = (1+lags):length(own.curve))) +
  theme_bw() +
  labs(y="Probability of owning a home in 1910",
       x="Draw number",
       color="Subgroup:") +
  geom_hline(yintercept=0) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  #geom_smooth(aes(y=fit.male, color="Male"), stat = "smooth",method = stats::loess, n = 100, span = 0.5, alpha=0.1) +
  #geom_smooth(aes(y=fit.female, color="Female"), stat = "smooth", method =stats::loess, n = 100, span = 0.5,alpha=0.3) +
  geom_smooth(aes(y=fit.male, ymin = lower.male, ymax = upper.male, color="Male"), stat = "identity",alpha=0.1) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.3) +
  theme(legend.position="bottom") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave("plots/own-mte-plot.png", own.mte.plot, scale = 1.25)

if(slides){
  own.mte.plot <- own.mte.plot + ggtitle(paste0("Conditional marginal treatment effect function"))
  ggsave("plots/own-mte-plot-slides.png",own.mte.plot,scale=1.25)
}