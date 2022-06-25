#####################################
### Individual-level analysis    ###
#####################################

set.seed(42)
RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

slides <- TRUE

# Est. balancing weights

colnames(state.dummies) <- make.names(colnames(state.dummies), unique=TRUE) # rm spaces
colnames(loc.dummies) <- make.names(colnames(loc.dummies), unique=TRUE)
balance.vars <- make.names(balance.vars, unique=TRUE)

treatment.fit <- SuperLearner(Y=link.patents$draw,
                              X=cbind(link.patents,state.dummies,loc.dummies)[balance.vars],
                              SL.library=SL.library.reg,
                              family="gaussian")
#Save pred model
saveRDS(treatment.fit, file = paste0(data.directory,"treatment-fit.rds"))

# summarize ensemble
print(treatment.fit)

# calculate propensity scores
p.scores <- predict(treatment.fit, type = "response")$pred[,1]

## Estimate dose-response curves

# Sale (binary)

sale.fit <- SuperLearner(Y=link.patents$sale,
                            X=data.frame("draw"=link.patents$draw, "p.scores"=p.scores),
                            SL.library=SL.library.class[-grep("SL.nnet",SL.library.class)],
                            family="binomial")

#Save pred model
saveRDS(sale.fit, file = paste0(data.directory,"sale-fit.rds"))

print(sale.fit)

draw.max <- 6500
sale.preds <- lapply(1:draw.max, function (j) predict(sale.fit, newdata=data.frame("p.scores"=p.scores,
                                                                                "sale"=link.patents$sale,
                                                                                "draw"=rep(j,times=length(link.patents$draw))), type="response", onlySL = FALSE))
saveRDS(sale.preds, file = paste0(data.directory,"sale-preds.rds"))

sale.curve <- sapply(sale.preds, function(x) mean(x$pred))
sale.curve.upper <- sale.curve + (qnorm(0.975)*sapply(sale.preds, function(x) sd(x$library.predict)))
sale.curve.lower <- sale.curve - (qnorm(0.975)*sapply(sale.preds, function(x) sd(x$library.predict)))

sale.curve.female <- sapply(sale.preds, function(x) mean(x$pred[which(link.patents$female==1)]))
sale.curve.female.upper <- sale.curve.female + (qnorm(0.975)*sapply(sale.preds, function(x) sd(x$library.predict[which(link.patents$female==1)])))
sale.curve.female.lower <- sale.curve.female - (qnorm(0.975)*sapply(sale.preds, function(x) sd(x$library.predict[which(link.patents$female==1)])))

sale.curve.male <- sapply(sale.preds, function(x) mean(x$pred[which(link.patents$female==0)]))
sale.curve.male.upper <- sale.curve.male + (qnorm(0.975)*sapply(sale.preds, function(x) sd(x$library.predict[which(link.patents$female==0)])))
sale.curve.male.lower <- sale.curve.male - (qnorm(0.975)*sapply(sale.preds, function(x) sd(x$library.predict[which(link.patents$female==0)])))

sale.curve.df <- data.frame("fit.male"=sale.curve.male,
                                    "lower.male"=sale.curve.male.lower,
                                    "upper.male"= sale.curve.male.upper,
                                    "fit.female"=sale.curve.female,
                                    "lower.female"=sale.curve.female.lower,
                                    "upper.female"= sale.curve.female.upper,
                                    "draw"=1:draw.max)

sale.plot <- ggplot(sale.curve.df, aes(x = as.numeric(draw))) +
  theme_bw() +
  geom_line(aes(y = fit.male, color="Male")) +
  geom_line(aes(y = fit.female, color="Female")) +
  labs(y="Probability of land patent purchase",
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

ggsave("plots/sale-plot.png", sale.plot, scale = 1.25)

if(slides){
  sale.plot <- sale.plot + ggtitle(paste0("Conditional average dose-response function"))
  ggsave("plots/sale-plot-slides.png",sale.plot,scale=1.25)
}

# Mann-Whitney U test for difference

print(wilcox.test(x=sale.curve.male,y=sale.curve.female,alternative="two.sided", exact=FALSE, conf.int = TRUE))
 
# MTE
lags <-1000
sale.mte <-  sapply((1+lags):length(sale.curve), function(j) sale.curve[j] - sale.curve[j-lags])/lags 
sale.mte.upper <- sale.mte + (sale.curve.upper-sale.curve)[(1+lags):length(sale.curve)]
sale.mte.lower <- sale.mte - (sale.curve.upper-sale.curve)[(1+lags):length(sale.curve)]

sale.mte.female <-  sapply((1+lags):length(sale.curve.female), function(j) sale.curve.female[j] - sale.curve.female[j-lags])/lags 
sale.mte.female.upper <- sale.mte.female + sapply((1+lags):length(sale.curve), function(j) (sale.curve.female.upper-sale.curve.female)[j] - (sale.curve.female.upper-sale.curve.female)[j-lags])/lags 
sale.mte.female.lower <- sale.mte.female - sapply((1+lags):length(sale.curve), function(j) (sale.curve.female.upper-sale.curve.female)[j] - (sale.curve.female.upper-sale.curve.female)[j-lags])/lags 

sale.mte.male <-  sapply((1+lags):length(sale.curve.male), function(j) sale.curve.male[j] - sale.curve.male[j-lags])/lags 
sale.mte.male.upper <- sale.mte.male + sapply((1+lags):length(sale.curve), function(j) (sale.curve.male.upper-sale.curve.male)[j] - (sale.curve.male.upper-sale.curve.male)[j-lags])/lags 
sale.mte.male.lower <- sale.mte.male - sapply((1+lags):length(sale.curve), function(j) (sale.curve.male.upper-sale.curve.male)[j] - (sale.curve.male.upper-sale.curve.male)[j-lags])/lags

sale.mte.df <- data.frame("fit.male"=sale.mte.male,
                          "lower.male"=sale.mte.male.lower,
                          "upper.male"= sale.mte.male.upper,
                          "fit.female"=sale.mte.female,
                          "lower.female"=sale.mte.female.lower,
                          "upper.female"= sale.mte.female.upper,
                          "draw"=(1+lags):length(sale.curve))

sale.mte.plot <- ggplot(sale.mte.df, aes(x = (1+lags):length(sale.curve))) +
  theme_bw() +
  labs(y="Probability of land patent purchase",
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

ggsave("plots/sale-mte-plot.png", sale.mte.plot, scale = 1.25)

if(slides){
  sale.mte.plot <- sale.mte.plot + ggtitle(paste0("Conditional marginal treatment effect function"))
  ggsave("plots/sale-mte-plot-slides.png",sale.mte.plot,scale=1.25)
}

# Homestead (binary)

homestead.fit <- SuperLearner(Y=link.patents$homestead,
                         X=data.frame("draw"=link.patents$draw, "p.scores"=p.scores),
                         SL.library=SL.library.class[-grep("SL.nnet",SL.library.class)],
                         family="binomial")

#Save pred model
saveRDS(homestead.fit, file = paste0(data.directory,"homestead-fit.rds"))

print(homestead.fit)

homestead.preds <- lapply(1:draw.max, function (j) predict(homestead.fit, newdata=data.frame("p.scores"=p.scores, 
                                                                                         "homestead"=link.patents$homestead,
                                                                                         "draw"=rep(j,times=length(link.patents$draw))), type="response", onlySL = FALSE))
saveRDS(homestead.preds, file = paste0(data.directory,"homestead-preds.rds"))

homestead.curve <- sapply(homestead.preds, function(x) mean(x))
homestead.curve.upper <- homestead.curve + (qnorm(0.975)*sapply(homestead.preds, function(x) sd(x$library.predict)))
homestead.curve.lower <- homestead.curve - (qnorm(0.975)*sapply(homestead.preds, function(x) sd(x$library.predict)))

homestead.curve.female <- sapply(homestead.preds, function(x) mean(x$pred[which(link.patents$female==1)]))
homestead.curve.female.upper <- homestead.curve.female + (qnorm(0.975)*sapply(homestead.preds, function(x) sd(x$library.predict[which(link.patents$female==1)])))
homestead.curve.female.lower <- homestead.curve.female - (qnorm(0.975)*sapply(homestead.preds, function(x) sd(x$library.predict[which(link.patents$female==1)])))

homestead.curve.male <- sapply(homestead.preds, function(x) mean(x$pred[which(link.patents$female==0)]))
homestead.curve.male.upper <- homestead.curve.male + (qnorm(0.975)*sapply(homestead.preds, function(x) sd(x$library.predict[which(link.patents$female==0)])))
homestead.curve.male.lower <- homestead.curve.male - (qnorm(0.975)*sapply(homestead.preds, function(x) sd(x$library.predict[which(link.patents$female==0)])))

homestead.curve.df <- data.frame("fit.male"=homestead.curve.male,
                            "lower.male"=homestead.curve.male.lower,
                            "upper.male"= homestead.curve.male.upper,
                            "fit.female"=homestead.curve.female,
                            "lower.female"=homestead.curve.female.lower,
                            "upper.female"= homestead.curve.female.upper,
                            "draw"=1:draw.max)

homestead.plot <- ggplot(homestead.curve.df, aes(x = as.numeric(1:draw.max))) +
  theme_bw() +
  geom_line(aes(y = fit.male, color="Male")) +
  geom_line(aes(y = fit.female, color="Female")) +
  labs(y="Probability of homestead patent",
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

ggsave("plots/homestead-plot.png", homestead.plot, scale = 1.25)

if(slides){
  homestead.plot <- homestead.plot + ggtitle(paste0("Conditional average dose-response function"))
  ggsave("plots/homestead-plot-slides.png",homestead.plot,scale=1.25)
}

# Mann-Whitney U test for difference

print(wilcox.test(x=homestead.curve.male,y=homestead.curve.female,alternative="two.sided", exact=FALSE, conf.int = TRUE))

# MTE
lags <-1000
homestead.mte <-  sapply((1+lags):length(homestead.curve), function(j) homestead.curve[j] - homestead.curve[j-lags])/lags 
homestead.mte.upper <- homestead.mte + (homestead.curve.upper-homestead.curve)[(1+lags):length(homestead.curve)]
homestead.mte.lower <- homestead.mte - (homestead.curve.upper-homestead.curve)[(1+lags):length(homestead.curve)]

homestead.mte.female <-  sapply((1+lags):length(homestead.curve.female), function(j) homestead.curve.female[j] - homestead.curve.female[j-lags])/lags 
homestead.mte.female.upper <- homestead.mte.female + sapply((1+lags):length(homestead.curve), function(j) (homestead.curve.female.upper-homestead.curve.female)[j] - (homestead.curve.female.upper-homestead.curve.female)[j-lags])/lags 
homestead.mte.female.lower <- homestead.mte.female - sapply((1+lags):length(homestead.curve), function(j) (homestead.curve.female.upper-homestead.curve.female)[j] - (homestead.curve.female.upper-homestead.curve.female)[j-lags])/lags 

homestead.mte.male <-  sapply((1+lags):length(homestead.curve.male), function(j) homestead.curve.male[j] - homestead.curve.male[j-lags])/lags 
homestead.mte.male.upper <- homestead.mte.male + sapply((1+lags):length(homestead.curve), function(j) (homestead.curve.male.upper-homestead.curve.male)[j] - (homestead.curve.male.upper-homestead.curve.male)[j-lags])/lags 
homestead.mte.male.lower <- homestead.mte.male - sapply((1+lags):length(homestead.curve), function(j) (homestead.curve.male.upper-homestead.curve.male)[j] - (homestead.curve.male.upper-homestead.curve.male)[j-lags])/lags

homestead.mte.df <- data.frame("fit.male"=homestead.mte.male,
                          "lower.male"=homestead.mte.male.lower,
                          "upper.male"= homestead.mte.male.upper,
                          "fit.female"=homestead.mte.female,
                          "lower.female"=homestead.mte.female.lower,
                          "upper.female"= homestead.mte.female.upper,
                          "draw"=(1+lags):length(homestead.curve))

homestead.mte.plot <- ggplot(homestead.mte.df, aes(x = (1+lags):length(homestead.curve))) +
  theme_bw() +
  labs(y="Probability of homestead patent",
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

ggsave("plots/homestead-mte-plot.png", homestead.mte.plot, scale = 1.25)

if(slides){
  homestead.mte.plot <- homestead.mte.plot + ggtitle(paste0("Conditional marginal treatment effect function"))
  ggsave("plots/homestead-mte-plot-slides.png",homestead.mte.plot,scale=1.25)
}