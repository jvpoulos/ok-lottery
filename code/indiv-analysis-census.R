#####################################
### Individual-level analysis (1910 Census Dvs)    ###
#####################################

slides <- TRUE

# Est. balancing weights

quintile.fit <- cv.glmnet(x=as.matrix(census.covars), y=link.1900.1910.subs$quintile, family = "multinomial") # ungrouped lasso
plot(quintile.fit)

# calculate propensity scores
p.scores <- predict(quintile.fit, newx=as.matrix(census.covars), s = "lambda.min", type = "response")[,,1]

print(sum(rowSums(p.scores))==length(link.1900.1910.subs$quintile)) # ensure probs. sum to 1

## Estimate dose-response curves

# farm (binary)

lm.farm <- glm(farm~ I(quintile) + p.scores  + p.scores**2 + I(quintile)*p.scores,
               family="binomial",
               data=data.frame("p.scores"=p.scores,
                               "farm"=link.1900.1910.subs$farm,
                               "quintile"=link.1900.1910.subs$quintile))

lm.farm.preds <- lapply(1:length(unique(link.1900.1910.subs$quintile)), function (j) predict(lm.farm, newdata=data.frame("p.scores"=p.scores,
                                                                                                                  "farm"=link.1900.1910.subs$farm,
                                                                                                                  "quintile"=factor(rep(levels(link.1900.1910.subs$quintile)[j],times=length(link.1900.1910.subs$quintile)))), type="response", se.fit = TRUE))

lm.farm.curve <- sapply(lm.farm.preds, function(x) mean(x$fit))
lm.farm.curve.upper <- lm.farm.curve + (qnorm(0.975)*sapply(lm.farm.preds, function(x) mean(x$se.fit)))
lm.farm.curve.lower <- lm.farm.curve - (qnorm(0.975)*sapply(lm.farm.preds, function(x) mean(x$se.fit)))

lm.farm.curve.female <- sapply(lm.farm.preds, function(x) mean(x$fit[which(link.1900.1910.subs$female==1)]))
lm.farm.curve.female.upper <- lm.farm.curve.female + (qnorm(0.975)*sapply(lm.farm.preds, function(x) mean(x$se.fit[which(link.1900.1910.subs$female==1)])))
lm.farm.curve.female.lower <- lm.farm.curve.female - (qnorm(0.975)*sapply(lm.farm.preds, function(x) mean(x$se.fit[which(link.1900.1910.subs$female==1)])))

lm.farm.curve.df <- data.frame("fit"=lm.farm.curve,
                               "lower"=lm.farm.curve.lower,
                               "upper"= lm.farm.curve.upper,
                               "fit.female"=lm.farm.curve.female,
                               "lower.female"=lm.farm.curve.female.lower,
                               "upper.female"= lm.farm.curve.female.upper,
                               "obs.adr"=MeanDR(link.1900.1910.subs$farm,link.1900.1910.subs$quintile),
                               "obs.cadr"=MeanDR(link.1900.1910.subs$farm,link.1900.1910.subs$quintile,x=link.1900.1910.subs$female),
                               "quintile"=sort(unique(link.1900.1910.subs$quintile)))

farm.plot <- ggplot(lm.farm.curve.df, aes(x = as.numeric(quintile))) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
  #geom_line(aes(y = fit.female, color="Female")) +
  geom_line(aes(y = obs.adr, color="All"),linetype="dashed") +
 # geom_line(aes(y = obs.cadr, color="Female"),linetype="dashed") +
  labs(y="Probability of owning a farm in 1910",
       x="Draw number decile (%)",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.3) +
 # geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_x_continuous(breaks=seq(1,10,1),labels=levels(sort(unique(link.1900.1910.subs$quintile)))) +
  theme(legend.position="top") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave(paste0(data.directory,"plots/farm-plot.png"), farm.plot, scale = 1.25)

if(slides){
  farm.plot <- farm.plot + ggtitle(paste0("Estimated average dose-response"))
  ggsave(paste0(data.directory,"plots/farm-plot-slides.png"),farm.plot,scale=1.25)
}

# own (binary)

lm.own <- glm(own~ I(quintile) + p.scores  + p.scores**2 + I(quintile)*p.scores,
                    family="binomial",
                    data=data.frame("p.scores"=p.scores,
                                    "own"=link.1900.1910.subs$own,
                                    "quintile"=link.1900.1910.subs$quintile))

lm.own.preds <- lapply(1:length(unique(link.1900.1910.subs$quintile)), function (j) predict(lm.own, newdata=data.frame("p.scores"=p.scores,
                                                                                                                            "own"=link.1900.1910.subs$own,
                                                                                                                            "quintile"=factor(rep(levels(link.1900.1910.subs$quintile)[j],times=length(link.1900.1910.subs$quintile)))), type="response", se.fit = TRUE))

lm.own.curve <- sapply(lm.own.preds, function(x) mean(x$fit))
lm.own.curve.upper <- lm.own.curve + (qnorm(0.975)*sapply(lm.own.preds, function(x) mean(x$se.fit)))
lm.own.curve.lower <- lm.own.curve - (qnorm(0.975)*sapply(lm.own.preds, function(x) mean(x$se.fit)))

lm.own.curve.female <- sapply(lm.own.preds, function(x) mean(x$fit[which(link.1900.1910.subs$female==1)]))
lm.own.curve.female.upper <- lm.own.curve.female + (qnorm(0.975)*sapply(lm.own.preds, function(x) mean(x$se.fit[which(link.1900.1910.subs$female==1)])))
lm.own.curve.female.lower <- lm.own.curve.female - (qnorm(0.975)*sapply(lm.own.preds, function(x) mean(x$se.fit[which(link.1900.1910.subs$female==1)])))

lm.own.curve.df <- data.frame("fit"=lm.own.curve,
                                    "lower"=lm.own.curve.lower,
                                    "upper"= lm.own.curve.upper,
                                    "fit.female"=lm.own.curve.female,
                                    "lower.female"=lm.own.curve.female.lower,
                                    "upper.female"= lm.own.curve.female.upper,
                                    "obs.adr"=MeanDR(link.1900.1910.subs$own,link.1900.1910.subs$quintile),
                                    "obs.cadr"=MeanDR(link.1900.1910.subs$own,link.1900.1910.subs$quintile,x=link.1900.1910.subs$female),
                                    "quintile"=sort(unique(link.1900.1910.subs$quintile)))

own.plot <- ggplot(lm.own.curve.df, aes(x = as.numeric(quintile))) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
#  geom_line(aes(y = fit.female, color="Female")) +
  geom_line(aes(y = obs.adr, color="All"),linetype="dashed") +
#  geom_line(aes(y = obs.cadr, color="Female"),linetype="dashed") +
  labs(y="Probability of owning a home in 1910",
       x="Draw number decile (%)",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.3) +
 # geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_x_continuous(breaks=seq(1,10,1),labels=levels(sort(unique(link.1900.1910.subs$quintile)))) +
  theme(legend.position="top") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave(paste0(data.directory,"plots/own-plot.png"), own.plot, scale = 1.25)

if(slides){
  own.plot <- own.plot + ggtitle(paste0("Estimated average dose-response"))
  ggsave(paste0(data.directory,"plots/own-plot-slides.png"),own.plot,scale=1.25)
}

## Nonparametric estimation

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation


## farm

set.seed(42)

# Calling the boot function with the dataset
# our function and no. of rounds
farm.boot <- boot(data.frame("y"=link.1900.1910.subs$farm,
                             "treat"=link.1900.1910.subs$quintile,
                             "w"=p.scores), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
# farm.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=farm.boot))
# 
# farm.cate.boot <- boot(data.frame("y"=link.1900.1910.subs$farm,
#                                   "treat"=link.1900.1910.subs$quintile,
#                                   "w"=p.scores,
#                                   "x"=link.1900.1910.subs$female), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
# farm.cate.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=farm.cate.boot))

## Forest plot for ATEs

# Create data for plot

comparisons <- sort(unique(link.1900.1910.subs$quintile))[-1]

farm.dat <- data.frame(x =rep(comparisons,1),
                       y = c(farm.boot$t0),  #farm.cate.boot$t0
                       y.lo = c(farm.boot.ci$lwr), #farm.cate.boot.ci$lwr
                       y.hi = c(farm.boot.ci$upr)) #farm.cate.boot.ci$upr

farm.dat$x <- as.factor(farm.dat$x)

farm.dat$Analysis <- c(rep("ATE",each=length(comparisons))) 

farm.plot <- ForestPlot(farm.dat,
                        xlab=c("Estimated treatment effect"),ylab="Draw number decile (treatment)") +
  scale_fill_manual(values= c("blue",labels=c("ATE"))) +
  labs(color="Analysis:") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = c(-0.4,-0.3,-0.2, -0.1, 0, 0.1, 0.2,0.3,0.4)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=16), plot.subtitle = element_text(hjust = 0.5, family="serif", size=12)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  #  theme(axis.text.x=element_text(family="serif", size=14,angle = 45, vjust = 0.5, hjust=1)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  # theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  #theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines"))

farm.plot <- farm.plot +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.005, yend = 0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("segment", x = 0.5, xend = 0.5, y = -0.005, yend = -0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("text", x = 0.75, y = -0.1, label = "Favors reference")

farm.plot <- farm.plot + annotate("text", x = 0.75, y = 0.1, label = "Favors treatment") 

ggsave(paste0(data.directory,"plots/farm-causal-plot.png"),farm.plot,scale=1.25)

if(slides){
  farm.plot <- farm.plot + ggtitle(paste0("Outcome: probability of owning a home in 1910"))
  ggsave(paste0(data.directory,"plots/farm-causal-plot-slides.png"),farm.plot,scale=1.25)
}

## own

# Calling the boot function with the dataset
# our function and no. of rounds
own.boot <- boot(data.frame("y"=link.1900.1910.subs$own,
                                  "treat"=link.1900.1910.subs$quintile,
                                  "w"=p.scores), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
own.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=own.boot))

# own.cate.boot <- boot(data.frame("y"=link.1900.1910.subs$own,
#                                        "treat"=link.1900.1910.subs$quintile,
#                                        "w"=p.scores,
#                                        "x"=link.1900.1910.subs$female), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
# own.cate.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=own.cate.boot))

## Forest plot for ATEs

# Create data for plot

own.dat <- data.frame(x =rep(comparisons,1),
                            y = c(own.boot$t0), #own.cate.boot$t0
                            y.lo = c(own.boot.ci$lwr), #own.cate.boot.ci$lwr
                            y.hi = c(own.boot.ci$upr)) #own.cate.boot.ci$upr

own.dat$x <- as.factor(own.dat$x)

own.dat$Analysis <- c(rep("ATE",each=length(comparisons)))  #rep("CATE",each=length(comparisons))

own.plot <- ForestPlot(own.dat,
                             xlab=c("Estimated treatment effect"),ylab="Draw number decile (treatment)") +
  labs(color="Analysis:") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(-0.5, 0.5), breaks = c(-0.4,-0.3,-0.2, -0.1, 0, 0.1, 0.2,0.3,0.4)) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5, family="serif", size=16), plot.subtitle = element_text(hjust = 0.5, family="serif", size=12)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  #  theme(axis.text.x=element_text(family="serif", size=14,angle = 45, vjust = 0.5, hjust=1)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  # theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
  #theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
  theme(panel.spacing = unit(1, "lines"))

own.plot <- own.plot +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.005, yend = 0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("segment", x = 0.5, xend = 0.5, y = -0.005, yend = -0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("text", x = 0.75, y = -0.1, label = "Favors reference")

own.plot <- own.plot + annotate("text", x = 0.75, y = 0.1, label = "Favors treatment") 

ggsave(paste0(data.directory,"plots/own-causal-plot.png"),own.plot,scale=1.25)

if(slides){
  own.plot <- own.plot + ggtitle(paste0("Outcome: probability of own patent"))
  ggsave(paste0(data.directory,"plots/own-causal-plot-slides.png"),own.plot,scale=1.25)
}