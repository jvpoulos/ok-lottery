#####################################
### Individual-level analysis    ###
#####################################

slides <- TRUE

# Est. balancing weights

quintile.fit <- cv.glmnet(x=as.matrix(cbind(link.patents,state.dummies,loc.dummies)[balance.vars]), y=link.patents$quintile, family = "multinomial", type.multinomial = "grouped")
plot(quintile.fit)

# calculate propensity scores
p.scores <- predict(quintile.fit, newx=as.matrix(cbind(link.patents,state.dummies,loc.dummies)[balance.vars]), s = "lambda.min", type = "response")[,,1]

print(sum(rowSums(p.scores))==length(link.patents$quintile)) # ensure probs. sum to 1

## Estimate dose-response curves

# Sale (binary)

lm.sale <- glm(sale~ I(quintile) + p.scores  + p.scores**2 + I(quintile)*p.scores,
                    family="binomial",
                    data=data.frame("p.scores"=p.scores,
                                    "sale"=link.patents$sale,
                                    "quintile"=link.patents$quintile))

lm.sale.preds <- lapply(1:length(unique(link.patents$quintile)), function (j) predict(lm.sale, newdata=data.frame("p.scores"=p.scores,
                                                                                                                            "sale"=link.patents$sale,
                                                                                                                            "quintile"=factor(rep(levels(link.patents$quintile)[j],times=length(link.patents$quintile)))), type="response", se.fit = TRUE))

lm.sale.curve <- sapply(lm.sale.preds, function(x) mean(x$fit))
lm.sale.curve.upper <- lm.sale.curve + (qnorm(0.975)*sapply(lm.sale.preds, function(x) mean(x$se.fit)))
lm.sale.curve.lower <- lm.sale.curve - (qnorm(0.975)*sapply(lm.sale.preds, function(x) mean(x$se.fit)))

lm.sale.curve.female <- sapply(lm.sale.preds, function(x) mean(x$fit[which(link.patents$female==1)]))
lm.sale.curve.female.upper <- lm.sale.curve.female + (qnorm(0.975)*sapply(lm.sale.preds, function(x) mean(x$se.fit[which(link.patents$female==1)])))
lm.sale.curve.female.lower <- lm.sale.curve.female - (qnorm(0.975)*sapply(lm.sale.preds, function(x) mean(x$se.fit[which(link.patents$female==1)])))

lm.sale.curve.df <- data.frame("fit"=lm.sale.curve,
                                    "lower"=lm.sale.curve.lower,
                                    "upper"= lm.sale.curve.upper,
                                    "fit.female"=lm.sale.curve.female,
                                    "lower.female"=lm.sale.curve.female.lower,
                                    "upper.female"= lm.sale.curve.female.upper,
                                    "obs.adr"=MeanDR(link.patents$sale,link.patents$quintile),
                                    "obs.cadr"=MeanDR(link.patents$sale,link.patents$quintile,x=link.patents$female),
                                    "quintile"=sort(unique(link.patents$quintile)))

sale.plot <- ggplot(lm.sale.curve.df, aes(x = as.numeric(quintile))) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
  geom_line(aes(y = fit.female, color="Female")) +
  geom_line(aes(y = obs.adr, color="All"),linetype="dashed") +
  geom_line(aes(y = obs.cadr, color="Female"),linetype="dashed") +
  labs(y="Probability of land patent purchase",
       x="Draw number decile (%)",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.3) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_x_continuous(breaks=seq(1,10,1),labels=levels(sort(unique(link.patents$quintile)))) +
  theme(legend.position="top") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave(paste0(data.directory,"plots/sale-plot.png"), sale.plot, scale = 1.25)

if(slides){
  sale.plot <- sale.plot + ggtitle(paste0("Estimated average dose-response"))
  ggsave(paste0(data.directory,"plots/sale-plot-slides.png"),sale.plot,scale=1.25)
}

# Homestead (binary)

lm.homestead <- glm(homestead~ I(quintile) + p.scores  + p.scores**2 + I(quintile)*p.scores,
                    family="binomial",
               data=data.frame("p.scores"=p.scores,
                               "homestead"=link.patents$homestead,
                               "quintile"=link.patents$quintile))

lm.homestead.preds <- lapply(1:length(unique(link.patents$quintile)), function (j) predict(lm.homestead, newdata=data.frame("p.scores"=p.scores,
                                                                                                              "homestead"=link.patents$homestead,
                                                                                                              "quintile"=factor(rep(levels(link.patents$quintile)[j],times=length(link.patents$quintile)))), type="response", se.fit = TRUE))

lm.homestead.curve <- sapply(lm.homestead.preds, function(x) mean(x$fit))
lm.homestead.curve.upper <- lm.homestead.curve + (qnorm(0.975)*sapply(lm.homestead.preds, function(x) mean(x$se.fit)))
lm.homestead.curve.lower <- lm.homestead.curve - (qnorm(0.975)*sapply(lm.homestead.preds, function(x) mean(x$se.fit)))

lm.homestead.curve.female <- sapply(lm.homestead.preds, function(x) mean(x$fit[which(link.patents$female==1)]))
lm.homestead.curve.female.upper <- lm.homestead.curve.female + (qnorm(0.975)*sapply(lm.homestead.preds, function(x) mean(x$se.fit[which(link.patents$female==1)])))
lm.homestead.curve.female.lower <- lm.homestead.curve.female - (qnorm(0.975)*sapply(lm.homestead.preds, function(x) mean(x$se.fit[which(link.patents$female==1)])))

lm.homestead.curve.df <- data.frame("fit"=lm.homestead.curve,
                                "lower"=lm.homestead.curve.lower,
                               "upper"= lm.homestead.curve.upper,
                               "fit.female"=lm.homestead.curve.female,
                               "lower.female"=lm.homestead.curve.female.lower,
                               "upper.female"= lm.homestead.curve.female.upper,
                               "obs.adr"=MeanDR(link.patents$homestead,link.patents$quintile),
                               "obs.cadr"=MeanDR(link.patents$homestead,link.patents$quintile,x=link.patents$female),
                               "quintile"=sort(unique(link.patents$quintile)))

homestead.plot <- ggplot(lm.homestead.curve.df, aes(x = as.numeric(quintile))) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
  geom_line(aes(y = fit.female, color="Female")) +
  geom_line(aes(y = obs.adr, color="All"),linetype="dashed") +
  geom_line(aes(y = obs.cadr, color="Female"),linetype="dashed") +
  labs(y="Probability of homestead patent",
       x="Draw number decile (%)",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.3) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_x_continuous(breaks=seq(1,10,1),labels=levels(sort(unique(link.patents$quintile)))) +
  theme(legend.position="top") +   theme(plot.title = element_text(hjust = 0.5, family="serif", size=16)) +
  theme(axis.title=element_text(family="serif", size=16)) +
  theme(axis.text.y=element_text(family="serif", size=14)) +
  theme(axis.text.x=element_text(family="serif", size=14)) +
  theme(legend.text=element_text(family="serif", size = 14)) +
  theme(legend.title=element_text(family="serif", size = 14)) +
  theme(strip.text.x = element_text(family="serif", size = 14)) +
  theme(strip.text.y = element_text(family="serif", size = 14)) +
  theme(panel.spacing = unit(1, "lines"))

ggsave(paste0(data.directory,"plots/homestead-plot.png"), homestead.plot, scale = 1.25)

if(slides){
  homestead.plot <- homestead.plot + ggtitle(paste0("Estimated average dose-response"))
  ggsave(paste0(data.directory,"plots/homestead-plot-slides.png"),homestead.plot,scale=1.25)
}

## Nonparametric estimation

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation


## Sale

set.seed(42)

# Calling the boot function with the dataset
# our function and no. of rounds
sale.boot <- boot(data.frame("y"=link.patents$sale,
                            "treat"=link.patents$quintile,
                            "w"=p.scores), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
sale.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=sale.boot))
  
sale.cate.boot <- boot(data.frame("y"=link.patents$sale,
                             "treat"=link.patents$quintile,
                             "w"=p.scores,
                             "x"=link.patents$female), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
sale.cate.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=sale.cate.boot))

## Forest plot for ATEs

# Create data for plot

comparisons <- sort(unique(link.patents$quintile))[-1]

sale.dat <- data.frame(x =rep(comparisons,2),
                       y = c(sale.boot$t0,sale.cate.boot$t0), 
                       y.lo = c(sale.boot.ci$lwr,sale.cate.boot.ci$lwr), 
                       y.hi = c(sale.boot.ci$upr,sale.cate.boot.ci$upr))

sale.dat$x <- as.factor(sale.dat$x)

sale.dat$Analysis <- c(rep("ATE",each=length(comparisons)), rep("CATE",each=length(comparisons))) 

sale.plot <- ForestPlot(sale.dat,
                       xlab=c("Estimated treatment effect"),ylab="Draw number decile (treatment)") +
  scale_fill_manual(values= c("blue","red"),labels=c("ATE","CATE")) +
  labs(color="Analysis:") +
  scale_x_discrete(limits = rev) +
    scale_y_continuous(limits = c(-0.25, 0.25), breaks = c(-0.2, -0.1, 0, 0.1, 0.2)) +
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

sale.plot <- sale.plot +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.005, yend = 0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("segment", x = 0.5, xend = 0.5, y = -0.005, yend = -0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("text", x = 0.75, y = -0.05, label = "Favors reference")

sale.plot <- sale.plot + annotate("text", x = 0.75, y = 0.05, label = "Favors treatment") 

ggsave(paste0(data.directory,"plots/sale-causal-plot.png"),sale.plot,scale=1.25)

if(slides){
  sale.plot <- sale.plot + ggtitle(paste0("Outcome: probability of land patent purchase"))
  ggsave(paste0(data.directory,"plots/sale-causal-plot-slides.png"),sale.plot,scale=1.25)
}

## homestead

# Calling the boot function with the dataset
# our function and no. of rounds
homestead.boot <- boot(data.frame("y"=link.patents$homestead,
                             "treat"=link.patents$quintile,
                             "w"=p.scores), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
homestead.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=homestead.boot))

homestead.cate.boot <- boot(data.frame("y"=link.patents$homestead,
                                  "treat"=link.patents$quintile,
                                  "w"=p.scores,
                                  "x"=link.patents$female), MeanDrBoot, R = 999, parallel="multicore", cl=cores)
homestead.cate.boot.ci <- do.call(rbind,lapply(1:9,getCI,x=homestead.cate.boot))

## Forest plot for ATEs

# Create data for plot

homestead.dat <- data.frame(x =rep(comparisons,2),
                       y = c(homestead.boot$t0,homestead.cate.boot$t0), 
                       y.lo = c(homestead.boot.ci$lwr,homestead.cate.boot.ci$lwr), 
                       y.hi = c(homestead.boot.ci$upr,homestead.cate.boot.ci$upr))

homestead.dat$x <- as.factor(homestead.dat$x)

homestead.dat$Analysis <- c(rep("ATE",each=length(comparisons)), rep("CATE",each=length(comparisons))) 

homestead.plot <- ForestPlot(homestead.dat,
                        xlab=c("Estimated treatment effect"),ylab="Draw number decile (treatment)") +
  labs(color="Analysis:") +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(limits = c(-0.25, 0.25), breaks = c(-0.2, -0.1, 0, 0.1, 0.2)) +
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

homestead.plot <- homestead.plot +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.005, yend = 0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("segment", x = 0.5, xend = 0.5, y = -0.005, yend = -0.1,
           colour = "blue", size = 1, arrow = arrow(length = unit(0.1, "inches"), ends="last", type="closed")) +
  annotate("text", x = 0.75, y = -0.05, label = "Favors reference")

homestead.plot <- homestead.plot + annotate("text", x = 0.75, y = 0.05, label = "Favors treatment") 

ggsave(paste0(data.directory,"plots/homestead-causal-plot.png"),homestead.plot,scale=1.25)

if(slides){
  homestead.plot <- homestead.plot + ggtitle(paste0("Outcome: probability of homestead patent"))
  ggsave(paste0(data.directory,"plots/homestead-causal-plot-slides.png"),homestead.plot,scale=1.25)
}