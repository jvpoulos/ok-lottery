#####################################
### Individual-level analysis    ###
#####################################

# Est. balancing weights

quintile.fit <- cv.glmnet(x=as.matrix(cbind(link.patents,state.dummies,loc.dummies)[balance.vars]), y=link.patents$quintile, family = "multinomial", type.multinomial = "grouped")
plot(quintile.fit)

# calculate propensity scores
p.scores <- predict(quintile.fit, newx=as.matrix(cbind(link.patents,state.dummies,loc.dummies)[balance.vars]), s = "lambda.min", type = "response")[,,1]

print(sum(rowSums(p.scores))==length(link.patents$quintile)) # ensure probs. sum to 1

## Estimate dose-response curves

# Sale (binary)

lm.sale <- lm(sale~p.scores + poly(p.scores,2) + I(quintile)*p.scores, 
                   data=data.frame("p.scores"=p.scores,
                                   "sale"=link.patents$sale,
                                   "quintile"=link.patents$quintile))

lm.sale.preds <- lapply(sort(unique(link.patents$quintile)), function (j) predict(lm.sale, newdata=data.frame("p.scores"=p.scores,
                                                                                                                        "sale"=link.patents$sale,
                                                                                                                        "quintile"=j), type="response", interval = "confidence"))

lm.sale.curve <- sapply(lm.sale.preds, function(x) colMeans(x))

lm.sale.curve.female <- sapply(lm.sale.preds, function(x) colMeans(x[which(link.patents$female==1),]))

lm.sale.curve.df <- data.frame("fit"=lm.sale.curve[1,],
                                    "lower"=lm.sale.curve[2,],
                                    "upper"= lm.sale.curve[3,],
                                    "fit.female"=lm.sale.curve.female[1,],
                                    "lower.female"=lm.sale.curve.female[2,],
                                    "upper.female"= lm.sale.curve.female[3,],
                                    "obs.adr"=MeanDR(link.patents$sale,link.patents$quintile),
                                    "obs.cadr"=MeanDR(link.patents$sale,link.patents$quintile,x=link.patents$female),
                                    "quintile"=sort(unique(link.patents$quintile)))

colors <- c("All" = "blue", "Female" = "red")

sale.plot <- ggplot(lm.sale.curve.df, aes(x = as.numeric(quintile))) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
  geom_line(aes(y = fit.female, color="Female")) +
  geom_line(aes(y = obs.adr, color="All"),linetype="dashed") +
  geom_line(aes(y = obs.cadr, color="Female"),linetype="dashed") +
 # ggtitle("Estimated average dose-response") +
  labs(y="Probability of land patent purchase",
       x="Draw number decile (%)",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.4) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_color_manual(values = colors) +
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

# Homestead (binary)

lm.homestead <- lm(homestead~p.scores + poly(p.scores,2) + I(quintile)*p.scores, 
               data=data.frame("p.scores"=p.scores,
                               "homestead"=link.patents$homestead,
                               "quintile"=link.patents$quintile))

lm.homestead.preds <- lapply(sort(unique(link.patents$quintile)), function (j) predict(lm.homestead, newdata=data.frame("p.scores"=p.scores,
                                                                                                              "homestead"=link.patents$homestead,
                                                                                                              "quintile"=j), type="response", interval = "confidence"))

lm.homestead.curve <- sapply(lm.homestead.preds, function(x) colMeans(x))

lm.homestead.curve.female <- sapply(lm.homestead.preds, function(x) colMeans(x[which(link.patents$female==1),]))

lm.homestead.curve.df <- data.frame("fit"=lm.homestead.curve[1,],
                                "lower"=lm.homestead.curve[2,],
                               "upper"= lm.homestead.curve[3,],
                               "fit.female"=lm.homestead.curve.female[1,],
                               "lower.female"=lm.homestead.curve.female[2,],
                               "upper.female"= lm.homestead.curve.female[3,],
                               "obs.adr"=MeanDR(link.patents$homestead,link.patents$draw),
                               "obs.cadr"=MeanDR(link.patents$homestead,link.patents$draw,x=link.patents$female),
                               "quintile"=sort(unique(link.patents$quintile)))

homestead.plot <- ggplot(lm.homestead.curve.df, aes(x = as.numeric(quintile))) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
  geom_line(aes(y = fit.female, color="Female")) +
  geom_line(aes(y = obs.adr, color="All"),linetype="dashed") +
  geom_line(aes(y = obs.cadr, color="Female"),linetype="dashed") +
  # ggtitle("Estimated average dose-response") +
  labs(y="Probability of homestead patent",
       x="Draw number decile (%)",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.4) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_color_manual(values = colors) +
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

## Nonparametric estimation

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

rand.p <- FALSE

## Sale
if(rand.p){
  # Get randomization p value
  perm.sale <- PermutationTest(y=link.patents$sale,
                               treat=link.patents$draw,
                               L=1000) 
  print(perm.sale$p)
}

# Get randomization CIs 
perm.sale.CI <- PermutationCI(y=link.patents$sale,
                              treat=link.patents$draw,
                              c.range=c(-0.3,0.3),
                              L=1000,
                              l=100) 
print(perm.sale.CI$CI)
print(perm.sale.CI$obs.t.stat) 

perm.sale.CATE.CI <- PermutationCI(y=link.patents$sale,
                              treat=link.patents$draw,
                              x=link.patents$female,
                              c.range=c(-0.3,0.3),
                              L=1000,
                              l=100) 
print(perm.sale.CATE.CI$CI)
print(perm.sale.CATE.CI$obs.t.stat) 

## homestead
if(rand.p){
  # Get randomization p value
  perm.homestead <- PermutationTest(y=link.patents$homestead,
                                    treat=link.patents$draw,
                                    L=1000) 
  print(perm.homestead$p)
}

# Get randomization CIs 
perm.homestead.CI <- PermutationCI(y=link.patents$homestead,
                                   treat=link.patents$draw,
                                   c.range=c(-0.3,0.3),
                                   L=1000,
                                   l=100)
print(perm.homestead.CI$CI)
print(perm.homestead.CI$MeanDR) 

# Get randomization CIs 
perm.homestead.CATE.CI <- PermutationCI(y=link.patents$homestead,
                                   treat=link.patents$draw,
                                   x=link.patents$female,
                                   c.range=c(-0.3,0.3),
                                   L=1000,
                                   l=100)
print(perm.homestead.CATE.CI$CI)
print(perm.homestead.CATE.CI$MeanDR)
