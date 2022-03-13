#####################################
### Individual-level analysis    ###
#####################################


# Est. balancing weights

draw.fit <- gbm(draw ~ ., data = cbind(link.patents,state.dummies,loc.dummies)[c("draw",balance.vars)],
               distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
               interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.8,  
               n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
               verbose = FALSE, n.cores = ncores)

best.iter <- gbm.perf(draw.fit, method = "cv") # 5 fold cv
print(best.iter)

# Plot relative influence of each variable
summary(draw.fit, n.trees = best.iter)  # using estimated best number of trees

# calculate propensity scores
p.scores <- predict(draw.fit, n.trees = best.iter, type = "response")

## Estimate dose-response curves

# Sale (binary)

lm.sale <- lm(sale~draw + I(draw^2) + I(draw^3) + p.scores + I(p.scores^2) + I(p.scores^3) + I(draw^2)*p.scores + draw*I(p.scores^2), data=data.frame("p.scores"=p.scores,
                                     "sale"=link.patents$sale,
                                     "draw"=link.patents$draw))

lm.sale.preds <- lapply(sort(unique(link.patents$draw)), function (j) predict(lm.sale, newdata=data.frame("p.scores"=p.scores,
                                    "sale"=link.patents$sale,
                                    "draw"=j), type="response", se.fit = TRUE))

lm.sale.curve <- sapply(lm.sale.preds, function(x) mean(x$fit))
lm.sale.curve.se <- sapply(lm.sale.preds, function(x) mean(x$se.fit))

lm.sale.curve.female <- sapply(lm.sale.preds, function(x) mean(x$fit[which(link.patents$female==1)]))
lm.sale.curve.female.se <- sapply(lm.sale.preds, function(x) mean(x$se.fit[which(link.patents$female==1)]))

lm.sale.curve.df <- data.frame("fit"=lm.sale.curve,
                               "upper"= lm.sale.curve + 1.96*lm.sale.curve.se,
                               "lower"=lm.sale.curve - 1.96*lm.sale.curve.se,
                               "fit.female"=lm.sale.curve.female,
                               "upper.female"= lm.sale.curve.female + 1.96*lm.sale.curve.female.se,
                               "lower.female"=lm.sale.curve.female - 1.96*lm.sale.curve.female.se,
                               "draw"=sort(unique(link.patents$draw)))

colors <- c("All" = "blue", "Female" = "red")

sale.plot <- ggplot(lm.sale.curve.df, aes(x = draw)) +
  theme_bw() +
  geom_line(aes(y = fit, color="All")) +
  geom_line(aes(y = fit.female, color="Female")) +
#  ylab("Estimated average dose-response") +
  labs(y="Probability of land patent purchase",
       x="Draw number",
       color="Group") +
  geom_smooth(aes(y=fit, ymin = lower, ymax = upper, color="All"), stat = "identity",alpha=0.1) +
  geom_smooth(aes(y=fit.female, ymin = lower.female, ymax = upper.female, color="Female"), stat = "identity",alpha=0.1) +
  scale_color_manual(values = colors)

ggsave(paste0(data.directory,"plots/sale-plot.png"), sale.plot, scale = 1.25)

# Homestead (binary)

lm.homestead <- lm(homestead~draw, data=link.patents)
summary(lm.homestead)
confint(lm.homestead)