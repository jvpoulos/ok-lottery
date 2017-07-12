######################################################################
# IV Estimation for long-run effect of land inequality               #
######################################################################

library(AER)

## tax2

tax2.vars <- colnames(tax2.ineq)[4:11]

tax2.ineq.1900 <- subset(tax2.ineq, year==1900)
tax2.ineq.1900$treat <- ifelse(tax2.ineq.1900$cat=="Treated", 1, 0)

tax2.ineq.iv <- sapply(1:length(tax2.vars), function(i){
  iv.result <- ivreg(tax2.ineq.1900[,tax2.vars[i]]~ tax2.ineq.1900[,'aG'] | tax2.ineq.1900[,'treat'])
  late.est <- summary(iv.result, vcov = sandwich)$coefficients[2]
  late.se <- summary(iv.result, vcov = sandwich)$coefficients[4]
  return(list("CI.lower"= late.est-1.96*late.se,
              "CI.upper"= late.est+1.96*late.se,
              "LATE"=late.est,
              "N"=summary(iv.result, vcov = sandwich)$df[2] + summary(iv.result, vcov = sandwich)$df[1]))})

tax2.ineq.iv <- data.frame(t(as.matrix(tax2.ineq.iv)))
tax2.ineq.iv$year <- tax2.years[-c(1:2)]
tax2.ineq.iv$variable <- "tax2"

# rev2
rev2.vars <- colnames(rev2.ineq)[2:10]

rev2.ineq.1900 <- subset(rev2.ineq, year==1900)
rev2.ineq.1900$treat <- ifelse(rev2.ineq.1900$cat=="Treated", 1, 0)

rev2.ineq.iv <- sapply(1:length(rev2.vars), function(i){
  iv.result <- ivreg(rev2.ineq.1900[,rev2.vars[i]]~ rev2.ineq.1900[,'aG'] | rev2.ineq.1900[,'treat'])
  late.est <- summary(iv.result, vcov = sandwich)$coefficients[2]
  late.se <- summary(iv.result, vcov = sandwich)$coefficients[4]
  return(list("CI.lower"= late.est-1.96*late.se,
              "CI.upper"= late.est+1.96*late.se,
              "LATE"=late.est,
              "N"=summary(iv.result, vcov = sandwich)$df[2] + summary(iv.result, vcov = sandwich)$df[1]))})

rev2.ineq.iv <- data.frame(t(as.matrix(rev2.ineq.iv)))
rev2.ineq.iv$year <- rev2.years
rev2.ineq.iv$variable <- "rev2"

# educ

educ.vars <- colnames(educ.ineq)[3:11]

educ.ineq.1900 <- subset(educ.ineq, year==1900)
educ.ineq.1900$treat <- ifelse(educ.ineq.1900$cat=="Treated", 1, 0)

educ.ineq.iv <- sapply(1:length(educ.vars), function(i){
  iv.result <- ivreg(educ.ineq.1900[,educ.vars[i]]~ educ.ineq.1900[,'aG'] | educ.ineq.1900[,'treat'])
  late.est <- summary(iv.result, vcov = sandwich)$coefficients[2]
  late.se <- summary(iv.result, vcov = sandwich)$coefficients[4]
  return(list("CI.lower"= late.est-1.96*late.se,
              "CI.upper"= late.est+1.96*late.se,
              "LATE"=late.est,
              "N"=summary(iv.result, vcov = sandwich)$df[2] + summary(iv.result, vcov = sandwich)$df[1]))})

educ.ineq.iv <- data.frame(t(as.matrix(educ.ineq.iv)))
educ.ineq.iv$year <- educ.years[-c(1)]
educ.ineq.iv$variable <- "educ"

## Plot estimates by year

iv.plot.dat <- rbind(tax2.ineq.iv,rev2.ineq.iv,educ.ineq.iv)
iv.plot.dat$N <- as.numeric(iv.plot.dat$N)
iv.plot.dat$LATE <- as.numeric(iv.plot.dat$LATE)
iv.plot.dat$CI.lower <- as.numeric(iv.plot.dat$CI.lower)
iv.plot.dat$CI.upper <- as.numeric(iv.plot.dat$CI.upper)
iv.plot.dat$year <- factor(iv.plot.dat$year) 

iv.plot <- ggplot(iv.plot.dat, aes(x=year, y = LATE, ymin=CI.lower, ymax=CI.upper,colour=factor(variable, labels=c("Education spending", "Revenues","Taxes")))) + 
    geom_pointrange(size=0.5, alpha=0.6) + 
    #  coord_flip() +
    geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0), colour="black", lty=1) +
    scale_y_continuous(labels = scales::comma) +
    labs(colour = "Per-capita outcome") +
    ylab("Treatment effect ($)") +
    xlab("Outcome year") + #switch because of the coord_flip() above
    ggtitle("Long-run effects of 1900 land Gini")

ggsave(paste0(data.directory,"plots/iv-plot.png"), iv.plot, width=11, height=8.5)  