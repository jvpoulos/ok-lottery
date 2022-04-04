#####################################
### Balance plot ###
#####################################

# Balance vars are state + city/state

balance.vars <- c("female",binary.covars)

# Collect bivariate p values
balance.p.values <- sapply(balance.vars, function(x) summary(lm(link.patents$first.quintile ~ cbind(link.patents,state.dummies,loc.dummies)[,x]))$"coefficients"[2,4]) 

# Create balance plot data

covars <- data.frame("covars"=balance.vars,
                      "p"=c(balance.p.values))

States  <- colnames(state.dummies)
Locations <- colnames(loc.dummies)

covars$group <- NA
covars$group[covars$covars %in% States]       <- "State at time of registration"
covars$group[covars$covars %in% Locations]       <- "Place of residence at time of registration"
covars$group[covars$covars %in% c("lawton")]       <- "District of registration"
covars$group[covars$covars %in% c("female")]       <- "Gender"

covars$covars[1] <- "Female"
covars$covars[2] <- "Lawton"

offset <- c("   ")
covars$covars <- paste(offset,covars$covars)

covars$order <- 1:nrow(covars)  # reorder  
order <- data.frame(covars= c("Gender:",
                              "    ",
                              "District of registration:",
                              "    ",
                              "State at time of registration:",
                              "    ",
                              "Place of residence at time of registration:"),order=c(.5,1.1,1.5,2.1,2.5,9.1,9.5),
p=NA,group=NA)

covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

p <- ggplot(covars[!rownames(covars)%in% c("2","4","6"),],aes(y=p,x=covars,colour=group)) +  
  coord_flip() + #ylim = c(0.03, 0.97)
  geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0.05/length(balance.vars)), colour="black", lty=3) +
  geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0.05), colour="black", lty=2) +
  geom_point(size=2, alpha=0.9) + 
  scale_y_continuous(name="p-value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
  scale_x_discrete(name="") + 
  ThemeBw1()

ggsave(paste0(data.directory,"plots/balance-plot.png"), p, scale = 1.25)