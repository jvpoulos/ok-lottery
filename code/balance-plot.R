#####################################
### Balance plot ###
#####################################

# Balance vars are state + city/state

# Collect bivariate p values
balance.states <- sapply(levels(hs$state), function(x) summary(lm(hs$draw.scale ~ I(hs$state==x)))$"coefficients"[2,4]) 

#exclude.loc <- c("Burneyville, Texas","Connerville, Indian Territory","Cosby, Missouri","Crafton, Texas","Dexil, Indian Territory","Fryburg, Texas",
#"Garner, Texas","Greinvi Lle, Texas","Havana, Kansas","Horton, Nebraska","Lamad, Missouri","Lomax, Illinois","Maryanville, Kansas","Olloway, Kansas",
#"Paige, Texas","Rising Sun, Ohio","River, Ohio","Russett, Indian Territory","Silverville, Indiana", "NA NA")

#balance.loc <- sapply(levels(hs$loc)[!levels(hs$loc) %in% exclude.loc], function(x) summary(lm(hs$draw.scale ~ I(hs$loc==x)))$"coefficients"[2,4]) 

# Create balance plot data

# covars <- data.frame("covars"=c(levels(hs$state),levels(hs$loc)[!levels(hs$loc) %in% exclude.loc]),
#                      "p"=c(balance.states,balance.loc))
covars <- data.frame("covars"=c(levels(hs$state)),
                      "p"=c(balance.states))

States  <- levels(hs$state) # group vars
#Cities <- levels(hs$loc)[!levels(hs$loc) %in% exclude.loc]

covars$group <- NA
covars$group[covars$covars %in% States]       <- "State of registration"
#covars$group[covars$covars %in% Cities]       <- "City of registration"

#covars <- subset(covars, group=="State of registration" | (group =="City of registration"  & p <=0.05))  # Keep only locs. with p<=0.05

offset <- c("   ")
covars$covars <- paste(offset,covars$covars)

covars$order <- 1:nrow(covars)  # reorder  
# order <- data.frame(covars= c("State of registration:",
#                               "    ",
#                               "City of registration:"
# ),order=c(.5,37.1,37.5),
# p=NA,group=NA)
order <- data.frame(covars= c("    "),
                    order=c(.5),
p=NA,group=NA)
covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

p <- ggplot(covars,aes(y=p,x=covars,colour=group)) +  
  coord_flip(ylim = c(0.03, 0.97)) + 
  geom_hline(data=data.frame(x=0, y = 1), aes(x=x, yintercept=0.05), colour="black", lty=2) +
  geom_point(size=2, alpha=0.9) + 
  scale_y_continuous(name="p-value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
  scale_x_discrete(name="") + 
  ThemeBw1()

ggsave(paste0(data.directory,"plots/balance-plot.png"), p, width=8.5, height=11)