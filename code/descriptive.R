#####################################
### Descriptive statistics         ###
#####################################

# Summary plot

binary.covars <-c("lawton", colnames(state.dummies), colnames(loc.dummies))

binary.outcomes <- c("sale","homestead")
continuous.outcomes <-c("sales","homesteads","total_acres")

tableNominal(cbind(link.patents,state.dummies,loc.dummies)[c("female",binary.covars, binary.outcomes)], group=link.patents$female, cumsum=FALSE, longtable = FALSE)

print(tableContinuous(data.frame(link.patents)[continuous.outcomes], group=link.patents$female, cumsum=FALSE, stats= c("n", "min", "mean", "max", "s"), longtable = FALSE))

## Plot densities of time lag in filing grants (Lawton)

# Make data for histogram
time.lapse.plot <- melt(data=lawton[c("time.lapse","comply")], 
                        id.vars="comply") 

# Plot the overlaid density of time lapse by compliance status
time.lapse.hist <- ggplot(time.lapse.plot, aes(x=value, fill=as.factor(comply))) + 
  geom_density(alpha=.3) +
  ylab("Density") + 
  xlab("# of days elapsed") +
  scale_fill_manual(values = c("yellow","green"), labels = c("No","Yes"), name= "Registered?") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/time-lapse.png"), time.lapse.hist, scale=1.25) 

## Plot draw # vs. time lapse (Lawton)

draw.time <- ggplot(lawton, aes(Drawing.., time.lapse)) + 
  geom_point(aes(colour = factor(comply)), alpha=.8) +
  scale_x_log10() +
  xlab("Common logarithm of draw #") + 
  ylab("# of days elapsed") +
  scale_colour_manual(values = c("yellow","green"), labels = c("no","yes"), name= "Registered?") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/draw-time.png"), draw.time, scale=1.25)