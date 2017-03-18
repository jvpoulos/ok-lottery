#####################################
### Descriptive statistics         ###
#####################################

## Plot densities of time lag in filing grants (Lawton)

# Make data for histogram
time.lapse.plot <- melt(data=lawton[c("time.lapse","comply")], 
                          id.vars="comply") 

# Plot the overlaid density of time lapse by compliance status
time.lapse.hist <- ggplot(time.lapse.plot, aes(x=value, fill=as.factor(comply))) + 
  geom_density(alpha=.3) +
  ylab("Density") + 
  xlab("# of days elapsed") +
  ggtitle(paste("Lawton homesteaders, N =", 
                format(nrow(lawton),big.mark=",",scientific=FALSE,trim=TRUE))) +
  scale_fill_manual(values = c("yellow","green"), labels = c("no","yes"), name= "Registered?") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/time-elapse.png"), time.lapse.hist, width=8.5, height=11) 