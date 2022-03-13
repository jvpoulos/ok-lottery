#####################################
### Descriptive statistics         ###
#####################################

# Summary plot

binary.covars <-c("lawton", colnames(state.dummies), colnames(loc.dummies))

binary.outcomes <- c("sale","homestead")
continuous.outcomes <-c("sales","homesteads","total_acres")

tableNominal(cbind(link.patents,state.dummies,loc.dummies)[c("female",binary.covars, binary.outcomes)], group=link.patents$female, cumsum=FALSE, longtable = FALSE, prec=2)

print(tableContinuous(data.frame(link.patents)[continuous.outcomes], group=link.patents$female, cumsum=FALSE, stats= c("n", "min", "mean", "max", "s"), longtable = FALSE, prec=2))

## Plot densities of time lag in filing grants (Lawton)

# Make data for histogram
time.lapse.plot <- melt(data=lawton[c("time.lapse","comply","female")], 
                        id.vars=c("female","comply"))

facet_names <- list(
  '0'="Did not claim land",
  '1'="Claimed land"
)

facet_labeller <- function(variable,value){
  return(facet_names[value])
}

# Plot the overlaid density of time lapse by compliance status
time.lapse.hist <- ggplot(time.lapse.plot, aes(x=value, fill=as.factor(female))) + 
  geom_density(alpha=.2) +
  facet_wrap(~as.factor(comply),labeller=facet_labeller) +
  ylab("Density") + 
  xlab("Days elapsed since filing date") +
  scale_fill_manual(values = c("red","blue"), labels = c("Male","Female"), name= "Gender") +
  theme(legend.justification = c(0, 1), legend.position = c(0.01, 0.99),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/time-lapse.png"), time.lapse.hist, scale=1.25) 

## Plot draw # vs. time lapse (Lawton)

draw.time <- ggplot(lawton, aes(Drawing.., time.lapse)) + 
  geom_point(aes(colour = factor(female)), alpha=.9) +
  facet_wrap(~as.factor(comply),labeller=facet_labeller) +
  scale_x_log10() +
  xlab("Draw number (log)") + 
  ylab("Days elapsed since filing date") +
  scale_colour_manual(values = c("red","blue"), labels = c("Male","Female"), name= "Gender") +
  theme(legend.justification = c(0, 1), legend.position = c(0.01, 0.99),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/draw-time.png"), draw.time, scale=1.25)