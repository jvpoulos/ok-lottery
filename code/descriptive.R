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
  scale_fill_manual(values = c("yellow","green"), labels = c("no","yes"), name= "Registered?") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/time-lapse.png"), time.lapse.hist, width=8.5, height=11) 

## Plot draw # vs. time lapse (Lawton)

draw.time <- ggplot(lawton, aes(Drawing.., time.lapse)) + 
  geom_point(aes(colour = factor(comply)), alpha=.8) +
  scale_x_log10() +
  xlab("Common logarithm of draw #") + 
  ylab("# of days elapsed") +
  scale_colour_manual(values = c("yellow","green"), labels = c("no","yes"), name= "Registered?") +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1),legend.background = element_rect(colour = "black"))

ggsave(paste0(data.directory,"plots/draw-time.png"), draw.time, width=8.5, height=11)

## Plot patents time series

# By time

patents.tab <- patents %>%
  group_by(date) %>%
  summarise_each(funs(sum),sales,homesteads,total_acres)

colnames(patents.tab) <- c("Date","Sales", "Homesteads", "Total Acres")

patents.tab$Date <- as.POSIXct(patents.tab$Date,format="%m/%d/%Y",tz="UTC")

patents.time <- ggplot(patents.tab, aes(x=Date,y=Sales)) + 
  geom_line(aes(colour='Sales')) +
  geom_line(aes(y=Homesteads,colour='Homesteads')) +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1800","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Number of land patents", labels = comma) +
  xlab("") +
  scale_colour_manual(name="Patent type",
                      values=c(Sales="red", Homesteads="blue"))

ggsave(paste0(data.directory,"plots/patents-time.png"), patents.time, width=8.5, height=11)

# By time x state (sales)

#select top 10 states in terms of homesteads
top10 <- patents %>%
  group_by(state_code) %>%
  summarise_each(funs(sum), homesteads) %>%
  arrange(desc(homesteads))

patents.state.tab <- patents %>%
  filter(state_code %in% c(top10[,1][1:7,][[1]])) %>%
  group_by(date,state_code) %>%
  summarise_each(funs(sum),sales,homesteads,total_acres)

colnames(patents.state.tab) <- c("Date","State","Sales", "Homesteads", "Total Acres")

patents.state.tab$Date <- as.POSIXct(patents.state.tab$Date,format="%m/%d/%Y",tz="UTC")

sales.state.time <- ggplot(patents.state.tab, aes( Date, Sales ,color=State )) + 
  geom_line() +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1850","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Number of sales", labels = comma) +
  xlab("")

ggsave(paste0(data.directory,"plots/sales-state-time.png"), sales.state.time, width=8.5, height=11)

# By time x state (homesteads)

homesteads.state.time <- ggplot(patents.state.tab, aes( Date, Homesteads ,color=State )) + 
  geom_line() +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1850","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Number of homesteads", labels = comma) +
  xlab("")

ggsave(paste0(data.directory,"plots/homesteads-state-time.png"), homesteads.state.time, width=8.5, height=11)

## Bar chart of homesteader states

hs.state.dat <- count(hs$state)[count(hs$state)$freq >100,]
hs.state.dat <- hs.state.dat[!is.na(hs.state.dat$x),]

colnames(hs.state.dat) <- c("State", "Count")

hs.state <- ggplot(hs.state.dat, aes(State, Count)) +
  # scale_y_continuous(labels = c("0", "10,000", "20,000", "30,000", "40,000")) +
  geom_bar(stat="identity",fill='blue') +
  geom_text(aes(label = Percent(Count/nrow(hs))), size = 3, hjust = 0.5, vjust = -1) +
  xlab("State") +
  ylab("Count")

ggsave(paste0(data.directory,"plots/hs-states.png"), hs.state, width=8.5, height=11)

## Plot county-level time-series outcomes by group

# land inequality

gini.county <- ggplot(c.county[!c.county$cat==0,], aes(x=year, y = G)) + 
  geom_point() + 
  geom_point(data=c.county[c.county$cat==1,]) +
  geom_point(data=c.county[c.county$cat==2,]) +
  geom_point(data=c.county[c.county$cat==3,]) +
  geom_point(data=c.county[c.county$cat==4,]) +
  geom_smooth(method="lm",se=FALSE, colour="red",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==1,],method="loess",se=FALSE, colour="green",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==2,],method="loess",se=FALSE, colour="blue",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==3,],method="loess",se=FALSE, colour="orange",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==4,],method="loess",se=FALSE, colour="yellow",size=0.5) +
  scale_x_continuous(breaks= years) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Land Gini") +
  xlab("")

ggsave(paste0(data.directory,"plots/gini-county.png"), gini.county, width=8.5, height=11)


# tenancy

county.tenancy <- ggplot(c.county[!c.county$cat==0,], aes(x=year, y = tenancy)) + 
  geom_point() + 
  geom_point(data=c.county[c.county$cat==1,]) +
  geom_point(data=c.county[c.county$cat==2,]) +
  geom_point(data=c.county[c.county$cat==3,]) +
  geom_point(data=c.county[c.county$cat==4,]) +
  geom_smooth(method="lm",se=FALSE, colour="red",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==1,],method="loess",se=FALSE, colour="green",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==2,],method="loess",se=FALSE, colour="blue",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==3,],method="loess",se=FALSE, colour="orange",size=0.5) +
  geom_smooth(data=c.county[c.county$cat==4,],method="loess",se=FALSE, colour="yellow",size=0.5) +
  scale_x_continuous(breaks= years) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Share of tenant farms") +
  xlab("")

ggsave(paste0(data.directory,"plots/county-tenancy.png"), county.tenancy, width=8.5, height=11)

# avg farm sizes 

county.farmsize <- ggplot(c.county2[!c.county2$cat==0,], aes(x=year, y = farmsize)) + 
  geom_point() + 
  geom_point(data=c.county2[c.county2$cat==1,]) +
  geom_point(data=c.county2[c.county2$cat==2,]) +
  geom_point(data=c.county2[c.county2$cat==3,]) +
  geom_point(data=c.county2[c.county2$cat==4,]) +
  geom_smooth(method="lm",se=FALSE, colour="red",size=0.5) +
  geom_smooth(data=c.county2[c.county2$cat==1,],method="loess",se=FALSE, colour="green",size=0.5) +
  geom_smooth(data=c.county2[c.county2$cat==2,],method="loess",se=FALSE, colour="blue",size=0.5) +
  geom_smooth(data=c.county2[c.county2$cat==3,],method="loess",se=FALSE, colour="orange",size=0.5) +
  geom_smooth(data=c.county2[c.county2$cat==4,],method="loess",se=FALSE, colour="yellow",size=0.5) +
  scale_x_continuous(breaks= years) +
#  scale_y_continuous(limits=c(0,2000)) +
  coord_cartesian(ylim=c(0, 10000)) +
  ylab("Average farm size") +
  xlab("")

ggsave(paste0(data.directory,"plots/county-farmsize.png"), county.farmsize, width=8.5, height=11)


## Plot county-level time-series pretreatment covariates by group

bin.melt <- melt(c.county[c("year","totpop","mtot","ftot","farm100","farm500","farm1000","farms","faval","cat")],
                 id.vars=c("year","cat"))

county.pretreatment <- ggplot(data=na.omit(bin.melt[bin.melt$year==1900,]),aes(x=variable,y=value,colour=as.factor(cat))) + 
  scale_x_discrete(labels=c("Total pop.", "Urban pop.","Total males","Total females", 
                            "# farms 100-499 acres" , "# farms 500-999 acres", "# farms 1000+ acres","# farms","Farm value")) +
  geom_boxplot() +
  scale_y_log10() +
#  facet_wrap(~year,  nrow=1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="Value (common logarithm)",x="") +
  scale_color_discrete("Group",
                       labels=c("KS/TX contiguous", "OK other", "OK land run", "OK lottery", "Other"))

ggsave(paste0(data.directory,"plots/county-pretreatment.png"), county.pretreatment, width=8.5, height=11)