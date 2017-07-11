#####################################
### Descriptive statistics         ###
#####################################
library(scales)
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)

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

ggsave(paste0(data.directory,"plots/patents-time.png"), patents.time, width=11, height=8.5)

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

ggsave(paste0(data.directory,"plots/sales-state-time.png"), sales.state.time, width=11, height=8.5)

# By time x state (homesteads)

homesteads.state.time <- ggplot(patents.state.tab, aes( Date, Homesteads ,color=State )) + 
  geom_line() +
  coord_cartesian(xlim=as.POSIXct(c("01/01/1850","12/31/1950"), format="%m/%d/%Y",tz="UTC")) +
  scale_y_continuous(name="Number of homesteads", labels = comma) +
  xlab("")

ggsave(paste0(data.directory,"plots/homesteads-state-time.png"), homesteads.state.time, width=11, height=8.5)

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

c.county.out <- RbindMatchColumns(df1, df8) 

# Get state codes
library(noncensus)
data(counties)
counties$state_fips <- as.numeric(counties$state_fips)
counties$county_fips <- as.numeric(counties$county_fips)
counties$state_abbr <- counties$state
counties$fips <- counties$state_fips*1000 + counties$county_fips

c.county.out <- merge(c.county.out, counties[c("state_fips","state_abbr")], by.x ="state", by.y="state_fips", all.x=TRUE)

c.county.out <- c.county.out %>% 
  filter(state_abbr %in% c("CA","CO","MN","MT","ND","NE","OK")) %>%
  group_by(year,state_abbr) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),G, tenancy, aG) 

# land inequality

agini.county <- ggplot(c.county.out, aes(x=year, y = aG, colour=state_abbr)) + 
  geom_line() + 
  scale_x_continuous(breaks= years) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_discrete(name= "State") +
  ylab("Adjusted land Gini") +
  xlab("")

ggsave(paste0(data.directory,"plots/agini-county.png"), agini.county, width=11, height=8.5)

gini.county <- ggplot(c.county.out, aes(x=year, y = G, colour=state_abbr)) + 
  geom_line() + 
  scale_x_continuous(breaks= years) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_discrete(name= "State") +
  ylab("Land Gini") +
  xlab("")

ggsave(paste0(data.directory,"plots/gini-county.png"), gini.county, width=11, height=8.5)

# tenancy

tenancy.county <- ggplot(c.county.out, aes(x=year, y = tenancy, colour=state_abbr)) + 
  geom_line() + 
  scale_x_continuous(breaks= years) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_discrete(name= "State") +
  ylab("Tenancy") +
  xlab("")

ggsave(paste0(data.directory,"plots/tenancy-county.png"), tenancy.county, width=11, height=8.5)

# # revenue (1)
# rev1.years <- c(1890,1902,1913,1932,1942)
# 
# rev1.out <- reshape(rev1, direction="long", varying=list(names(rev1)[2:6]), v.names="revpc1", 
#                     idvar=c("fips"), timevar="year", times=rev1.years) # reshape long
# 
# rev1.out <- rev1.out %>% 
#   filter(state.fips %in% c(6,8,27,30,38,31,40)) %>%
#   group_by(year,state.fips) %>% 
#   summarise_each(funs(mean(., na.rm = TRUE)),revpc1) 
# 
# revpc1.county <- ggplot(rev1.out, aes(x=year, y = revpc1, colour=as.factor(state.fips))) + 
#   geom_line() + 
#   scale_x_continuous(breaks= rev1.years) +
#   scale_colour_discrete(name= "State", labels = c("CA","CO","MN","MT","ND","NE","OK")) +
#   ylab("Per-capita revenue collected by counties") +
#   xlab("") 
# 
# ggsave(paste0(data.directory,"plots/revpc1-county.png"), revpc1.county, width=11, height=8.5)

# revenue (2)

rev2.years <- c(1932,1957,1962,1967,1972,1977,1982,1987,1992)

rev2.out <- reshape(rev2, direction="long", varying=list(names(rev2)[2:10]), v.names="revpc2", 
                    idvar=c("fips"), timevar="year", times=rev2.years) # reshape long

rev2.out <- rev2.out %>% 
  filter(state.fips %in% c(6,8,27,30,38,31,40)) %>%
  group_by(year,state.fips) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),revpc2) 

revpc2.county <- ggplot(rev2.out, aes(x=year, y = revpc2, colour=as.factor(state.fips))) + 
  geom_line() + 
  scale_x_continuous(breaks= rev2.years) +
  scale_colour_discrete(name= "State", labels =c("CA","CO","MN","MT","ND","NE","OK")) +
  ylab("Per-capita revenues collected by all local governments within counties ($)") +
  xlab("") 

ggsave(paste0(data.directory,"plots/revpc2-county.png"), revpc2.county, width=11, height=8.5)

# tax (2)

tax2.years <- c(1870,1880, 1932,1962,1967,1972,1977,1982,1987,1992)

tax2.out <- reshape(tax2, direction="long", varying=list(names(tax2)[2:11]), v.names="taxpc2", 
                    idvar=c("fips"), timevar="year", times=tax2.years) # reshape long

tax2.out <- tax2.out %>% 
  filter(state.fips %in% c(6,8,27,30,38,31,40)) %>% 
  group_by(year,state.fips) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),taxpc2) 

taxpc2.county <- ggplot(tax2.out, aes(x=year, y = taxpc2, colour=as.factor(state.fips))) + 
  geom_line() + 
  scale_x_continuous(breaks= tax2.years) +
  scale_colour_discrete(name= "State", labels =c("CA","CO","MN","MT","ND","NE","OK")) +
  ylab("Per-capita taxes collected by all local governments within counties ($)") +
  xlab("") 

ggsave(paste0(data.directory,"plots/taxpc2-county.png"), taxpc2.county, width=11, height=8.5)

# # tax (1)
# 
# tax1.years <- c(1870,1880,1922,1932,1942)
# 
# tax1.out <- reshape(tax1, direction="long", varying=list(names(tax1)[2:6]), v.names="taxpc1", 
#                     idvar=c("fips"), timevar="year", times=tax1.years) # reshape long
# 
# tax1.out <- tax1.out %>% 
#   filter(state.fips %in% c(6,8,27,30,38,31,40)) %>% 
#   group_by(year,state.fips) %>% 
#   summarise_each(funs(mean(., na.rm = TRUE)),taxpc1) 
# 
# taxpc1.county <- ggplot(tax1.out, aes(x=year, y = taxpc1, colour=as.factor(state.fips))) + 
#   geom_line() + 
#   scale_x_continuous(breaks= tax1.years) +
#   scale_colour_discrete(name= "State", labels =c("CA","CO","MN","MT","ND","NE","OK")) +
#   ylab("Per-capita taxes collected by counties") +
#   xlab("") 
# 
# ggsave(paste0(data.directory,"plots/taxpc1-county.png"), taxpc1.county, width=11, height=8.5)

# Education spending

educ.years <- c(1890,1932,1957,1962,1967,1972,1977,1982,1987,1992)

educ.out <- reshape(educ, direction="long", varying=list(names(educ)[2:11]), v.names="educpc", 
                    idvar=c("fips"), timevar="year", times=educ.years) # reshape long

educ.out <- educ.out %>% 
  filter(state.fips %in% c(6,8,27,30,38,31,40)) %>% 
  group_by(year,state.fips) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),educpc) 

educpc.county <- ggplot(educ.out, aes(x=year, y = educpc, colour=as.factor(state.fips))) + 
  geom_line() + 
  scale_x_continuous(breaks= educ.years) +
  scale_colour_discrete(name= "State", labels =c("CA","CO","MN","MT","ND","NE","OK")) +
  ylab("Per-capita education spending ($)") +
  xlab("") 

ggsave(paste0(data.directory,"plots/educpc-county.png"), educpc.county, width=11, height=8.5)

## Plot county-level time-series pretreatment covariates by group

c.county.cov <- RbindMatchColumns(df1, df8)

# Bind farm values time series

farmvals <- read.csv(paste0(data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmvals$year <- farmvals$year +1000

farmvals <- farmvals[farmvals$year %in% years,][c(1:5)] # keep decenial years

c.county.cov <- merge(c.county.cov, farmvals, by=c("county","state","year"),
                  all.x=TRUE)

c.county.cov <- c.county.cov[!names(c.county.cov) %in% c("beta","name.y","level")]
names(c.county.cov)[4] <- c("name")

c.county.cov <- c.county.cov[!is.na(c.county.cov$county),] # drop if missing county

# Categories

c.county.cov$cat <- ifelse(c.county.cov$state==53 & c.county.cov$county %in% c(ok.lottery), "Treated", "Control") # compare lottery counties vs. all other

# Prepare plot data
bin.melt <- melt(data.frame(c.county.cov[c("year","totpop","mtot","ftot","farm100","farm500","farm1000","farms","faval","cat")]),
                 id.vars=c("year","cat"))

county.pretreatment <- ggplot(data=bin.melt[bin.melt$year <= 1900,],aes(x=variable,y=value,colour=as.factor(cat))) + 
  scale_x_discrete(labels=c("Total pop.", "Total males","Total females", 
                            "# farms 100-499 acres" , "# farms 500-999 acres", "# farms 1000+ acres","# farms", "Farm value")) +
  geom_boxplot() +
  scale_y_log10() +
#  facet_wrap(~year,  nrow=1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="Value (common log)",x="") +
  scale_color_discrete("Group",
                       labels=c("Other U.S.", "OK lottery"))

ggsave(paste0(data.directory,"plots/county-pretreatment.png"), county.pretreatment, width=11, height=8.5)