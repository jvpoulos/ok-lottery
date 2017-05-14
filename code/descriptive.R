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

## Plot sales time series

sales.tab <- aggregate(cbind(count = State) ~ Date, 
                       data = sales, 
                       FUN = function(x){NROW(x)})

sales.tab$Date <- as.POSIXct(sales.tab$Date,format="%m/%d/%Y",tz="UTC")

sales.time <- ggplot(sales.tab, aes( Date, count )) + 
  geom_line() +
  ylab("Number of land patents by cash entry") +
  xlab("") +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, se=FALSE) # apply a locally weighted regression

ggsave(paste0(data.directory,"plots/sales-time.png"), sales.time, width=8.5, height=11)

## Plot map of homesteader origin city/state

# geocode cities with >1
cities <- as.character(count(hs$county)[count(hs$county)$freq>2,]$x)
cities <- cities[!is.na(cities)]
geocodes <- geocode(as.character(cities))

city.data <- cbind(cities, geocodes,count(hs$county)[count(hs$county)$freq>2,]$freq[1:870])
colnames(city.data) <- c("city","Longitude","Latitude","Count")

# map.us <- ggmap(get_map(location = 'oklahoma', zoom = 3)) +
#   geom_point(data=city.data, aes(x=lon, y=lat, size=count), color="orange") +
#   scale_x_continuous(limits = c(-126, -66), expand = c(0, 0)) +
#   scale_y_continuous(limits = c(25, 51), expand = c(0, 0))
# 
# ggsave(paste0(data.directory,"plots/map.us.png"), sales.time, width=8.5, height=11)

map.ok <- ggmap(get_map(location = 'oklahoma', zoom = 6)) +
  geom_point(data=city.data, aes(x=Longitude, y=Latitude, size=Count), color="orange") 

ggsave(paste0(data.directory,"plots/map.png"), map.ok, width=8.5, height=11)

## Table of homesteader states
tableNominal(vars = hs[c("state")], prec = 3,cumsum = FALSE,cap = "States of El Reno and Lawton participants at time of registration.", lab = "sum-hs")
