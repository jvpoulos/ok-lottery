#####################
### County-level censuses   ###
#####################

## Load data

years <- c(seq(1860,1950,10)) 

census.county <- sapply(years, function(i) {
  census.county.i <- read.csv(paste0(data.directory,"census-county/",i,".csv"), stringsAsFactors=FALSE)
  census.county.i <- census.county.i[census.county.i$county!=0,] # remove state total
  census.county.i <- cbind(census.county.i, "year"=rep(i, nrow(census.county.i)))
}
)

names(census.county) <- years

## Create response variables for: 
## land inequality (1860-1950)
## farm tenancy (1880-1950)

#1860 # no # farms
census.county[[1]] <- census.county[[1]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
         farms = sum(farm39,farm1019,farm2049,farm5099,farm100,farm500,farm1000),
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-top.farms/farms)^(beta)) %>% # by construction, S is 20% in 1860
         filter(!is.na(farms))  # remove counties with NA farms

#1870, 1880

for(i in c(2:3)){ 
  census.county[[i]] <- census.county[[i]] %>%
    filter(!is.na(farms)) %>%
    group_by(state,county) %>%
    mutate(G = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
           beta = (1+G)/(1-G),
           top.farms = farms*(1-(0.8)^(1/beta)),
           S= 1-(1-0.2/farms)^(beta))
}

census.county[[3]] <- census.county[[3]] %>% #1880 tenancy rate /farmsize
  group_by(state,county) %>%
  mutate(tenancy = sum(farmten,farmsc)/farms,
         farmsize = farmsize)

# 1890
census.county[[4]] <- census.county[[4]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm09*4.9),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/farms)^(beta),
         tenancy = (sum(fa09te,fa1019te,fa2049te,fa5099te,fa100te,fa500te,fa1000te,
                        fa09sc,fa1019sc,fa2049sc,fa5099sc,fa100sc,fa500sc,fa1000sc))/(farms),
         farmsize = farmsize)


# 1900
census.county[[5]] <- census.county[[5]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm12*1.5),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000)),
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/farms)^(beta),
         tenancy=(sum(farmwhct,farmcoct,farmwhst,farmcost)/(farms)),
         farmsize = farmsize)


# 1910, 1920
for(i in c(6:7)){ 
census.county[[i]] <- census.county[[i]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000)),
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/farms)^(beta),
         tenancy=(farmten)/(farms))
}

# 1930
census.county[[8]] <- census.county[[8]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000*2999.5),(farm5000)*5000)),
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/farms)^(beta),
         tenancy=(farmten)/(farms),
         farmsize = farmsize)

#1940 
census.county[[9]] <- census.county[[9]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1.5),(farm39*6),(farm1029*19.5),(farm3049*39.5),(farm5069*59.5),
                    (farm7099*84.5),(farm100*119.5),(farm140*159.5),(farm175*177),(farm180*199.5),
                    (farm220*239.5),(farm260*319.5),(farm380*439.5),(farm500*599.5),(farm700*849.5),(farm1000*1000))),
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/farms)^(beta),
         tenancy=(farmten)/(farms),
         farmsize = farmsize)


#1950
census.county[[10]] <- census.county[[10]] %>%
  filter(!is.na(farms2)) %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1.5),c(farm39*6),(farm1029*19.5),(farm3049*39.5),(farm5069*59.5),
                    (farm7099*84.5),(farm100*119.5),(farm140*159.5),(farm180*199.5),
                    (farm220*239.5),(farm260*379.5),(farm500*749.5),(farm1000*1000))),
         farms = farms2,
         beta = (1+G)/(1-G),
         top.farms = farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/farms)^(beta),
         tenancy=(farmten)/(farms),
         farmsize = farmsize)

## Create time-series 

# Rbind by common column names
df1 <- RbindMatchColumns(census.county[[5]], census.county[[4]]) # backward merge
df2 <- RbindMatchColumns(df1, census.county[[3]])
df3 <- RbindMatchColumns(df2, census.county[[2]])
df4 <- RbindMatchColumns(df3, census.county[[1]])

df5 <- RbindMatchColumns(census.county[[6]], census.county[[7]]) # forward merge
df6 <- RbindMatchColumns(df5, census.county[[8]]) 
df7 <- RbindMatchColumns(df6, census.county[[9]]) 
df8 <- RbindMatchColumns(df7, census.county[[10]]) 

c.county <- RbindMatchColumns(df1, df8)[c("year","state", "county", "G", "tenancy")] #1890-1950 # Gini & Tenancy

# Year to time

c.county$date <- as.yearmon(paste0("12/01/",c.county$year), "%m/%d/%Y",tz="UTC") # convert to monthly data

# Create county category var 

c.county$cat <- ifelse(c.county$state==53 & c.county$county %in% c(ok.lottery), "Treated", "Control") # compare lottery counties vs. all other

## Create time series df

# Create control and treated sums
cats.sums <- c.county %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),G, tenancy) 

cats.sums <- reshape(data.frame(cats.sums), idvar = "date", timevar = "cat", direction = "wide")

# Reshape long to wide

c.county$id <- interaction(c.county$county,c.county$state)

c.county <- c.county[c.county$cat=="Control",] # discard treated since we have treated time-series

gini <- reshape(data.frame(c.county[c("date","id","G")]), idvar = "date", timevar = "id", direction = "wide")

tenancy <- reshape(data.frame(c.county[c("date","id","tenancy")]), idvar = "date", timevar = "id", direction = "wide")

# Labels

gini.y <- cats.sums[c("date", "G.Treated")]

tenancy.y <- cats.sums[c("date", "tenancy.Treated")]

# Splits

gini.x.train <- gini[gini$date %in% gini.y$date & gini$date <= "Dec 1900",]
gini.x.test <- gini[gini$date %in% gini.y$date & gini$date > "Dec 1900",]

gini.y.train <- gini.y[gini.y$date <= "Dec 1900",]
gini.y.test <- gini.y[gini.y$date > "Dec 1900",]

tenancy.x.train <- tenancy[tenancy$date %in% tenancy.y$date & tenancy$date <= "Dec 1900",]
tenancy.x.test <- tenancy[tenancy$date %in% tenancy.y$date & tenancy$date > "Dec 1900",]

tenancy.y.train <- tenancy.y[tenancy.y$date <= "Dec 1900",]
tenancy.y.test <- tenancy.y[tenancy.y$date > "Dec 1900",]

# Preprocess
gini.pre.train <- preProcess(gini.x.train[!colnames(gini.x.train) %in% c("date")], method = c("medianImpute"))
gini.x.train[!colnames(gini.x.train) %in% c("date")] <- predict(gini.pre.train, gini.x.train[!colnames(gini.x.train) %in% c("date")] )

gini.x.test[!colnames(gini.x.test) %in% c("date")] <- predict(gini.pre.train, gini.x.test[!colnames(gini.x.test) %in% c("date")] ) # use training values for test set 

tenancy.pre.train <- preProcess(tenancy.x.train[!colnames(tenancy.x.train) %in% c("date")], method = c("medianImpute"))
tenancy.x.train[!colnames(tenancy.x.train) %in% c("date")] <- predict(tenancy.pre.train, tenancy.x.train[!colnames(tenancy.x.train) %in% c("date")] )

tenancy.x.test[!colnames(tenancy.x.test) %in% c("date")] <- predict(tenancy.pre.train, tenancy.x.test[!colnames(tenancy.x.test) %in% c("date")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/census/"

write.csv(gini.x.train[!colnames(gini.x.train) %in% c("date")], paste0(data.directory,"gini-x-train.csv"), row.names=FALSE) 
write.csv(gini.x.test[!colnames(gini.x.test) %in% c("date")], paste0(data.directory,"gini-x-test.csv"), row.names=FALSE) 
write.csv(gini.y.train[!colnames(gini.y.train) %in% c("date")], paste0(data.directory,"gini-y-train.csv"), row.names=FALSE) 
write.csv(gini.y.test[!colnames(gini.y.test) %in% c("date")], paste0(data.directory,"gini-y-test.csv"), row.names=FALSE) 

write.csv(tenancy.x.train[!colnames(tenancy.x.train) %in% c("date")], paste0(data.directory,"tenancy-x-train.csv"), row.names=FALSE) 
write.csv(tenancy.x.test[!colnames(tenancy.x.test) %in% c("date")] , paste0(data.directory,"tenancy-x-test.csv"), row.names=FALSE) 
write.csv(tenancy.y.train[!colnames(tenancy.y.train) %in% c("date")], paste0(data.directory,"tenancy-y-train.csv"), row.names=FALSE) 
write.csv(tenancy.y.test[!colnames(tenancy.y.test) %in% c("date")], paste0(data.directory,"tenancy-y-test.csv"), row.names=FALSE) 