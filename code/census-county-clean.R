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
## avg farm size (1880-1900,1930-1950)

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

c.county <- RbindMatchColumns(df2, df8) #1880-1950

# Bind farm values time series

farmvals <- read.csv(paste0(data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmvals$year <- farmvals$year +1000

farmvals <- farmvals[farmvals$year %in% years,][c(1:5)] # keep decenial years

c.county <- merge(c.county, farmvals, by=c("county","state","year"),
                  all.x=TRUE)

c.county <- c.county[!names(c.county) %in% c("beta","name.y","level")]
names(c.county)[4] <- c("name")

c.county <- c.county[!is.na(c.county$county),] # drop if missing county

# Create county category var 

# Categories
# 0: Other state
# 1 : OK none
# 2: OK sealed bid
# 3 : OK allotment
# 4: OK land run
# 5 OK Lottery

c.county$cat <- ifelse(c.county$state!=53,0,
       ifelse(c.county$state==53 & ! c.county$county %in% c(ok.lottery,ok.run,ok.allotment,ok.sealed), 1, 
              ifelse(c.county$state==53 & c.county$county %in% ok.sealed, 2,
                     ifelse(c.county$state==53 & c.county$county %in% ok.allotment, 3,
                            ifelse(c.county$state==53 & c.county$county %in% ok.run, 4,
                                   ifelse(c.county$state==53 & c.county$county %in% ok.lottery, 5, NA) )))))

## Create time series df

continuous.vars <- c("totpop","urb25","mtot","ftot","farms","farm100","farm500","farm1000","faval")

# ICPSR Codes: OK (53), TX (49), KS (32)

county.x <- cbind("id"=as.numeric(interaction(c.county$county, c.county$state)), 
                  "year"=as.numeric(c.county$year),
                  dummify(as.factor(c.county$state)),
                  dummify(as.factor(c.county$region1)),
                  c.county[continuous.vars],
                  "gini" = c.county$G,
                  "tenancy" = c.county$tenancy)

# Order by id, year
county.x <- county.x[with(county.x, order(id,year)), ]

# Drop IDs with missing labels

drops.x <- sort(unique(c(county.x$id[is.na(county.x$gini)], county.x$id[is.na(county.x$tenancy)])))

county.x <- county.x[!county.x$id %in% drops.x,]

# Impute missing features using proximity from randomForest

set.seed(42) 
county.x[continuous.vars] <- rfImpute(x=county.x[continuous.vars],
                                     y=county.x$gini,
                                     ntree=50)[-1] # remove response

# Scale and center continuous vars
preProcValues <- preProcess(county.x[continuous.vars], method = c("center", "scale"))
county.x[continuous.vars] <- predict(preProcValues, county.x[continuous.vars])

# Train/test splits
county.x.train <- county.x[county.x$year <=1900,]
county.x.test <- county.x[county.x$year > 1900,]

# Export each as csv (label + features)
write.csv(county.x.train, paste0(data.directory,"county-x-train.csv"), row.names=FALSE) # counties with nonmissing farm and tenancy data
write.csv(county.x.test, paste0(data.directory,"county-x-test.csv"), row.names=FALSE)