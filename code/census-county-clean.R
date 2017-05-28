#####################
### County-level censuses   ###
#####################

## Load data, subset to OK (53) and neighboring states: TX (49), KS (32), AR (42), MO (34), CO (62), NM (66)

years <- c(seq(1860,1950,10)) 

census.county <- sapply(years, function(i) {
  census.county.i <- read.csv(paste0(data.directory,"census-county/",i,".csv"), stringsAsFactors=FALSE)
  census.county.i <- census.county.i[census.county.i$state==53 | 
                                       census.county.i$state==49 | 
                                       census.county.i$state==32 |
                                       census.county.i$state==42 | 
                                       census.county.i$state==34 | 
                                       census.county.i$state==62 | 
                                       census.county.i$state==66,] # ICPSR codes
  census.county.i <- census.county.i[census.county.i$county!=0,] # remove state total
  # census.county.i <- census.county.i[!duplicated(c(census.county.i$county,census.county.i$state)),] # rm state/county dups
  census.county.i <- cbind(census.county.i, "year"=rep(i, nrow(census.county.i)))
#  census.county.i$year <- i # add year variable
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
         n.farms = sum(farm39,farm1019,farm2049,farm5099,farm100,farm500,farm1000),
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-top.farms/n.farms)^(beta)) # by construction, S is 20% in 1860

#1870, 1880

for(i in c(2:3)){ 
  census.county[[i]] <- census.county[[i]] %>%
    group_by(state,county) %>%
    mutate(G = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
           n.farms = sum(farm02,farm39,farm1019,farm2049,farm5099,farm100,farm500,farm1000),
           beta = (1+G)/(1-G),
           top.farms = n.farms*(1-(0.8)^(1/beta)),
           S= 1-(1-0.2/n.farms)^(beta))
}
summary(census.county[[2]]$n.farms) == summary(census.county[[2]]$farms)
summary(census.county[[3]]$n.farms) == summary(census.county[[3]]$farms)

census.county[[3]] <- census.county[[3]] %>% #1880 tenancy rate /farmsize
  group_by(state,county) %>%
  mutate(tenancy = sum(farmten,farmsc)/farms,
         farmsize = farmsize)

# 1890
census.county[[4]] <- census.county[[4]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm09*4.9),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
         n.farms = sum(farm09,farm1019,farm2049,farm5099,farm100,farm500,farm1000),
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/n.farms)^(beta),
         tenancy = (sum(fa09te,fa1019te,fa2049te,fa5099te,fa100te,fa500te,fa1000te,
                        fa09sc,fa1019sc,fa2049sc,fa5099sc,fa100sc,fa500sc,fa1000sc))/(farms),
         farmsize = farmsize)

summary(census.county[[4]]$n.farms) == summary(census.county[[4]]$farms)

# 1900
census.county[[5]] <- census.county[[5]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm12*1.5),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000)),
         n.farms = sum(farm12,farm39,farm1019,farm2049,farm5099,farm100,farm175,farm260,farm500,farm1000),
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/n.farms)^(beta),
         tenancy=(sum(farmwhct,farmcoct,farmwhst,farmcost)/(farms)),
         farmsize = farmsize)

summary(census.county[[5]]$n.farms) == summary(census.county[[5]]$farms)

# 1910, 1920
for(i in c(6:7)){ 
census.county[[i]] <- census.county[[i]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000)),
         n.farms = sum(farm02,farm39,farm1019,farm2049,farm5099,farm100,farm175,farm260,farm500,farm1000),
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/n.farms)^(beta),
         tenancy=(farmten)/(farms))
}

summary(census.county[[6]]$n.farms) == summary(census.county[[6]]$farms)
summary(census.county[[7]]$n.farms) == summary(census.county[[7]]$farms)

# 1930
census.county[[8]] <- census.county[[8]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000*2999.5),(farm5000)*5000)),
         n.farms = sum(farm02,farm39,farm1019,farm2049,farm5099,farm100,farm175,farm260,farm500,farm1000,farm5000),
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/n.farms)^(beta),
         tenancy=(farmten)/(farms),
         farmsize = farmsize)

summary(census.county[[8]]$n.farms) == summary(census.county[[8]]$farms)

#1940 
census.county[[9]] <- census.county[[9]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1.5),(farm39*6),(farm1029*19.5),(farm3049*39.5),(farm5069*59.5),
                    (farm7099*84.5),(farm100*119.5),(farm140*159.5),(farm175*177),(farm180*199.5),
                    (farm220*239.5),(farm260*319.5),(farm380*439.5),(farm500*599.5),(farm700*849.5),(farm1000*1000))),
#        n.farms = sum(farm02,farm39,farm1029,farm3049,farm5069,farm7099,farm100,farm140,farm175,farm180,farm220,farm260,farm380,farm500,farm700,farm1000),
         n.farms = farms,
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/n.farms)^(beta),
         tenancy=(farmten)/(farms),
         farmsize = farmsize)

#summary(census.county[[9]]$n.farms) == summary(census.county[[9]]$farms)


#1950
census.county[[10]] <- census.county[[10]] %>%
  group_by(state,county) %>%
  mutate(G = gini(c((farm02*1.5),c(farm39*6),(farm1029*19.5),(farm3049*39.5),(farm5069*59.5),
                    (farm7099*84.5),(farm100*119.5),(farm140*159.5),(farm180*199.5),
                    (farm220*239.5),(farm260*379.5),(farm500*749.5),(farm1000*1000))),
         n.farms = sum(farm02,farm39,farm1029,farm3049,farm5069,farm7099,farm100,farm140,farm180,farm220,farm260,farm500,farm1000),
         beta = (1+G)/(1-G),
         top.farms = n.farms*(1-(0.8)^(1/beta)),
         S= 1-(1-0.2/n.farms)^(beta),
         tenancy=(farmten)/(farms),
         farmsize = farmsize)

# check 
summary(census.county[[10]]$n.farms) == summary(census.county[[10]]$farms2)

## County definitions

# OK lottery counties

# Wichita & Caddo: 0110 Blaine; 0150 Caddo; 0170 Canadian; 0390 Custer; 1490 Washita; 8050 Wichita Res (NB: Canadian also contains land run )

# Comanche, Kiowa, and Apache: 0310 Comanche; 0750 Kiowa; 8030 Kiowa/Comanche/Apache Res (NB: Comanche also contains land by sealed bid (big pasture))

ok.lottery <- c(110,150,170,390,1490,310,750,8030,8050)

# OK allotment counties

# Osage:  1130 Osage; 8040 Osage Res
# Kaw:  0710 Kay; 8020 Kaw res
# Ponca: 0710 Kay; 1030 Noble; 8060 Ponca/Otoe Res
# Tonkawa: 0710 Kay
# Oto-Missouri: 1030 Noble; 1170 Pawnee
# Pawnee: 1170 Pawnee

ok.allotment <- c(1130,710,1030,1170,8020,8040,8060)

# OK sealed bid

# Big pasture: 0310 Comanche; 0330 Cotton; 1410 Tillman 

ok.sealed <- c(330,1410) #(NB: Comanche counted as land lottery lottery)

# OK land run

# Unassigned lands: 0170 Canadian; 0270 Cleveland; 0730 Kingfisher; 0830 Logan; 1090 Oklahoma; 1190 Payne 
# Iowa / Sac & Fox / - Pottawatomie & Shawnee /Kickapoo: 1250 Pottawattamie; 0270 Cleveland; 0810 Lincoln; 1190 Payne; 1090 Oklahoma
# Cheyenne & Arapaho: 0430 Dewey; 0390 Custer; 1290 Roger Mills; 0090 Beckham 
# Cherokee Outlet: 0530 Grant; 0470 Garfield; 0030 Alfalfa; 0930 Major; 1510 Woods; 1530 Woodward; 0590 Harper; 0450 Ellis

ok.run <- c(270,730,830,1090,1190,1250,810,430,390,1290,90,530,470,30,930,1510,1530,590,450) # (NB: Canadian counted as land lottery )

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

c.county1 <- RbindMatchColumns(df4, df8) #1850-1950 (Inequality)
c.county2 <- RbindMatchColumns(df2, df8) #1880-1950 (Tenancy)
c.county3 <- RbindMatchColumns(df2, RbindMatchColumns(RbindMatchColumns(census.county[[8]],census.county[[9]]),census.county[[10]])) #1880-1900,1930-1950 (farm size)

# Bind farm values time series

farmvals <- read.csv(paste0(data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmvals$year <- farmvals$year +1000

farmvals <- farmvals[farmvals$year %in% years,][c(1:5)] # keep decenial years

c.county1 <- merge(c.county1, farmvals, by=c("county","state","year"),
                  all.x=TRUE)

c.county1 <- c.county1[!names(c.county1) %in% c("beta","name.y","level")]
names(c.county1)[4] <- c("name")

c.county1 <- c.county1[!is.na(c.county1$county),] # drop if missing county

c.county2 <- merge(c.county2, farmvals, by=c("county","state","year"),
                   all.x=TRUE)

c.county2 <- c.county2[!names(c.county2) %in% c("beta","name.y","level")]
names(c.county2)[4] <- c("name")

c.county2 <- c.county2[!is.na(c.county2$county),] # drop if missing county

c.county3 <- merge(c.county3, farmvals, by=c("county","state","year"),
                   all.x=TRUE)

c.county3 <- c.county3[!names(c.county3) %in% c("beta","name.y","level")]
names(c.county3)[4] <- c("name")

c.county3 <- c.county3[!is.na(c.county3$county),] # drop if missing county

# County, state, county * state dummies

continous.vars <- c("totpop","urb25","mtot","ftot","faval")

county.x1 <- cbind("id"=as.numeric(interaction(c.county1$county, c.county1$state)), 
                  "year"=as.numeric(c.county1$year),
                  dummify(as.factor(c.county1$state)),
                  dummify(as.factor(c.county1$region1)),
                  dummify(as.factor(c.county1$region2)),
                  c.county1[continous.vars])

county.x2 <- cbind("id"=as.numeric(interaction(c.county2$county, c.county2$state)), 
                   "year"=as.numeric(c.county2$year),
                   dummify(as.factor(c.county2$state)),
                   dummify(as.factor(c.county2$region1)),
                   dummify(as.factor(c.county2$region2)),
                   c.county2[continous.vars])

county.x3 <- cbind("id"=as.numeric(interaction(c.county3$county, c.county3$state)), 
                   "year"=as.numeric(c.county3$year),
                   dummify(as.factor(c.county3$state)),
                   dummify(as.factor(c.county3$region1)),
                   dummify(as.factor(c.county3$region2)),
                   c.county3[continous.vars])

# Response variables (gini inequality) 

gini <- as.numeric(c.county1$G) # gini inequality
S <- as.numeric(c.county1$S) # land share of the largest landowners

tenancy <- as.numeric(c.county2$tenancy) # tenancy

farmsize <- as.numeric(c.county3$farmsize) # avg. farm size

# Impute missing features using proximity from randomForest

set.seed(42) 
county.x1[continous.vars] <- rfImpute(x=county.x1[continous.vars],
                     y=county.x1$id)[-1] # remove response
county.x2[continous.vars] <- rfImpute(x=county.x2[continous.vars],
                                      y=county.x2$id)[-1] 
county.x3[continous.vars] <- rfImpute(x=county.x3[continous.vars],
                                      y=county.x3$id)[-1] 

# Scale and center continuous vars
preProcValues1 <- preProcess(county.x1[continous.vars], method = c("center", "scale"))
county.x1[continous.vars] <- predict(preProcValues1, county.x1[continous.vars])

preProcValues2 <- preProcess(county.x2[continous.vars], method = c("center", "scale"))
county.x2[continous.vars] <- predict(preProcValues2, county.x2[continous.vars])

preProcValues3 <- preProcess(county.x3[continous.vars], method = c("center", "scale"))
county.x3[continous.vars] <- predict(preProcValues3, county.x3[continous.vars])
