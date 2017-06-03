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

# TX contiguous
# 1110 Dallam
# 4210 Sherman
# 1950 Hansford
# 3570 Ochiltree
# 2950 Lipscomb
# 2110 Hemphill
# 0870 Collingsworth
# 4830 Wheeler
# 0750 Childress
# 
# 1970 Hardeman
# 4870 Wilbarger
# 4850 Wichita
# 0770 Clay
# 0970 Cooke
# 3370 Montague
# 1810 Grayson
# 1470 Fannin
# 2770 Lamar
# 3870 Red River
# 0370 Bowie

tx.contig <- c(1110,4210,1950,3570,2950,2110,0870,4830,0750,1970,4870,4850,0770,0970,3370,1810,1470,2770,3870,0370)

# KS contiguous 
# 1290 Morton
# 1890 Stevens
# 1750 Seward
# 1190 Meade
# 0250 Clark
# 0330 Comanche
# 0070 Barber/Barbour
# 0770 Harper
# 1910 Sumner
# 0350 Cowley
# 0190 Chautauqua
# 1250 Montgomery
# 0990 Labette
# 0210 Cherokee/Mcghee

ks.contig <- c(1290,1890,1750,1190,0250,0330,0070,1910,0350,0190,1250,0990,0210)

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

c.county <- RbindMatchColumns(df2, df8) #1880-1950 (Inequality)

# Bind farm values time series

farmvals <- read.csv(paste0(data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmvals$year <- farmvals$year +1000

farmvals <- farmvals[farmvals$year %in% years,][c(1:5)] # keep decenial years

c.county <- merge(c.county, farmvals, by=c("county","state","year"),
                  all.x=TRUE)

c.county <- c.county[!names(c.county) %in% c("beta","name.y","level")]
names(c.county)[4] <- c("name")

c.county <- c.county[!is.na(c.county$county),] # drop if missing county

# County, state, county * state dummies

continous.vars <- c("totpop","urb25","mtot","ftot","farms","farm100","farm500","farm1000","faval")

#Categories
# 0 : TX/KS contiguous
# 1 : OK other [allotment; sealed bid; none]
# 2: OK land run
# 3 OK Lottery
# 4: TX/KS non-contiguous

# ICPSR Codes: OK (53), TX (49), KS (32)

county.x <- cbind("id"=as.numeric(interaction(c.county$county, c.county$state)), 
                  "year"=as.numeric(c.county$year),
                  dummify(as.factor(c.county$state)),
                  dummify(as.factor(c.county$region1)),
                  c.county[continous.vars],
                  "cat" = ifelse((c.county$state==49 | c.county$state==32) & c.county$county %in% c(tx.contig,ks.contig),0,
                                 ifelse(c.county$state==53 & ! c.county$county %in% c(ok.lottery,ok.run), 1, 
                                        ifelse(c.county$state==53 & c.county$county %in% ok.run, 2,
                                               ifelse(c.county$state==53 & c.county$county %in% ok.lottery, 3, 4) ))),
                  "gini" = c.county$G,
                  "tenancy" = c.county$tenancy,
                  "faval" = c.county$faval)


# Impute missing features using proximity from randomForest

set.seed(42) 
county.x[continous.vars] <- rfImpute(x=county.x[continous.vars],
                     y=county.x$id,
                     ntree=100)[-1] # remove response

# Scale and center continuous vars
preProcValues <- preProcess(county.x[continous.vars], method = c("center", "scale"))
county.x[continous.vars] <- predict(preProcValues, county.x[continous.vars])

# Order by year, id
county.x <- county.x[with(county.x, order(year,id)), ]

# Subset data so that each id has same # timesteps 
county.x$count <- unsplit(lapply(split(county.x, county.x[c("id")]), nrow), county.x[c("id")])

county.x <- county.x[county.x$count==8,] 
#count(county.x$year) # 8 years/2511 obs (timesteps)

county.x <- county.x[!colnames(county.x) %in% c("count")] # drop count

# Export each as csv (label + features)
write.csv(cbind(gini,county.x), paste0(data.directory,"county-df.csv"))