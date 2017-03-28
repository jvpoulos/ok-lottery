#####################################
### Link participant records to census samples###
#####################################

## Link participants to 1900 census
# Merge by surname, first initial, and state

c.1900.s <- transform(c.1900.s,merge.id=paste(c.1900.s$surname,substring(c.1900.s$first, 1, 1),c.1900.s$state))

hs <- transform(hs,merge.id=paste(hs$surname,substring(hs$first, 1, 1),hs$state))

link.df <- merge(c.1900.s,hs,
                          by=c("merge.id")) 

# Create link id
link.df <- link.df[order(link.df$PID),]
link.df$hs.link.id <- 1:nrow(link.df)

# Create Jaro similarity measure on first name and surname, and county

link.df$jaro.first <- jarowinkler(link.df$first.x,as.character(link.df$first.y))
link.df$jaro.county <- jarowinkler(link.df$county.x,as.character(link.df$county.y))

# Create exact link variables 

link.df$exact.first <- 0
link.df$exact.middle.name <- 0
link.df$exact.first[link.df$first.x==link.df$first.y] <- 1
link.df$exact.middle.name[link.df$middle.name.x==link.df$middle.name.y] <- 1

link.df$exact.sound.first <- 0
link.df$exact.sound.first[link.df$sound.first.x==link.df$sound.first.y] <-1

link.df$exact.county <- 0
link.df$exact.county[link.df$county.x==link.df$county.y] <- 1

# Scale and center continuous vars
continuous <- c("jaro.first","jaro.county","first.length","surname.length")
preProcValues <- preProcess(link.df[continuous], method = c("center", "scale")) 
link.df[continuous] <- predict(preProcValues, link.df[continuous])

# Split train/test 
bound.hs <- floor((nrow(link.df))*0.01)         #define minimal training set

set.seed(42) # set seed for reproducibility
df.hs <- link.df[sample(nrow(link.df)), ]           #sample rows 
df.hs.train <- df.hs[1:bound.hs, ]              #get training set
df.hs.test <- df.hs[(bound.hs+1):nrow(df.hs), ]    #get test set

#df.hs.train[c("hs.link.id","first.x","first.y","middle.name.x","middle.name.y","surname.x","surname.y")]
df.hs.train$is.link <- 0
hs.train.links <- c(df.hs.train$hs.link.id[ df.hs.train$exact.first==1 & df.hs.train$exact.middle.name==1],
                    88331,12694,88595,13073,78781,64564,19567,85425,91727,40680,60297,73926,16135,4050,20382,
                    18594,54778,88741,215,42625, 68200,58697,76917,73295,73012,12843,81158,69832,4998,41650,
                    23745,42446,78884,90417,77160,33016,81797,29239,30008,17608,92351,30851,81497,31240,3056,
                    87438,92011,8745,40427,26286,70520,15070,37504,33792,3803,41329,6919,86867,36622,27034,30237,
                    9804,66688,44134,90468,80845,48174,54581,8081,2131,45303,91887,7871,28231,43688,61851,7416,
                    10765,89263,46043,84975,73704,12399,42213,7849,3077,28632,11906,326,8702,25963,26665,32683,
                    23580,48482,38,45546,50466,15353,41375,86336,24731,67543,20249,10121,19153,78486,7742,4327,18951,
                    66088,36057,85536,7752,88687,88068,83453,17072,38186,50237,70503,88990,48558,26309,86919,4379,1065,
                    336,32868,3449,81292,11083,26077,57711,41799,22571,73920) 

df.hs.train$is.link[df.hs.train$hs.link.id %in% hs.train.links] <-1

# Create features vector 
hs.features <- c(continous,"exact.first","exact.middle.name","exact.sound.first","exact.county")

X.hs.train <-df.hs.train[hs.features]
X.hs.test <-df.hs.test[hs.features]

if(sum((is.na(X.hs.train$jaro.first)))>0){
jaro.first.mode <- Mode(X.hs.train$jaro.first) # impute missing data with training mode
X.hs.train$jaro.first[is.na(X.hs.train$jaro.first)] <- jaro.first.mode

X.hs.test$jaro.first[is.na(X.hs.test$jaro.first)] <- jaro.first.mode
}

# Create outcomes vector
Y.hs.train <- as.matrix(df.hs.train$is.link)

# # Train
# set.seed(42)
# fitSL.hs.link <- SuperLearner(Y=Y.hs.train[,1],
#                            X=data.frame(X.hs.train),
#                            SL.library=SL.library.class,
#                            family="binomial") # glmnet response is 2-level factor
# 
# #Save pred model
# saveRDS(fitSL.hs.link, file = paste0(data.directory,"ipums-hs-link.rds"))

# Print summary table
fitSL.hs.link <- readRDS(paste0(data.directory,"ipums-hs-link.rds"))
fitSL.hs.link

# Use response model to predict test
hs.link.pred.test <- predict(fitSL.hs.link, data.frame(X.hs.test))$pred

# Add predictions to test data
X.hs.test$is.link <- ifelse(as.numeric(hs.link.pred.test)>=0.4135,1,0) 

df.hs.test$is.link <- 0 
df.hs.test$is.link[df.hs.test$hs.link.id %in% df.hs.test$hs.link.id[X.hs.test$is.link==1]] <-1

# Merge training, test links to ipums
hs.link.df <- rbind(df.hs.train[df.hs.train$is.link==1,], df.hs.test[df.hs.test$is.link==1,])
hs.link.df <- hs.link.df[!duplicated(hs.link.df$PID),] # remove 1900 dups

# Select and rename vars
hs.link.df <- hs.link.df[-c(102:109)] # remove link vars
names(hs.link.df) <- gsub(".x", "", names(hs.link.df), fixed = TRUE)

###############################

## Link participants to 1910 census
# Merge by surname, first initial, birthplace, and birth year

hs.link.df <- transform(hs.link.df,merge.id=paste0(hs.link.df$surname, substring(hs.link.df$first, 1, 1), hs.link.df$birthplace, hs.link.df$birth.year))

census.10 <- transform(census.10,merge.id=paste0(census.10$surname, substring(census10f$first, 1, 1), census.10$birthplace, census.10$birth.year))

census.link <- merge(hs.link.df,census.10,by=c("merge.id"))

# Create link id
census.link <- census.link[order(census.link$pid.x),]
census.link$link.id <- 1:nrow(census.link)

# Make factor levels comparable
census.link$self_residence_place_county.x <- factor(census.link$self_residence_place_county.x, levels=levels(census.link$self_residence_place_county.y))

# Create Jaro similarity measure on first name and surname

census.link$jaro.first <- jarowinkler(census.link$first.x,census.link$first.y)
census.link$jaro.surname <- jarowinkler(census.link$surname.x,census.link$surname.y)

# Create exact link variables

census.link$exact.middle.name <- 0
census.link$exact.surname <- 0
census.link$exact.middle.name[census.link$middle.name.x==census.link$middle.name.y] <- 1
census.link$exact.surname[census.link$surname.x==census.link$surname.y] <- 1

census.link$exact.first.length <- 0
census.link$exact.sound.first <- 0
census.link$exact.first.length[census.link$first.length.x==census.link$first.length.y] <- 1
census.link$exact.sound.first[census.link$sound.first.x==census.link$sound.first.y] <-1

census.link$exact.state <- 0
census.link$exact.county <- 0
census.link$exact.state[census.link$state.x==census.link$state.y] <- 1
census.link$exact.county[census.link$self_residence_place_county.x==census.link$self_residence_place_county.y] <- 1

# Scale and center continuous vars
preProcValues <- preProcess(census.link[continuous], method = c("center", "scale"))
census.link[continuous] <- predict(preProcValues, census.link[continuous])

# Split train/test
bound <- floor((nrow(census.link))*0.01)         #define minimal training set

set.seed(42) # set seed for reproducibility
df <- census.link[sample(nrow(census.link)), ]           #sample rows
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.link <- 0
train.links <- c()

df.train$is.link[df.train$link.id %in% train.links] <-1

# Create features vector
features <- c(hs.features,"exact.state")

X.train <-df.train[features]
X.test <-df.test[features]

# Create outcomes vector
Y.train <- as.matrix(df.train$is.link)

# Train
set.seed(42)
fitSL.link <- SuperLearner(Y=Y.train[,1],
                           X=data.frame(X.train),
                           SL.library=SL.library.class,
                           family="binomial") # glmnet response is 2-level factor

#Save pred model
saveRDS(fitSL.link, file = paste0(data.directory,"census-link.rds"))

# Print summary table
# fitSL.link <- readRDS(paste0(data.directory,"census-link.rds"))
fitSL.link

# Use response model to predict test
link.pred.test <- predict(fitSL.link, data.frame(X.test))$pred

# Add predictions to test data
X.test$is.link <- ifelse(as.numeric(link.pred.test)>0.5,1,0)

df.test$is.link <- 0
df.test$is.link[df.test$link.id %in% df.test$link.id[X.test$is.link==1]] <-1

# Merge training, test links to census
link.df <- rbind(df.train[df.train$is.link==1,], df.test[df.test$is.link==1,])
link.df <- link.df[!duplicated(link.df$pid.x),] # remove 1860 dups

# Subset and rename 1870 vars
link.df <- subset(link.df, select=c("pid.x","StableURL.y","pid.y","self_empty_name_surname.y", "self_empty_name_given.y", "self_residence_place_state.y",
                                    "self_residence_place_county.y","self_residence_place_city.y","self_residence_info_age.y"))

colnames(link.df) <- c("pid","StableURL.1870","pid.1870","self_empty_name_surname.1870", "self_empty_name_given.1870", "self_residence_place_state.1870",
                       "self_residence_place_county.1870","self_residence_place_city.1870","self_residence_info_age.1870")

# Merged to 1860 linked sample
link.1860.1870 <- merge(hs.link.df, link.df, by=c("pid"),all.x=TRUE)

# Remove link variables
drops <- c("sound.surname","first.initial","state","county","surname","first","middle.name","surname.length","first.length","sound.first")
link.1860.1870 <- link.1860.1870[ , !(names(link.1860.1870) %in% drops)]

# Write linked sample to file
write.csv(link.1860.1870, paste0(data.directory,"linked-sample-00-10.csv"))