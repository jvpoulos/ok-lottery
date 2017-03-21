#####################################
### Link participant records to census samples###
#####################################

## Link participants to 1900 census
# Merge by soundex surname/first name, state and county

c.1900.s <- transform(c.1900.s,merge.id=paste(c.1900.s$sound.surname,c.1900.s$sound.first,c.1900.s$county,c.1900.s$state))

hs <- transform(hs,merge.id=paste(hs$sound.surname,hs$sound.first,hs$county,hs$state))

link.df <- merge(c.1900.s,hs,
                          by=c("merge.id")) 

# Create link id
link.df <- link.df[order(link.df$pid),]
link.df$hs.link.id <- 1:nrow(link.df)

# Create Jaro similarity measure on first name and surname

link.df$jaro.first <- jarowinkler(link.df$first.x,link.df$first.y)
link.df$jaro.surname <- jarowinkler(link.df$surname.x,link.df$surname.y)

# Create exact link variables

link.df$exact.first <- 0
link.df$exact.middle.name <- 0
link.df$exact.surname <- 0
link.df$exact.first[link.df$first.x==link.df$first.y] <- 1
link.df$exact.middle.name[link.df$middle.name.x==link.df$middle.name.y] <- 1
link.df$exact.surname[link.df$surname.x==link.df$surname.y] <- 1

link.df$exact.first.length <- 0
link.df$exact.surname.length <- 0
link.df$exact.sound.first <- 0
link.df$exact.sound.first[link.df$sound.first.x==link.df$sound.first.y] <-1

link.df$exact.state <- 0
link.df$exact.county <- 0
link.df$exact.state[link.df$state.x==link.df$state.y] <- 1
link.df$exact.county[link.df$self_residence_place_county.x==link.df$self_residence_place_county.y] <- 1

link.df$exact.age <- 0
link.df$exact.age[link.df$self_residence_info_age.x+10==link.df$self_residence_info_age.y] <- 1

# Scale and center continuous vars
preProcValues <- preProcess(link.df[c("jaro.first","jaro.surname")], method = c("center", "scale")) 
link.df[c("jaro.first","jaro.surname")] <- predict(preProcValues, link.df[c("jaro.first","jaro.surname")])

# Split train/test 
bound.hs <- floor((nrow(link.df))*0.1)         #define minimal training set

set.seed(42) # set seed for reproducibility
df.hs <- link.df[sample(nrow(link.df)), ]           #sample rows 
df.hs.train <- df.hs[1:bound.hs, ]              #get training set
df.hs.test <- df.hs[(bound.hs+1):nrow(df.hs), ]    #get test set

df.hs.train$is.link <- 0
hs.train.links <- c(df.hs.train$hs.link.id[ df.hs.train$exact.surname==1],
                       21106,10724,10797,16199,2026,10419,3820,7104,2988,4362,10848,5813,4148,
                       21016,480,3815,7031,14552,6504,12819,16455,16012,19072,10702,18685,20600,
                       10578,15730,2337,15845,15233,4490,8648,13329,11057,4195,1258,21025,20623,
                       3494,13403,16203,8764,15350,17172,587,9648,657,161,4204,11448,13482,
                       13284,17354,11694,5277,1694,15882,4029,4067,10569,13660,14998,17272,11009,
                       13856,17175,16024,8155,21199,10383,3503,21618,5633,16030,12145,21196,
                       18411,12322,11952,11627,13851,1685,10358,15031,391,8522,15165,4058,
                       21123,3102,1455,14157,5351,9641,8550,3069,101,8885,22586,10920,4825,
                       4998,6258,9663,7466,8173,4032,1468,1213,1676,17054,22112,5758,14358,
                       12329,8600,6509,15880,21588,16716,17585,15997) 

df.hs.train$is.link[df.hs.train$hs.link.id %in% hs.train.links] <-1

# Create features vector
hs.features <- c("jaro.first","jaro.surname","exact.first","exact.middle.name","exact.surname","exact.sound.first","exact.state","exact.county")

X.hs.train <-df.hs.train[hs.features]
X.hs.test <-df.hs.test[hs.features]

jaro.first.mode <- Mode(X.hs.train$jaro.first) # impute missing data with training mode
X.hs.train$jaro.first[is.na(X.hs.train$jaro.first)] <- jaro.first.mode

X.hs.test$jaro.first[is.na(X.hs.test$jaro.first)] <- jaro.first.mode

# Create outcomes vector
Y.hs.train <- as.matrix(df.hs.train$is.link)

Train
set.seed(42)
fitSL.hs.link <- SuperLearner(Y=Y.hs.train[,1],
                           X=data.frame(X.hs.train),
                           SL.library=SL.library.class,
                           family="binomial") # glmnet response is 2-level factor

#Save pred model
saveRDS(fitSL.hs.link, file = paste0(data.directory,"ipums-hs-link.rds"))

# Print summary table
# fitSL.hs.link <- readRDS(paste0(data.directory,"ipums-hs-link.rds"))
fitSL.hs.link

# Use response model to predict test
hs.link.pred.test <- predict(fitSL.hs.link, data.frame(X.hs.test))$pred

# Add predictions to test data
X.hs.test$is.link <- ifelse(as.numeric(hs.link.pred.test)>0.5,1,0) 

df.hs.test$is.link <- 0 
df.hs.test$is.link[df.hs.test$hs.link.id %in% df.hs.test$hs.link.id[X.hs.test$is.link==1]] <-1

# Merge training, test links to ipums
hs.link.df <- rbind(df.hs.train[df.hs.train$is.link==1,], df.hs.test[df.hs.test$is.link==1,])
hs.link.df <- hs.link.df[!duplicated(hs.link.df$pid),] # remove 1860 dups

# Select and rename hspums vars
hs.link.df <- hs.link.df[c(2:47,56)] # remove link vars
names(hs.link.df) <- gsub(".x", "", names(hs.link.df), fixed = TRUE)

###############################

## Link participants to 1910 census
# Merge by soundex surname/first name, birthplace

hs.link.df <- transform(hs.link.df,merge.id=paste0(hs.link.df$sound.surname, hs.link.df$sound.first, " ", hs.link.df$birthplace))

census.10 <- transform(census.10,merge.id=paste0(census.10$sound.surname, census.10$sound.first, " ", census.10$birthplace))

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

census.link$exact.first <- 0
census.link$exact.middle.name <- 0
census.link$exact.surname <- 0
census.link$exact.first[census.link$first.x==census.link$first.y] <- 1
census.link$exact.middle.name[census.link$middle.name.x==census.link$middle.name.y] <- 1
census.link$exact.surname[census.link$surname.x==census.link$surname.y] <- 1

census.link$exact.first.length <- 0
census.link$exact.surname.length <- 0
census.link$exact.sound.first <- 0
census.link$exact.first.length[census.link$first.length.x==census.link$first.length.y] <- 1
census.link$exact.surname.length[census.link$surname.length.x==census.link$surname.length.y] <- 1
census.link$exact.sound.first[census.link$sound.first.x==census.link$sound.first.y] <-1

census.link$exact.state <- 0
census.link$exact.county <- 0
census.link$exact.state[census.link$state.x==census.link$state.y] <- 1
census.link$exact.county[census.link$self_residence_place_county.x==census.link$self_residence_place_county.y] <- 1

census.link$exact.age <- 0
census.link$exact.age[census.link$self_residence_info_age.x+10==census.link$self_residence_info_age.y] <- 1

# Scale and center continuous vars
preProcValues <- preProcess(census.link[c("jaro.first","jaro.surname")], method = c("center", "scale"))
census.link[c("jaro.first","jaro.surname")] <- predict(preProcValues, census.link[c("jaro.first","jaro.surname")])

# Split train/test
bound <- floor((nrow(census.link))*0.1)         #define minimal training set

set.seed(42) # set seed for reproducibility
df <- census.link[sample(nrow(census.link)), ]           #sample rows
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.link <- 0
train.links <- c(df.train$link.id[(df.train$exact.first + df.train$exact.surname)==2 |
                                    (df.train$exact.first + df.train$exact.surname + df.train$exact.state + df.train$exact.county + df.train$exact.age)>=3],
                 749405,792867,787958,254092,677719,393384,156054,749487,425576)

df.train$is.link[df.train$link.id %in% train.links] <-1

# Create features vector
features <- c("jaro.first","jaro.surname","exact.first","exact.middle.name","exact.surname",
              "exact.first.length","exact.surname.length","exact.sound.first","exact.state","exact.county","exact.age")

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