#####################################
### Link participant records to census samples###
#####################################

## Link participants to 1900 census
# Merge by surname, first initial, and state

c.1900.s <- transform(c.1900.s,merge.id.1900=paste(c.1900.s$surname,substring(c.1900.s$first, 1, 1),c.1900.s$state))

hs <- transform(hs,merge.id.1900=paste(hs$surname,substring(hs$first, 1, 1),hs$state))


link.df <- merge(c.1900.s,hs,
                          by=c("merge.id.1900"),
                 suffixes = c(".hs",".1900")) 

# Create link id
link.df <- link.df[order(link.df$PID),]
link.df$hs.link.id <- 1:nrow(link.df)

# Create Jaro similarity measure on first name and surname, and county

link.df$jaro.first <- jarowinkler(link.df$first.hs,as.character(link.df$first.1900))
link.df$jaro.county <- jarowinkler(link.df$county.hs,as.character(link.df$county.1900))

# Create exact link variables 

link.df$exact.first <- 0
link.df$exact.middle.name <- 0
link.df$exact.first[link.df$first.hs==link.df$first.1900] <- 1
link.df$exact.middle.name[link.df$middle.name.hs==link.df$middle.name.1900] <- 1

link.df$exact.sound.first <- 0
link.df$exact.sound.first[link.df$sound.first.hs==link.df$sound.first.1900] <-1

link.df$exact.county <- 0
link.df$exact.county[link.df$county.hs==link.df$county.1900] <- 1

# Scale and center continuous vars
continuous <- c("jaro.first","jaro.county","first.length","surname.length")
preProcValues <- preProcess(link.df[continuous], method = c("center", "scale")) 
link.df[continuous] <- predict(preProcValues, link.df[continuous])

# Split train/test
bound.hs <- floor((nrow(link.df))*0.1)         #define minimal training set

set.seed(42) # set seed for reproducibility
df.hs <- link.df[sample(nrow(link.df)), ]           #sample rows
df.hs.train <- df.hs[1:bound.hs, ]              #get training set
df.hs.test <- df.hs[(bound.hs+1):nrow(df.hs), ]    #get test set

#df.hs.train[c("hs.link.id","first.hs","first.1900","middle.name.hs","middle.name.1900","surname.hs","surname.1900")]
df.hs.train$is.link <- 0
hs.train.links <- c(df.hs.train$hs.link.id[ df.hs.train$exact.first==1 & df.hs.train$exact.middle.name==1],
                    df.hs.train$hs.link.id[ df.hs.train$exact.first==1 & is.na(df.hs.train$middle.name.1900)],
                    df.hs.train$hs.link.id[ df.hs.train$exact.first==1 & is.na(df.hs.train$middle.name.hs)])

df.hs.train$is.link[df.hs.train$hs.link.id %in% hs.train.links] <-1

# Create features vector
hs.features <- c(continuous,"exact.first","exact.middle.name","exact.sound.first","exact.county")

X.hs.train <-df.hs.train[hs.features]
X.hs.test <-df.hs.test[hs.features]

if(sum((is.na(X.hs.train$jaro.first)))>0){
jaro.first.mode <- Mode(X.hs.train$jaro.first) # impute missing data with training mode
X.hs.train$jaro.first[is.na(X.hs.train$jaro.first)] <- jaro.first.mode

X.hs.test$jaro.first[is.na(X.hs.test$jaro.first)] <- jaro.first.mode
}

# Create outcomes vector
Y.hs.train <- as.matrix(df.hs.train$is.link)
# 
# Train
# set.seed(42)
# fitSL.hs.link <- SuperLearner(Y=Y.hs.train[,1],
#                            X=data.frame(X.hs.train),
#                            SL.library=SL.library.class,
#                            family="binomial") # glmnet response is 2-level factor
# 
# #Save pred model
# saveRDS(fitSL.hs.link, file = paste0(data.directory,"census-hs-link.rds"))

# Open model & print summary table
fitSL.hs.link <- readRDS(paste0(data.directory,"census-hs-link.rds"))
fitSL.hs.link

# Use response model to predict test
hs.link.pred.test <- predict(fitSL.hs.link, data.frame(X.hs.test))$pred

# Add predictions to test data
X.hs.test$is.link <- ifelse(as.numeric(hs.link.pred.test)>0.5,1,0) 

df.hs.test$is.link <- 0 
df.hs.test$is.link[df.hs.test$hs.link.id %in% df.hs.test$hs.link.id[X.hs.test$is.link==1]] <-1

# Merge training, test links to ipums
hs.link.df <- rbind(df.hs.train[df.hs.train$is.link==1,], df.hs.test[df.hs.test$is.link==1,])
hs.link.df <- hs.link.df[!duplicated(hs.link.df$hs.id),] # remove hs dups
hs.link.df <- hs.link.df[!duplicated(hs.link.df$PID),] # remove 1900 dups

# Remove feature and link vars
drops <- c(hs.features, "is.link","hs.link.id","sound.surname.1900","sound.first.1900")
hs.link.df <- hs.link.df[ , !(names(hs.link.df) %in% drops)]

# Out of 14,235 participants, 2,610 linked to 1900 Census ( 0.1833509)
# Bleakly (manual): 1,580/6,500 (0.2430769)

############################################################################################################################

## Link participants to 1910 census
# Merge by surname, first initial, birthplace, and birth year

hs.link.df <- transform(hs.link.df,merge.id.1910=paste(hs.link.df$surname.hs, substring(hs.link.df$first.hs, 1, 1), hs.link.df$birthplace, hs.link.df$birth.year))

c.1910.s <- transform(c.1910.s,merge.id.1910=paste(c.1910.s$surname, substring(c.1910.s$first, 1, 1), c.1910.s$birthplace, c.1910.s$birth.year))

census.link <- merge(hs.link.df,c.1910.s,by=c("merge.id.1910"), 
                     suffixes = c(".1900",".1910")) 

# Create link id
census.link <- census.link[order(census.link$pid),]
census.link$link.id <- 1:nrow(census.link)

# Make factor levels comparable
census.link$state <- factor(census.link$state, levels=levels(census.link$state.1900))

# Create Jaro similarity measure on first name and surname

census.link$jaro.first <- jarowinkler(census.link$first,as.character(census.link$first.1900))
census.link$jaro.surname <- jarowinkler(census.link$surname,as.character(census.link$surname.1900))

census.link$jaro.county <- jarowinkler(census.link$county,as.character(census.link$county.1900))

# Create exact link variables

census.link$exact.middle.name <- 0
census.link$exact.first <- 0
census.link$exact.middle.name[census.link$middle.name==census.link$middle.name.1900] <- 1
census.link$exact.first[census.link$first==census.link$first.1900] <- 1

census.link$exact.sound.first <- 0
census.link$exact.sound.first[census.link$sound.first==census.link$sound.first.1900] <-1

census.link$exact.state <- 0
census.link$exact.county <- 0
census.link$exact.state[census.link$state==census.link$state.1900] <- 1
census.link$exact.county[census.link$county==census.link$county.1900] <- 1

# Scale and center continuous vars
preProcValues <- preProcess(census.link[continuous], method = c("center", "scale"))
census.link[continuous] <- predict(preProcValues, census.link[continuous])

# Split train/test
bound <- floor((nrow(census.link))*0.1)         #define minimal training set

set.seed(42) # set seed for reproducibility
df <- census.link[sample(nrow(census.link)), ]           #sample rows
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.link <- 0
train.links <- c(df.train$link.id[ df.train$exact.first==1 & df.train$exact.middle.name==1],
                  df.train$link.id[ df.train$exact.first==1 & is.na(df.train$middle.name.1900)],
                  df.train$link.id[ df.train$exact.first==1 & is.na(df.train$middle.name)],
                  df.train$link.id[ df.train$exact.first==1 & df.train$exact.state==1],
                  df.train$link.id[ df.train$exact.county==1 & df.train$exact.state==1])

df.train$is.link[df.train$link.id %in% train.links] <-1

# Create features vector
features <- c(hs.features,"exact.state")

X.train <-df.train[features]
X.test <-df.test[features]

# Create outcomes vector
Y.train <- as.matrix(df.train$is.link)

# Train
# set.seed(42)
# fitSL.link <- SuperLearner(Y=Y.train[,1],
#                            X=data.frame(X.train),
#                            SL.library=SL.library.class,
#                            family="binomial") # glmnet response is 2-level factor
#
# #Save pred model
# saveRDS(fitSL.link, file = paste0(data.directory,"census-link.rds"))

# Print summary table
fitSL.link <- readRDS(paste0(data.directory,"census-link.rds"))
fitSL.link

# Use response model to predict test
link.pred.test <- predict(fitSL.link, data.frame(X.test))$pred

# Add predictions to test data
X.test$is.link <- ifelse(as.numeric(link.pred.test)>0.5,1,0)

df.test$is.link <- 0
df.test$is.link[df.test$link.id %in% df.test$link.id[X.test$is.link==1]] <-1

# Merge training, test links to census
link.df <- rbind(df.train[df.train$is.link==1,], df.test[df.test$is.link==1,])
link.df <- link.df[!duplicated(link.df$hs.id),] # remove hs dups
link.df <- link.df[!duplicated(link.df$PID),] # remove 1900 dups
link.df <- link.df[!duplicated(link.df$pid),] # remove 1910 dups

# drop features and link vars
drops <- c(drops, features, "link.id","is.link","sound.surname.hs","sound.first.hs","surname.length","first.length","sound.surname","sound.first","jaro.surname")
link.df <- link.df[ , !(names(link.df) %in% drops)]
link.df <- link.df[ , !(names(link.df) %in% names(hs.link.df)[-99])] # keep unique (1910) vars and hs ID

# Merged to 1900 linked sample
link.1900.1910 <- merge(hs.link.df, link.df, by=c("hs.id"), all.x=TRUE)

# Write linked sample to file
write.csv(link.1900.1910, paste0(data.directory,"linked-sample-00-10.csv"))

# Save data image
rm(tuneGrid,census.link,df,df.hs,df.hs.test,df.hs.train,hs.link.df,hs.link.pred.test,link.df,
   X.hs.test,X.hs.train,Y.hs.train,Y.train,df.test,df.train,link.pred.test,X.test,X.train,
   bound,bound.hs,continuous,drops, features,hs.features,hs.train.links,preProcValues,train.links) # clean up workspace

save.image(paste0(data.directory,"record-link.RData"))
           
# Out of 2,619 linked to 1900 Census, 439 linked to 1910 census (0.1676212)
# Bleakly (manual): 529/1580 (0.3348101)