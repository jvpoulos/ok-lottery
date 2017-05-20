#####################################
### Link participant records to GLO Records###
#####################################

## Link participants to GLO sales records
# Merge by surname, middle, and first name 

# link.sales <- merge(hs,
#                     sales,
#                     by=c("surname","middle.name","first"),
#                     all.x=TRUE)
# 
# link.sales <- link.sales[!duplicated(link.sales$hs.id),] # remove dups


# Merge by surname, middle, and first name 

link.sales <- merge(hs,
                    sales,
                    by=c("surname","middle.name","first"),
                    all.x=TRUE)

# Create link id
link.sales <- link.sales[order(link.sales$hs.id),]
link.sales$hs.link.id <- 1:nrow(link.sales)

# Create Jaro similarity measure on county

link.sales$jaro.county <- jarowinkler(link.sales$county,link.sales$County) # n.b. sales county is actually "city"
link.sales$jaro.state <- jarowinkler(link.sales$state,link.sales$State) # n.b. sales county is actually "city"

# Create exact link variables 

link.sales$exact.county <- 0
link.sales$exact.county[link.sales$county==link.sales$County] <- 1

link.sales$exact.state <- 0
link.sales$exact.state[link.sales$state==link.sales$State] <- 1

# Scale and center continuous vars
continuous <- c("jaro.county","jaro.state")
preProcValues <- preProcess(link.sales[continuous], method = c("center", "scale")) 
link.sales[continuous] <- predict(preProcValues, link.sales[continuous])

# Split train/test
bound.hs <- floor((nrow(link.sales))*0.1)         #define minimal training set

set.seed(42) # set seed for reproducibility
df.hs <- link.sales[sample(nrow(link.sales)), ]           #sample rows
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
hs.link.sales <- rbind(df.hs.train[df.hs.train$is.link==1,], df.hs.test[df.hs.test$is.link==1,])
hs.link.sales <- hs.link.sales[!duplicated(hs.link.sales$hs.id),] # remove hs dups
hs.link.sales <- hs.link.sales[!duplicated(hs.link.sales$PID),] # remove 1900 dups

# Remove feature and link vars
drops <- c(hs.features, "is.link","hs.link.id","sound.surname.1900","sound.first.1900")
hs.link.sales <- hs.link.sales[ , !(names(hs.link.sales) %in% drops)]

# Save data image
rm(tuneGrid,census.link,df,df.hs,df.hs.test,df.hs.train,hs.link.sales,hs.link.pred.test,link.sales,
   X.hs.test,X.hs.train,Y.hs.train,Y.train,df.test,df.train,link.pred.test,X.test,X.train,
   bound,bound.hs,continuous,drops, features,hs.features,hs.train.links,preProcValues,train.links) # clean up workspace

save.image(paste0(data.directory,"census-link.RData"))

# Make binary response =1 if sale record

link.sales$sale <- ifelse(!is.na(link.sales$Date),1, 0)