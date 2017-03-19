#####################
### 1900 Census   ###
#####################

## 100% Sample
# Load 1900 100% sample and clean
unzip(paste0(data.directory, "census-1900-100-sample.csv.zip"), exdir=data.directory) # unzip sample
census.00 <- read.csv(paste0(data.directory,"census-1900-100-sample.csv"),header=TRUE, sep = ",") 

# Clean
census.00 <- CleanIpums(census.00,complete = TRUE)