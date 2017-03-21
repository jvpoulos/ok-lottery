#####################
### 1900 Census   ###
#####################

## 100% Sample
# Load 1900 100% sample and clean
unzip(paste0(data.directory, "census-1900-100-sample.csv.zip"), exdir=data.directory) # unzip sample
c.1900.s <- read.csv(paste0(data.directory,"census-1900-100-sample.csv"),header=TRUE, sep = ",") 

# Clean
c.1900.s <- CleanCensus(c.1900.s)