#####################
### 1910 Census   ###
#####################

## 110% Sample
# Load 1910 100% sample and clean
unzip(paste0(data.directory, "census-1910-100-sample.csv.zip"), exdir=data.directory) # unzip sample
c.1910.s <- read.csv(paste0(data.directory,"census-1910-100-sample.csv"),header=TRUE, sep = ",") 

# Clean
c.1910.s <- CleanCensus(c.1910.s)