###############################################
### Prepared linked dataset for analyses    ###
###############################################

# Rm participants with missing draw number

link.patents <- link.patents[!is.na(link.patents$draw),]

# Create dummy var for gender based on census first names
# https://github.com/SocialHarvest/harvester/blob/master/data/census-female-names.csv

female.census.names <- read.csv("/media/jason/Dropbox/github/ok-lottery/data/census-female-names.csv", header=FALSE)$V1
male.census.names <- read.csv("/media/jason/Dropbox/github/ok-lottery/data/census-male-names.csv", header=FALSE)$V1

female.census.names <- female.census.names[!female.census.names%in%male.census.names] # ensure mutually exclusive

link.patents$female <- ifelse(link.patents$first %in% female.census.names, 1, 0)

# etc.

link.patents$comply <- as.numeric(link.patents$comply)

link.patents$lawton <- ifelse(!is.na(link.patents$comply),1,0) 
  
# state and location dummies

state.dummies <- dummify(link.patents$state)[,names(sort(table(hs$state),TRUE)[sort(table(hs$state),TRUE)>200])]
loc.dummies <- dummify(link.patents$loc)[,names(sort(table(hs$loc),TRUE)[sort(table(hs$loc),TRUE)>200])]