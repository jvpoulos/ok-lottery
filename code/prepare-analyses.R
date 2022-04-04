###############################################
### Prepared linked dataset for analyses    ###
###############################################

# Rm participants with missing draw number

link.patents <- link.patents[!is.na(link.patents$draw),]
## there should be 13,000 participants (minus those with missing draw #s)- 6500x2

# Create dummy var for gender based on census first names
# https://github.com/SocialHarvest/harvester/blob/master/data/census-female-names.csv

female.census.names <- read.csv("/media/jason/Dropbox/github/ok-lottery/data/census-female-names.csv", header=FALSE)$V1
male.census.names <- read.csv("/media/jason/Dropbox/github/ok-lottery/data/census-male-names.csv", header=FALSE)$V1

female.census.names <- female.census.names[!female.census.names%in%male.census.names] # ensure mutually exclusive

link.patents$female <- ifelse(link.patents$first %in% female.census.names, 1, 0)

lawton$female <- ifelse(lawton$first %in% female.census.names, 1, 0)

# etc.

link.patents$comply <- as.numeric(link.patents$comply)

link.patents$lawton <- ifelse(!is.na(link.patents$comply),1,0) 

link.patents$quintile <- quintileCut(link.patents$draw) # quintiles

link.patents$first.quintile <- ifelse(link.patents$quintile=="0-10",1,0)

# state and location dummies

state.dummies <- dummify(link.patents$state)[,names(sort(table(link.patents$state),TRUE)[sort(table(link.patents$state),TRUE)>200])]
loc.dummies <- dummify(link.patents$loc)[,names(sort(table(link.patents$loc),TRUE)[sort(table(link.patents$loc),TRUE)>=200])]