###############################################
### Prepared linked dataset for analyses    ###
###############################################

# Rm participants with missing or invalid draw number

link.patents <- link.patents[!is.na(link.patents$draw),] #n= 13995 # there should be 13,000 participants (minus those with missing draw #s)- 6500x2

dup.draws <- as.numeric(names(which(sort(table(link.patents$draw[link.patents$draw<=6500]),decreasing = TRUE)>2)))
link.patents <- link.patents[!link.patents$draw %in% dup.draws,] # remove duplicated draws # n=13837
link.1900.1910 <- link.1900.1910[!is.na(link.1900.1910$draw),] # n=2591

# Create dummy var for gender based on census first names
# https://github.com/SocialHarvest/harvester/blob/master/data/census-female-names.csv

female.census.names <- read.csv("/media/jason/Dropbox/github/ok-lottery/data/census-female-names.csv", header=FALSE)$V1
male.census.names <- read.csv("/media/jason/Dropbox/github/ok-lottery/data/census-male-names.csv", header=FALSE)$V1

female.census.names <- female.census.names[!female.census.names%in%male.census.names] # ensure mutually exclusive

link.patents$female <- ifelse(link.patents$first %in% female.census.names, 1, 0)

link.1900.1910$female <- ifelse(link.1900.1910$first.hs %in% female.census.names, 1, 0)

# etc.

link.patents$comply <- as.numeric(link.patents$comply)
link.patents$lawton <- 0
link.patents$lawton[!is.na(link.patents$comply)] <-1 

link.patents$quintile <- quintileCut(link.patents$draw) # deciles
link.1900.1910$quintile <- quintileCut(link.1900.1910$draw) # deciles

link.patents$first.quintile <- ifelse(link.patents$quintile=="0-10",1,0)

# state and location dummies

state.dummies <- dummify(link.patents$state)[,names(sort(table(link.patents$state),TRUE)[sort(table(link.patents$state),TRUE)>1])]
loc.dummies <- dummify(link.patents$loc)[,names(sort(table(link.patents$loc),TRUE)[sort(table(link.patents$loc),TRUE)>=100])]

state.dummies <- state.dummies[ , order(colnames(state.dummies))]
loc.dummies <- loc.dummies[ , order(colnames(loc.dummies))]

# for descriptive plots
binary.covars <-c("lawton", colnames(state.dummies), colnames(loc.dummies))

binary.outcomes <- c("sale","homestead")
continuous.outcomes <-c("sales","homesteads","total_acres")

# Balance vars are state + city/state

balance.vars <- c("female",binary.covars)

# subset to nonmissing outcomes
link.1900.1910.subs <- link.1900.1910[!is.na(link.1900.1910$farm) & !is.na(link.1900.1910$own),] # n=397

state.dummies.census <- dummify(link.1900.1910.subs$state.1900)[,names(sort(table(link.1900.1910.subs$state.1900),TRUE)[sort(table(link.1900.1910.subs$state.1900),TRUE)>1])]

# Census covars

census.covars <- cbind(state.dummies.census,
                       "black"=ifelse(link.1900.1910.subs$general_self_empty_info_race_multiple_1=="Black",1,0),
                       "white"=ifelse(link.1900.1910.subs$general_self_empty_info_race_multiple_1=="White",1,0),
                       dummify(cut(link.1900.1910.subs$self_residence_info_age,5)),
                       "female"=link.1900.1910.subs$female)

# Census covars (1900 linked)

state.dummies.census.1900 <- dummify(link.1900.1910$state.1900)[,names(sort(table(link.1900.1910$state.1900),TRUE)[sort(table(link.1900.1910$state.1900),TRUE)>1])]

census.covars.1900 <- cbind(state.dummies.census.1900,
                       "black"=ifelse(link.1900.1910$general_self_empty_info_race_multiple_1=="Black",1,0),
                       "white"=ifelse(link.1900.1910$general_self_empty_info_race_multiple_1=="White",1,0),
                       dummify(cut(link.1900.1910$self_residence_info_age,5)),
                       "female"=link.1900.1910$female)