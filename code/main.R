run.descriptive <- FALSE
run.power <-FALSE

# Libraries
require(reshape2)
require(RecordLinkage)
require(ggplot2)
require(rdrobust)
require(doParallel)
require(data.table)
require(caret)

if(run.descriptive){
  require(reporttools)
  require(weights)
  require(plyr)
  require(gridExtra)
  require(reshape)
  require(scales)
}

# Set directories
data.directory <- "~/Dropbox/github/ok-lottery/data/"
code.directory <- "~/Dropbox/github/ok-lottery/code/"

setwd(code.directory)

# Source scripts (in order)
source("utils.R")
source("SuperLearner.R")

source("ok-participants.R") # load and clean Lawton and El Reno participants

source("census-1900-clean.R") # load 1900 100% sample and clean # States with 25+ participants

# 3            Arkansas  155
# 5            Colorado   35
# 9            Illinois  206
# 10   Indian Territory 1511
# 11            Indiana   76
# 12               Iowa  254
# 13             Kansas 2500
# 14           Kentucky   47
# 22           Missouri 1224
# 24           Nebraska  147
# 29 Oklahoma Territory 6713
# 33          Tennessee   44
# 34              Texas 1164

if(run.descriptive){
  source("descriptive.R")
}

if(run.power){
  source("ok-power.R")
}