run.descriptive <- FALSE
run.power <-FALSE

# Libraries
require(reshape2)
require(RecordLinkage)
require(ggplot2)
require(doParallel)
require(data.table)
require(caret)
require(stringr)

if(run.descriptive){
  require(ggmap)
  require(maps)
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

#source("census-1900-clean.R") # load 1900 100% sample and clean
#source("census-1910-clean.R") # load 1910 100% sample and clean
source("glo-clean.R") # load GLO sales and clean

#source("census-link.R") # Link participants to 1900 & 1910 Census 
source("glo-link.R") # Link participants to GLO sales

if(run.descriptive){
  source("descriptive.R")
  source("balance-plot.R")
}

if(run.power){
  source("ok-power.R")
}