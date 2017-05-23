run.appendix <- TRUE
run.cennsus <-FALSE
run.power <-FALSE

# Libraries

library(reshape2)
library(RecordLinkage)
library(ggplot2)
library(doParallel)
library(data.table)
library(caret)
library(stringr)
library(zoo)
library(plyr)
library(weights)
library(e1071)
library(parallel)
library(doParallel)

install.packages("Rcpp") # need to update before loading dplyr

library(dplyr)

if(run.appendix){
  library(ggmap)
  library(maps)
  library(reporttools)
  library(gridExtra)
  library(reshape)
}

# Set directories
data.directory <- "~/Dropbox/github/ok-lottery/data/"
code.directory <- "~/Dropbox/github/ok-lottery/code/"

setwd(code.directory)

# Source scripts (in order)

source("utils.R")

source("ok-participants.R") # load and clean Lawton and El Reno participants

source("glo-clean.R") # load GLO sales and clean

source("glo-link.R") # Link participants to GLO sales

if(run.appendix){
  source("descriptive.R")
  source("balance-plot.R")
}

if(run.census){
  library(lmtest)
  library(scales)
  source("SuperLearner.R")
  source("census-1900-clean.R") # load 1900 100% sample and clean
  source("census-1910-clean.R") # load 1910 100% sample and clean
  source("census-link.R") # Link participants to 1900 & 1910 Census 
}

if(run.power){
  source("ok-power.R")
}

source("indiv-analysis.R") # individual-level analysis