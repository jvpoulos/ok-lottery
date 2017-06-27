run.appendix <- TRUE # create descriptive figures for appendix?
run.census <-FALSE # run individual-level analyses on census outcomes?
run.power <-FALSE # run power analyses?

# Libraries

library(reshape)
library(reshape2)
library(RecordLinkage)
library(ggplot2)
library(doParallel)
library(data.table)
library(caret)
library(stringr)
library(zoo)
library(plyr)

install.packages('survival')# need to update before loading weights

library(weights)
library(parallel)
library(doParallel)
library(reldist)
library(randomForest)
library(noncensus)

install.packages("Rcpp") # need to update before loading dplyr

library(dplyr)

if(run.appendix){
  library(reporttools)
  library(gridExtra)
  library(reshape)
}

# Set directories
data.directory <- "~/Dropbox/github/ok-lottery/data/"
code.directory <- "~/Dropbox/github/ok-lottery/code/"

setwd(code.directory)

source("utils.R")

## Individual-level analyses 

source("ok-participants.R") # load and clean Lawton and El Reno participants

source("glo-link.R") # Link participants to GLO patents

if(run.census){
  library(lmtest)
  library(scales)
  source("census-1900-clean.R") # load 1900 100% sample and clean
  source("census-1910-clean.R") # load 1910 100% sample and clean
  source("census-link.R") # Link participants to 1900 & 1910 Census 
  
  source("indiv-analysis-census.R") # individual-level analyses (census outcomes)
}

if(run.power){
  source("ok-power.R")
}

source("indiv-analysis.R") # individual-level analyses (patents outcomes)

## County-level data

detach("package:plyr", unload=TRUE) # make sure plyr is not loaded
source("census-county-clean.R") 

source("impact-plots-census.R")

# glo-patents-county.R
source("impact-plots-patents.R")

source("did.R")

if(run.appendix){ # appendix plots
  library(devtools)
  source_gist("524eade46135f6348140")
  
  source("descriptive.R")
  source("balance-plot.R")
}
