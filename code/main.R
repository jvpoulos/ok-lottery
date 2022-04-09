run.appendix <- FALSE # create descriptive figures for appendix?
link.GLO <- TRUE # link participants to GLO records?
link.census <- TRUE # link participants to census records?

# Libraries

library(reshape)
library(reshape2)
library(phonics)
library(RecordLinkage)
library(ggplot2)
library(doParallel)
library(data.table)
library(caret)
library(stringr)
library(zoo)
library(plyr)
library(glmnet)
library(boot)
ibrary(reporttools)

library(parallel)
ncores<- detectCores()

#install.packages('survival')# need to update before loading weights

library(weights)
library(parallel)
library(doParallel)
library(reldist)

#install.packages("Rcpp") # need to update before loading dplyr

library(dplyr)

if(run.appendix){
  library(reporttools)
  library(gridExtra)
}

library(lmtest)
library(scales)

# Set directories
data.directory <- "data/"
code.directory <- "code/"

source("code/utils.R")
source("code/SuperLearner.R")

## Individual-level analyses 

source("code/ok-participants.R") # load and clean Lawton and El Reno participants

if(link.GLO){
  source("code/glo-link.R") # Link participants to GLO patents
}else{
  load('data/sales-link.RData')
}

if(link.census){
  source("code/census-1900-clean.R") # load 1900 100% sample and clean
  source("code/census-1910-clean.R") # load 1910 100% sample and clean
  source("code/census-link.R") # Link participants to 1900 & 1910 Census 
}else{
  load('data/census-link.RData')
}

source("code/prepare-analyses.R")

source("code/balance-plot.R")

if(run.appendix){ # appendix summaries
  source("code/descriptive.R")
}

source("code/indiv-analysis-patents.R") # individual-level analyses (patents outcomes)
source("code/indiv-analysis-census.R") # individual-level analyses (census outcomes)