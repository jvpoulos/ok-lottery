MeanDR <- function(y,treat,x=NULL){ 
  # Calculate avg. dose-response fn
  #
  # Args:
  #   y: Response vector.
  #   treat: Treatment assignment vector.
  #   x: covariate to condition on. 
  #
  # Returns:
  #   Mean of dose-response fn
  
  treat.quintile <- quintileCut(treat) # deciles
  if(is.null(x)){
    dr <- ddply(data.frame("y"=y,
                           "treat"=treat.quintile),~treat,summarise,mean=mean(y))
  }else{
    dr <- ddply(data.frame("y"=y[which(x==1)],
                           "treat"=treat.quintile[which(x==1)]),~treat,summarise,mean=mean(y))
  }

  return(dr$mean)
} 

PermutationTest<-function(y,treat,L=10000,reference=1,alternative="two.sided",allow.parallel=TRUE,workers=cores,x=NULL){
  # Calculate randomization p value for ITT weighted difference-in-means.
  #
  # Args:
  #   y: Vector of non-missing responses.
  #   treat: Vector of non-missing treatment assignments. Must be equal in length to y and the sum must be nonzero. 
  #   L: Number of iterations for the permutation. Default is L=10000. 
  #   reference: reference treatment
  #   alternative: Character string specifying alternative hypothesis. Must be one of "two.sided" (default), "greater" or "less". 
  #   allow.parallel: Character string specifying whether to use parallel backend. Default is TRUE.
  #   workers: Number of workers used for parallel execution. Default is set to number of cores.
  #
  # Returns:
  #   Randomization p value. 
  library(plyr)
  # # Error handling
  # if (sum(is.na(y))>0){
  #   stop("y contains missing values.")
  # }
  # if (sum(is.na(treat))>0 | length(treat)!=length(y)){
  #   stop("treat must be equal in length to y and contain non-missing values.")
  # }
  # if (sum(treat)==0){
  #   stop("Treatment group is empty.")
  # }
  # if (sum(treat)==length(treat)){
  #   warning("Control group is empty.")
  # }
  # Apply permutation test L times
  if(allow.parallel){
    library(parallel)
    new.t.stats <- mclapply(1:L, function(i){
      # Create permutation assignment vector 
      treat.perm <-sample(c(treat), 
                          length(treat),
                          replace=FALSE) 
      # Calculate permutation test statistic
      return(outer(MeanDR(y,treat.perm,x), MeanDR(y,treat.perm,x)[reference], `-`)[-1])
    }, 
    mc.set.seed=FALSE,
    mc.cores=workers)
  }
  else{
    new.t.stats <- lapply(1:L, function(i){
      # Create permutation assignment vector 
      treat.perm <-sample(c(treat), 
                          length(treat),
                          replace=FALSE) 
      # Calculate permutation test statistic
      return(outer(MeanDR(y,treat.perm,x), MeanDR(y,treat.perm,x)[reference], `-`)[-1])
    })
  }
  new.t.stats <- do.call("rbind", new.t.stats)
  obs.t.stat <- outer(MeanDR(y,treat,x), MeanDR(y,treat,x)[reference], `-`)[-1] # treat-reference
  # Calculate p value
  if (alternative=="two.sided"){
    pvalue <- colSums(abs(new.t.stats) >= abs(obs.t.stat))/L 
  }
  if (alternative=="greater"){
    pvalue <- sum(new.t.stats > obs.t.stat)/L
  }
  if (alternative=="less"){
    pvalue <- sum(new.t.stats < obs.t.stat)/L
  }
  # Return p-value and permutation vectors
  return(list("p" = pvalue, "perm.t.stats" = new.t.stats,"obs.t.stat"=obs.t.stat))
}

PermutationCI <- function(y,treat,c.range=c(-1,1),L=100,alpha=0.025,l=100,d=9,x=NULL) { 
  # Calculate randomization confidence interval for ITT weighted difference-in-means.
  #
  # Args:
  #   y: Vector of non-missing responses.
  #   treat: Vector of non-missing treatment assignments. Must be equal in length to y and the sum must be nonzero. 
  #   c.range: Range of constant treatment effects. Default is c(-1,1).
  #   L: Number of iterations for the permutation. Default is L=100.
  #   alpha: Two-sided significance level. Default is 0.025.
  #   l: Number of constant treatment effects. Default is 100. 
  #   d: Number of treatment groups. 
  #
  # Returns:
  #   List containing randomization confidence interval and observed ITT weighted difference-in-means. 
  # Create vector to store CIs
  CI<-matrix(0,l,d)
  for(i in 1:l){
    # Choose constant treatment effect
    delta.c <- sample(seq(c.range[1],c.range[2],by=0.00001),1,replace=FALSE) 
    # Subtract from all of treated outcomes
    y.delta <- y-abs(delta.c)
    # Run permuation test
    results <- PermutationTest(y=y.delta,treat,L,x=NULL) 
    # If result not significant, delta.c is in confidence interval
    CI[i,] <- ifelse(results$p>(2*alpha),delta.c,NA)
  } 
  return(list("CI"=apply(CI, 2, range, na.rm=TRUE),
              "obs.t.stat"=results$obs.t.stat))
} 