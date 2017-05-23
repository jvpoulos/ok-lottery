#####################################
### Individual-level analysis    ###
#####################################

patient <- FALSE

## OLS without covariates

# Sales

lm.sale <- lm(sale~ draw.scale, data=link.sales)
summary(lm.sale)
confint(lm.sale)

# estimates by sale year
link.sales$year <- lubridate::year(link.sales$Date)

lm.sale.year <- lapply(c(1901:1910),
                       function(t){
                         lm <- lm(sale~ draw.scale, data=link.sales[link.sales$year==t | is.na(link.sales$year),])
                         return(data.frame("Est" = coeftest(lm)[,1]["draw.scale"],
                                           "p"= coeftest(lm)[,4]["draw.scale"],
                                           "CI" = confint(lm)[2,],
                                           "N" = summary(lm)$df[2]))
                       })
  
# Create data for plot 
plot.data.year <- data.frame(y = c(lm.sale.year[[1]]['Est'][1,],
                                  lm.sale.year[[2]]['Est'][1,],
                                  lm.sale.year[[3]]['Est'][1,],
                                  lm.sale.year[[4]]['Est'][1,],
                                  lm.sale.year[[5]]['Est'][1,],
                                  lm.sale.year[[6]]['Est'][1,],
                                  lm.sale.year[[7]]['Est'][1,],
                                  lm.sale.year[[8]]['Est'][1,],
                                  lm.sale.year[[9]]['Est'][1,],
                                  lm.sale.year[[10]]['Est'][1,]),
                            y.lo = c(lm.sale.year[[1]]['CI'][1,],lm.sale.year[[2]]['CI'][1,],lm.sale.year[[3]]['CI'][1,],lm.sale.year[[4]]['CI'][1,],lm.sale.year[[5]]['CI'][1,],lm.sale.year[[6]]['CI'][1,],lm.sale.year[[7]]['CI'][1,],lm.sale.year[[8]]['CI'][1,],lm.sale.year[[9]]['CI'][1,],lm.sale.year[[10]]['CI'][1,]),
                            y.hi = c(lm.sale.year[[1]]['CI'][2,],lm.sale.year[[2]]['CI'][2,],lm.sale.year[[3]]['CI'][2,],lm.sale.year[[4]]['CI'][2,],lm.sale.year[[5]]['CI'][2,],lm.sale.year[[6]]['CI'][2,],lm.sale.year[[7]]['CI'][2,],lm.sale.year[[8]]['CI'][2,],lm.sale.year[[9]]['CI'][2,],lm.sale.year[[10]]['CI'][2,]))
plot.data.year <- transform(plot.data.year, y.lo = y.lo, y.hi=y.hi)
plot.data.year$x <- c(1901:1910)  
  
# Plot forest plots
plot.data.year$x <- factor(plot.data.year$x, levels=plot.data.year$x) # reverse order
summary.plot.year <- ForestPlot2(plot.data.year,ylab="Treatment effect",xlab="Year of sale") + scale_y_continuous(labels = percent_format())

ggsave(paste0(data.directory,"plots/forest-year.png"), summary.plot.year, width=8.5, height=11)  

## Covariate selection with Lasso 

link.sales$lawton <- ifelse(link.sales$comply==0 |link.sales$comply==1 ,1,0) # lawton dummy

link.sales.x <- link.sales[!is.na(link.sales$draw) & !is.na(link.sales$state) & !is.na(link.sales$loc),][c("draw","lawton","state","loc")] # rm NA values

link.sales.x <- cbind(link.sales.x$draw, link.sales.x$lawton, dummify(link.sales.x$state), dummify(link.sales.x$loc))

link.sales.y <- link.sales[!is.na(link.sales$draw) & !is.na(link.sales$state) & !is.na(link.sales$loc),][c("sale")]

# Lasso with cv lambda

if(patient){
lasso.cv.sale <- glmnet::cv.glmnet(x=as.matrix(link.sales.x), y=as.factor(link.sales.y[,1]), 
                                family = "binomial", 
                                type.measure = "class",
                                alpha=1)


saveRDS(lasso.cv.sale, paste0(data.directory,"lasso_cv_sale.rds"))
}

lasso.cv.sale <- readRDS(paste0(data.directory,"lasso_cv_sale.rds"))

# Extract non-empty coefficients

sum(coef(lasso.cv.sale, s = "lambda.min")) # all but intercept zeroed-out

## Nonparametric estimation

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

link.sales <- link.sales[!is.na(link.sales$draw),] # rm 111 obs w missing draw

if(patient){
  # Get randomization p value
  perm.sale <- PermutationTest(y=link.sales$sale,
                             treat=link.sales$draw,
                             L=10000) 
  print(perm.sale$p)
 
  # Get randomization CIs 
  perm.sale.CI <- PermutationTest(y=link.sales$sale,
                                  treat=link.sales$draw)
  print(perm.sale.CI$CI)
  print(perm.sale.CI$MaxDR) # observed t stat is mean DR
}