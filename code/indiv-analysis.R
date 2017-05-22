#####################################
### Individual-level analysis    ###
#####################################

patient <- FALSE

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

## OLS without covariates

# Sales (binary)

lm.sale <- lm(sale ~ draw.scale, data=link.sales)
summary(lm.sale)
confint(lm.sale)

# Sales (continuous)

lm.n.sales <- lm(n.sales ~ draw.scale, data=link.sales)
summary(lm.n.sales)
confint(lm.n.sales)

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

link.sales <- link.sales[!is.na(link.sales$draw),] # rm 111 obs w missing draw

# Get randomization p value
if(patient){
  perm.sale <- PermutationTest(y=link.sales$sale,
                             treat=link.sales$draw,
                             L=500) 
  print(perm.sale$p)
  
  perm.sale.CI <- PermutationTest(y=link.sales$sale,
                                  treat=link.sales$draw)
}

# Get randomization CIs


