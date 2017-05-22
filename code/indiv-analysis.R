#####################################
### Individual-level analysis    ###
#####################################


## OLS without covariates

# Sales (binary)

lm.sale <- lm(sale ~ draw.scale, data=link.sales)
summary(lm.sale)
confint(lm.sale)

# Sales (continuous)

lm.n.sales <- lm(n.sales ~ draw.scale, data=link.sales)
summary(lm.n.sales)
confint(lm.n.sales)

## Lasso regression with covariates

link.sales$lawton <- ifelse(link.sales$comply==0 |link.sales$comply==1 ,1,0) # lawton dummy

link.sales.x <- link.sales[!is.na(link.sales$draw) & !is.na(link.sales$state) & !is.na(link.sales$loc),][c("draw","lawton","state","loc")] # rm NA values

link.sales.x$state <- as.numeric(link.sales.x$state) # need to be numeric for matrix
link.sales.x$loc <- as.numeric(link.sales.x$loc)

link.sales.y <- link.sales[!is.na(link.sales$draw) & !is.na(link.sales$state) & !is.na(link.sales$loc),][c("sale")]

lasso.sale <- glmnet::glmnet(x=as.matrix(link.sales.x), y=as.factor(link.sales.y[,1]), family = "binomial", alpha=1)

lasso.sale.cv = glmnet::cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha=1)

plot(cvfit)

cvfit$lambda.min

cvfit$lambda.1se

coef(cvfit, s = "lambda.min")


## Nonparametric estimation




# Plot dose-response fn