#####################################
### Individual-level analysis    ###
#####################################

patient <- FALSE
rand.p <- FALSE

## OLS without covariates

# Sale (binary/continuous)

lm.sale <- lm(sale~ draw.scale, data=link.patents)
summary(lm.sale)
confint(lm.sale)

lm.sales <- lm(sales~ draw.scale, data=link.patents)
summary(lm.sales)
confint(lm.sales)

# Homestead (binary/continuous)

lm.homestead <- lm(homestead~ draw.scale, data=link.patents)
summary(lm.homestead)
confint(lm.homestead)

lm.homesteads <- lm(homesteads ~ draw.scale, data=link.patents)
summary(lm.homesteads)
confint(lm.homesteads)

# Total acres

lm.acres <- lm(total_acres ~ draw.scale, data=link.patents)
summary(lm.acres)
confint(lm.acres)

# estimates by year

for(x in c(grep('total_acres', colnames(link.patents), value=TRUE), 
    grep('sales', colnames(link.patents), value=TRUE),
    grep('homesteads', colnames(link.patents), value=TRUE))){ # Scale total acres to 0 -to 1
  link.patents[,x] <-  scales:::rescale(link.patents[,x] , to = c(0, 1))
}

lm.year <- lapply(outcome.vars,
                       function(x){
                         formula <- paste(x, "~ draw.scale")
                         lm <- lm(formula, data=link.patents)
                         return(data.frame("Est" = lmtest::coeftest(lm)[,1]["draw.scale"],
                                           "p"= lmtest::coeftest(lm)[,4]["draw.scale"],
                                           "CI" = confint(lm)[2,],
                                           "N" = summary(lm)$df[2]))
                       })

  
# Create data for plot 
plot.data.year <- data.frame(variable= outcome.vars,
                             y = sapply(lm.year, "[[", "Est")[1,],
                            y.lo = sapply(lm.year, "[[", "CI")[1,],
                            y.hi = sapply(lm.year, "[[", "CI")[2,])

plot.data.year <- plot.data.year[!plot.data.year$variable %in% c('sales','homesteads','total_acres'),] # rm totals
plot.data.year$x <- as.numeric(str_sub(plot.data.year$variable, start= -4))

plot.data.year$variable <- as.character(plot.data.year$variable)
plot.data.year$variable[grep('total_acres',plot.data.year$variable)] <- "Total acres"
plot.data.year$variable[grep('homesteads',plot.data.year$variable)] <- "Homesteads"
plot.data.year$variable[grep('sales',plot.data.year$variable)] <- "Sales"

# Plot forest plots
plot.data.year$x <- factor(plot.data.year$x, levels=sort(plot.data.year$x)) 
summary.plot.year <- ForestPlot2(plot.data.year,ylab="Treatment effect",xlab="Year of patent", leglab="Outcome") 

ggsave(paste0(data.directory,"plots/forest-year.png"), summary.plot.year, width=11, height=8.5)  

## Nonparametric estimation

# Setup parallel processing 
cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

link.patents <- link.patents[!is.na(link.patents$draw),] # rm 111 obs w missing draw

if(patient){
  
  ## Sale
  if(rand.p){
  # Get randomization p value
  perm.sale <- PermutationTest(y=link.patents$sale,
                             treat=link.patents$draw,
                             L=1000) 
  print(perm.sale$p)
  }

  # Get randomization CIs 
  perm.sale.CI <- PermutationCI(y=link.patents$sale,
                                  treat=link.patents$draw,
                                c.range=c(0,0.75))
  print(perm.sale.CI$CI)
  print(perm.sale.CI$MeanDR) # observed t stat is mean DR
  
  ## homestead
  if(rand.p){
  # Get randomization p value
  perm.homestead <- PermutationTest(y=link.patents$homestead,
                               treat=link.patents$draw,
                               L=1000) 
  print(perm.homestead$p)
  }
  
  # Get randomization CIs 
  perm.homestead.CI <- PermutationCI(y=link.patents$homestead,
                                treat=link.patents$draw,
                                c.range=c(0,0.75))
  print(perm.homestead.CI$CI)
  print(perm.homestead.CI$MeanDR) # observed t stat is mean DR

}