#####################################
### County-level analysis        ###
#####################################





## OLS without covariates

# Scale draw #
link.1900.1910$draw.scale <- (link.1900.1910$draw-mean(hs$draw,na.rm=TRUE))/(2*sd(hs$draw,na.rm=TRUE)) # center and divide by 2 sds (summary stats of entire sample)

census.dvs <- c("farmer","employer","self","unemp","own","free","farm")

lm.census <- lapply(census.dvs,
                    function(x){
                      lm <- lm(link.1900.1910[,x] ~ link.1900.1910$draw.scale)
                      return(data.frame("Est" = coeftest(lm)[,1]["link.1900.1910$draw.scale"],
                                        "p"= coeftest(lm)[,4]["link.1900.1910$draw.scale"],
                                        "CI" = confint(lm)[2,],
                                        "N" = summary(lm)$df[2]))
                    })

# Create data for plot for county analyses
plot.data.county <- data.frame(y = c(lm.census[[1]]['Est'][1,],
                                  lm.census[[2]]['Est'][1,],
                                  lm.census[[3]]['Est'][1,],
                                  lm.census[[4]]['Est'][1,],
                                  lm.census[[5]]['Est'][1,],
                                  lm.census[[6]]['Est'][1,],
                                  lm.census[[7]]['Est'][1,]),
                            y.lo = c(lm.census[[1]]['CI'][1,],lm.census[[2]]['CI'][1,],lm.census[[3]]['CI'][1,],lm.census[[4]]['CI'][1,],lm.census[[5]]['CI'][1,],lm.census[[6]]['CI'][1,],lm.census[[7]]['CI'][1,]),
                            y.hi = c(lm.census[[1]]['CI'][2,],lm.census[[2]]['CI'][2,],lm.census[[3]]['CI'][2,],lm.census[[4]]['CI'][2,],lm.census[[5]]['CI'][2,],lm.census[[6]]['CI'][2,],lm.census[[7]]['CI'][2,]))
plot.data.county <- transform(plot.data.county, y.lo = y.lo, y.hi=y.hi)
plot.data.county$x <- c(rep(paste("Farmer, N =", 
                                     format(lm.census[[1]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1),
                           rep(paste("Employer, N =", 
                                     format(lm.census[[2]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1),
                           rep(paste("Self-employed, N =", 
                                     format(lm.census[[3]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1),
                           rep(paste("Unemployed, N =", 
                                     format(lm.census[[4]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1),
                           rep(paste("Own home, N =", 
                                     format(lm.census[[5]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1),
                           rep(paste("Own home free, N =", 
                                     format(lm.census[[6]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1),
                           rep(paste("Own farm, N =", 
                                     format(lm.census[[7]]['N'][1,],big.mark=",",scientific=FALSE,trim=TRUE)),1))

# Plot forest plots
plot.data.county$x <- factor(plot.data.county$x, levels=plot.data.county$x) # reverse order
summary.plot.county <- ForestPlot(plot.data.county,xlab="Treatment effect",ylab="Outcome") + scale_y_continuous(labels = percent_format(), 
                                                                                                           limits = c(-0.25,0.25))

ggsave(paste0(data.directory,"plots/forest-county.png"), summary.plot.county, width=8.5, height=11)  