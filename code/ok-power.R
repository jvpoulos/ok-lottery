#####################################
### Power analysis                ###
#####################################

require(ggplot2)

# Define simulation parameters

alpha <- 0.05 # significance level 
L <- 1000 # no. iterations
r.prob <- c(0.0125,0.025,0.05,0.1) # effect size for binary response
s.size <- c(500, 1000, 2000, 4000, 8000, 13000) # sample size

lambda <- 0.001 # decay rate of exponential function
comply <- 0.9 # compliance rate
amp <- c(0,1,3) # amplify to affect noise

# Create grid for parameters

grid.bin <- expand.grid("r.prob"=r.prob, "s.size"=s.size, "amp"=amp)

# Define regression simulation function

SimRegression <- function(r.prob,s.size,lambda,comply,amp){
  # Simulate draw number, compliers, and response independently by district
  
  n <- s.size/2 # split sample size by number of counties
  
  district <- c(rep("El Reno", n), 
              rep("Lawton", n)) # district names
  
  draw <- c(sample(c(1:6500), n, replace=FALSE),
            sample(c(1:6500), n, replace=FALSE))  # sample draw number
  
  complier <- c(rbinom(n, 1, comply), 
                rbinom(n, 1, comply)) # draw compliers from binom dis
  
  # draw response from binom dis
  # treatment effect decays exponentially as a function of lambda and draw number
  # non-compliers do not have treatment effects
  response <- c(rbinom(n, 1, runif(1,0,0.01)*amp + r.prob*exp(-lambda*draw*complier)), 
                rbinom(n, 1, runif(1,0,0.02)*amp + r.prob*exp(-lambda*draw*complier)))  

  # Tranform inputs 
  draw.scaled <- (draw-mean(draw))/(2*sd(draw)) # center and divide by 2 sds
  complier.center <- complier-mean(complier) # center binary variables
  lawton <- ifelse(district=="Lawton",1,0)
  lawton.center <- lawton-mean(lawton)
  
  # Fit the model
  fit <- lm(response ~ draw.scaled + complier.center + lawton.center)
  
  # Return p value
  return(summary(fit)$coef[,4][2]) 
}

# Run simulation

p.vals.bin <- replicate(L,
                         sapply(1:nrow(grid.bin), function(i){
                           SimRegression(grid.bin$r.prob[i],
                                         grid.bin$s.size[i],
                                         lambda, 
                                         comply,
                                         grid.bin$amp[i])}))

p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,i])))  # rows: iterations

# Calculate power

p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,i])))

grid.bin$power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/L)

#Create plots

power.plot.bin <- ggplot(data=grid.bin, aes(x=s.size, 
                                            y=power, 
                                            colour = as.factor(r.prob),
                                            linetype = as.factor(amp))) +
  geom_line() +
  scale_colour_discrete(name = "Effect size", labels=c("1.25%","2.5%","5%","10%")) +
  scale_linetype_discrete(name = "Noise", labels=c("None","Low","High")) +
  scale_x_continuous(breaks=s.size, labels = c("500", "1,000", "2,000", "4,000", "8,000", "13,000")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  ylab("Power") +
  xlab("Sample size") +
  ggtitle("Power analysis for binary response")

ggsave(paste0("ok-power.pdf"), power.plot.bin, width=8.5, height=11) 