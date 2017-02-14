#####################################
### Power analysis                ###
#####################################

require(ggplot2)

# Define simulation parameters

alpha <- 0.05 # significance level 
L <- 100 # no. iterations
r.prob <- c(0.0125,0.025,0.05,0.1) # effect size for binary response
s.size <- c(500, 1000, 2000, 4000, 8000, 13000) # sample size

lambda <- 0.001 # decay rate of exponential function
n <- 6500 # county pop. size
comply <- 0.9 # compliance rate

# Create grid for parameters

grid.bin <- expand.grid("r.prob"=r.prob, "s.size"=s.size)

# Define regression simulation function

SimRegression <- function(r.prob,s.size,lambda,n,comply){
  # Simulate draw number, compliers, and response independently by county
  
  n <- s.size/2 # split sample size by number of counties
  
  county <- c(rep("El Reno", n), 
              rep("Lawton", n)) # county names
  
  draw <- c(sample(c(1:n), n, replace=FALSE),
            sample(c(1:n), n, replace=FALSE))  # sample draw number
  
  complier <- c(rbinom(n, 1, comply), 
                rbinom(n, 1, comply)) # draw compliers from binom dis
  
  # draw response from binom dis
  # treatment effect decays exponentially as a function of lambda and draw number
  # non-compliers do not have treatment effects
  response <- c(rbinom(n, 1, r.prob*exp(-lambda*draw*complier)), 
                rbinom(n, 1, r.prob*exp(-lambda*draw*complier)))  

  # Fit the model
  fit <- lm(response ~ draw + complier + factor(county))
  
  # Return p value
  return(summary(fit)$coef[,4]['draw']) 
}

# Run simulation

p.vals.bin <- replicate(L,
                         sapply(1:nrow(grid.bin), function(i){
                           SimRegression(grid.bin$r.prob[i],
                                         grid.bin$s.size[i],
                                         lambda, n, comply)}))

p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,i])))  # rows: iterations

# Calculate power

p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,i])))

grid.bin$power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/L)

#Create plots

power.plot.bin <- ggplot(data=grid.bin, aes(x=s.size, 
                                            y=power, 
                                            group = as.factor(r.prob), 
                                            colour = as.factor(r.prob))) +
  geom_line() +
  scale_colour_discrete(name = "Effect size", labels=c("1.25%","2.5%","5%","10%")) +
  scale_x_continuous(breaks=s.size, labels = c("500", "1,000", "2,000", "4,000", "8,000", "13,000")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  ylab("Power") +
  xlab("Sample size") +
  ggtitle("Power analysis for binary response")

ggsave(paste0("ok-power.pdf"), power.plot.bin, width=8.5, height=11) 