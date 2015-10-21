#load required libraries
library(ggplot2)

#initialize values
lambda <- 0.2
sample.size <- 40
theoretical.mean <- 1/lambda
theoretical.sd <- 1/lambda
theoretical.var <- theoretical.sd^2
no.simulations <- 1000

#set random seed for simulation
set.seed(123456)

#function to normalize the distribution of the averages of 40 exponentials
normalizeDist <- function(x, sample.size) sqrt(sample.size) * (mean(x) - theoretical.mean) / theoretical.sd

#Distribution of 1000 random exponentials
random.exponentials <- data.frame(x=rexp(1000,0.2))

g1 <- ggplot(random.exponentials,aes(x = x,fill="red")) +
  geom_histogram(binwidth=1, colour = "black", 
                aes(y = ..density..)) +
  labs(title = "Distribution of 1000 Random Exponentials", 
       x = "Values", y = "Frequency") +
  theme(legend.position="none")

#create matrix of 1000 exponentials of sample size 40
simulation.matrix <- matrix(rexp(no.simulations * sample.size,rate = lambda),no.simulations,sample.size)

#calculate simulation sample mean and variance
simulation.mean <- mean(simulation.matrix)
simulation.var <- simulation.mean^2
simulation.sd <- simulation.mean

dat1 <- data.frame(
  x = rowMeans(simulation.matrix),size = factor(rep(c(sample.size),no.simulations)))

#place theoretical and simulation means in a single data frame so they can be added to the plot legend
means.df <- data.frame(means=c("Theoretical","Simulation"),
                       vals=c(1/lambda,simulation.mean))

#plot sample distribution histogram and density curve using ggplot2
g2 <- ggplot(dat1, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", 
                aes(y = ..density..)) +
  stat_function(fun = dnorm,
                     size = 1.5,
                     col="blue",
                     show_guide=FALSE,
                     arg = list(mean = simulation.mean,sd = sd(dat1$x))) +
  geom_vline(data=means.df,
                    aes(xintercept=vals,
                        colour=means),
                    show_guide = TRUE) +
  labs(title = "Distribution of Averages of 40 Exponentials", 
       x = "Means of Exponentials, lambda = 0.2")


#run the simulation and save results in a data frame
dat <- data.frame(
  x = c(apply(matrix(rexp(no.simulations * sample.size, lambda), 
                     no.simulations), 1, normalizeDist, sample.size)),
  size = factor(rep(c(sample.size),no.simulations)))

#plot cumulative means vs no of observations
cumulative.means <- cumsum(dat$x/(1:no.simulations))
g <- ggplot(data.frame(x=1:no.simulations,y=cumulative.means),aes(x=x,y=y))
g <- g + geom_hline(yintercept=0) + geom_line(size=1.5)
g <- g + labs(x = "Number of Observations",y = "Cumulative Means")
print(g)

#plot sample distribution histogram and density curve using ggplot2
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 1.5)
print(g)

#output the theoretical and simulation sample distribution means
print(paste("Theoretical Mean = ",theoretical.mean,sep=""))
print(paste("Sample Mean = ",round(simulation.mean,2),sep=""))

#output the theoretical and simulation sample distribution variances
print(paste("Theoretical Variance = ",theoretical.var,sep=""))
print(paste("Sample Variance = ",round(simulation.var,2),sep=""))
