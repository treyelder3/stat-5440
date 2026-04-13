######## Monte Carlo Example ##########

#10000 samples from unit square
n_samp <- 10000
#assume independence of x and y so sample separately
x <- runif(n_samp)
y <- runif(n_samp)
plot(x,y, pch = 19, cex = 0.5)
#Generate points for the unit circle
theta <- seq(0, 2 * pi, length.out = 100)  # angles from 0 to 2π
x_cir <- cos(theta)  # x-coordinates of the circle
y_cir <- sin(theta)  # y-coordinates of the circle
# Add the unit circle in red
lines(x_cir, y_cir, col = "red", lwd = 3)

#probability of being in the unit circle
mean(x^2 + y^2 <= 1)



#could we have done that without sampling? 
#Yes but not always the case,
# Plus this was fast

#################################################
###### BIVARIATE NORMAL GIBBS SAMPLER ###########
#################################################

numiters <- 10000

## setting correlation of bivariate normal
rho <- 0.8

## initial values:
theta1 <- rep(NA,numiters)
theta2 <- rep(NA,numiters)
theta1[1] <- 50
theta2[1] <- 50

## Gibbs sampler:
for (i in 2:numiters){
  theta1[i] <- rnorm(1,rho*theta2[i-1],sqrt(1-rho^2))
  theta2[i] <- rnorm(1,rho*theta1[i],sqrt(1-rho^2))
}


## checking convergence by comparing multiple chains from different start points:
theta1.alt <- rep(NA,numiters)
theta2.alt <- rep(NA,numiters)
theta1.alt[1] <- -10
theta2.alt[1] <- -10
for (i in 2:numiters){
  theta1.alt[i] <- rnorm(1,rho*theta2.alt[i-1],sqrt(1-rho^2))
  theta2.alt[i] <- rnorm(1,rho*theta1.alt[i],sqrt(1-rho^2))
}

mintheta1 <- min(theta1,theta1.alt)
mintheta2 <- min(theta2,theta2.alt)
maxtheta1 <- max(theta1,theta1.alt)
maxtheta2 <- max(theta2,theta2.alt)

par(mfrow=c(1,1))
plot(theta1,theta2,col=1,xlim=c(mintheta1,maxtheta1),ylim=c(mintheta2,maxtheta2),type="l")
points(theta1.alt,theta2.alt,col=2,xlim=c(mintheta1,maxtheta1),ylim=c(mintheta2,maxtheta2),type="l")

par(mfrow=c(2,1))
plot(1:numiters,theta1,col=1,ylim=c(mintheta1,maxtheta1),type="l", xlab="Iterations")
lines(1:numiters,theta1.alt,col=2)
plot(1:numiters,theta2,col=1,ylim=c(mintheta2,maxtheta2),type="l", xlab="Iterations")
lines(1:numiters,theta2.alt,col=2)

par(mfrow=c(2,1))
plot(1:200,theta1[1:200],col=1,ylim=c(mintheta1,maxtheta1),type="l", xlab="Iterations")
lines(1:200,theta1.alt[1:200],col=2)
plot(1:200,theta2[1:200],col=1,ylim=c(mintheta2,maxtheta2),type="l", xlab="Iterations")
lines(1:200,theta2.alt[1:200],col=2)

## throwing out first 100 as "burn-in" samples and combining chains:

theta1.new <- c(theta1[101:numiters],theta1.alt[101:numiters])
theta2.new <- c(theta2[101:numiters],theta2.alt[101:numiters])

numsamples <- length(theta1.new)
numsamples

## plotting auto-correlation of each parameter

par(mfrow=c(2,1))
acf(theta1.new)
acf(theta2.new) 

## retaining independent samples by only taking every tenth sample:

temp <- 10*c(1:(numsamples/10))
theta1.final <- theta1.new[temp]
theta2.final <- theta2.new[temp]

par(mfrow=c(2,1))
acf(theta1.final)
acf(theta2.final) 

mean(theta1.final)
mean(theta2.final)
sd(theta1.final)
sd(theta2.final)
cor(theta1.final,theta2.final)

#########################################
## What about a correlation of 0.99 ?  ##
#########################################

rho <- 0.99

theta1 <- rep(NA,numiters)
theta2 <- rep(NA,numiters)
theta1[1] <- 10
theta2[1] <- 10
theta1.alt <- rep(NA,numiters)
theta2.alt <- rep(NA,numiters)
theta1.alt[1] <- -10
theta2.alt[1] <- -10

## Gibbs sampler:
for (i in 2:numiters){
  theta1[i] <- rnorm(1,rho*theta2[i-1],sqrt(1-rho^2))
  theta2[i] <- rnorm(1,rho*theta1[i],sqrt(1-rho^2))
}
for (i in 2:numiters){
  theta1.alt[i] <- rnorm(1,rho*theta2.alt[i-1],sqrt(1-rho^2))
  theta2.alt[i] <- rnorm(1,rho*theta1.alt[i],sqrt(1-rho^2))
}

mintheta1 <- min(theta1,theta1.alt)
mintheta2 <- min(theta2,theta2.alt)
maxtheta1 <- max(theta1,theta1.alt)
maxtheta2 <- max(theta2,theta2.alt)

par(mfrow=c(1,1))
plot(theta1,theta2,col=1,xlim=c(mintheta1,maxtheta1),ylim=c(mintheta2,maxtheta2),type="l")
points(theta1.alt,theta2.alt,col=2,xlim=c(mintheta1,maxtheta1),ylim=c(mintheta2,maxtheta2),type="l")

par(mfrow=c(2,1))
plot(1:numiters,theta1,col=1,ylim=c(mintheta1,maxtheta1),type="l")
lines(1:numiters,theta1.alt,col=2)
plot(1:numiters,theta2,col=1,ylim=c(mintheta2,maxtheta2),type="l")
lines(1:numiters,theta2.alt,col=2)

par(mfrow=c(2,1))
plot(1:2000,theta1[1:2000],col=1,ylim=c(mintheta1,maxtheta1),type="l")
lines(1:2000,theta1.alt[1:2000],col=2)
plot(1:2000,theta2[1:2000],col=1,ylim=c(mintheta2,maxtheta2),type="l")
lines(1:2000,theta2.alt[1:2000],col=2)


## throwing out first 1000 as "burn-in" samples and combining chains:

theta1.new <- c(theta1[1001:numiters],theta1.alt[1001:numiters])
theta2.new <- c(theta2[1001:numiters],theta2.alt[1001:numiters])

numsamples <- length(theta1.new)
numsamples

## plotting auto-correlation of each parameter

par(mfrow=c(2,1))
acf(theta1.new,lag.max=200)
acf(theta2.new,lag.max=200) 

## retaining independent samples by only taking every 200th sample:

temp <- 1*c(1:(numsamples/1))
theta1.final <- theta1.new[temp]
theta2.final <- theta2.new[temp]

par(mfrow=c(2,1))
acf(theta1.final)
acf(theta2.final) 

numsamples <- length(theta1.final)
numsamples

mean(theta1.final)
mean(theta2.final)
sd(theta1.final)
sd(theta2.final)
cor(theta1.final,theta2.final)


#####################################################
########## METROPOLIS-HASTINGS EXAMPLE   ############
#####################################################

## Reading in data:
data <- read.table("../../data/planes.txt",skip=1)
y <- data[,2]
t <- data[,1]-1976
n <- length(y)

plot(t,y,pch=19)
planes.lm <- lm(y~t)
alpha.point <- planes.lm$coef[1]
beta.point <- planes.lm$coef[2]
abline(alpha.point,beta.point,col="red")

## function for evaluating true distribution:
logtruedist <- function(y, alpha,beta){
  # need to make sure all rates are positive
  true <- -Inf
  check <- 1
  for (i in 1:n){
    if (alpha + beta*t[i] <= 0){
      check <- 0
    }
  }
  if (check == 1){
    true <- 0
    for (i in 1:n){
      true <- true + y[i]*log(alpha+beta*t[i]) - (alpha+beta*t[i])
    }
  }
  true
}

# tuning parameters (variances of proposals)
a <- 1 
b <- 1

#initializing sample vectors:
numiters <- 101000
alpha.samp <- rep(NA,numiters)
beta.samp <- rep(NA,numiters)
alpha.samp[1] <- alpha.point
beta.samp[1] <- beta.point

#metropolis algorithm:
for (j in 2:numiters){
  alpha.prop <- rnorm(1,alpha.samp[j-1],a)
  beta.prop <- rnorm(1,beta.samp[j-1],b)
  lognumer <- logtruedist(y, alpha.prop,beta.prop)
  logdenom <- logtruedist(y, alpha.samp[j-1],beta.samp[j-1])
  logr <- lognumer-logdenom
  u <- runif(1,0,1)
  logu <- log(u)
  if (logu <= logr){
    alpha.samp[j] <- alpha.prop
    beta.samp[j] <- beta.prop
  }else{
    alpha.samp[j] <- alpha.samp[j-1]
    beta.samp[j] <- beta.samp[j-1]
  }
  print(j)
}

#examining samples

#first 100
par(mfrow=c(2,1))
plot(1:100,alpha.samp[1:100],type="l")
plot(1:100,beta.samp[1:100],type="l")
#first 2000
par(mfrow=c(2,1))
plot(1:2000,alpha.samp[1:2000],type="l")
plot(1:2000,beta.samp[1:2000],type="l")

# all
par(mfrow=c(2,1))
plot(1:numiters,alpha.samp,type="l")
plot(1:numiters,beta.samp,type="l")
length(unique(alpha.samp))
length(unique(beta.samp))

#save chains
alpha.samp1 <- alpha.samp
beta.samp1 <- beta.samp



# re-running with different tuning parameters
a <- 1
b <- 0.25


# running another chain from a different starting point

alpha.samp[1] <- 10
beta.samp[1] <- 1

# running new chain using code above 

alpha.samp2 <- alpha.samp
beta.samp2 <- beta.samp

# using both chains to check convergence
par(mfrow=c(2,1))
ymin<-min(alpha.samp1,alpha.samp2)
ymax<-max(alpha.samp1,alpha.samp2)
plot(1:numiters,alpha.samp1,type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,alpha.samp2,col=3)
ymin<-min(beta.samp1,beta.samp2)
ymax<-max(beta.samp1,beta.samp2)
plot(1:numiters,beta.samp1,type="l",col=2,ylim=c(ymin,ymax))
lines(1:numiters,beta.samp2,col=3)

# foucssing on first part of chains
par(mfrow=c(2,1))
ymin<-min(alpha.samp1,alpha.samp2)
ymax<-max(alpha.samp1,alpha.samp2)
plot(1:2000,alpha.samp1[1:2000],type="l",col=2,ylim=c(ymin,ymax))
lines(1:2000,alpha.samp2[1:2000],col=3)
ymin<-min(beta.samp1,beta.samp2)
ymax<-max(beta.samp1,beta.samp2)
plot(1:2000,beta.samp1[1:2000],type="l",col=2,ylim=c(ymin,ymax))
lines(1:2000,beta.samp2[1:2000],col=3)

# getting rid of first 1000 samples
alpha.samp1 <- alpha.samp1[1001:101000]
beta.samp1 <- beta.samp1[1001:101000]
alpha.samp2 <- alpha.samp2[1001:101000]
beta.samp2 <- beta.samp2[1001:101000]

# checking autocorrelation
par(mfrow=c(2,2))
acf(alpha.samp1,lag.max=200)
acf(alpha.samp2,lag.max=200)
acf(beta.samp1,lag.max=200)
acf(beta.samp2,lag.max=200)

# Thinning
# taking every 200th sample
temp <- 200*(c(1:(length(alpha.samp1)/200)))
alpha.samp1.red <- alpha.samp1[temp]
beta.samp1.red <- beta.samp1[temp]
alpha.samp2.red <- alpha.samp2[temp]
beta.samp2.red <- beta.samp2[temp]
par(mfrow=c(2,2))
acf(alpha.samp1.red,lag.max=200)
acf(alpha.samp2.red,lag.max=200)
acf(beta.samp1.red,lag.max=200)
acf(beta.samp2.red,lag.max=200)

# combining chains
alpha.samp.final <- c(alpha.samp1.red,alpha.samp2.red)
beta.samp.final <- c(beta.samp1.red,beta.samp2.red)

numsamp <- length(alpha.samp.final)
numsamp

# examining samples

par(mfrow=c(2,1))
hist(alpha.samp.final,col="blue")
hist(beta.samp.final,col="blue")

#P(beta|y < 0)
mean(beta.samp.final < 0)

# plotting different posterior regression lines

par(mfrow=c(1,1))
plot(t,y,pch=19)
for (i in 1:numsamp){
  abline(alpha.samp.final[i],beta.samp.final[i],col="green")
}
points(t,y,pch=19)



# posterior predictive for 1974

t.1974 <- -1
y.pred.1974 <- rep(NA,numsamp)
for (i in 1:numsamp){
  rate <- alpha.samp.final[i] + beta.samp.final[i]*t.1974
  y.pred.1974[i] <- rpois(1,rate)
}


# posterior predictive for 1986 

t.1986 <- 10
y.pred.1986 <- rep(NA,numsamp)
for (i in 1:numsamp){
  rate <- alpha.samp.final[i] + beta.samp.final[i]*t.1986
  y.pred.1986[i] <- rpois(1,rate)
}

ymin <- min(y.pred.1974,y.pred.1986,y)
ymax <- max(y.pred.1974,y.pred.1986,y)
par(mfrow=c(3,1))
hist(y.pred.1974,main="Predictions for 1974 Accidents",xlim=c(ymin,ymax),col="gray")
abline(v=mean(y.pred.1974),col="red",lwd=2)
hist(y,main="Observed Accidents 1975-1985",xlim=c(ymin,ymax),col="gray")
abline(v=mean(y),col="red",lwd=2)
hist(y.pred.1986,main="Predictions for 1986 Accidents",xlim=c(ymin,ymax),col="gray")
abline(v=mean(y.pred.1986),col="red",lwd=2)


quantile(y.pred.1974,c(0.025,0.975))
quantile(y.pred.1986,c(0.025,0.975))



