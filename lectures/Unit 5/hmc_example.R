########################################################
##### HAMILTONIAN MONTE CARLO FOR BIVARIATE NORMAL #####
########################################################

epsilon <- 0.3
numsamp <- 1000
L <- 20

Sigma <- as.matrix(rbind(c(1,0.9),c(0.9,1)))
Sigma.inv <- solve(Sigma)

E <- function(theta){
	out <- 0.5*t(theta)%*%Sigma.inv%*%theta
	out
}

dE <- function(theta){
	out <- t(theta)%*%Sigma.inv
	out
}

K <- function(rho){
	out <- t(rho)%*%rho/2
	out
}

theta.samp <- matrix(NA,numsamp,2)
theta <- c(0,6)
theta <- t(t(theta))
theta.samp[1,] <- theta
for (t in 2:numsamp){
	rho.0 <- t(t(rnorm(2)))
	E.old <- E(theta)
	K.old <- K(rho.0)
	rho.star <- rho.0 - (epsilon/2)*t(dE(theta))
	theta.star <- theta + epsilon*rho.star
	for (l in 1:L){
		rho.star <- rho.star - epsilon*t(dE(theta.star))
		theta.star <- theta.star + epsilon*rho.star	
	}
    rho.star <- rho.star - (epsilon/2)*t(dE(theta.star))
    E.star <- E(theta.star)
    K.star <- K(rho.star)
    r <- exp(E.old+K.old-E.star-K.star)
    u <- runif(1)
    if (u <= r){
    	theta <- theta.star
    }    
    theta.samp[t,] <- theta
}

par(mfrow=c(1,2))
plot(theta.samp,pch=19)
lines(theta.samp[1:100,],col="red")
plot(theta.samp,pch=19)
lines(theta.samp[901:1000,],col="green")


theta.hmc <- theta.samp

############################################
##### COMPARING TO GIBBS SAMPLER       #####
############################################

rho <- 0.9
theta.samp <- matrix(NA,numsamp,2)
theta.samp[1,] <- c(0,6)
for (i in 2:numsamp){
  theta.samp[i,1] <- rnorm(1,rho*theta.samp[i-1,2],sqrt(1-rho))
  theta.samp[i,2] <- rnorm(1,rho*theta.samp[i,1],sqrt(1-rho))
}

par(mfrow=c(1,2))
plot(theta.samp,pch=19)
lines(theta.samp[1:100,],col="red")
plot(theta.samp,pch=19)
lines(theta.samp[901:1000,],col="green")

theta.gibbs <- theta.samp

par(mfrow=c(2,2))
plot(theta.hmc,pch=19,main="Hamiltonian MCMC: First 100 samples")
lines(theta.hmc[1:100,],col="red",lwd=2)
plot(theta.hmc,pch=19,main="Hamiltonian MCMC: Last 100 samples")
lines(theta.hmc[901:1000,],col="green",lwd=2)
plot(theta.gibbs,pch=19,main="Gibbs Sampling: First 100 samples")
lines(theta.gibbs[1:100,],col="red",lwd=2)
plot(theta.gibbs,pch=19,main="Gibbs Sampling: Last 100 samples")
lines(theta.gibbs[901:1000,],col="green",lwd=2)

par(mfrow=c(2,2))
acf(theta.gibbs[,1])
acf(theta.gibbs[,2])
acf(theta.hmc[,1])
acf(theta.hmc[,2])


# https://github.com/stan-dev/rstan/blob/develop/README.md

# http://mc-stan.org/users/documentation/tutorials


