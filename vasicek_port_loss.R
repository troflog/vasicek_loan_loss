library(ggplot2)

###########################################################################
###########################################################################
###                                                                     ###
###             VASICEK PORTFOLIO LOSS MODEL                            ###  
###                                                                     ###
###########################################################################
###########################################################################

# This program plots both the density and the cumulative portfolio loan loss distribution
#http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.139.5752&rep=rep1&type=pdf


#Probability of default
pd = 0.2
rho = 0.2

#Make a sequence of portfolio loan loss x = amount defaulted loan/total number of loan
x <- seq(0,1,by=0.0001)
x <- x[2:(length(x)-1)]
x[1]<-0.00001
x[length(x)]<-(1-x[1])

#Vector with pds
p <- rep(1-pd,length(x))

#Portfolio loan loss x cummulative distribution
portfolio_loss_cum <- function(x,pd,rho){
        c <- pnorm((sqrt(1-rho)* qnorm(x)-qnorm(pd))/sqrt(rho))
        return(c)
}

#Portfolio loan loss x density
portfolio_loss_density <- function(x,pd,rho){
  d <- sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x)-qnorm(pd))^2+0.5*qnorm(x)^2)
  return(d)
}

#Check the integral of the density between 0 and 1. Should be one
area <- integrate(portfolio_loss_density,0.00001,0.99999,pd,rho)
print(area)

#Density of loan loss distribution
t <- portfolio_loss_density(x,pd,rho)

#Plot cummulative distribution
plot(x,c,type ='l')

#Plot density
plot(x,t,type ='l')


#Simulate portfoilo loss


sim_port_loss <- function(n,number_of_years,pd,rho){
  loss_lim <- qnorm(pd)
  systematic_factor <-matrix(rep(rnorm(number_of_years),each=n) ,nrow=n)
  idiosyncratic_factor <- matrix(rnorm(n*number_of_years),nrow=n,ncol=number_of_years)  
  firm_value <- sqrt(rho)*systematic_factor+sqrt(1-rho)*idiosyncratic_factor
  sim_port_loss = colSums(firm_value<loss_lim)/n
}


n=1500
number_of_years= 20000
x_sim <- sim_port_loss(n,number_of_years,pd,rho)

hist(x_sim)
par(new=T)
plot(x,t,type='l',col='blue',ylab='',xlab='', xaxt = "n",yaxt = "n")
legend("topright",legend=c('Simulated loan loss','Teoretical loan loss'),
       col = c("black", "red"), lty = c(1, 2))









