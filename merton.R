library(ggplot2)
pd = 0.25
rho = 0.8
x <- seq(0,1,by=0.0001)
x <- x[2:(length(x)-1)]
x[1]<-0.00001
x[length(x)]<-(1-x[1])
p <- rep(1-pd,length(x))
x_norm <- x/n
#Loan loss cummulative distribution
c <- pnorm((sqrt(1-rho)* qnorm(x)-qnorm(pd))/sqrt(rho))
#Densisty of loan loss distribution
t <- sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x)-qnorm(pd))^2+0.5*qnorm(x)^2)

#Putting results into a dataframe
vas <- data.frame(x,x_norm,c,t)





