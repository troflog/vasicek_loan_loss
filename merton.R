library(ggplot2)
pd = 0.25
rho = 0.9999
x <- seq(0,1,by=0.001)
x <- x[2:(length(x)-1)]
x[1]<-0.00001
x[length(x)]<-(1-x[1])
p <- rep(1-pd,length(x))
x_norm <- x/n
#Loan loss cummulative distribution
c <- pnorm((sqrt(1-rho)* qnorm(x)-qnorm(pd))/sqrt(rho))
#Densisty of loan loss distribution
t <- sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x)-qnorm(pd))^2+0.5*qnorm(x)^2)
print(sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x[1])-qnorm(pd))^2+0.5*qnorm(x[1])^2))
vfun <- function(x){
  pd = 0.25
  rho = 0.8
  t <- sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x)-qnorm(pd))^2+0.5*qnorm(x)^2)
  return(t)
}
area <- integrate(vfun,x[1],x[length(x)])$value


#Putting results into a dataframe
vas <- data.frame(x,x_norm,c,t)

#Plot cummulative distribution
plot(x,c,type = 'l',ylim = c(0,1))
lines(x,p)
plot(x,t,type = 'l')







