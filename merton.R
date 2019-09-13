library(ggplot2)
pd = 0.4
rho = 0.0001
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
print(sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x[1])-qnorm(pd))^2+0.5*qnorm(x[1])^2))
vfun <- function(x){
  pd = 0.25
  rho = 0.05
  t <- sqrt((1-rho)/rho)*exp( -1/(2*rho)*(sqrt(1-rho)*qnorm(x)-qnorm(pd))^2+0.5*qnorm(x)^2)
  return(t)
}
area <- integrate(vfun,x[1],x[length(x)])$value


#Putting results into a dataframe
vas <- data.frame(x,x_norm,c,t)


plot(x*length(x),t,type = 'l')
u=pd*length(x)
sdu = sqrt(pd*length(x)*(1-pd))

n_data <-  dnorm(x*length(x),mean = u,sd=sdu)
lines(x*length(x),n_data)
plot(x*length(x),n_data,type='l')




#Plot cummulative distribution
plot(x,c,type = 'l',ylim = c(0,1))
lines(x,p)
n_data <-  pnorm(x*length(x),mean = u,sd=sdu)
lines(x,n_data,col='red')








