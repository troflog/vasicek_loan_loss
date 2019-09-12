library(ggplot2)
pd = 0.2
rho = 0.5
x <- seq(0,1,by=0.0001)
x <- x[2:(length(x)-1)]
c <- pnorm((sqrt(1-rho)* qnorm(x)-qnorm(pd))/sqrt(rho))
qplot(x,c,geom = 'line')
