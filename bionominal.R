library(ggplot2)
n = 1000000
s <- 0.01
pd = 0.3
rng <- floor(n*pd*s)
center <- floor(pd*n)
x_center <- seq(center-rng,center+rng,by=1)
x <- c(0,x_center,n)
# Create the binomial distribution.
y <- dbinom(x,n,pd)
x <- x/n

# Give the chart file a name.

# Plot the graph for this sample.
qplot(x,y,geom = 'line')
