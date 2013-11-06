#!/usr/bin/Rscript


## single frequency

p <- 0.1
n <- 20

p <- 0.1
n <- 200

nreps <- 1000
y <- rbinom( nreps, n, p )

plot(table(y),xlim=c(0,n),xaxt='n',xlab="number of successes",main=paste("n =",n," p =", p), ylab="frequency")
axis(1)
abline(v=n*p+c(0,-2,2)*sqrt(n*p*(1-p)), col='red',lty=c(1,2,2))


## check Wilson's CIs

# single frequency

p <- 3/9
n <- 20
nreps <- 100
xy <- data.frame( pp = rep(p,nreps) )
xy$y <- rbinom( nreps, n, xy$pp )
xy$phat <- xy$y/n
xy$wilson <- (xy$y+2)/(n+4)
xy$ci.lower <- pmax(0, with(xy, wilson - 1.96*sqrt( wilson * (1-wilson) / (n+4) ) ) )
xy$ci.upper <- pmin(1, with(xy, wilson + 1.96*sqrt( wilson * (1-wilson) / (n+4) ) ) )

# proportion that lie in the CI
mean( (xy$pp > xy$ci.lower) & (xy$pp < xy$ci.upper) )

with( xy,  {
        plot( wilson, 1:nreps, ylab='', xlim=c(0,1) )
        abline(v=p,col='red',lty=2)
        segments( x0=ci.lower, x1=ci.upper, y0=1:nreps, col='green' )
    } )

# ordered
with( xy[order(xy$y),], {
        plot( wilson, 1:nreps, ylab='', xlim=c(0,1) )
        abline(v=p,col='red',lty=2)
        segments( x0=ci.lower, x1=ci.upper, y0=1:nreps, col='green' )
    } )


# population of different frequencies

n <- 20
nreps <- 100
xy <- data.frame( pp = runif(nreps) )
xy$y <- rbinom( nreps, n, xy$pp )
xy$phat <- xy$y/n
xy$wilson <- (xy$y+2)/(n+4)
xy$ci.lower <- pmax(0, with(xy, wilson - 1.96*sqrt( wilson * (1-wilson) / (n+4) ) ) )
xy$ci.upper <- pmin(1, with(xy, wilson + 1.96*sqrt( wilson * (1-wilson) / (n+4) ) ) )

# proportion that lie in the CI
mean( (xy$pp > xy$ci.lower) & (xy$pp < xy$ci.upper) )

with( xy,  {
        plot( wilson, 1:nreps, ylab='', xlim=c(0,1) )
        points( pp, 1:nreps, col='red', pch=20 )
        segments( x0=ci.lower, x1=ci.upper, y0=1:nreps, col='green' )
    } )

# ordered
with( xy[order(xy$pp),], {
        plot( wilson, 1:nreps, ylab='', xlim=c(0,1) )
        points( pp, 1:nreps, col='red', pch=20 )
        segments( x0=ci.lower, x1=ci.upper, y0=1:nreps, col='green' )
    } )

# ordered by size of the error
with( xy[order(xy$wilson-xy$pp),], {
        plot( wilson-pp, 1:nreps, ylab='', xlim=c(-1,1), xlab="error" )
        points( rep(0,nreps), 1:nreps, col='red', pch=20 )
        segments( x0=ci.lower-pp, x1=ci.upper-pp, y0=1:nreps, col='green' )
    } )
