#!/usr/bin/Rscript

ndraws <- 20
nreps <- 100
xy <- data.frame( pp = rbeta( nreps, 2, 2 ) )
xy$nn <- rbinom( nreps, ndraws, xy$pp )
xy$phat <- xy$nn/ndraws
xy$wilson <- (xy$nn+2)/(ndraws+4)

with(xy, plot(phat, wilson) )
abline(0,1)

with( subset(xy,nn=5), hist(pp) )
abline(v=(5+2)/(ndraws+4))

plot( (0:ndraws+2)/(ndraws+4), with(xy, tapply(pp,nn,mean) ) )
points( (0:ndraws)/(ndraws), with(xy, tapply(pp,nn,mean) ), col='red' )
