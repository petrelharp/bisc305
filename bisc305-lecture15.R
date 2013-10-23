#!/usr/bin/R


# Explore what happens to a $t$-test under deviations from the assumptions.

#####
# Normal data:
n <- 100
effect.size <- 1
rdata <- rnorm  # want this to return random variates with mean=0 and SD=1

xlims <- c(-6,6)
hbreaks <- seq(xlims[1],xlims[2],length.out=20)

# plot one example
f <- function (...,plotit=TRUE) {
    data <- data.frame( x=rdata(n)+effect.size, y=rdata(n) )
    if (plotit) {
        hist(data$x,xlim=xlims,col=adjustcolor("blue",.5),breaks=hbreaks,main='observed values',...)
        hist(data$y,add=TRUE,col=adjustcolor("red",.5),breaks=seq(-5,5,length.out=20))
        abline(v=c(mean(data$x),mean(data$y)),lwd=2)
    }
    t.test(data$x,data$y)
}

# plot lots of examples
fmat <- function() {
    layout(matrix(1:25,nrow=5))
    opar <- par(mar=c(0,0,0,0)+.1)
    replicate( 25, f(xaxt='n',yaxt='n',main='',xlab='',ylab='') )
    par(opar)
    layout(1)
}

f()
fmat()

# plot distribution of differences in means
ff <- function (nreps=1000,...) {
    dmeans <- replicate( nreps, f(plotit=FALSE) )
    dh <- hist(dmeans, breaks=50, col=adjustcolor("grey",.5), xlim=c(0,2), main=paste("difference in sample means,", n, "samples"), ... )
    text(effect.size,max(dh$counts),labels="truth",col='red',pos=4,cex=2)
    abline(v=effect.size,lwd=2,col='red')
    lines(dh$mids,sum(dh$counts)*diff(pnorm(dh$breaks,mean=effect.size,sd=sqrt(2/n))),lwd=2,col='green')
}

ff(1000)


######
# Now look at t-distribution

effect.size <- 0

t.tests <- lapply(1:10000, function (x) f(plotit=FALSE) )
t.vals <- sapply( t.tests, "[[", "statistic" )
t.df <- sapply( t.tests, "[[", "parameter" )
th <- hist(t.vals,breaks=30,main='t-statistics')
lines( th$mids, sum(th$counts) * diff( pt(th$breaks,df=2*(n-1)) ) )



#####
# explore changing sample size
n <- 20
fmat()

ff(1000)

#
n <- 50
fmat()

ff(1000)

#
n <- 100
fmat()

ff(1000)


####
# make distr'n more skewed

p.out <- .04
sd.out <- 4
rdata <- function(n) {
    n.out <- rbinom(1,n,p.out)
    c( rnorm(n.out,sd=sd.out), rnorm(n-n.out,sd=sqrt((1-p.out*sd.out^2)/(1-p.out))) )
}

n <- 20
f()
ff(1000)


####
# make distr'n less skewed

rdata <- function(n) {
    sqrt(12)*(runif(n)-.5)
}

n <- 20
f()
ff(1000)
