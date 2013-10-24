#!/usr/bin/R

dchart <- function(x, ..., gcex=2.5, gcol='red', gbg=adjustcolor("red",.5), gpch=21 ) {
    dotchart(x,...)
    points( colMeans(x), (ncol(x):1)*(nrow(x)+2)-nrow(x)/2-2, cex=gcex, col=gcol, bg=gbg, pch=gpch )
}

#######
# look at CLT and approach to normality of sampling distribution

n <- 10
rdata <- rnorm  
rmean <- 0
rsd <- 1

xlims <- c(-6,6)
hbreaks <- seq(xlims[1],xlims[2],length.out=20)

# plot one example
f <- function (...,plotit=TRUE) {
    x <- rdata(n)
    if (plotit) { dotchart(x,xlim=xlims,pch=20,...) }
    mean(x)
}

# plot lots of examples
fmat <- function(nreps=20,...) {
    nreps <- min( nreps, floor(2e6/n) )
    xx <- matrix(rdata(n*nreps),nrow=n)
    dothese <- 1:min(ncol(xx), floor(5000/n))
    layout(1:2)
    dchart(xx[,dothese,drop=FALSE],xlim=xlims,gdata=colMeans(xx[,dothese,drop=FALSE]),labels=NA,pch=20,gcex=2,ylab='datasets',...)
    cmh <- hist(colMeans(xx),xlim=xlims,col=adjustcolor("red",.5),freq=FALSE,main='',xlab='')
    xbreaks <- seq(xlims[1],xlims[2],length.out=1000)
    xmids <- xbreaks[-1]-diff(xbreaks)/2
    lines(xmids, diff( pnorm( xbreaks, mean=rmean, sd=rsd/sqrt(n) ) )/diff(xbreaks), col='red', lwd=2 )
    hist(as.vector(xx),add=TRUE,freq=FALSE,col=adjustcolor("black",.5),breaks=min(50,length(xx)/4))
    xvals <- seq(xlims[1],xlims[2],length.out=100)
    layout(1)
    invisible(colMeans(xx))
}

# do each of these a few times
f()
fmat(2)
fmat(10)
fmat(100)
fmat(1000)

## compare sample sizes
rdata <- rnorm  # want this to return random variates with SD=1
rmean <- 0
rsd <- 1
xlims <- c(-6,6)

n <- 5
fmat(1000000)

xlims <- c(-6,6)
n <- 100
fmat(10000)

xlims <- c(-2,2)
n <- 100
fmat(10000)


##
# ok, now with exponential data
rdata <- rexp
rmean <- 1
rsd <- 1

xlims <- c(-1,7)
n <- 5
fmat(1000000)

xlims <- c(-1,7)
n <- 100
fmat(10000)

xlims <- c(0,2)
n <- 100
fmat(10000)

##
# and, transformed
rdata <- function (n) { (log(rexp(n)) + .577)/1.28 }
rmean <- 0
rsd <- 1

xlims <- c(-3,3)
n <- 5
fmat(1000000)

xlims <- c(-3,3)
n <- 100
fmat(10000)

xlims <- c(-1,1)
n <- 100
fmat(10000)

##
#  Cauchy??
rdata <- function (n) { x <- rcauchy(n); x[abs(x)>50] <- rnorm(sum(abs(x)>50)); x }
rmean <- 0
rsd <- 5.5

xlims <- c(-20,20)
n <- 5
fmat(1000000)

xlims <- c(-20,20)
n <- 100
fmat(10000)

xlims <- c(-2,2)
n <- 100
fmat(10000)



#######
# Explore what happens to a $t$-test under deviations from the assumptions.

# plot lots of examples
ft <- function(nreps=10000,...) {
    nreps <- min( nreps, floor(2e6/n) )
    xx <- matrix(rdata(n*nreps),nrow=n)
    yy <- matrix(rdata(n*nreps),nrow=n)
    ttt <- lapply( 1:nreps, function (k) t.test( xx[,k], yy[,k] ) )
    tt <- sapply( ttt, "[[", "statistic" )
    # attr(tt,"df") <- mean( sapply( ttt, "[[", "parameter" ) )
    df <- attr(tt,"df") <- 2*(n-1)
    layout(1:2)
    th <- hist(tt,xlim=c(-3,3),breaks=50)
    xvals <- seq(-3,3,length.out=100)
    xmids <- xvals[-1] - diff(xvals)/2
    lines( th$mids, sum(th$counts) * diff( pt(th$breaks, df=attr(tt,"df")) ), col='red' )
    qtiles <- seq(0,.1,length.out=100)
    plot( qtiles, pt(quantile(tt,prob=qtiles),df=df), type='l', lwd=2, xlab='true P-value', ylab='estimated P-value' )
    points( .05, pt(quantile(tt,prob=.05),df=df) )
    lines( c(.05,.05,0), c(0, pt(quantile(tt,prob=.05),df=df), pt(quantile(tt,prob=.05),df=df) ) )
    abline(0,1,col='red')
    layout(1)
    return(invisible(tt))
}

# Normal
n <- 10
rdata <- rnorm  # want this to return random variates with SD=1
ft()


# Exponential plus outliers
rdata <- function (n) c( rexp(floor(.9*n)), rexp(n-floor(.9*n),rate=1/1000) )
ft()

# Cauchy???
rdata <- rcauchy
ft()













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
