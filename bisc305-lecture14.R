#!/usr/bin/R

dchart <- function(x, ..., gcex=2.5, gcol='red', gbg=adjustcolor("red",.5), gpch=21 ) {
    dotchart(x,...)
    points( colMeans(x), (ncol(x):1)*(nrow(x)+2)-nrow(x)/2-2, cex=gcex, col=gcol, bg=gbg, pch=gpch )
}

#######
# look at CLT and approach to normality of sampling distribution

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
    abline(v=0)
    cmh <- hist(colMeans(xx),xlim=xlims,col=adjustcolor("red",.5),freq=FALSE,main='',xlab='')
    xbreaks <- seq(xlims[1],xlims[2],length.out=1000)
    xmids <- xbreaks[-1]-diff(xbreaks)/2
    lines(xmids, diff( pnorm( xbreaks, mean=rmean, sd=rsd/sqrt(n) ) )/diff(xbreaks), col='red', lwd=2 )
    hist(as.vector(xx),add=TRUE,freq=FALSE,col=adjustcolor("black",.5),breaks=min(50,length(xx)/4))
    xvals <- seq(xlims[1],xlims[2],length.out=100)
    layout(1)
    invisible(colMeans(xx))
}

#
n <- 10
rdata <- rnorm  
rmean <- 0
rsd <- 1

xlims <- c(-6,6)
hbreaks <- seq(xlims[1],xlims[2],length.out=20)

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
#  Truncated Cauchy??
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

