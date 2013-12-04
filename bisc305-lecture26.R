galton <- read.csv("Galton.csv")

pdf(file='galton.pdf', width=3, height=2, pointsize=10)
par(mar=c(4,4,0,0)+.3)
with(galton, plot(jitter(Midparent),jitter(Child), pch=20, cex=.5, col=adjustcolor('black',.25), xlab='Midparent', ylab='Child') )
with(galton, abline( lm( Child ~ Midparent ) ) )
dev.off()

pdf(file='galton-mean.pdf', width=3, height=2, pointsize=10)
par(mar=c(4,4,0,0)+.3)
with(galton, plot(jitter(Midparent),jitter(Child), pch=20, cex=.5, col=adjustcolor('black',.25), xlab='Midparent', ylab='Child') )
with(galton, abline( lm( Child ~ Midparent ) ) )
abline(h=mean(galton$Child), col='red')
points( c(66,69), rep( mean(galton$Child), 2) , col='red' )
dev.off()

pdf(file='galton-pred.pdf', width=3, height=2, pointsize=10)
par(mar=c(4,4,0,0)+.3)
with(galton, plot(jitter(Midparent),jitter(Child), pch=20, cex=.5, col=adjustcolor('black',.25), xlab='Midparent', ylab='Child') )
with(galton, abline( lm( Child ~ Midparent ) ) )
abline(v=c(66,69), col='red')
points( c(66,69), predict( lm( Child ~ Midparent, data=galton ), newdata=list(Midparent=c(66,69)) ), col='red' )
dev.off()


# nonlinearities

pdf(file='nonlinear1.pdf', width=4, height=2, pointsize=10)
layout(t(1:2))
par(mar=c(3.5,3.5,1,0)+.1,mgp=c(2,1,0))
xx <- runif( 40 )*10
yy <- (4-((5-xx)/3)^2)+rnorm(40)*.1 
zz <- (25-((12-xx)/3)^2)+rnorm(40)*.1 
plot( xx, yy, xlab='X', ylab='Y', cex=0.5 )
abline( coef(lm(yy~xx)) )
plot( xx, zz, xlab='X', ylab='Y', cex=0.5 )
abline( coef(lm(zz~xx)) )
dev.off()


pdf(file='nonlinear2.pdf', width=4, height=2, pointsize=10)
layout(t(1:2))
par(mar=c(3.5,3.5,1,0)+.1,mgp=c(2,1,0))
xx <- runif( 40 )*10
yy <- (4-((5-xx)/3)^2)+rnorm(40)
zz <- (25-((12-xx)/3)^2)+rnorm(40)
plot( xx, yy, xlab='X', ylab='Y', cex=0.5 )
abline( coef(lm(yy~xx)) )
plot( xx, zz, xlab='X', ylab='Y', cex=0.5 )
abline( coef(lm(zz~xx)) )
dev.off()


pdf(file='nonlinear2-resids.pdf', width=4, height=2, pointsize=10)
layout(t(1:2))
par(mar=c(3.5,3.5,1,0)+.1,mgp=c(2,1,0))
xx <- runif( 40 )*10
zz <- (25-((12-xx)/3)^2)+rnorm(40)
plot( xx, zz, xlab='X', ylab='Y', cex=0.5 )
abline( coef(lm(zz~xx)) )
plot( fitted(lm(zz~xx)), resid(lm(zz~xx)), xlab='predicted', ylab='residuals', cex=0.5 )
abline(h=0)
dev.off()


pdf(file='hetersked-resids.pdf', width=4, height=2, pointsize=10)
layout(t(1:2))
par(mar=c(3.5,3.5,1,0)+.1,mgp=c(2,1,0))
xx <- sort(runif( 40 )*10)
zz <- xx+rnorm(40)*seq(.05,4,length.out=40)
plot( xx, zz, xlab='X', ylab='Y', cex=0.5 )
abline( coef(lm(zz~xx)) )
plot( fitted(lm(zz~xx)), resid(lm(zz~xx)), xlab='predicted', ylab='residuals', cex=0.5 )
abline(h=0)
dev.off()


# what can go wrong?

n <- 20
xxyy <- lapply(1:20, function (k) {
    xx <- sort(runif( n )*10)
    zz <- xx+rnorm(n)*(c(rep(1,n-1),10))
    data.frame(x=xx,y=zz) } )

pdf(file='many-regressions-1.pdf', width=4.5, height=3, pointsize=10)
layout(matrix(1:20,nrow=4))
par(mar=c(0,0,0,0)+.2)
for (xy in xxyy) {
    plot(xy,xaxt='n',yaxt='n',xlab='',ylab='',pch=20, col=c(rep('black',n-1),'red') )
    # abline(coef(lm(y~x,data=xy)))
    # abline(coef(lm(y~x,data=xy,subset=((1:n)<n))), col='green')
    abline(0,1,col='red')
}
dev.off()


pdf(file='many-regressions-2.pdf', width=4.5, height=3, pointsize=10)
layout(matrix(1:20,nrow=4))
par(mar=c(0,0,0,0)+.2)
for (xy in xxyy) {
    plot(xy,xaxt='n',yaxt='n',xlab='',ylab='',pch=20, col=c(rep('black',n-1),'red') )
    abline(coef(lm(y~x,data=xy)))
    abline(coef(lm(y~x,data=xy,subset=((1:n)<n))), col='green')
    abline(0,1,col='red')
}
dev.off()


pdf(file='many-regressions-3.pdf', width=4.5, height=3, pointsize=10)
layout(matrix(1:20,nrow=4))
par(mar=c(0,0,0,0)+.2)
for (xy in xxyy) {
    plot(xy,xaxt='n',yaxt='n',xlab='',ylab='',pch=20, col=adjustcolor(c(rep('black',n-1),'red'),.1) )
    abline(coef(lm(y~x,data=xy)))
    abline(coef(lm(y~x,data=xy,subset=((1:n)<n))), col='green')
    abline(0,1,col='red')
}
dev.off()

