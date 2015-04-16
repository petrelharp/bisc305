# simulate null distribution for contingency table

p <- c(.01,.1,.3,.59)
df <- length(p)-1
n <- 1000
nreps <- 100
nn <- matrix( rmultinom( nreps, n, prob=p ), nrow=length(p) )

matplot(jitter(row(nn)), nn, pch=20, col=adjustcolor(rainbow(64),.5), main="counts" )
segments(x0=seq_along(p)-.1,x1=seq_along(p)+.1,y0=p*n, lwd=2)

matplot(jitter(row(nn)), nn-p*n, pch=20, col=adjustcolor(rainbow(64),.5), main="difference from expected" )
abline(h=0,lwd=2)

matplot( jitter(row(nn)), (nn-p*n)/sqrt(p*n), pch=20, col=adjustcolor(rainbow(64),.5), main="normalized difference from expected" )
abline(h=0,lwd=2)


nreps <- 10000
nn <- matrix( rmultinom( nreps, n, prob=p ), nrow=length(p) )
chisqs <- colSums((nn-p*n)^2/(p*n))

chih <- hist(chisqs,breaks=40)
with( chih, lines( mids, sum(counts)*diff( pchisq(breaks, df=df ) ), col='red', lwd=2 ) )


# compare table to simulation
q05 <- quantile( chisqs, probs=.95 )
q05
abline( v=q05, col='green' )

t05 <- qchisq(.95,df=df)
t05
abline(v=t05, col='blue' )
