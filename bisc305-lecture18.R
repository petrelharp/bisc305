#!/usr/bin/Rscript

nflips <- 3
nflips <- 5

nreps <- 1e4
flips <- as.data.frame( ifelse( matrix( rbinom( nreps * nflips, size=1, prob=0.5 ), nrow=nreps )>0, "H", "T" ) )

## look at pattern counts
table( apply( flips, 1, paste, collapse='' ) )

layout(1:2)

counts <- table( apply( flips, 1, paste, collapse='' ) )
plot( (1:2^nflips), as.numeric(counts)/nreps, col='blue', xaxt='n', yaxt='s', ylim=c(0,2/2^nflips), ylab='proportion', xlab=''  )
axis(1,at=1:2^nflips,las=3, labels=names(counts) )
segments( x0=(1:2^nflips)-.2, x1=(1:2^nflips)+.2, y0=1/2^nflips )

## now distribution of number of heads
table( rowSums( flips=="H" ) )

headcounts <- table( rowSums( flips=="H" ) )
plot( (0:nflips), as.numeric(headcounts)/nreps, col='blue', yaxt='s', ylim=c(0,0.5), ylab='proportion', xlab='number of H', xaxt='n'  )
segments( x0=(0:nflips)-.2, x1=(0:nflips)+.2, y0=dbinom(0:nflips,nflips,p=0.5) )
axis(1,at=0:nflips)

## the table
rbind( "num heads"=0:nflips, "prob"=dbinom( 0:nflips, nflips, p=0.5 ) )
