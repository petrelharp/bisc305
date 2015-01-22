if (interactive()) {
# Example: why n-1?

nsamps <- 1e4

x <- rnorm(nsamps)
hist(x, col=adjustcolor("blue",0.5), breaks=100, xlim=c(-4.5,4.5))
abline( v=mean(x), col='red' )
abline( v=mean(x)+c(-2,2)*sd(x), lty=2, col='red' )

n.vec <- 10^(seq(1,4,length.out=1000))
sim.sds <- sapply( n.vec, function (n) { x <- rnorm(n); c( sd( x ), sqrt(mean((x-mean(x))^2)) ) } )

plot( n.vec, sim.sds[1,], log='x', ylim=c(0,2) )
points( n.vec, sim.sds[2,], col='red' )
abline(h=1)

n <- 10
small.sds <- replicate( 1e4, { x <- rnorm(n); c( sd( x ), sqrt(mean((x-mean(x))^2)) ) } )
rownames(small.sds) <- c("n-1","n")
hist(small.sds[1,], xlim=c(0.3,1.8), col=adjustcolor("blue",0.5), breaks=100, main='')
hist(small.sds[2,], col=adjustcolor("red",0.5), breaks=100, add=TRUE)
abline(v=1,lwd=3)

rowMeans(small.sds^2)



###
# CV plots

layout(1:3)
par(mar=c(2,1,3,0)+.1)
for (cv in c(1,10,100)){
    hist( x+cv, breaks=30, xlim=range(0,x+cv), main=paste("CV =", cv), xlab='', ylab='' )
}


}

###
# figures for slides

quakes <- read.table("2014-quakes-catalog.tsv", stringsAsFactors=FALSE)
names(quakes) <- c("orig.date","orig.time", "ET", "MAG", "M", "LAT", "LON", "DEPTH", "Q", "EVID", "NPH", "NGRM")
quakes$date <- as.Date(paste(quakes$orig.date,substr(quakes$orig.time,1,8)),format="%Y/%m/%d %H:%M:%S")

pdf(file="quakes-hist-IQR.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
xh <- with(quakes, hist(MAG, breaks=60,xlab='magnitude',main='') )
with( subset(quakes, rank(MAG)>nrow(quakes)/4 & rank(MAG)<nrow(quakes)*3/4), 
        hist(MAG, col=adjustcolor("blue",0.5), breaks=xh$breaks, add=TRUE ) )
with( quakes, {
    text( quantile(MAG,.75), 1000, labels="50%", col='red', cex=0.75 )
    abline( v=median(quakes$MAG), col='red' )
    abline( v=quantile(quakes$MAG,c(.25,.75)), lty=2, col='red' )
        } )
dev.off()

pdf(file="quakes-hist-only.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
xh <- with(quakes, hist(MAG, breaks=60,main='',xlab='magnitude') )
with( quakes, {
        abline( v=mean(MAG), col='red' )
        for (k in 1:3) {
            with( subset(quakes, abs(MAG-mean(MAG))<k*sd(MAG) ), 
                    hist(MAG, col=adjustcolor(k,0.3), breaks=xh$breaks, add=TRUE ) )
        }
    } )
dev.off()

pdf(file="quakes-hist-mean-sd.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
xh <- with(quakes, hist(MAG, breaks=60,main='',xlab='magnitude') )
with( quakes, {
        abline( v=mean(MAG), col='red' )
        for (k in 1:3) {
            with( subset(quakes, abs(MAG-mean(MAG))<k*sd(MAG) ), 
                    hist(MAG, col=adjustcolor(k,0.3), breaks=xh$breaks, add=TRUE ) )
            abline( v=mean(MAG)+c(-k,k)*sd(MAG), lty=1+k, col='red' )
            pmag <- round( mean(abs(MAG-mean(MAG))<k*sd(MAG)), 2 )
            text( mean(MAG)+k*sd(MAG), 1000-200*(k-1), labels=paste(100*pmag,"%"), col='red', cex=0.75 )
        }
    } )
dev.off()


pdf(file="quakes-sqrt-hist-IQR.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
xh <- with(quakes, hist(sqrt(MAG), breaks=60,xlab='magnitude',main='') )
with( subset(quakes, rank(sqrt(MAG))>nrow(quakes)/4 & rank(sqrt(MAG))<nrow(quakes)*3/4), 
        hist(sqrt(MAG), col=adjustcolor("blue",0.5), breaks=xh$breaks, add=TRUE ) )
with( quakes, {
    text( quantile(sqrt(MAG),.75), 1000, labels="50%", col='red', cex=0.75 )
    abline( v=median(sqrt(MAG)), col='red' )
    abline( v=quantile(sqrt(MAG),c(.25,.75)), lty=2, col='red' )
    } )
dev.off()

pdf(file="quakes-sqrt-hist-mean-sd.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
xh <- with(quakes, hist(sqrt(MAG), breaks=60,main='',xlab='magnitude') )
with( quakes, {
        abline( v=mean(sqrt(MAG)), col='red' )
        for (k in 1:3) {
            with( subset(quakes, abs(sqrt(MAG)-mean(sqrt(MAG)))<k*sd(sqrt(MAG)) ), 
                    hist(sqrt(MAG), col=adjustcolor(k,0.3), breaks=xh$breaks, add=TRUE ) )
            abline( v=mean(sqrt(MAG))+c(-k,k)*sd(sqrt(MAG)), lty=1+k, col='red' )
            pmag <- round( mean(abs(sqrt(MAG)-mean(sqrt(MAG)))<k*sd(sqrt(MAG))), 2 )
            text( mean(sqrt(MAG))+k*sd(sqrt(MAG)), 1000-200*(k-1), labels=paste(100*pmag,"%"), col='red', cex=0.75 )
        }
    } )
dev.off()


pdf(file="quakes-lat-mean-sd.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
xh <- with(quakes, hist(LAT, breaks=60,main='',xlab='magnitude') )
with( quakes, {
        abline( v=mean(LAT), col='red' )
        for (k in 1:3) {
            with( subset(quakes, abs(LAT-mean(LAT))<k*sd(LAT) ), 
                    hist(LAT, col=adjustcolor(k,0.3), breaks=xh$breaks, add=TRUE ) )
            abline( v=mean(LAT)+c(-k,k)*sd(LAT), lty=1+k, col='red' )
            pmag <- round( mean(abs(LAT-mean(LAT))<k*sd(LAT)), 2 )
            text( mean(LAT)+k*sd(LAT), 1000-200*(k-1), labels=paste(100*pmag,"%"), col='red', cex=0.75 )
        }
    } )
dev.off()

