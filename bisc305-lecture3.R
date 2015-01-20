
dotgroup <- function (x,f,gm=TRUE,sds=FALSE,col='black',gm.col='black',length=.1,factor=.2,ylims=mean(range(x,na.rm=TRUE))+c(-.8,.8)*diff(range(x,na.rm=TRUE)),...) {
    if (!is.factor(f)) { f <- factor(f) }
    dotcol <- adjustcolor(col,if(sds) {.25} else {1})
    gmeans <- tapply(x,f,mean,na.rm=TRUE)
    gsds <- tapply(x,f,sd,na.rm=TRUE)
    plot( jitter(as.numeric(f),factor), x, xlim=c(0,nlevels(f)+1), xlab='', xaxt='n', ylim=ylims, col=dotcol, ... )
    axis(1,at=1:nlevels(f),labels=paste(levels(f), " (",table(f),")",sep=''),las=3,...)
    if (gm) { segments( x0=(1:nlevels(f))-.15, x1=(1:nlevels(f))+.15, y0=gmeans, lwd=2, col=gm.col ) }
    if(sds) { arrows( x0=(1:nlevels(f)), y0=gmeans-gsds, y1=gmeans+gsds, code=3, length=length ) }
}

dotgroup.horiz <- function (x,f,gm=TRUE,sds=FALSE,col='black',gm.col='black',length=.1,factor=.2,ylims=c(0,nlevels(f)+1),...) {
    if (!is.factor(f)) { f <- factor(f) }
    dotcol <- adjustcolor(col,if(sds) {.25} else {1})
    gmeans <- tapply(x,f,mean,na.rm=TRUE)
    gsds <- tapply(x,f,sd,na.rm=TRUE)
    plot( x, jitter(as.numeric(f),factor), ylim=ylims, ylab='', yaxt='n', xlim=mean(range(x,na.rm=TRUE))+c(-.8,.8)*diff(range(x,na.rm=TRUE)), col=dotcol, ... )
    axis(2,at=1:nlevels(f),labels=paste(levels(f), " (",table(f),")",sep=''),las=3,...)
    if (gm) { segments( y0=(1:nlevels(f))-.15, y1=(1:nlevels(f))+.15, x0=gmeans, lwd=2, col=gm.col ) }
    if(sds) { arrows( y0=(1:nlevels(f)), x0=gmeans-gsds, x1=gmeans+gsds, code=3, length=length ) }
}



quakes <- read.table("2014-quakes-catalog.tsv", stringsAsFactors=FALSE)
names(quakes) <- c("orig.date","orig.time", "ET", "MAG", "M", "LAT", "LON", "DEPTH", "Q", "EVID", "NPH", "NGRM")
quakes$date <- as.Date(paste(quakes$orig.date,substr(quakes$orig.time,1,8)),format="%Y/%m/%d %H:%M:%S")
quakes$ddate <- difftime( quakes$date[c(2:nrow(quakes),nrow(quakes))] , quakes$date[c(1,1:(nrow(quakes)-1))] )
#YYY/MM/DD HH:mm:SS.ss 

dotloc <- 1.5
boxloc <- 2.5
dotjits <- dotloc+rnorm(sum(quakes$MAG>3))/20
pdf(file="quakes-dotplot.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
    with( subset(quakes, MAG>3), {
            plot(MAG, dotjits, xlab="magnitude", ylim=c(0,3), cex=0.5, pch=20, yaxt='n', ylab='' )
            # dotgroup.horiz( MAG, rep("2014",length(MAG)), xlab='magnitude', ylims=c(0,2.5) ) 
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=mean(MAG), lwd=2, col='green' )
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=median(MAG), lwd=2, col='red' )
        } )
dev.off()

pdf(file="quakes-hist.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
    with( subset(quakes, MAG>3), {
            plot(MAG, dotjits, xlab="magnitude", ylim=c(0,3), cex=0.5, pch=20, yaxt='n', ylab='' )
            # dotgroup.horiz( MAG, rep("2014",length(MAG)), xlab='magnitude', ylims=c(0,2.5) ) 
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=mean(MAG), lwd=2, col='green' )
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=median(MAG), lwd=2, col='red' )
            hist( MAG, col=adjustcolor("blue",.5), add=TRUE, freq=FALSE ) 
        } )
dev.off()

pdf(file="quakes-boxplot.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
    with( subset(quakes, MAG>3), {
            plot(MAG, dotjits, xlab="magnitude", ylim=c(0,3), cex=0.5, pch=20, yaxt='n', ylab='' )
            # dotgroup.horiz( MAG, rep("2014",length(MAG)), xlab='magnitude', ylims=c(0,2.5) ) 
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=mean(MAG), lwd=2, col='green' )
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=median(MAG), lwd=2, col='red' )
            hist( MAG, col=adjustcolor("blue",.5), add=TRUE, freq=FALSE ) 
            boxplot( MAG, add=TRUE, at=boxloc, horizontal=TRUE, range=0 )
        } )
dev.off()

pdf(file="quakes-modified-boxplot.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,3,1,1)+.1)
    with( subset(quakes, MAG>3), {
            plot(MAG, dotjits, xlab="magnitude", ylim=c(0,3), cex=0.5, pch=20, yaxt='n', ylab='' )
            # dotgroup.horiz( MAG, rep("2014",length(MAG)), xlab='magnitude', ylims=c(0,2.5) ) 
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=mean(MAG), lwd=2, col='green' )
            segments( y0=dotloc-.25, y1=dotloc+.25, x0=median(MAG), lwd=2, col='red' )
            hist( MAG, col=adjustcolor("blue",.5), add=TRUE, freq=FALSE ) 
            boxplot( MAG, add=TRUE, at=boxloc, horizontal=TRUE )
        } )
dev.off()

pdf(file="quakes-category-dotplot.pdf",width=3,height=2.5,pointsize=10)
par(mar=c(4,4,1,1)+.1)
    with( subset(quakes, MAG>3), {
            dotgroup( MAG, cut(date,breaks="month"), xaxt='n', ylab='Magnitude', pch=20, cex=0.5, gm.col='red', ylims=c(3,5.2) )
            axis(1,at=1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), las=3)
        } )
dev.off()

pdf(file="quakes-category-boxplot.pdf",width=3,height=2.5,pointsize=10)
par(mar=c(4,4,1,1)+.1)
    with( subset(quakes, MAG>3), {
            boxplot( MAG ~ cut(date,breaks="month"), xaxt='n', ylab='Magnitude', ylim=c(3,5.2) )
            axis(1,at=1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), las=3)
        } )
dev.off()

pdf(file="quakes-mag-depth.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,1,1)+.1)
with( subset(quakes, MAG>2), plot( MAG ~ DEPTH, xlab="depth", ylab="magnitude", pch=20, cex=0.5 ) )
dev.off()

pdf(file="quakes-mag-depth-lines.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,1,1)+.1)
with( subset(quakes, MAG>2), {
        plot( MAG ~ DEPTH, xlab="depth", ylab="magnitude", pch=20, cex=0.5 ) 
        lines( lowess( DEPTH, MAG ), col='red' )
    } )
dev.off()

pdf(file="quakes-mag-time.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,1,1)+.1)
with( subset(quakes, MAG>1), plot( MAG ~ date, xlab="", ylab="magnitude", pch=20, cex=0.1 ) )
dev.off()

pdf(file="quakes-mag-time-lines.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,1,1)+.1)
with( subset(quakes, MAG>1), {
         plot( MAG ~ date, xlab="", ylab="magnitude", pch=20, cex=0.1 ) 
         lines( lowess( date, MAG, f=.05), col='red' )
    } )
dev.off()

# map
if (interactive()) {
    plot( LAT ~ LON, data=quakes, cex=MAG/2 )

    hist(as.numeric(diff(subset(quakes,MAG>3)$date)))

    with( subset(quakes, MAG>3), dotgroup.horiz( MAG, cut(date,breaks="year") ) )
}
