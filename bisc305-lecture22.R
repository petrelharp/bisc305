xy <- data.frame( x=rnorm(18)+4, a=sample(c('treatment','control'),18,replace=TRUE) )
xy$x[xy$a=='treatment'] <- xy$x[xy$a=='treatment'] + 1

dotgroup <- function (x,f,gm=TRUE,sds=FALSE,col='black',length=.1,factor=.2,...) {
    if (!is.factor(f)) { f <- factor(f) }
    dotcol <- adjustcolor(col,if(sds) {.25} else {1})
    gmeans <- tapply(x,f,mean,na.rm=TRUE)
    gsds <- tapply(x,f,sd,na.rm=TRUE)
    plot( jitter(as.numeric(f),factor), x, xlim=c(0,nlevels(f)+1), xlab='', xaxt='n', ylim=mean(range(x,na.rm=TRUE))+c(-.8,.8)*diff(range(x,na.rm=TRUE)), col=dotcol, ... )
    axis(1,at=1:nlevels(f),labels=paste(levels(f), " (",table(f),")",sep=''),las=3,...)
    if (gm) { segments( x0=(1:nlevels(f))-.15, x1=(1:nlevels(f))+.15, y0=gmeans, lwd=2 ) }
    if(sds) { arrows( x0=(1:nlevels(f)), y0=gmeans-gsds, y1=gmeans+gsds, code=3, length=length ) }
}

pdf(file='dots1ex.pdf',width=3,height=2,pointsize=10)
par(mar=c(5,3,0,0)+.1)
dotgroup(xy$x, xy$a)
dev.off()


####
nsamples <- 32
beaks <- data.frame( beak=rnorm(32)+4, a=sample(c('A','B',"C"),nsamples,replace=TRUE) )
beaks$beak[beaks$a=='A'] <- beaks$beak[beaks$a=='A'] + 1
beaks$beak[beaks$a=='C'] <- beaks$beak[beaks$a=='C'] + .1


pdf(file='dots2ex.pdf',width=3,height=2,pointsize=10)
par(mar=c(5,3,0,0)+.1)
dotgroup(beaks$beak, beaks$a)
dev.off()

pdf(file='dots2ex-within.pdf',width=3,height=2,pointsize=10)
par(mar=c(5,3,0,0)+.1)
dotgroup(beaks$beak, beaks$a,sds=TRUE)
dev.off()

pdf(file='dots2ex-between.pdf',width=3,height=2,pointsize=10)
par(mar=c(5,3,0,0)+.1)
dotgroup(beaks$beak, beaks$a, col=adjustcolor("black",.25))
dev.off()

pdf(file='dots2ex-within-between.pdf',width=4,height=3,pointsize=10)
layout(t(1:2))
par(mar=c(5,3,1,0)+.1)
gmeans <- tapply(beaks$beak,beaks$a,mean,na.rm=TRUE)
dotgroup(beaks$beak, beaks$a, col=adjustcolor("black",.25),gm=FALSE,main="between")
points( jitter(as.numeric(beaks$a)), gmeans[beaks$a], pch=20 )
dotgroup(beaks$beak-gmeans[beaks$a], beaks$a, gm=FALSE,pch=20,main="within")
dev.off()



########
# simple examples
require(xtable)

nsamples <- 12
xy <- data.frame( x=sample( 1:10, nsamples, replace=TRUE ), group=sample(c('A','B'),nsamples,replace=TRUE), stringsAsFactors=FALSE )
xy <- xy[order(xy$group),]
xy$x[xy$group=='A'] <- xy$x[xy$group=='A']+1
gmeans <- tapply(xy$x,xy$group,mean)
xy$gpmean <- gmeans[xy$group] 
xy$residual <- xy$x - xy$gpmean 

pdf(file='dots3ex.pdf',width=3,height=2,pointsize=10)
par(mar=c(4,3,1,0)+.1)
dotgroup(xy$x,xy$group)
dev.off()

# and


nsamples <- 12
xy <- data.frame( x=sample( 1:10, nsamples, replace=TRUE ), group=sample(c('A','B'),nsamples,replace=TRUE), stringsAsFactors=FALSE )
xy <- xy[order(xy$group),]
xy$x[xy$group=='A'] <- xy$x[xy$group=='A']+6
gmeans <- tapply(xy$x,xy$group,mean)
xy$gpmean <- gmeans[xy$group] 
xy$residual <- xy$x - xy$gpmean 


pdf(file='dots4ex.pdf',width=3,height=2,pointsize=10)
par(mar=c(4,3,1,0)+.1)
dotgroup(xy$x,xy$group)
dev.off()



#####
whales <- read.csv("50_make_datamatrix.out")

xy <- droplevels( subset(whales,species %in% c("DELPHINUS_DELPHIS","LAGENORHYNCHUS_OBLIQUIDENS","DELPHINUS_CAPENSIS","ZIPHIUS_CAVIROSTRIS") & bone=="pelvic") )
levels(xy$species)[levels(xy$species)=="LAGENORHYNCHUS_OBLIQUIDENS"] <- "LAGENORHYNCHUS"
levels(xy$species) <- tolower(levels(xy$species))

pdf(file='whale-pelvics.pdf',width=3,height=2,pointsize=10)
par(mar=c(5,3,0,0)+.1)
dotgroup(xy$absolute_volume, xy$species,cex.axis=.5,pch=20)
dev.off()
