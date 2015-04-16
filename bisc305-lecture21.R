# heteroscedastic example

xy <- data.frame(
        x=rnorm(80),
        group=sample(letters[1:5],80,replace=TRUE)
        )
means <- rpois( nlevels(xy$group), 1 )
sds <- 5 * rexp( nlevels(xy$group) )
xy$x <- sds[xy$group] * xy$x + means[xy$group]

pdf(file="ex23-1.pdf",width=3,height=2,pointsize=10)
par(mar=c(4,1,0,0)+.1)
with(xy, {
            plot( as.numeric(group), x, xaxt='n', xlab='group', ylim=mean(x)+c(-.8,.8)*diff(range(x)), yaxt='n', ylab='value' );
            axis(1, at=1:nlevels(group), labels=levels(group));
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(x,group,mean), lwd=2 )
        } )
dev.off()

require(xtable)
with(xy, xtable(rbind(tapply(x,group,length),tapply(x,group,mean),tapply(x,group,sd))))

###
# beak size


if (FALSE) {
xy <- cbind( data.frame(
        x=rnorm(80) ),
    expand.grid(
            sex=c("M","F"),
            island=letters[1:5]
        )[rep(1:10,each=8),]
    ) 
sexeffect <- c(0,1)
islandeffect <- rpois( nlevels(xy$island), 10 )
xy$x <- with(xy, x + sexeffect[sex] + islandeffect[island] )
xy$group <- factor( paste(xy$island,xy$sex,sep='') )
xy <- xy[ order(xy$island,xy$sex), ]
}

save(xy,file="ex23-beaks.RData")

load("ex23-beaks.RData")

pdf(file="ex23-beaks.pdf",width=3,height=3,pointsize=10)
par(mar=c(4,1,0,0)+.1)
with(xy, {
            plot( (as.numeric(sex)-2) + 2*as.numeric(island), x, xaxt='n', ylim=mean(x)+c(-.8,.8)*diff(range(x)), yaxt='n', xlab='', ylab='' );
            abline(v=2*(1:nlevels(island))+.5,lty=3)
            # axis(1, at=1:nlevels(group), labels=levels(group) )
            axis(1, at=2*(1:nlevels(island))-.5, labels=levels(island), line=1.5, lwd=0, lwd.tick=0);
            axis(1, at=1:(2*nlevels(island)), labels=rep(levels(sex),nlevels(island)) )
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(x,group,mean), lwd=2 )
            segments( x0=2*(1:nlevels(island))-1-.15, x1=2*(1:nlevels(island))+.15, y0=tapply(x,island,mean), col='red', lty=2, lwd=2  )
        } )
dev.off()

pdf(file="ex23-beaks-nosex.pdf",width=3,height=3,pointsize=10)
par(mar=c(4,1,0,0)+.1)
with(xy, {
            plot( (as.numeric(sex)-2) + 2*as.numeric(island), x-tapply(x,sex,mean)[as.numeric(sex)], xaxt='n', ylim=mean(x-tapply(x,sex,mean)[as.numeric(sex)])+c(-.8,.8)*diff(range(x-tapply(x,sex,mean)[as.numeric(sex)])), yaxt='n', xlab='', ylab='' );
            abline(v=2*(1:nlevels(island))+.5,lty=3)
            # axis(1, at=1:nlevels(group), labels=levels(group) )
            axis(1, at=2*(1:nlevels(island))-.5, labels=levels(island), line=1.5, lwd=0, lwd.tick=0);
            axis(1, at=1:(2*nlevels(island)), labels=rep(levels(sex),nlevels(island)) )
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(x-tapply(x,sex,mean)[as.numeric(sex)],group,mean), lwd=2 )
            segments( x0=2*(1:nlevels(island))-1-.15, x1=2*(1:nlevels(island))+.15, y0=tapply(x-tapply(x,sex,mean)[as.numeric(sex)],island,mean), col='red', lty=2, lwd=2  )
        } )
dev.off()

pdf(file="ex23-beaks-noisland.pdf",width=3,height=3,pointsize=10)
par(mar=c(4,1,0,0)+.1)
with(xy, {
        xx <- x-tapply(x,island,mean)[as.numeric(island)];
            plot( (as.numeric(sex)-2) + 2*as.numeric(island), xx, xaxt='n', ylim=mean(xx)+c(-.8,.8)*diff(range(xx)), yaxt='n', xlab='', ylab='' );
            abline(v=2*(1:nlevels(island))+.5,lty=3)
            # axis(1, at=1:nlevels(group), labels=levels(group) )
            axis(1, at=2*(1:nlevels(island))-.5, labels=levels(island), line=1.5, lwd=0, lwd.tick=0);
            axis(1, at=1:(2*nlevels(island)), labels=rep(levels(sex),nlevels(island)) )
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(xx,group,mean), lwd=2 )
            abline(h=0,lty=3,col='red')
        } )
dev.off()

pdf(file="ex23-beaks-nosex-noisland.pdf",width=3,height=3,pointsize=10)
par(mar=c(4,1,0,0)+.1)
with(xy, {
        xx <- x-tapply(x,sex,mean)[as.numeric(sex)]-tapply(x,island,mean)[as.numeric(island)]+mean(x);
            plot( (as.numeric(sex)-2) + 2*as.numeric(island), xx, xaxt='n', ylim=mean(xx)+c(-.8,.8)*diff(range(xx)), yaxt='n', xlab='', ylab='' );
            abline(v=2*(1:nlevels(island))+.5,lty=3)
            # axis(1, at=1:nlevels(group), labels=levels(group) )
            axis(1, at=2*(1:nlevels(island))-.5, labels=levels(island), line=1.5, lwd=0, lwd.tick=0);
            axis(1, at=1:(2*nlevels(island)), labels=rep(levels(sex),nlevels(island)) )
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(xx,group,mean), lwd=2 )
            abline(h=0,lty=3,col='red')
        } )
dev.off()

xtable::xtable(rbind(do.call(cbind,tapply(xy$x,xy$group,c)),mean=tapply(xy$x,xy$group,mean),sd=tapply(xy$x,xy$group,sd)))

xtable::xtable(addmargins(with(xy, tapply( x, list(sex,island), mean ) ), FUN=mean) )
xtable::xtable(addmargins(with(xy, tapply( x-mean(x), list(sex,island), mean ) ), FUN=mean) )
xtable::xtable(addmargins(with(xy, tapply( x, list(sex,island), mean ) + mean(x) - outer( tapply(x,sex,mean), tapply(x,island,mean), "+" )), FUN=mean) )

with(xy, tapply( x, paste(sex,island), mean ) )
with(xy, tapply(x,sex,mean) )
with(xy, tapply(x,island,mean) )

with(xy, tapply( x, paste(sex,island), mean )  - mean(x) )
with(xy, tapply(x,sex,mean) - mean(x) )
with(xy, tapply(x,island,mean) - mean(x) )

with(xy, tapply( x, paste(sex,island), mean )  - as.vector( outer( tapply(x,sex,mean), tapply(x,island,mean), "+" ) ) + mean(x) )


anova( lm(x ~ sex*island, data=xy) )


###
# no main effects

zz <- data.frame(
        x=rnorm(80),
        sex=sample(c("M","F"),80,replace=TRUE),
        treatment=sample(c("treatment","control"),80,replace=TRUE)
        )
zz$group <- factor( paste(zz$treatment,zz$sex,sep='') )
groupeffect <- c(0,0,0,3)
zz$x <- with(zz, x + groupeffect[group])
zz <- zz[ order(zz$treatment,zz$sex), ]


pdf(file="ex23-interaction.pdf",width=3,height=3,pointsize=10)
par(mar=c(4,1,0,0)+.1)
with(zz, {
            plot( (as.numeric(sex)-2) + 2*as.numeric(treatment), x, xaxt='n', ylim=mean(x)+c(-.8,.8)*diff(range(x)), yaxt='n', xlab='', ylab='' );
            abline(v=2*(1:nlevels(treatment))+.5,lty=3)
            # axis(1, at=1:nlevels(group), labels=levels(group) )
            axis(1, at=2*(1:nlevels(treatment))-.5, labels=levels(treatment), line=1.5, lwd=0, lwd.tick=0);
            axis(1, at=1:(2*nlevels(treatment)), labels=rep(levels(sex),nlevels(treatment)) )
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(x,group,mean), lwd=2 )
            segments( x0=2*(1:nlevels(treatment))-1-.15, x1=2*(1:nlevels(treatment))+.15, y0=tapply(x,treatment,mean), col='red', lty=2, lwd=2  )
        } )
dev.off()

anova( lm( x ~ sex + treatment + sex*treatment, data=zz ) )
