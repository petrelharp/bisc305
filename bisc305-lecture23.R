# heteroscedastic example

xy <- data.frame(
        x=rnorm(80),
        group=sample(letters[1:5],80,replace=TRUE)
        )
means <- rpois( nlevels(xy$group), 1 )
sds <- 5 * rexp( nlevels(xy$group) )
xy$x <- sds[xy$group] * xy$x + means[xy$group]

pdf(file="ex23-1.pdf",width=3,height=2,pointsize=10)
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
xy <- data.frame(
        x=rnorm(80),
        sex=sample(c("M","F"),80,replace=TRUE),
        island=sample(letters[1:5],80,replace=TRUE)
        )
sexeffect <- c(0,1)
islandeffect <- rpois( nlevels(xy$island), 10 )
xy$x <- with(xy, x + sexeffect[sex] + islandeffect[island] )
xy$group <- factor( paste(xy$island,xy$sex,sep='') )
xy <- xy[ order(xy$island,xy$sex), ]
}

save(xy,file="ex23-beaks.RData")


load("ex23-beaks.RData")

pdf(file="ex23-beaks.pdf",width=3,height=2,pointsize=10)
with(xy, {
            plot( (as.numeric(sex)-2) + 2*as.numeric(island), x, xaxt='n', ylim=mean(x)+c(-.8,.8)*diff(range(x)), yaxt='n', ylab='value', xlab='' );
            abline(v=2*(1:nlevels(island))+.5,lty=3)
            # axis(1, at=1:nlevels(group), labels=levels(group) )
            axis(1, at=2*(1:nlevels(island))-.5, labels=levels(island), line=2, lwd=0, lwd.tick=0);
            axis(1, at=1:(2*nlevels(island)), labels=rep(levels(sex),nlevels(island)) )
            segments( x0=(1:nlevels(group))-.15, x1=(1:nlevels(group))+.15, y0=tapply(x,group,mean), lwd=2 )
        } )
dev.off()


