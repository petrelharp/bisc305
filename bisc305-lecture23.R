# CLDD - Cooling degree days
# HTDD - Heating degree days
# DT90 - Number days with maximum temperature greater than or equal 90.0 F
# DX32 - Number days with maximum temperature less than or equal to 32.0 F 
# DT00 - Number days with minimum temperature less than or equal to 0.0 F
# DT32 - Number days with minimum temperature less than or equal to 32.0 F
# DP01 - Number of days with greater than or equal to 0.1 inch of precipitation
# DP05 - Number of days with greater than or equal to 0.5 inch of precipitation
# DP10 - Number of days with greater than or equal to 1.0 inch of precipitation
# EMXP - Extreme maximum daily precipitation
# MXSD - Maximum snow depth
# TPCP - Total precipitation
# TSNW - Total snow fall
# EMXT - Extreme maximum daily temperature
# EMNT - Extreme minimum daily temperature
# MMXT - Monthly Mean maximum temperature
# MMNT - Monthly Mean minimum temperature
# MNTM - Monthly mean temperature

usc <- read.csv("usc-weather.csv")
usc$DATE <- as.Date(as.character(usc$DATE),format="%Y%m%d")
for (k in 4:ncol(usc)) { usc[,k][usc[,k]==-9999] <- NA }
usc$MMXT <- usc$MMXT /10 
usc$MMNT <- usc$MMNT /10
usc$MNTM <- usc$MNTM /10
usc$month <- format.Date(usc$DATE,"%m")
usc$year <- as.numeric(format.Date(usc$DATE,"%Y"))

usc <- subset(usc,year<1998)  # station was moved in 1998

usc$temp <- usc$MNTM - with(subset(usc,year<1920), tapply( MNTM, month, mean, na.rm=TRUE ))[usc$month]
usc$year[ usc$year<1920 ] <- NA


pdf(file="usc-temps.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(as.numeric(format.Date(DATE,"%Y")),MNTM, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor(month,.25), las=2 ) )
dev.off()

pdf(file="usc-temps-stdized.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor(month,.25), las=2 ) )
dev.off()

pdf(file="usc-temps-lines.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor('black',.25), las=2 ) )
points( mean(usc$year,na.rm=TRUE)+c(-1,0,1)*sd(usc$year,na.rm=TRUE), mean(usc$temp,na.rm=TRUE)+c(-1,0,1)*sd(usc$temp,na.rm=TRUE), pch=20, cex=1, col='red' )
abline( a=mean(usc$temp,na.rm=TRUE) - mean(usc$year,na.rm=TRUE) * sd(usc$temp,na.rm=TRUE)/sd(usc$year,na.rm=TRUE), b=sd(usc$temp,na.rm=TRUE)/sd(usc$year,na.rm=TRUE), col='red' )
dev.off()

pdf(file="usc-temps-lines-means.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor('black',.25), las=2 ) )
points( mean(usc$year,na.rm=TRUE)+c(-1,0,1)*sd(usc$year,na.rm=TRUE), mean(usc$temp,na.rm=TRUE)+c(-1,0,1)*sd(usc$temp,na.rm=TRUE), pch=20, cex=1, col='red' )
abline( a=mean(usc$temp,na.rm=TRUE) - mean(usc$year,na.rm=TRUE) * sd(usc$temp,na.rm=TRUE)/sd(usc$year,na.rm=TRUE), b=sd(usc$temp,na.rm=TRUE)/sd(usc$year,na.rm=TRUE), col='red' )
points( as.numeric(levels(factor(usc$year))), tapply(usc$temp,usc$year,mean,na.rm=TRUE), col=adjustcolor('blue',.5), pch=20 )
dev.off()

pdf(file="usc-temps-both-lines.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor('black',.25), las=2 ) )
points( mean(usc$year,na.rm=TRUE)+c(-1,0,1)*sd(usc$year,na.rm=TRUE), mean(usc$temp,na.rm=TRUE)+c(-1,0,1)*sd(usc$temp,na.rm=TRUE), pch=20, cex=1, col='red' )
abline( a=mean(usc$temp,na.rm=TRUE) - mean(usc$year,na.rm=TRUE) * sd(usc$temp,na.rm=TRUE)/sd(usc$year,na.rm=TRUE), b=sd(usc$temp,na.rm=TRUE)/sd(usc$year,na.rm=TRUE), col='red' )
points( as.numeric(levels(factor(usc$year))), tapply(usc$temp,usc$year,mean,na.rm=TRUE), col=adjustcolor('blue',.5), pch=20 )
abline( coef( lm( temp ~ year, data=usc ) ) )
dev.off()

pdf(file="usc-temps-regression.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor('black',.25), las=2 ) )
points( as.numeric(levels(factor(usc$year))), tapply(usc$temp,usc$year,mean,na.rm=TRUE), col=adjustcolor('blue',.5), pch=20 )
abline( coef( lm( temp ~ year, data=usc ) ) )
dev.off()


pdf(file="usc-temps-just-regression.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor('black',.25), las=2 ) )
abline( coef( lm( temp ~ year, data=usc ) ) )
dev.off()


pdf(file="usc-temps-fit-resid.pdf", width=5, height=2.5)
layout(t(1:2))
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
with(usc, plot(year,temp, xlab="", ylab="mean temperature", pch=20, cex=.5, col=adjustcolor('black',.25), las=2 ) )
templm <- lm( temp ~ year, data=usc )
abline( coef( templm ) )
plot( fitted(templm), resid(templm), xlab='predicted', ylab='residual', cex=.5, pch=20, col=adjustcolor('black',.25) )
abline(h=0)
lines( lowess( fitted(templm), resid(templm), f=.2 ), col='red', lwd=2 )
dev.off()


### fake examples



pdf(file="r2ex-1.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
xy <- data.frame(x=rnorm(100),y=rnorm(100))
xy$y <- xy$x + xy$y/20
with(xy, plot(x,y, xlab="", ylab="", pch=20, cex=.5, col=adjustcolor('black',.25) ) )
abline( coef( lm( y ~ x, data=xy ) ) )
dev.off()
cor(xy)


pdf(file="r2ex-2.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
xy <- data.frame(x=rnorm(100),y=rnorm(100))
xy$y <- xy$x + xy$y
with(xy, plot(x,y, xlab="", ylab="", pch=20, cex=.5, col=adjustcolor('black',.25) ) )
abline( coef( lm( y ~ x, data=xy ) ) )
dev.off()
cor(xy)


pdf(file="r2ex-3.pdf", width=3, height=2.5)
par(mar=c(3,3,1,0)+.1,mgp=c(2,1,0))
xy <- data.frame(x=rnorm(100),y=rnorm(100))
xy$y <- xy$x + xy$y*3
with(xy, plot(x,y, xlab="", ylab="", pch=20, cex=.5, col=adjustcolor('black',.25) ) )
abline( coef( lm( y ~ x, data=xy ) ) )
dev.off()
cor(xy)


