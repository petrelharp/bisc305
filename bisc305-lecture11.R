#!/usr/bin/Rscript

quakes <- read.table("2014-quakes-catalog.tsv", stringsAsFactors=FALSE)
names(quakes) <- c("orig.date","orig.time", "ET", "MAG", "M", "LAT", "LON", "DEPTH", "Q", "EVID", "NPH", "NGRM")
quakes$date <- as.Date(paste(quakes$orig.date,substr(quakes$orig.time,1,8)),format="%Y/%m/%d %H:%M:%S")

quakes$season <- cut(quakes$date,"month")
levels(quakes$season) <- c("winter","winter","spring","spring","spring","summer","summer","summer","fall","fall","fall","winter")
quakes$subseason <- quakes$season
levels(quakes$subseason) <- c("winter/summer","spring/fall","winter/summer","spring/fall")


# Are there bigger earthquakes in winter and summer?

with( quakes, {
            xh <- hist(MAG,plot=FALSE)
            hist( MAG[subseason=="winter/summer"], col=adjustcolor("red",.75), breaks=xh$breaks )
            abline( v=mean(MAG[subseason=="winter/summer"]), col='red' )
            hist( MAG[subseason!="winter/summer"], col=adjustcolor("blue",.75), breaks=xh$breaks, add=TRUE )
            abline( v=mean(MAG[subseason!="winter/summer"]), col='blue' )
        } )

# difference in means:
mean.solstice <- mean(quakes$MAG[quakes$subseason=="winter/summer"])
mean.equinox <- mean(quakes$MAG[quakes$subseason!="winter/summer"])
cat("Mean winter/summer: ", mean.solstice, "\n")
cat("Mean spring/fall: ", mean.equinox, "\n")
cat("Difference: ", mean.solstice-mean.equinox, "\n")


##### randomization:
quakes$fake_season <- sample(quakes$subseason)
with( quakes, {
            xh <- hist(MAG,plot=FALSE)
            hist( MAG[fake_season=="winter/summer"], col=adjustcolor("red",.75), breaks=xh$breaks )
            abline( v=mean(MAG[fake_season=="winter/summer"]), col='red' )
            hist( MAG[fake_season!="winter/summer"], col=adjustcolor("blue",.75), breaks=xh$breaks, add=TRUE )
            abline( v=mean(MAG[fake_season!="winter/summer"]), col='blue' )
        } )

random.samples <- replicate(1000, {
    fake_season <- sample(quakes$subseason)
    mean(quakes$MAG[fake_season=="winter/summer"])-mean(quakes$MAG[fake_season!="winter/summer"]) } )

hist(random.samples)

abline(v=mean.solstice-mean.equinox,col='red',lwd=2)



##### t-test version
# SE for solstice
sd.solstice <- sd(quakes$MAG[quakes$subseason=="winter/summer"]) 
n.solstice <- length(quakes$MAG[quakes$subseason=="winter/summer"]) 
se.solstice <- sd.solstice / sqrt(n.solstice)
# SE for equinox
sd.equinox <- sd(quakes$MAG[quakes$subseason!="winter/summer"]) 
n.equinox <- length(quakes$MAG[quakes$subseason!="winter/summer"]) 
se.equinox <- sd.equinox / sqrt(n.equinox)

# degrees of freedom
df <- ( se.solstice^2 + se.equinox^2 )^2 / ( (se.solstice^4/(n.solstice-1)) + (se.equinox^4/(n.equinox-1)) )


se.diff <- sqrt( se.solstice^2 + se.equinox^2 )
t.statistic <- ( mean.solstice-mean.equinox ) / se.diff

random.stats <- replicate(1000, {
    fake_season <- sample(quakes$subseason)
    fake.diff <- mean(quakes$MAG[fake_season=="winter/summer"])-mean(quakes$MAG[fake_season!="winter/summer"]) 
    return( fake.diff / se.diff )
} )

xh <- hist(random.stats,breaks=25)
abline(v=t.statistic,col='red',lwd=2)

# t-distribution approximation
polygon( x=c(xh$mids,rev(xh$mids)), y=sum(xh$counts)*c(diff(pt(xh$breaks,df=df)),rep(0,length(xh$mids))), col=adjustcolor("blue",0.5) )

# and t-test
2*(1-pt( t.statistic, df=df ))

# or, built-in
t.test( quakes$MAG[quakes$subseason=="winter/summer"], quakes$MAG[quakes$subseason!="winter/summer"] )
