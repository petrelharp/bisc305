# "Can the common brain parasite, Toxoplasma gondii, influence human culture?"
#  Kevin D Lafferty, Proc Royal Soc B 2006
# from http://rspb.royalsocietypublishing.org/content/273/1602/2749/T1.expansion.html

toxo <- read.table("toxo.csv",header=TRUE)

pdf(file="ex24-toxo.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,0,0)+.1,mgp=c(2.1,1,0))
plot( N18 ~ prevalence, data=toxo, ylab="neuroticism", xlab="prevalence of toxoplasmosis" )
dev.off()

pdf(file="ex24-toxo-line.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,0,0)+.1,mgp=c(2.1,1,0))
plot( N18 ~ prevalence, data=toxo, ylab="neuroticism", xlab="prevalence of toxoplasmosis" )
abline(coef( lm(N18 ~ prevalence, data=toxo) ) )
dev.off()

pdf(file="ex24-toxo-sdized.pdf", width=3, height=2.5, pointsize=10)
par(mar=c(4,4,0,0)+.1,mgp=c(2.1,1,0))
plot( scale(N18) ~ scale(prevalence), data=toxo, ylab="neuroticism", xlab="prevalence of toxoplasmosis" )
abline(coef( lm(scale(N18) ~ scale(prevalence), data=toxo) ) )
dev.off()
