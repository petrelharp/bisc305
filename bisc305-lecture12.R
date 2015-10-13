# add gaussian curve
add.t <- function(df,xh=.Last.value,q=c(0,1)) {
    xx <- seq(-6,6,length.out=300)
    xmids <- xx[-1]-diff(xx)/2
    px <- sum(xh$counts)*mean(diff(xh$breaks))*diff(pt(xx,df=df))/diff(xx)
    polygon( x=c(xmids,rev(xmids)), 
            y=c(px, rep(0,length(xx)-1)), 
            col=adjustcolor("blue",0.25) )
    qx <- pt(xmids,df=df)
    lower <- (qx<q[1])
    upper <- (qx>q[2])
    for (ut in list(lower,upper)) {
        if (sum(ut)>0) {
            polygon( x=c(xmids[ut],rev(xmids[ut])), 
                    y=c(px[ut], rep(0,sum(ut))),
                    col=adjustcolor("blue",0.5) )
        }
    }
}

# simulate one dataset
t.sim <- function (mu) {
    z1 <- rnorm(n,mean=0);
    z2 <- rnorm(n,mean=mu);
    SE1 <- sd(z1)/sqrt(n)
    SE2 <- sd(z2)/sqrt(n)
    return( c( mean1=mean(z1), mean2=mean(z2),
              t.statistic=(mean(z1)-mean(z2))/sqrt(SE1^2+SE2^2) ) )
}

t.sim(0)

# simulate many datasets, under the null
n <- 30
df <- n-1

t.stats <- replicate(10000, t.sim(0)[3])

xh <- hist(t.stats,breaks=100,col=adjustcolor("grey",0.5),yaxt='n',ylab='')
add.t(df=df,xh=xh)

add.t(df=df,xh=xh,q=c(.025,.975))

t.025 <- qt(0.025,df=df)
t.975 <- qt(0.975,df=df)
t.breaks <- c(max(xh$breaks[xh$breaks<t.025]),min(xh$breaks[xh$breaks>t.975]))
hist(t.stats[t.stats<=t.breaks[1]|t.stats>=t.breaks[2]],breaks=xh$breaks,col=adjustcolor("green",0.5),xlim=range(c(t.stats,3)),ylim=c(0,max(xh$counts)),yaxt='n',ylab='',main="mean difference=0")
hist(t.stats[t.stats>t.breaks[1]&t.stats<t.breaks[2]],breaks=xh$breaks,col=adjustcolor("red",0.5),add=TRUE)
abline(v=qt(c(0.025,0.975),df=df),lwd=2,col='blue')
legend("topleft",fill=c("green","red","green"),legend=paste(c("bad:","good:","bad:"),table(cut(t.stats,breaks=c(-Inf,t.025,t.975,Inf)))))


# simulate many datasets, with a difference in means
n <- 30
df <- n-1
mu <- 1/3

t.stats <- replicate(10000, t.sim(mu)[3])

xh <- hist(t.stats,breaks=100,col=adjustcolor("grey",0.5),xlim=range(c(t.stats,3)),yaxt='n',ylab='')
add.t(df=df,xh=xh,q=c(.025,.975))

t.025 <- qt(0.025,df=df)
abline(v=t.025,lwd=2,col='blue')

t.025 <- qt(0.025,df=df)
t.975 <- qt(0.975,df=df)
t.breaks <- c(max(xh$breaks[xh$breaks<t.025]),min(xh$breaks[xh$breaks>t.975]))
hist(t.stats[t.stats<=t.breaks[1]|t.stats>=t.breaks[2]],breaks=xh$breaks,col=adjustcolor("green",0.5),xlim=range(c(t.stats,3)),ylim=c(0,max(xh$counts)),yaxt='n',ylab='',main="mean difference=1/3")
hist(t.stats[t.stats>t.breaks[1]&t.stats<t.breaks[2]],breaks=xh$breaks,col=adjustcolor("red",0.5),add=TRUE)
abline(v=qt(c(0.025,0.975),df=df),lwd=2,col='blue')
legend("topleft",fill=c("green","red","green"),legend=paste(c("good:","bad:","good?:"),table(cut(t.stats,breaks=c(-Inf,t.025,t.975,Inf)))))


# now, try one-sided!

# first, with no difference

t.stats <- replicate(10000, t.sim(0)[3])

xh <- hist(t.stats,breaks=100,col=adjustcolor("grey",0.5),yaxt='n',ylab='')
add.t(df=df,xh=xh,q=c(.05,1))

t.05 <- qt(0.05,df=df)
t.breaks <- max(xh$breaks[xh$breaks<t.05])
hist(t.stats[t.stats<=t.breaks],breaks=xh$breaks,col=adjustcolor("green",0.5),xlim=range(c(t.stats,3)),ylim=c(0,max(xh$counts)),yaxt='n',ylab='',main="mean difference=0")
hist(t.stats[t.stats>t.breaks],breaks=xh$breaks,col=adjustcolor("red",0.5),add=TRUE)
abline(v=qt(0.05,df=df),lwd=2,col='blue')
legend("topleft",fill=c("green","red"),legend=paste(c("bad:","good:"),table(cut(t.stats,breaks=c(-Inf,t.05,Inf)))))



# now, with a difference

t.stats <- replicate(10000, t.sim(1/3)[3])

xh <- hist(t.stats,breaks=100,col=adjustcolor("grey",0.5),yaxt='n',ylab='')
add.t(df=df,xh=xh,q=c(.05,1))

t.05 <- qt(0.05,df=df)
t.breaks <- max(xh$breaks[xh$breaks<t.05])
hist(t.stats[t.stats<=t.breaks],breaks=xh$breaks,col=adjustcolor("green",0.5),xlim=range(c(t.stats,3)),ylim=c(0,max(xh$counts)),yaxt='n',ylab='',main="mean difference=0")
hist(t.stats[t.stats>t.breaks],breaks=xh$breaks,col=adjustcolor("red",0.5),add=TRUE)
abline(v=qt(0.05,df=df),lwd=2,col='blue')
legend("topleft",fill=c("green","red"),legend=paste(c("good:","bad:"),table(cut(t.stats,breaks=c(-Inf,t.05,Inf)))))
