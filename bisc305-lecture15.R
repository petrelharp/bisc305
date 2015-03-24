#!/usr/bin/Rscript

conts.pvals <- replicate(1000, {
        n <- m <- 100
        sample.1 <- rnorm(n)
        sample.2 <- rnorm(m)
        wilcox.test(sample.1,sample.2)$p.value
    } )

hist(conts.pvals)
mean(conts.pvals < 0.05)

disc.pvals <- replicate(1000, {
        n <- m <- 100
        sample.1 <- c(1,10)[1+rbinom(n,size=1,prob=1/2)]
        sample.2 <- c(5,6)[1+rbinom(m,size=1,prob=1/2)]
        wilcox.test(sample.1,sample.2)$p.value
    } )

hist(disc.pvals)
mean(disc.pvals < 0.05)



# biased
asym.pvals <- replicate(1000, {
        n <- m <- 100
        sample.1 <- c(1,10)[1+rbinom(n,size=1,prob=1/2)]
        sample.2 <- c(6,7)[1+rbinom(m,size=1,prob=1/2)]
        wilcox.test(sample.1,sample.2)$p.value
    } )

hist(asym.pvals)
mean(asym.pvals < 0.05)
