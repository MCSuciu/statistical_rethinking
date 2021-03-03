## chapter 4

###### finding the posterior distribution with quap

devtools::install_github("rmcelreath/rethinking")

library(rethinking)

data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,]

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178,20),
  sigma ~dunif(0,50)
)

m4.1 <- quap(flist, d2)

precis(m4.1)


start <- list(
  mu=mean(d2$height),
  sigma=sd(d2$height)
)

m4.1<-quap(flist, data=d2, start=start)


m4.2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~dnorm(178, 0.1),
    sigma ~dunif(0,50)
  ), data=d2)

precis(m4.2)

vcov(m4.1)

diag(vcov(m4.1))
cov2cor(vcov(m4.1))

post <- extract.samples(m4.1, n=1e4)
head(post)
precis(post)

plot(post)


plot(d2$height, d2$weight)
plot(d2$height ~d2$weight)
