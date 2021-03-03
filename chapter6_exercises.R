library(rethinking)
library(ggplot2)
library(ggpubr)

set.seed(1914)
n_grant_prop <- 200
proportion_select <- 0.1

newsworthiness <- rnorm(n_grant_prop)
trustworthiness <- rnorm(n_grant_prop)

df <- data.frame(newsworthiness, trustworthiness)
df$score <- df$newsworthiness + df$trustworthiness

df$quantile <- NULL

quantile_threshold <- quantile(df$score, 1-proportion_select)

df$selected <- ifelse(df$score >quantile_threshold, TRUE, FALSE )
df

p <- ggplot(df, aes(x=newsworthiness, y=trustworthiness, colour = selected)) +
  geom_point() +
  geom_smooth(method=lm) 
p

## add the correlation
p+stat_cor(method="pearson")

#### 6.1. Multi-collinear legs
set.seed(909)

# nr of individuals
N <- 100

height <- rnorm(N, 10, 2)  # sim total height of each
leg_prop <- runif(N, 0.4, 0.5)  # leg as proportion of height

# sim left leg as proportion + error
leg_left <- leg_prop * height +rnorm(N, 0, 0.2)
leg_right <- leg_prop * height + rnorm(N, 0, 0.2)

df <- data.frame(height, leg_left, leg_right)

df

m6.1 <- quap(
  alist(
    height ~ dnorm(mu, sigma) ,
    mu <- a+bl*leg_left+br*leg_right ,
    a ~ dnorm(10, 100) ,
    bl ~ dnorm(2, 10) ,
    br ~ dnorm(2, 10) ,
    sigma ~ dexp(1)
  ), data = df )

precis(m6.1)

plot(precis(m6.1))

post <- extract.samples(m6.1)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )

sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab='sum of bl and br')

m6.2 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=df )


precis(m6.2)

## Multicollinear milk
library(rethinking)
data(milk)
d <- milk
d$K <- standardize( d$kcal.per.g )
d$F <- standardize( d$perc.fat )
d$L <- standardize( d$perc.lactose )
head(d)
