R² and Pseudo R²
================
Rodolfo Pelinson
2024-10-11

We first will simulate a linear effe between two variables.

``` r
x <- runif(50, min = 0, max = 100)
y <- rnorm(50, mean = 10 + 1.5*x, sd = 30)

plot(y ~ x)
```

![](pseudo_r2s_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

First, lets fit a model, and a “null” or intercept model

``` r
model <- lm(y~x)
model0 <- lm(y~1)
```

The regular r²

``` r
regular_r2 <- summary(model)$r.squared
regular_r2
```

    ## [1] 0.6083242

The likelihood r² is geven by

R² = 1 - ( (L0/LB)^(2/n) )

where L0 is likelihood of the intercept model and LB is do likelihood of
the model with the predictor.

But since R only gives us log-likelihoods it turns into

R² = 1 - ( exp(lnL0-lnLB)^(2/n) )

``` r
n <- length(y)

L_r2 <- c(1 - ( exp(logLik(model0)-logLik(model))^(2/n)) )
L_r2
```

    ## [1] 0.6083242

This R² cannot be 1 even if the model fits the data perfectly, thus
Nagelkerke 1991 proposed a correction

R² = ( 1 - ( exp(lnL0-lnLB)^(2/n) ) ) / (1 - exp(lnL0)^(2/n) )

``` r
Lc_r2 <- c(1 - ( exp(logLik(model0)-logLik(model))^(2/n)) ) / (1 - exp(logLik(model0))^(2/n) )
Lc_r2
```

    ## 'log Lik.' 0.6083387 (df=2)

A similar proposal is the deviance R², which is

R² = 1 - (-2lnLB / -2lnL0)

Which is similar to

R² = 1 - deviance(LB) / deviance(L0)

``` r
DEV_r2 <- 1 - (deviance(model)/ deviance(model0))
DEV_r2
```

    ## [1] 0.6083242

Package MuMIn has a function that computes those

``` r
library(MuMIn)
LR_r2 <- r.squaredLR(model)
LR_r2
```

    ## [1] 0.6083242
    ## attr(,"adj.r.squared")
    ## [1] 0.6083387
