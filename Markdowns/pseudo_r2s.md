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

Now lets compute the regular coefficient of determination

``` r
model <- lm(y~x)
model0 <- lm(y~1)

regular_r2 <- summary(model)$r.squared
mcfadden_r2 <- c(1 - (logLik(model)/logLik(model0)))
Cohen_r2 <- (deviance(model0) - deviance(model))/ deviance(model0)

regular_r2
```

    ## [1] 0.5811655

``` r
mcfadden_r2
```

    ## [1] 0.08131167

``` r
Cohen_r2
```

    ## [1] 0.5811655

Now lets simulate some scenarios and see how things vary acoording to a
large variation in r² values.

``` r
sds <- runif(50, max = 100, min = 1)

regular_r2 <- rep(NA, 50)
mcfadden_r2 <- rep(NA, 50) 
Cohen_r2 <- rep(NA, 50)
  
for (i in 1:50){
  x <- runif(50, min = 0, max = 100)
  y <- rnorm(50, mean = 10 + 1.5*x, sd = sds[i])
  
  model <- lm(y~x)
  model0 <- lm(y~1)

  regular_r2[i] <- summary(model)$r.squared
  mcfadden_r2[i] <- c(1 - (logLik(model)/logLik(model0)))
  Cohen_r2[i] <- (deviance(model0) - deviance(model))/ deviance(model0)
}

plot(mcfadden_r2 ~ regular_r2)
```

![](pseudo_r2s_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
plot(Cohen_r2 ~ regular_r2)
```

![](pseudo_r2s_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Cohen R² seems to make a better job in terms os linear scaling and
similarity to absolute values.
