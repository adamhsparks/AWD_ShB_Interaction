Analysis
================

2015 Leaf Sheath Blight Severity Model
--------------------------------------

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_lmm_2015 <- MCMCglmm(LShB_AUDPS ~ WMGT * NRTE, ~REP, 
                 data = as.data.frame(AUDPS_2015),
                 verbose = FALSE,
                 prior = eprior,
                 pr = TRUE)

summary(LShB_lmm_2015)
```

    ## 
    ##  Iterations = 3001:12991
    ##  Thinning interval  = 10
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 32.60179 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP    0.2121 1.39e-06   0.6034    883.4
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.1728  0.07084    0.311     1000
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)       0.17420 -0.34072  0.67898   1000.0 0.444
    ## WMGTFLD           0.01727 -0.53164  0.62042   1000.0 0.956
    ## NRTE100           0.36342 -0.25784  0.94804   1000.0 0.228
    ## NRTE120           0.28231 -0.33739  0.81664   1000.0 0.312
    ## WMGTFLD:NRTE100  -0.35745 -1.11946  0.40758   1504.6 0.372
    ## WMGTFLD:NRTE120  -0.17774 -0.95485  0.61361    733.6 0.644

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_lmm_2015$Sol[, c(1, 7:10)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_lmm_2015$Sol[, 2:6])
names(trts)[names(trts) == "WMGTFLD.NRTE100"] <- "FLD.NRTE100"
names(trts)[names(trts) == "WMGTFLD.NRTE120"] <- "FLD.NRTE120"
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(LShB_lmm_2015$Sol)

# Diagnostic line plots for replicate
ggplot(data = reps, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
# Diagnostic line plots for treatements
ggplot(data = trts, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
# joint distibution of error
rdf <- data.frame(LShB_lmm_2015$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-5.png)

2015 Tiller Sheath Blight Severity Model
----------------------------------------

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_lmm_2015 <- MCMCglmm(TShB_AUDPS ~ WMGT * NRTE, ~REP, 
                 data = as.data.frame(AUDPS_2015),
                 verbose = FALSE,
                 prior = eprior,
                 pr = TRUE)

summary(TShB_lmm_2015)
```

    ## 
    ##  Iterations = 3001:12991
    ##  Thinning interval  = 10
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 121.2453 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     4.391 4.49e-09    14.81     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units         7    2.998    12.03     1000
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        1.4941  -1.5024   4.6955     1000 0.280
    ## WMGTFLD           -0.5128  -4.0409   2.9314     1000 0.768
    ## NRTE100            1.7575  -1.8255   5.1167     1000 0.310
    ## NRTE120            1.9038  -2.0309   5.2894     1000 0.284
    ## WMGTFLD:NRTE100   -0.9220  -6.0413   4.1205     1000 0.710
    ## WMGTFLD:NRTE120    0.4259  -4.4139   5.8227     1000 0.868

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_lmm_2015$Sol[, c(1, 7:10)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_lmm_2015$Sol[, 2:6])
names(trts)[names(trts) == "WMGTFLD.NRTE100"] <- "FLD.NRTE100"
names(trts)[names(trts) == "WMGTFLD.NRTE120"] <- "FLD.NRTE120"
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(TShB_lmm_2015$Sol)

# Diagnostic line plots for replicate
ggplot(data = reps, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-4.png)

``` r
# joint distibution of error
rdf <- data.frame(TShB_lmm_2015$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-5.png)

------------------------------------------------------------------------

2016 Leaf Sheath Blight Severity Model
--------------------------------------

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_lmm_2016 <- MCMCglmm(LShB_AUDPS ~ WMGT * NRTE, ~REP, 
                 data = as.data.frame(AUDPS_2016),
                 verbose = FALSE,
                 prior = eprior,
                 pr = TRUE)

summary(LShB_lmm_2016)
```

    ## 
    ##  Iterations = 3001:12991
    ##  Thinning interval  = 10
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 32.28413 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP    0.7934 4.732e-08    2.692     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.3295   0.1068   0.6648     1000
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC  
    ## (Intercept)       1.19681  0.35586  2.28501   1000.0 0.032 *
    ## WMGTFLD          -0.21435 -1.01302  0.60959    911.4 0.566  
    ## NRTE180           0.07098 -0.69349  0.86485   1000.0 0.848  
    ## WMGTFLD:NRTE180   0.44304 -0.66139  1.53911    948.1 0.420  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_lmm_2016$Sol[, c(1, 5:8)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_lmm_2016$Sol[, 2:4])
names(trts)[names(trts) == "WMGTFLD.NRTE180"] <- "FLD.NRTE180"
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(LShB_lmm_2016$Sol)

# Diagnostic line plots for replicate
ggplot(data = reps, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 3), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
# joint distibution of error
rdf <- data.frame(TShB_lmm_2015$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-5.png)

Tiller Sheath Blight Severity Model
-----------------------------------

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_lmm_2016 <- MCMCglmm(TShB_AUDPS ~ WMGT * NRTE, ~REP, 
                 data = as.data.frame(AUDPS_2016),
                 verbose = FALSE,
                 prior = eprior,
                 pr = TRUE)

summary(TShB_lmm_2016)
```

    ## 
    ##  Iterations = 3001:12991
    ##  Thinning interval  = 10
    ##  Sample size  = 1000 
    ## 
    ##  DIC: 96.58054 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     43.77 0.005528    145.5     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     19.06    5.545    39.93     1319
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC   
    ## (Intercept)       19.7653  12.3452  27.6237     1000 0.002 **
    ## WMGTFLD            0.7624  -5.6605   6.1509     1000 0.786   
    ## NRTE180            0.1310  -6.6563   6.0767     1000 0.994   
    ## WMGTFLD:NRTE180    3.4876  -6.5469  11.8206     1000 0.386   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_lmm_2016$Sol[, c(1, 5:8)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_lmm_2016$Sol[, 2:4])
names(trts)[names(trts) == "WMGTFLD.NRTE180"] <- "FLD.NRTE180"
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(TShB_lmm_2016$Sol)

# Diagnostic line plots for replicate
ggplot(data = reps, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 3), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
# joint distibution of error
rdf <- data.frame(TShB_lmm_2015$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-5.png)
