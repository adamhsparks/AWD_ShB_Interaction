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
    ##  DIC: 32.47386 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP    0.3343 5.598e-08   0.6265     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     0.173  0.07333   0.3078    834.9
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)       0.17701 -0.47341  0.65769     1000 0.456
    ## WMGTFLD           0.02364 -0.53286  0.64310     1000 0.942
    ## NRTE100           0.34997 -0.23490  0.92657     1111 0.216
    ## NRTE120           0.30448 -0.21590  0.88197     1000 0.290
    ## WMGTFLD:NRTE100  -0.34807 -1.10277  0.47017     1000 0.388
    ## WMGTFLD:NRTE120  -0.22242 -0.96722  0.65383     1000 0.614

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
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2015 Leaf Sheath Blight Replicates")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2015 Replicate Posteriors for Leaf Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2015 Leaf Sheath Blight Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2015 Treatment Posteriors for Leaf Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
# joint distibution of error
rdf <- data.frame(LShB_lmm_2015$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("2015 Leaf Blight Error") +
  theme_tufte()
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
    ##  DIC: 121.3136 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     3.265 2.509e-05    11.32    593.7
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     7.111    3.076    12.71    875.2
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        1.5535  -1.5489   4.3614     1000 0.310
    ## WMGTFLD           -0.4865  -4.1982   3.2363     1000 0.778
    ## NRTE100            1.6807  -1.7354   5.3427     1000 0.332
    ## NRTE120            2.0413  -1.6816   5.4392     1094 0.260
    ## WMGTFLD:NRTE100   -0.9006  -6.1528   4.3188     1000 0.768
    ## WMGTFLD:NRTE120    0.2237  -5.1232   4.9635     1000 0.934

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
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2015 Tiller Sheath Blight Replicates")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2015 Replicate Posteriors for Tiller Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 5), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2015 Tiller Sheath Blight Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2015 Treatment Posteriors for Tiller Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-4.png)

``` r
# joint distibution of error
rdf <- data.frame(TShB_lmm_2015$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("2015 Tiller Blight Error") +
  theme_tufte()
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
    ##  DIC: 32.19748 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP    0.8855 5.267e-07     2.51     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.3343   0.1104   0.6794     1000
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC  
    ## (Intercept)       1.25156  0.41892  2.06092     1000 0.020 *
    ## WMGTFLD          -0.22189 -1.00596  0.59780     1000 0.570  
    ## NRTE180           0.04217 -0.80450  0.77884     1205 0.872  
    ## WMGTFLD:NRTE180   0.48932 -0.57419  1.77417     1000 0.412  
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
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2016 Leaf Sheath Blight Replicates")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2016 Replicate Posteriors for Leaf Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 3), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2016 Leaf Sheath Blight Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2016 Treatment Posteriors for Leaf Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
# joint distibution of error
rdf <- data.frame(LShB_lmm_2016$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("2016 Leaf Blight Error") +
  theme_tufte()
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
    ##  DIC: 96.5886 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     49.58 0.0003463    171.6     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     19.55    5.296    42.93     1000
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC   
    ## (Intercept)      19.81617 12.28910 28.04975     1000 0.004 **
    ## WMGTFLD           0.64530 -5.55225  6.14605     1000 0.784   
    ## NRTE180           0.02834 -5.79429  6.37829     1000 0.992   
    ## WMGTFLD:NRTE180   3.63555 -4.91068 13.19824     1000 0.394   
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
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2016 Tiller Sheath Blight Replicates")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Posterior distributions for replicate
ggplot(reps[reps$variable != "X.Intercept.", ], aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2016 Replicate Posteriors for Tiller Sheath Blight")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# Diagnostic line plots for treatments
ggplot(data = trts, aes(x = rep(x, 3), y = value, group = variable)) +
  geom_line() +
  theme_tufte() +
  xlab("Iteration Number") +
  ylab(NULL) +
  facet_grid(variable ~ .) +
  ggtitle("Diagnostic Plots for 2016 Tiller Sheath Blight Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
# Posterior distributions for treatment
ggplot(trts, aes(x = value, color = variable)) + 
  geom_density() +
  theme_tufte() +
  ggtitle("2016 Treatement Posteriors")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
# joint distibution of error
rdf <- data.frame(TShB_lmm_2016$VCV)
ggplot(rdf, aes(x = sqrt(REP), y = sqrt(units))) + 
  geom_density2d() + 
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("2016 Tiller Sheath Blight Error") +
  theme_tufte()
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-5.png)
