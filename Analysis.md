Analysis
================

Before the analysis, note that due to changes between the years, the analysis must be carried out on each year separately. The 2015 data and 2016 data cannot be combined due to changes in inoculation methods; there are other changes too, but the main one is this. Therefore, the analyses will be conducted separately for leaf severity and tiller severity for 2015 dry season and 2016 dry season. Comparisons will only be observational and cannot be statistically compared.

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
    ##  DIC: 32.48425 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP    0.1863 2.434e-07    0.578    858.8
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.1684  0.06626   0.2874     1000
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)       0.17978 -0.35671  0.71886    911.5 0.484
    ## WMGTFLD           0.01793 -0.54677  0.55549    911.0 0.924
    ## NRTE100           0.36604 -0.20925  0.93137   1000.0 0.184
    ## NRTE120           0.28348 -0.30871  0.80277   1149.5 0.318
    ## WMGTFLD:NRTE100  -0.35264 -1.17528  0.45638   1000.0 0.378
    ## WMGTFLD:NRTE120  -0.18740 -0.95351  0.61132   1252.8 0.620

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
    ##  DIC: 121.3833 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     3.537 1.501e-06    14.12    828.2
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     7.141    2.796    12.66     1000
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        1.6055  -1.3570   4.7509     1000 0.242
    ## WMGTFLD           -0.6278  -4.2519   3.0447     1273 0.740
    ## NRTE100            1.5753  -2.0746   5.1220     1000 0.370
    ## NRTE120            1.9573  -1.7137   5.5836     1000 0.272
    ## WMGTFLD:NRTE100   -0.7569  -6.3022   4.1761     1000 0.750
    ## WMGTFLD:NRTE120    0.3639  -5.2348   5.1283     1000 0.872

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
    ##  DIC: 32.33241 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     2.327 2.912e-06    2.647     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.3389   0.0895   0.6928     1000
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC  
    ## (Intercept)        1.2350   0.3635   2.1238     1010 0.026 *
    ## WMGTFLD           -0.2104  -0.9690   0.4998     1277 0.524  
    ## NRTE180            0.0671  -0.6717   0.8075     1000 0.826  
    ## WMGTFLD:NRTE180    0.4510  -0.6174   1.5429     1000 0.372  
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

2016 Tiller Sheath Blight Severity Model
----------------------------------------

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
    ##  DIC: 96.70196 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     54.64 0.0005162    151.5     1000
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     18.84    4.524     39.9     1000
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean  l-95% CI  u-95% CI eff.samp pMCMC   
    ## (Intercept)     19.651411 10.718371 26.906323     1000 0.004 **
    ## WMGTFLD          0.757718 -5.747448  7.148240     1000 0.802   
    ## NRTE180          0.005225 -5.547854  6.155133     1000 0.994   
    ## WMGTFLD:NRTE180  3.534241 -5.670048 11.922383     1000 0.394   
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
