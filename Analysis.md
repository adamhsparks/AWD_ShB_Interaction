Analysis
================

Notes on this analysis before starting
--------------------------------------

#### Changes between 2015 and 2016

Before the analysis, note that due to changes between the years, the analysis must be carried out on each year separately. The 2015 data and 2016 data cannot be combined due to changes in inoculation methods; there are other changes too, but the main one is this. Therefore, the analyses will be conducted separately for leaf severity and Leaf severity for 2015 dry season and 2016 dry season. Comparisons will only be observational and cannot be statistically compared.

#### Random effects

Fitting the models with the default iterations resulted in some less than acceptable models. Because of this, the number of iterations has been increased, `nitt = 5e+05` and `burnin = 5000` and `thin = 100`. This results in much smaller errors for random effects and better models.

2015
----

### 2015 Leaf Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_lmm_2015 <- MCMCglmm(LShB_AUDPS ~ WMGT * NRTE, ~REP, 
                          data = as.data.frame(AUDPS_2015),
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(LShB_lmm_2015)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 32.52772 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP    0.1968 7.405e-09   0.6452     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.1744  0.07087   0.3088     4950
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)       0.18311 -0.38928  0.73378     4950 0.447
    ## WMGTFLD           0.01575 -0.54937  0.60005     4950 0.953
    ## NRTE100           0.35867 -0.23042  0.96088     4950 0.215
    ## NRTE120           0.29051 -0.31785  0.87024     4804 0.295
    ## WMGTFLD:NRTE100  -0.34203 -1.18285  0.44515     5526 0.402
    ## WMGTFLD:NRTE120  -0.19035 -1.06060  0.63251     4950 0.637

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_lmm_2015$Sol[, c(1, 7:10)])
x <- 1:nrow(reps)
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
# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2015 Diagnostic Plots for Leaf ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Replicate Posteriors for Leaf ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Leaf ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Treatment Posteriors for Leaf ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
# check random effects
plotTrace(LShB_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(LShB_lmm_2015$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2015 Leaf ShB Random Error Distribution"))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-6.png)

### 2015 Tiller Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_lmm_2015 <- MCMCglmm(TShB_AUDPS ~ WMGT * NRTE, ~REP, 
                          data = as.data.frame(AUDPS_2015),
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(TShB_lmm_2015)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 121.3677 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     3.675 6.729e-09    13.16     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     7.088    2.942    12.56     4950
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        1.6132  -1.5138   4.7550     4974 0.269
    ## WMGTFLD           -0.5339  -3.9293   3.3258     4950 0.755
    ## NRTE100            1.6242  -2.1789   5.3723     4950 0.382
    ## NRTE120            1.9299  -1.9397   5.5084     4950 0.284
    ## WMGTFLD:NRTE100   -0.8106  -6.0038   4.7800     4950 0.746
    ## WMGTFLD:NRTE120    0.3288  -4.6964   5.6070     4950 0.895

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

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2015 Diagnostic Plots for Tiller ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Replicate Posteriors for Tiller ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Tiller ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Treatment Posteriors for Tiller ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-4.png)

``` r
# check random effects
plotTrace(TShB_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_lmm_2015$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2015 Tiller ShB Random Error Distribution"))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-6.png)

------------------------------------------------------------------------

2016
----

### 2016 Leaf Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_lmm_2016 <- MCMCglmm(LShB_AUDPS ~ WMGT * NRTE, ~REP, 
                          data = as.data.frame(AUDPS_2016),
                          verbose = FALSE,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          prior = eprior,
                          pr = TRUE)

summary(LShB_lmm_2016)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 32.25267 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP    0.7034 4.069e-08    2.185     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.3394  0.09674   0.6987     4950
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp  pMCMC  
    ## (Intercept)       1.22392  0.32882  2.12552     4950 0.0222 *
    ## WMGTFLD          -0.22232 -1.07358  0.56346     4950 0.5580  
    ## NRTE180           0.06566 -0.72952  0.90009     4950 0.8558  
    ## WMGTFLD:NRTE180   0.47573 -0.69350  1.63383     4950 0.3794  
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

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2016 Diagnostic Plots for Leaf ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Replicate Posteriors for Leaf ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Leaf ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Treatment Posteriors for Leaf ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
# check random effects
plotTrace(LShB_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(LShB_lmm_2015$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2016 Leaf ShB Random Error Distribution"))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-6.png)

### 2016 Tiller Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_lmm_2016 <- MCMCglmm(TShB_AUDPS ~ WMGT * NRTE, ~REP, 
                          data = as.data.frame(AUDPS_2016),
                          verbose = FALSE,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          prior = eprior,
                          pr = TRUE)

summary(TShB_lmm_2016)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 96.60297 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     46.61 4.006e-08    149.1     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     18.81     5.02    40.58     4950
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp   pMCMC   
    ## (Intercept)      19.91603 12.37529 27.73789     4950 0.00242 **
    ## WMGTFLD           0.67762 -5.48090  6.66754     4950 0.80000   
    ## NRTE180           0.02448 -6.19748  5.92548     4950 0.97939   
    ## WMGTFLD:NRTE180   3.66697 -4.46998 12.74043     4950 0.35798   
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

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2016 Diagnostic Plots for Tiller ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Replicate Posteriors for Tiller ShB Reps")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2106 Diagnostic Plots for Tiller ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Treatment Posteriors for Tiller ShB Treatments")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
# check random effects
plotTrace(TShB_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_lmm_2016$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2016 Tiller ShB Random Error Distribution"))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-6.png)

Conclusions
===========

The models all appear to be good fits after increasing the number of iterations. None of the treatments in either year, 2015 or 2016, were significant. The see `pMCMC` values and also posterior graphs. There is a large amount of overlap in all of the posterior graphs.
