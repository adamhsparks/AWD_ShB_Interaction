Analysis with nested effects
================

Notes on this analysis before starting
--------------------------------------

These analyses nest the main plot effect as a random effect with the replicate.

#### How to interpret these analyses

For a quick overview, see this thread on Stack Overflow that walks through the output of `summary.MCMCglmm`, which is used below. [R: Making sense of the output of a MCMCglmm](http://stackoverflow.com/questions/20993643/r-making-sense-of-the-output-of-a-mcmcglmm)

The MCMCglmm documentation is more in-depth but should still be referenced.

``` r
library(MCMCglmm)
vignette("Overview")
vignette("CourseNotes")
```

#### Changes between 2015 and 2016

Before the analysis, note that due to changes between the years, the analysis must be carried out on each year separately. The 2015 data and 2016 data cannot be combined due to changes in inoculation methods; there are other changes too, but the main one is this. Therefore, the analyses will be conducted separately for leaf severity and Leaf severity for 2015 dry season and 2016 dry season. Comparisons will only be observational and cannot be statistically compared.

#### Iterations

Fitting the models with the default iterations resulted in some less than acceptable models. Because of this, the number of iterations has been increased, `nitt = 5e+05` and `burnin = 5000` and `thin = 100`. This results in much smaller errors for random effects and better models.

Setup
-----

Use `set.seed()` for reproducibility.

``` r
set.seed(27)
```

2015
----

### 2015 Leaf Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_lmm_2015 <- MCMCglmm(LShB_AUDPS ~ WMGT * NRTE,
                          random = ~WMGT:REP, 
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
    ##  DIC: 33.33689 
    ## 
    ##  G-structure:  ~WMGT:REP
    ## 
    ##          post.mean  l-95% CI u-95% CI eff.samp
    ## WMGT:REP   0.04099 1.082e-10   0.1636     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.1791  0.07547     0.32     4950
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)       0.18381 -0.29541  0.64554     4950 0.402
    ## WMGTFLD           0.01695 -0.59538  0.67331     4685 0.979
    ## NRTE100           0.35062 -0.26968  0.91808     4950 0.219
    ## NRTE120           0.28587 -0.28092  0.90537     4950 0.321
    ## WMGTFLD:NRTE100  -0.34186 -1.18366  0.46770     4950 0.402
    ## WMGTFLD:NRTE120  -0.19779 -1.05624  0.59392     4950 0.621

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_lmm_2015$Sol[, c(1, 7:14)])
x <- 1:nrow(reps)
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_lmm_2015$Sol[, 2:6])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(LShB_lmm_2015$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2015 Diagnostic Plots for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_LShB-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Reps Replicate Posteriors for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_LShB-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_LShB-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Posteriors for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_LShB-4.png)

``` r
# check random effects
plotTrace(LShB_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/2015_LShB-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(LShB_lmm_2015$VCV)
plot_joint_random_error_dist_nested(d = rdf,
                             title = ("2015 Random Error Distribution for Leaf ShB Severity"))
```

![](Analysis_files/figure-markdown_github/2015_LShB-6.png)

### 2015 Tiller Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_severity_lmm_2015 <- MCMCglmm(TShB_AUDPS ~ WMGT * NRTE,
                                   random = ~WMGT:REP, 
                          data = as.data.frame(AUDPS_2015),
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(TShB_severity_lmm_2015)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 121.4278 
    ## 
    ##  G-structure:  ~WMGT:REP
    ## 
    ##          post.mean  l-95% CI u-95% CI eff.samp
    ## WMGT:REP     1.593 1.641e-09    6.077     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     7.049    2.731    12.72     4950
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        1.5971  -1.5035   4.2799     4950 0.256
    ## WMGTFLD           -0.5397  -4.6499   3.4627     4487 0.783
    ## NRTE100            1.6468  -2.1110   5.4934     5312 0.375
    ## NRTE120            1.9433  -1.6560   5.7187     4950 0.268
    ## WMGTFLD:NRTE100   -0.8552  -6.0472   4.2223     4951 0.752
    ## WMGTFLD:NRTE120    0.3258  -4.9007   5.4478     4645 0.909

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_severity_lmm_2015$Sol[, c(1, 7:14)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_severity_lmm_2015$Sol[, 2:6])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(TShB_severity_lmm_2015$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2015 Diagnostic Plots for Replicates, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_TShB-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Reps Replicate Posteriors for Replicates, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_TShB-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_TShB-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Posteriors for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2015_TShB-4.png)

``` r
# check random effects
plotTrace(TShB_severity_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/2015_TShB-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_severity_lmm_2015$VCV)
plot_joint_random_error_dist_nested(d = rdf,
                             title = ("2015 Random Error Distribution for Tiller ShB Severity"))
```

![](Analysis_files/figure-markdown_github/2015_TShB-6.png)

### 2015 Tiller Sheath Blight Incidence Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_incidence_lmm_2015 <- MCMCglmm(TShB_incidence_mean ~ WMGT * NRTE,
                                    random = ~WMGT:REP, 
                          data = as.data.frame(AUDPS_2015),
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(TShB_incidence_lmm_2015)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 54.18184 
    ## 
    ##  G-structure:  ~WMGT:REP
    ## 
    ##          post.mean  l-95% CI u-95% CI eff.samp
    ## WMGT:REP   0.08722 4.933e-08   0.3538     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.4238   0.1767   0.7476     4950
    ## 
    ##  Location effects: TShB_incidence_mean ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)       0.30766 -0.39290  0.98146     4950 0.364
    ## WMGTFLD           0.17697 -0.82349  1.16379     4695 0.714
    ## NRTE100           0.22196 -0.70373  1.11198     4950 0.625
    ## NRTE120           0.58479 -0.28941  1.47209     4950 0.193
    ## WMGTFLD:NRTE100  -0.07141 -1.35847  1.26279     5645 0.917
    ## WMGTFLD:NRTE120  -0.40351 -1.68330  0.87150     4676 0.530

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_incidence_lmm_2015$Sol[, c(1, 7:14)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_incidence_lmm_2015$Sol[, 2:6])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(TShB_incidence_lmm_2015$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2015 Diagnostic Plots for Replicates, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Reps Replicate Posteriors for Replicates, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Posteriors for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-4.png)

``` r
# check random effects
plotTrace(TShB_incidence_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_incidence_lmm_2015$VCV)
plot_joint_random_error_dist_nested(d = rdf,
                             title = ("2015 Random Error Distribution for Tiller ShB Incidence"))
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-6.png)

------------------------------------------------------------------------

2016
----

### 2016 Leaf Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_lmm_2016 <- MCMCglmm(LShB_AUDPS ~ WMGT * NRTE,
                          random = ~WMGT:REP, 
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
    ##  DIC: 33.45351 
    ## 
    ##  G-structure:  ~WMGT:REP
    ## 
    ##          post.mean  l-95% CI u-95% CI eff.samp
    ## WMGT:REP    0.1522 1.091e-10   0.5533     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.3648   0.1097   0.7573     5218
    ## 
    ##  Location effects: LShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp   pMCMC   
    ## (Intercept)       1.23469  0.51856  1.98697     4904 0.00525 **
    ## WMGTFLD          -0.22263 -1.25852  0.81426     4950 0.62020   
    ## NRTE180           0.06184 -0.81975  0.88529     4950 0.88808   
    ## WMGTFLD:NRTE180   0.46062 -0.77972  1.59637     3627 0.41051   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_lmm_2016$Sol[, c(1, 5:12)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_lmm_2016$Sol[, 2:4])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(LShB_lmm_2016$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2016 Diagnostic Plots for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_LShB-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Reps Replicate Posteriors for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_LShB-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_LShB-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Posteriors for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_LShB-4.png)

``` r
# check random effects
plotTrace(LShB_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/2016_LShB-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(LShB_lmm_2016$VCV)
plot_joint_random_error_dist_nested(d = rdf,
                             title = ("2016 Random Error Distribution for Leaf ShB Severity"))
```

![](Analysis_files/figure-markdown_github/2016_LShB-6.png)

### 2016 Tiller Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_severity_lmm_2016 <- MCMCglmm(TShB_AUDPS ~ WMGT * NRTE,
                                   random = ~WMGT:REP, 
                          data = as.data.frame(AUDPS_2016),
                          verbose = FALSE,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          prior = eprior,
                          pr = TRUE)

summary(TShB_severity_lmm_2016)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 88.83837 
    ## 
    ##  G-structure:  ~WMGT:REP
    ## 
    ##          post.mean  l-95% CI u-95% CI eff.samp
    ## WMGT:REP     31.35 2.296e-06    85.07     4825
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     12.12     2.38    31.55     4984
    ## 
    ##  Location effects: TShB_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## (Intercept)      19.86920 13.41562 26.74047     4950 <2e-04 ***
    ## WMGTFLD           0.59600 -8.83091  9.90697     4950  0.876    
    ## NRTE180           0.03692 -4.57007  5.03527     4950  0.990    
    ## WMGTFLD:NRTE180   3.67426 -3.05293 10.80580     4950  0.244    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_severity_lmm_2016$Sol[, c(1, 5:12)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_severity_lmm_2016$Sol[, 2:4])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(TShB_severity_lmm_2016$Sol)
# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2016 Diagnostic Plots for Replicates, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_TShB-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Reps Replicate Posteriors for Replicates, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_TShB-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_TShB-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Posteriors for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github/2016_TShB-4.png)

``` r
# check random effects
plotTrace(TShB_severity_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/2016_TShB-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_severity_lmm_2016$VCV)
plot_joint_random_error_dist_nested(d = rdf,
                             title = ("2016 Random Error Distribution for Tiller ShB Severity"))
```

![](Analysis_files/figure-markdown_github/2016_TShB-6.png)

### 2016 Tiller Sheath Blight Incidence Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_incidence_lmm_2016 <- MCMCglmm(TShB_incidence_mean ~ WMGT * NRTE, 
                                    random = ~WMGT:REP, 
                          data = as.data.frame(AUDPS_2016),
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(TShB_incidence_lmm_2016)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 61.96193 
    ## 
    ##  G-structure:  ~WMGT:REP
    ## 
    ##          post.mean  l-95% CI u-95% CI eff.samp
    ## WMGT:REP     1.157 6.035e-07    4.215     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     2.133   0.5471    4.354     4950
    ## 
    ##  Location effects: TShB_incidence_mean ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## (Intercept)       11.1170   9.3059  12.8663     4950 <2e-04 ***
    ## WMGTFLD           -0.7557  -3.2482   1.9519     5185  0.511    
    ## NRTE180            1.5275  -0.4599   3.6348     4950  0.129    
    ## WMGTFLD:NRTE180    0.7650  -2.1088   3.7187     4950  0.571    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_incidence_lmm_2016$Sol[, c(1, 5:12)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_incidence_lmm_2016$Sol[, 2:4])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(TShB_incidence_lmm_2016$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2016 Diagnostic Plots for Replicates, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Reps Replicate Posteriors for Replicates, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Posteriors for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-4.png)

``` r
# check random effects
plotTrace(TShB_incidence_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_incidence_lmm_2016$VCV)
plot_joint_random_error_dist_nested(d = rdf,
                             title = ("2016 Random Error Distribution for Tiller ShB Incidence"))
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-6.png)

Conclusions
===========

The models all appear to be good fits.

None of the diagnostic plots show any signs of autocorrelation or any other obvious patterns.

None of the treatments in either year, 2015 or 2016, were significant. The see `pMCMC` values and also posterior graphs. There is a large amount of overlap in all of the posterior graphs.

The random effects all appear to be acceptable, the dotted line stays near to the solid line with no discernable patterns.

The random effects are all fairly equally distributed except for 2016 tiller sheath blight severity where water management:replicate has a larger effect. This is not surprising given that the plot and replicate sizes were different in 2016 due to the use of two fields in the IRRI experiment station, see the [2016 plot layout plan](https://github.com/adamhsparks/AWD_ShB_Interaction/blob/master/doc/Metadata/2016%20AWD%20ShB%20Metadata/ExptLayoutAWD2016.pdf).

R Session Info
==============

``` r
sessionInfo()
```

    ## R version 3.3.3 (2017-03-06)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: OS X El Capitan 10.11.6
    ## 
    ## locale:
    ## [1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] plotMCMC_2.0-0      fitdistrplus_1.0-9  survival_2.40-1    
    ##  [4] lattice_0.20-34     MCMCglmm_2.24       ape_4.1            
    ##  [7] coda_0.19-1         Matrix_1.2-8        car_2.1-4          
    ## [10] MASS_7.3-45         dplyr_0.5.0         purrr_0.2.2        
    ## [13] readr_1.1.0         tidyr_0.6.1         tibble_1.3.0       
    ## [16] ggplot2_2.2.1       tidyverse_1.1.1     viridis_0.4.0      
    ## [19] viridisLite_0.2.0   agricolae_1.2-4     lubridate_1.6.0    
    ## [22] ggthemes_3.4.0      plyr_1.8.4          reshape2_1.4.2     
    ## [25] ProjectTemplate_0.7
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-131         bitops_1.0-6         pbkrtest_0.4-7      
    ##  [4] gmodels_2.16.2       httr_1.2.1           rprojroot_1.2       
    ##  [7] tensorA_0.36         tools_3.3.3          backports_1.0.5     
    ## [10] R6_2.2.0             KernSmooth_2.23-15   AlgDesign_1.1-7.3   
    ## [13] DBI_0.6-1            lazyeval_0.2.0       mgcv_1.8-17         
    ## [16] colorspace_1.3-2     nnet_7.3-12          sp_1.2-4            
    ## [19] gridExtra_2.2.1      mnormt_1.5-5         klaR_0.6-12         
    ## [22] rvest_0.3.2          quantreg_5.29        SparseM_1.76        
    ## [25] expm_0.999-2         xml2_1.1.1           labeling_0.3        
    ## [28] caTools_1.17.1       scales_0.4.1         psych_1.7.3.21      
    ## [31] stringr_1.2.0        digest_0.6.12        foreign_0.8-67      
    ## [34] minqa_1.2.4          rmarkdown_1.4.0.9001 htmltools_0.3.5     
    ## [37] lme4_1.1-12          readxl_0.1.1         combinat_0.0-8      
    ## [40] jsonlite_1.4         gtools_3.5.0         spdep_0.6-12        
    ## [43] magrittr_1.5         Rcpp_0.12.10         munsell_0.4.3       
    ## [46] stringi_1.1.5        yaml_2.1.14          gplots_3.0.1        
    ## [49] grid_3.3.3           parallel_3.3.3       gdata_2.17.0        
    ## [52] forcats_0.2.0        deldir_0.1-12        haven_1.0.0         
    ## [55] splines_3.3.3        hms_0.3              knitr_1.15.1        
    ## [58] boot_1.3-18          cubature_1.3-6       corpcor_1.6.9       
    ## [61] LearnBayes_2.15      packrat_0.4.8-1      evaluate_0.10       
    ## [64] modelr_0.1.0         nloptr_1.0.4         MatrixModels_0.4-1  
    ## [67] gtable_0.2.0         assertthat_0.2.0     broom_0.4.2         
    ## [70] cluster_2.0.5
