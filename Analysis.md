Analysis of split plot design using MCMCglmm
================

Notes on this analysis before starting
--------------------------------------

The analyses are semi-automated. When this file is knit, it automatically loads the *ProjectTemplate* package and calls the `load.project()` function. This loads the data, cleans and munges it creating the `AUDPS` data frame used for the analysis below. The scripts used to clean and munge the data are located in the [munge](./munge) folder and numbered in the sequential order in which they are run.

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

The `AUDPS` object is created when the project is loaded, in the file [munge/03\_preprocess\_data.R](munge/03_preprocess_data.R). However, because it is a `tibble` and the treatments exist in a single column for graphing the raw data, this object needs a few minor changes to be usable for the analysis.

First, separate the TRT column into the two treatments for analysis

``` r
AUDPS <- separate(data = AUDPS, col = TRT, sep = "_", into = c("WMGT", "NRTE"))
AUDPS <- mutate_at(.tbl = AUDPS, .funs = factor, .cols = c("WMGT", "NRTE"))
```

    ## `.cols` has been renamed and is deprecated, please use `.vars`

Now create individual data frames for the analysis.

``` r
AUDPS_15 <- as.data.frame(AUDPS[AUDPS$YEAR == 2015, ])
AUDPS_15 <- droplevels(AUDPS_15)

AUDPS_16 <- as.data.frame(AUDPS[AUDPS$YEAR == 2016, ])
AUDPS_16 <- droplevels(AUDPS_16)
```

Now that the `AUDPS_15` and `AUDPS_16` `data.frames` exist, we can start the analysis.

2015
----

### 2015 Tiller Sheath Blight Incidence Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_incidence_lmm_2015 <- MCMCglmm(TShB_inc_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_15,
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
    ##  DIC: 179.4771 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP    0.6689 0.006269    2.016     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.2423   0.1836   0.3096     5096
    ## 
    ##  Location effects: TShB_inc_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## (Intercept)        0.30427 -0.38568  1.01449     4950  0.248    
    ## WMGTFLD            0.18547 -0.11693  0.50422     4950  0.246    
    ## NRTEN100           0.24161 -0.06968  0.54405     4950  0.123    
    ## NRTEN120           0.58155  0.26143  0.88559     4950 <2e-04 ***
    ## WMGTFLD:NRTEN100  -0.09362 -0.53339  0.33760     4950  0.678    
    ## WMGTFLD:NRTEN120  -0.36516 -0.82421  0.06362     4950  0.102    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_incidence_lmm_2015$Sol[, c(1, 7:10)])
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

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Reps Replicate Posteriors for Replicates, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Posteriors for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-4.png)

``` r
# check random effects
plotTrace(TShB_incidence_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_incidence_lmm_2015$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2015 Random Error Distribution for Tiller ShB Incidence"))
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-6.png)

### 2015 Tiller Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_severity_lmm_2015 <- MCMCglmm(TShB_sev_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_15,
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
    ##  DIC: 645.3289 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     16.78   0.3232    51.69     4431
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     11.78    8.861       15     4950
    ## 
    ##  Location effects: TShB_sev_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC  
    ## (Intercept)         4.5710   0.5424   8.8091     4950 0.0339 *
    ## WMGTFLD            -0.5974  -2.8103   1.4701     4950 0.5786  
    ## NRTEN100            1.4007  -0.6460   3.7407     4950 0.2117  
    ## NRTEN120            2.7747   0.5226   4.8641     4950 0.0141 *
    ## WMGTFLD:NRTEN100   -1.2761  -4.2439   1.8370     4950 0.4109  
    ## WMGTFLD:NRTEN120   -0.8917  -3.9626   1.9592     5249 0.5434  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_severity_lmm_2015$Sol[, c(1, 7:10)])
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

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Reps Replicate Posteriors for Replicates, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Posteriors for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-4.png)

``` r
# check random effects
plotTrace(TShB_severity_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_severity_lmm_2015$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2015 Random Error Distribution for Tiller ShB Severity"))
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-6.png)

### 2015 Leaf Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_severity_lmm_2015 <- MCMCglmm(LShB_sev_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_15,
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(LShB_severity_lmm_2015)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 161.4038 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP    0.9671  0.01567    3.158     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.2095   0.1535   0.2659     4950
    ## 
    ##  Location effects: LShB_sev_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        0.57150 -0.28186  1.54311     4950 0.118
    ## WMGTFLD           -0.22898 -0.49812  0.05708     4950 0.112
    ## NRTEN100           0.17793 -0.09471  0.46454     4950 0.217
    ## NRTEN120           0.10595 -0.16553  0.41045     4950 0.458
    ## WMGTFLD:NRTEN100  -0.29047 -0.69757  0.10243     4950 0.144
    ## WMGTFLD:NRTEN120  -0.08826 -0.49071  0.28548     4519 0.673

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_severity_lmm_2015$Sol[, c(1, 7:10)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_severity_lmm_2015$Sol[, 2:6])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(LShB_severity_lmm_2015$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2015 Diagnostic Plots for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2015 Reps Replicate Posteriors for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2015 Diagnostic Plots for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2015 Posteriors for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-4.png)

``` r
# check random effects
plotTrace(LShB_severity_lmm_2015$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(LShB_severity_lmm_2015$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2015 Random Error Distribution for Leaf ShB Severity"))
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-6.png)

------------------------------------------------------------------------

2016
----

### 2016 Tiller Sheath Blight Incidence Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))

TShB_incidence_lmm_2016 <- MCMCglmm(TShB_inc_AUDPS ~ WMGT * NRTE, 
                                    random = ~REP, 
                          data = AUDPS_16,
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
    ##  DIC: 187.0551 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     5.186  0.09985    16.64     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.9824   0.6364    1.363     5135
    ## 
    ##  Location effects: TShB_inc_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
    ## (Intercept)     12.684651 10.498329 14.690101     4950 <2e-04 ***
    ## WMGTFLD          0.004514 -0.712805  0.666176     4559  0.996    
    ## NRTEN60         -1.537113 -2.231441 -0.891587     4950 <2e-04 ***
    ## WMGTFLD:NRTEN60 -0.774722 -1.767406  0.193410     4950  0.115    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_incidence_lmm_2016$Sol[, c(1, 5:8)])
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

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Reps Replicate Posteriors for Replicates, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Posteriors for Treatments, Tiller ShB Incidence")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-4.png)

``` r
# check random effects
plotTrace(TShB_incidence_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_incidence_lmm_2016$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2016 Random Error Distribution for Tiller ShB Incidence"))
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-6.png)

### 2016 Tiller Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_severity_lmm_2016 <- MCMCglmm(TShB_sev_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_15,
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(TShB_severity_lmm_2016)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 645.3083 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     17.12   0.3972    58.22     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units      11.8    8.965    15.13     6356
    ## 
    ##  Location effects: TShB_sev_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC  
    ## (Intercept)         4.5611   0.6357   9.0101     5315 0.0412 *
    ## WMGTFLD            -0.6089  -2.7281   1.5798     4950 0.5778  
    ## NRTEN100            1.3715  -0.7216   3.5797     4950 0.2101  
    ## NRTEN120            2.7405   0.5635   4.7512     4950 0.0101 *
    ## WMGTFLD:NRTEN100   -1.2441  -4.3705   1.7433     5219 0.4182  
    ## WMGTFLD:NRTEN120   -0.8826  -3.9524   2.1528     4950 0.5624  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_severity_lmm_2016$Sol[, c(1, 7:10)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(TShB_severity_lmm_2016$Sol[, 2:6])
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

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Reps Replicate Posteriors for Replicates, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Posteriors for Treatments, Tiller ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-4.png)

``` r
# check random effects
plotTrace(TShB_severity_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(TShB_severity_lmm_2016$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2016 Random Error Distribution for Tiller ShB Severity"))
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-6.png)

### 2016 Leaf Sheath Blight Severity Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
LShB_severity_lmm_2016 <- MCMCglmm(LShB_sev_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_15,
                          verbose = FALSE,
                          prior = eprior,
                          nitt = 5e+05,
                          burnin = 5000,
                          thin = 100,
                          pr = TRUE)

summary(LShB_severity_lmm_2016)
```

    ## 
    ##  Iterations = 5001:499901
    ##  Thinning interval  = 100
    ##  Sample size  = 4950 
    ## 
    ##  DIC: 161.3886 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     0.983  0.01324    3.427     4525
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.2097    0.156   0.2673     4950
    ## 
    ##  Location effects: LShB_sev_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp pMCMC
    ## (Intercept)        0.57248 -0.36830  1.52108     5416 0.122
    ## WMGTFLD           -0.22700 -0.50529  0.04655     5490 0.109
    ## NRTEN100           0.18311 -0.10218  0.46265     5216 0.195
    ## NRTEN120           0.10670 -0.17971  0.38569     5622 0.452
    ## WMGTFLD:NRTEN100  -0.29915 -0.69075  0.10354     5472 0.147
    ## WMGTFLD:NRTEN120  -0.08880 -0.48083  0.30279     4950 0.661

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_severity_lmm_2016$Sol[, c(1, 7:10)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_severity_lmm_2016$Sol[, 2:6])
trts <- melt(trts)
```

    ## No id variables; using all as measure variables

``` r
# Create a dummy x-axis variable for plotting
x <- 1:nrow(LShB_severity_lmm_2016$Sol)

# diagnostic line plots for replicate
plot_diagnostic_lines(d = reps,
                      x = x,
                      title = "2016 Diagnostic Plots for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-1.png)

``` r
# posterior distributions for replicate
plot_replicate_posteriors(d = reps,
                          title = "2016 Reps Replicate Posteriors for Replicates, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-2.png)

``` r
# diagnostic line plots for treatments
plot_diagnostic_lines(d = trts,
                      x = x,
                      title = "2016 Diagnostic Plots for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-3.png)

``` r
# Posterior distributions for treatment
plot_treatment_posteriors(d = trts,
                          title = "2016 Posteriors for Treatments, Leaf ShB Severity")
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-4.png)

``` r
# check random effects
plotTrace(LShB_severity_lmm_2016$VCV, log = TRUE)
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-5.png)

``` r
# plot joint distibution of error
rdf <- data.frame(LShB_severity_lmm_2016$VCV)
plot_joint_random_error_dist(d = rdf,
                             title = ("2016 Random Error Distribution for Leaf ShB Severity"))
```

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-6.png) \#\# Conclusions

### Tiller Sheath Blight Incidence and Severity

In 2015 and 2016 the highest N treatment was significant in both the tiller sheath blight incidence and severity.

### Leaf Sheath Blight Severity

In neither experiment was the leaf sheath blight affected by nitrogen rates or irrigation regime.

### Interactions of Nitrogen and Irrigation Regime

In neither experiment was the interaction of nitrogen with irrigation regime significant.

### Model Fit

The models all appear to be good fits.

None of the diagnostic plots show any signs of autocorrelation or any other obvious patterns.

The random effects all appear to be acceptable, the dotted line stays near to the solid line with no discernible patterns.

The random effects are all fairly equally distributed except for 2016 tiller sheath blight severity where water management:replicate has a larger effect. This is not surprising given that the plot and replicate sizes were different in 2016 due to the use of two fields in the IRRI experiment station, see the [2016 plot layout plan](https://github.com/adamhsparks/AWD_ShB_Interaction/blob/master/doc/Metadata/2016%20AWD%20ShB%20Metadata/ExptLayoutAWD2016.pdf).

R Session Info
--------------

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.4.0 (2017-04-21)
    ##  system   x86_64, darwin15.6.0        
    ##  ui       unknown                     
    ##  language (EN)                        
    ##  collate  en_AU.UTF-8                 
    ##  tz       Australia/Brisbane          
    ##  date     2017-06-16

    ## Packages -----------------------------------------------------------------

    ##  package         * version date       source         
    ##  agricolae       * 1.2-4   2016-06-12 CRAN (R 3.4.0) 
    ##  AlgDesign         1.1-7.3 2014-10-15 CRAN (R 3.4.0) 
    ##  ape             * 4.1     2017-02-14 CRAN (R 3.4.0) 
    ##  assertthat        0.2.0   2017-04-11 CRAN (R 3.4.0) 
    ##  backports         1.1.0   2017-05-22 cran (@1.1.0)  
    ##  base            * 3.4.0   2017-05-11 local          
    ##  bindr             0.1     2016-11-13 cran (@0.1)    
    ##  bindrcpp        * 0.1     2016-12-11 cran (@0.1)    
    ##  bitops            1.0-6   2013-08-17 CRAN (R 3.4.0) 
    ##  boot              1.3-19  2017-02-11 CRAN (R 3.4.0) 
    ##  broom             0.4.2   2017-02-13 CRAN (R 3.4.0) 
    ##  car             * 2.1-4   2016-12-02 CRAN (R 3.4.0) 
    ##  caTools           1.17.1  2014-09-10 CRAN (R 3.4.0) 
    ##  cellranger        1.1.0   2016-07-27 CRAN (R 3.4.0) 
    ##  cluster           2.0.6   2017-03-10 CRAN (R 3.4.0) 
    ##  coda            * 0.19-1  2016-12-08 CRAN (R 3.4.0) 
    ##  colorspace        1.3-2   2016-12-14 CRAN (R 3.4.0) 
    ##  combinat          0.0-8   2012-10-29 CRAN (R 3.4.0) 
    ##  compiler          3.4.0   2017-05-11 local          
    ##  corpcor           1.6.9   2017-04-01 CRAN (R 3.4.0) 
    ##  cubature          1.3-8   2017-05-11 cran (@1.3-8)  
    ##  datasets        * 3.4.0   2017-05-11 local          
    ##  deldir            0.1-14  2017-04-22 CRAN (R 3.4.0) 
    ##  devtools          1.13.2  2017-06-02 cran (@1.13.2) 
    ##  digest            0.6.12  2017-01-27 CRAN (R 3.4.0) 
    ##  dplyr           * 0.7.0   2017-06-09 cran (@0.7.0)  
    ##  evaluate          0.10    2016-10-11 CRAN (R 3.4.0) 
    ##  expm              0.999-2 2017-03-29 CRAN (R 3.4.0) 
    ##  fitdistrplus    * 1.0-9   2017-03-24 CRAN (R 3.4.0) 
    ##  forcats           0.2.0   2017-01-23 CRAN (R 3.4.0) 
    ##  foreign           0.8-67  2016-09-13 CRAN (R 3.4.0) 
    ##  gdata             2.18.0  2017-06-06 cran (@2.18.0) 
    ##  ggplot2         * 2.2.1   2016-12-30 CRAN (R 3.4.0) 
    ##  glue              1.1.0   2017-06-13 cran (@1.1.0)  
    ##  gmodels           2.16.2  2015-07-22 CRAN (R 3.4.0) 
    ##  gplots            3.0.1   2016-03-30 CRAN (R 3.4.0) 
    ##  graphics        * 3.4.0   2017-05-11 local          
    ##  grDevices       * 3.4.0   2017-05-11 local          
    ##  grid              3.4.0   2017-05-11 local          
    ##  gridExtra         2.2.1   2016-02-29 CRAN (R 3.4.0) 
    ##  gtable            0.2.0   2016-02-26 CRAN (R 3.4.0) 
    ##  gtools            3.5.0   2015-05-29 CRAN (R 3.4.0) 
    ##  haven             1.0.0   2016-09-23 CRAN (R 3.4.0) 
    ##  hms               0.3     2016-11-22 CRAN (R 3.4.0) 
    ##  htmltools         0.3.6   2017-04-28 cran (@0.3.6)  
    ##  httr              1.2.1   2016-07-03 CRAN (R 3.4.0) 
    ##  jsonlite          1.5     2017-06-01 cran (@1.5)    
    ##  KernSmooth        2.23-15 2015-06-29 CRAN (R 3.4.0) 
    ##  klaR              0.6-12  2014-08-06 CRAN (R 3.4.0) 
    ##  knitr             1.16    2017-05-18 cran (@1.16)   
    ##  labeling          0.3     2014-08-23 CRAN (R 3.4.0) 
    ##  lattice         * 0.20-35 2017-03-25 CRAN (R 3.4.0) 
    ##  lazyeval          0.2.0   2016-06-12 CRAN (R 3.4.0) 
    ##  LearnBayes        2.15    2014-05-29 CRAN (R 3.4.0) 
    ##  lme4              1.1-13  2017-04-19 CRAN (R 3.4.0) 
    ##  lubridate       * 1.6.0   2016-09-13 CRAN (R 3.4.0) 
    ##  magrittr          1.5     2014-11-22 CRAN (R 3.4.0) 
    ##  MASS            * 7.3-47  2017-02-26 CRAN (R 3.4.0) 
    ##  Matrix          * 1.2-9   2017-03-14 CRAN (R 3.4.0) 
    ##  MatrixModels      0.4-1   2015-08-22 CRAN (R 3.4.0) 
    ##  MCMCglmm        * 2.24    2016-11-14 CRAN (R 3.4.0) 
    ##  memoise           1.1.0   2017-04-21 CRAN (R 3.4.0) 
    ##  methods         * 3.4.0   2017-05-11 local          
    ##  mgcv              1.8-17  2017-02-08 CRAN (R 3.4.0) 
    ##  minqa             1.2.4   2014-10-09 CRAN (R 3.4.0) 
    ##  mnormt            1.5-5   2016-10-15 CRAN (R 3.4.0) 
    ##  modelr            0.1.0   2016-08-31 CRAN (R 3.4.0) 
    ##  munsell           0.4.3   2016-02-13 CRAN (R 3.4.0) 
    ##  nlme              3.1-131 2017-02-06 CRAN (R 3.4.0) 
    ##  nloptr            1.0.4   2014-08-04 CRAN (R 3.4.0) 
    ##  nnet              7.3-12  2016-02-02 CRAN (R 3.4.0) 
    ##  packrat           0.4.8-1 2016-09-07 CRAN (R 3.4.0) 
    ##  parallel          3.4.0   2017-05-11 local          
    ##  pbkrtest          0.4-7   2017-03-15 CRAN (R 3.4.0) 
    ##  pkgconfig         2.0.1   2017-03-21 cran (@2.0.1)  
    ##  plotMCMC        * 2.0-0   2014-03-12 CRAN (R 3.4.0) 
    ##  plyr              1.8.4   2016-06-08 CRAN (R 3.4.0) 
    ##  ProjectTemplate * 0.7     2016-08-11 CRAN (R 3.4.0) 
    ##  psych             1.7.5   2017-05-03 cran (@1.7.5)  
    ##  purrr           * 0.2.2.2 2017-05-11 cran (@0.2.2.2)
    ##  quantreg          5.33    2017-04-18 CRAN (R 3.4.0) 
    ##  R6                2.2.1   2017-05-10 cran (@2.2.1)  
    ##  Rcpp              0.12.11 2017-05-22 cran (@0.12.11)
    ##  readr           * 1.1.1   2017-05-16 cran (@1.1.1)  
    ##  readxl            1.0.0   2017-04-18 CRAN (R 3.4.0) 
    ##  reshape2        * 1.4.2   2016-10-22 CRAN (R 3.4.0) 
    ##  rlang             0.1.1   2017-05-18 cran (@0.1.1)  
    ##  rmarkdown         1.6     2017-06-15 cran (@1.6)    
    ##  rprojroot         1.2     2017-01-16 CRAN (R 3.4.0) 
    ##  rvest             0.3.2   2016-06-17 CRAN (R 3.4.0) 
    ##  scales            0.4.1   2016-11-09 CRAN (R 3.4.0) 
    ##  sp                1.2-4   2016-12-22 CRAN (R 3.4.0) 
    ##  SparseM           1.77    2017-04-23 CRAN (R 3.4.0) 
    ##  spdep             0.6-13  2017-04-25 CRAN (R 3.4.0) 
    ##  splines           3.4.0   2017-05-11 local          
    ##  stats           * 3.4.0   2017-05-11 local          
    ##  stringi           1.1.5   2017-04-07 CRAN (R 3.4.0) 
    ##  stringr           1.2.0   2017-02-18 CRAN (R 3.4.0) 
    ##  survival        * 2.41-3  2017-04-04 CRAN (R 3.4.0) 
    ##  tensorA           0.36    2010-12-01 CRAN (R 3.4.0) 
    ##  tibble          * 1.3.3   2017-05-28 cran (@1.3.3)  
    ##  tidyr           * 0.6.3   2017-05-15 CRAN (R 3.4.0) 
    ##  tidyverse       * 1.1.1   2017-01-27 CRAN (R 3.4.0) 
    ##  tools             3.4.0   2017-05-11 local          
    ##  utils           * 3.4.0   2017-05-11 local          
    ##  viridis           0.4.0   2017-03-27 CRAN (R 3.4.0) 
    ##  viridisLite       0.2.0   2017-03-24 CRAN (R 3.4.0) 
    ##  withr             1.0.2   2016-06-20 CRAN (R 3.4.0) 
    ##  xml2              1.1.1   2017-01-24 CRAN (R 3.4.0) 
    ##  yaml              2.1.14  2016-11-12 CRAN (R 3.4.0)
