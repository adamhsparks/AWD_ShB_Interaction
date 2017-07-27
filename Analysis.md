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

The `AUDPS` object is created when the project is loaded, in the file [munge/03\_preprocess\_data.R](./munge/03_preprocess_data.R). However, because it is a `tibble` and the treatments exist in a single column for graphing the raw data, this object needs a few minor changes to be usable for the analysis.

First, separate the TRT column into the two treatments for analysis

``` r
AUDPS <- separate(data = AUDPS, col = TRT, sep = "_", into = c("WMGT", "NRTE"))
AUDPS <- mutate_at(.tbl = AUDPS, .funs = factor, .vars = c("WMGT", "NRTE"))
```

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
    ##  DIC: 316.7767 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     1.454 0.0002698    4.884     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.7603   0.5759   0.9733     5054
    ## 
    ##  Location effects: TShB_inc_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp   pMCMC   
    ## (Intercept)         0.2163  -0.9698   1.3265     4950 0.57778   
    ## WMGTFLD             0.3464  -0.2150   0.8845     4950 0.22586   
    ## NRTEN100            0.2104  -0.3367   0.7468     4950 0.44283   
    ## NRTEN120            0.4183  -0.1494   0.9571     4950 0.13939   
    ## WMGTFLD:NRTEN100   -0.2895  -1.0681   0.4762     4950 0.46384   
    ## WMGTFLD:NRTEN120   -1.2389  -2.0525  -0.4795     4950 0.00242 **
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
TShB_severity_lmm_2015 <- MCMCglmm(TShB_percent_AUDPS ~ WMGT * NRTE,
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
    ##  DIC: 991.3887 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     388.9    16.64     1197     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     210.6    157.4    267.4     5161
    ## 
    ##  Location effects: TShB_percent_AUDPS ~ WMGT * NRTE 
    ## 
    ##                   post.mean   l-95% CI   u-95% CI eff.samp   pMCMC   
    ## (Intercept)        8.189102 -11.622386  28.104142     4950 0.31758   
    ## WMGTFLD           -8.753710 -18.132797  -0.007675     4950 0.05980 . 
    ## NRTEN100          13.789003   5.156191  23.688226     4950 0.00404 **
    ## NRTEN120           1.770126  -7.433339  10.952915     4950 0.70667   
    ## WMGTFLD:NRTEN100 -14.355876 -26.895081  -1.270376     4950 0.02828 * 
    ## WMGTFLD:NRTEN120  -0.349281 -13.672659  11.398836     5250 0.95919   
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
LShB_severity_lmm_2015 <- MCMCglmm(LShB_percent_AUDPS ~ WMGT * NRTE,
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
    ##  DIC: 634.062 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     18.49   0.3666    59.85     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     10.75    7.841    13.62     4950
    ## 
    ##  Location effects: LShB_percent_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp   pMCMC    
    ## (Intercept)         0.7613  -3.3422   5.0827     4950 0.61253    
    ## WMGTFLD             0.4019  -1.5253   2.4507     4950 0.71434    
    ## NRTEN100            4.8823   2.9307   6.9442     4950 < 2e-04 ***
    ## NRTEN120            3.1614   1.2132   5.3416     4950 0.00364 ** 
    ## WMGTFLD:NRTEN100   -6.1101  -9.0245  -3.2957     4950 < 2e-04 ***
    ## WMGTFLD:NRTEN120   -3.6350  -6.5083  -0.9457     4518 0.01414 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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
TShB_severity_lmm_2016 <- MCMCglmm(TShB_percent_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_16,
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
    ##  DIC: 462.8646 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     267.8     10.4    745.7     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     72.86     48.4    102.2     5351
    ## 
    ##  Location effects: TShB_percent_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## (Intercept)       26.2450  10.3569  40.9267     4950 0.0141 *  
    ## WMGTFLD           12.7532   7.0270  18.7851     4950 <2e-04 ***
    ## NRTEN60           -0.2008  -6.1862   5.5885     4950 0.9491    
    ## WMGTFLD:NRTEN60   -8.9841 -16.9150  -0.2855     4950 0.0352 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(TShB_severity_lmm_2016$Sol[, c(1, 5:8)])
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
LShB_severity_lmm_2016 <- MCMCglmm(LShB_percent_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_16,
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
    ##  DIC: 85.01022 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     1.051  0.01496    3.436     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.1994   0.1336   0.2751     4950
    ## 
    ##  Location effects: LShB_percent_AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp   pMCMC   
    ## (Intercept)       1.32767  0.41132  2.27194     4950 0.02384 * 
    ## WMGTFLD           0.24465 -0.07577  0.55093     4950 0.12283   
    ## NRTEN60           0.24335 -0.06666  0.55150     4315 0.12242   
    ## WMGTFLD:NRTEN60  -0.73441 -1.17030 -0.29765     4950 0.00242 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# create data frames for generating diagnostic plots
reps <- data.frame(LShB_severity_lmm_2016$Sol[, c(1, 5:8)])
reps <- melt(reps)
```

    ## No id variables; using all as measure variables

``` r
trts <-  data.frame(LShB_severity_lmm_2016$Sol[, 2:4])
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

![](Analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-6.png)

Conclusions
-----------

### Tiller Sheath Blight Incidence and Severity

In 2015 and 2016 the highest N treatment was significant in both the tiller sheath blight incidence and severity. In 2016 the flooding treatment was significant for tiller sheath blight severity.

### Leaf Sheath Blight Severity

In neither experiment was the leaf sheath blight affected by nitrogen rates or irrigation regime.

### Interactions of Nitrogen and Irrigation Regime

In 2015 no significant itneraction was observed. In 2016 a significant interaction between flooding and nitrogen rate is observed for both the tiller and leaf severity ratings.

### Model Fit

The models all appear to be good fits.

None of the diagnostic plots show any signs of autocorrelation or any other obvious patterns.

The random effects all appear to be acceptable, the dotted line stays near to the solid line with no discernible patterns.

The random effects are all fairly equally distributed except for 2016 tiller sheath blight severity where water management:replicate has a larger effect. This is not surprising given that the plot and replicate sizes were different in 2016 due to the use of two fields in the IRRI experiment station, see the [2016 plot layout plan](https://github.com/adamhsparks/AWD_ShB_Interaction/blob/master/doc/Metadata/2016%20AWD%20ShB%20Metadata/ExptLayoutAWD2016.pdf).

R Session Info
--------------

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.4.1 (2017-06-30)
    ##  system   x86_64, darwin16.7.0        
    ##  ui       unknown                     
    ##  language (EN)                        
    ##  collate  en_AU.UTF-8                 
    ##  tz       Australia/Brisbane          
    ##  date     2017-07-27

    ## Packages -----------------------------------------------------------------

    ##  package         * version date       source        
    ##  agricolae       * 1.2-5   2017-07-26 CRAN (R 3.4.1)
    ##  AlgDesign         1.1-7.3 2014-10-15 CRAN (R 3.4.1)
    ##  ape             * 4.1     2017-02-14 CRAN (R 3.4.1)
    ##  assertthat        0.2.0   2017-04-11 CRAN (R 3.4.1)
    ##  backports         1.1.0   2017-05-22 CRAN (R 3.4.1)
    ##  base            * 3.4.1   2017-07-24 local         
    ##  bindr             0.1     2016-11-13 CRAN (R 3.4.1)
    ##  bindrcpp        * 0.2     2017-06-17 CRAN (R 3.4.1)
    ##  bitops            1.0-6   2013-08-17 CRAN (R 3.4.1)
    ##  boot              1.3-19  2017-02-11 CRAN (R 3.4.1)
    ##  broom             0.4.2   2017-02-13 CRAN (R 3.4.1)
    ##  car             * 2.1-5   2017-07-04 CRAN (R 3.4.1)
    ##  caTools           1.17.1  2014-09-10 CRAN (R 3.4.1)
    ##  cellranger        1.1.0   2016-07-27 CRAN (R 3.4.1)
    ##  cluster           2.0.6   2017-03-10 CRAN (R 3.4.1)
    ##  coda            * 0.19-1  2016-12-08 CRAN (R 3.4.1)
    ##  colorspace        1.3-2   2016-12-14 CRAN (R 3.4.1)
    ##  combinat          0.0-8   2012-10-29 CRAN (R 3.4.1)
    ##  compiler          3.4.1   2017-07-24 local         
    ##  corpcor           1.6.9   2017-04-01 CRAN (R 3.4.1)
    ##  cubature          1.3-11  2017-07-19 CRAN (R 3.4.1)
    ##  datasets        * 3.4.1   2017-07-24 local         
    ##  deldir            0.1-14  2017-04-22 CRAN (R 3.4.1)
    ##  devtools          1.13.2  2017-06-02 CRAN (R 3.4.1)
    ##  digest            0.6.12  2017-01-27 CRAN (R 3.4.1)
    ##  dplyr           * 0.7.2   2017-07-20 CRAN (R 3.4.1)
    ##  evaluate          0.10.1  2017-06-24 CRAN (R 3.4.1)
    ##  expm              0.999-2 2017-03-29 CRAN (R 3.4.1)
    ##  fitdistrplus    * 1.0-9   2017-03-24 CRAN (R 3.4.1)
    ##  forcats           0.2.0   2017-01-23 CRAN (R 3.4.1)
    ##  foreign           0.8-69  2017-06-22 CRAN (R 3.4.1)
    ##  gdata             2.18.0  2017-06-06 CRAN (R 3.4.1)
    ##  ggplot2         * 2.2.1   2016-12-30 CRAN (R 3.4.1)
    ##  glue              1.1.1   2017-06-21 CRAN (R 3.4.1)
    ##  gmodels           2.16.2  2015-07-22 CRAN (R 3.4.1)
    ##  gplots            3.0.1   2016-03-30 CRAN (R 3.4.1)
    ##  graphics        * 3.4.1   2017-07-24 local         
    ##  grDevices       * 3.4.1   2017-07-24 local         
    ##  grid              3.4.1   2017-07-24 local         
    ##  gtable            0.2.0   2016-02-26 CRAN (R 3.4.1)
    ##  gtools            3.5.0   2015-05-29 CRAN (R 3.4.1)
    ##  haven             1.1.0   2017-07-09 CRAN (R 3.4.1)
    ##  hms               0.3     2016-11-22 CRAN (R 3.4.1)
    ##  htmltools         0.3.6   2017-04-28 CRAN (R 3.4.1)
    ##  httr              1.2.1   2016-07-03 CRAN (R 3.4.1)
    ##  jsonlite          1.5     2017-06-01 CRAN (R 3.4.1)
    ##  KernSmooth        2.23-15 2015-06-29 CRAN (R 3.4.1)
    ##  klaR              0.6-12  2014-08-06 CRAN (R 3.4.1)
    ##  knitr             1.16    2017-05-18 CRAN (R 3.4.1)
    ##  labeling          0.3     2014-08-23 CRAN (R 3.4.1)
    ##  lattice         * 0.20-35 2017-03-25 CRAN (R 3.4.1)
    ##  lazyeval          0.2.0   2016-06-12 CRAN (R 3.4.1)
    ##  LearnBayes        2.15    2014-05-29 CRAN (R 3.4.1)
    ##  lme4              1.1-13  2017-04-19 CRAN (R 3.4.1)
    ##  lubridate       * 1.6.0   2016-09-13 CRAN (R 3.4.1)
    ##  magrittr          1.5     2014-11-22 CRAN (R 3.4.1)
    ##  MASS            * 7.3-47  2017-02-26 CRAN (R 3.4.1)
    ##  Matrix          * 1.2-10  2017-05-03 CRAN (R 3.4.1)
    ##  MatrixModels      0.4-1   2015-08-22 CRAN (R 3.4.1)
    ##  MCMCglmm        * 2.24    2016-11-14 CRAN (R 3.4.1)
    ##  memoise           1.1.0   2017-04-21 CRAN (R 3.4.1)
    ##  methods         * 3.4.1   2017-07-24 local         
    ##  mgcv              1.8-17  2017-02-08 CRAN (R 3.4.1)
    ##  minqa             1.2.4   2014-10-09 CRAN (R 3.4.1)
    ##  mnormt            1.5-5   2016-10-15 CRAN (R 3.4.1)
    ##  modelr            0.1.1   2017-07-24 CRAN (R 3.4.1)
    ##  munsell           0.4.3   2016-02-13 CRAN (R 3.4.1)
    ##  nlme              3.1-131 2017-02-06 CRAN (R 3.4.1)
    ##  nloptr            1.0.4   2014-08-04 CRAN (R 3.4.1)
    ##  nnet              7.3-12  2016-02-02 CRAN (R 3.4.1)
    ##  parallel          3.4.1   2017-07-24 local         
    ##  pbkrtest          0.4-7   2017-03-15 CRAN (R 3.4.1)
    ##  pkgconfig         2.0.1   2017-03-21 CRAN (R 3.4.1)
    ##  plotMCMC        * 2.0-0   2014-03-12 CRAN (R 3.4.1)
    ##  plyr              1.8.4   2016-06-08 CRAN (R 3.4.1)
    ##  ProjectTemplate * 0.7     2016-08-11 CRAN (R 3.4.1)
    ##  psych             1.7.5   2017-05-03 CRAN (R 3.4.1)
    ##  purrr           * 0.2.2.2 2017-05-11 CRAN (R 3.4.1)
    ##  quantreg          5.33    2017-04-18 CRAN (R 3.4.1)
    ##  R6                2.2.2   2017-06-17 CRAN (R 3.4.1)
    ##  RColorBrewer      1.1-2   2014-12-07 CRAN (R 3.4.1)
    ##  Rcpp              0.12.12 2017-07-15 CRAN (R 3.4.1)
    ##  readr           * 1.1.1   2017-05-16 CRAN (R 3.4.1)
    ##  readxl            1.0.0   2017-04-18 CRAN (R 3.4.1)
    ##  reshape2        * 1.4.2   2016-10-22 CRAN (R 3.4.1)
    ##  rlang             0.1.1   2017-05-18 CRAN (R 3.4.1)
    ##  rmarkdown         1.6     2017-06-15 CRAN (R 3.4.1)
    ##  rprojroot         1.2     2017-01-16 CRAN (R 3.4.1)
    ##  rvest             0.3.2   2016-06-17 CRAN (R 3.4.1)
    ##  scales            0.4.1   2016-11-09 CRAN (R 3.4.1)
    ##  sp                1.2-5   2017-06-29 CRAN (R 3.4.1)
    ##  SparseM           1.77    2017-04-23 CRAN (R 3.4.1)
    ##  spdep             0.6-13  2017-04-25 CRAN (R 3.4.1)
    ##  splines           3.4.1   2017-07-24 local         
    ##  stats           * 3.4.1   2017-07-24 local         
    ##  stringi           1.1.5   2017-04-07 CRAN (R 3.4.1)
    ##  stringr           1.2.0   2017-02-18 CRAN (R 3.4.1)
    ##  survival        * 2.41-3  2017-04-04 CRAN (R 3.4.1)
    ##  tensorA           0.36    2010-12-01 CRAN (R 3.4.1)
    ##  tibble          * 1.3.3   2017-05-28 CRAN (R 3.4.1)
    ##  tidyr           * 0.6.3   2017-05-15 CRAN (R 3.4.1)
    ##  tidyverse       * 1.1.1   2017-01-27 CRAN (R 3.4.1)
    ##  tools             3.4.1   2017-07-24 local         
    ##  utils           * 3.4.1   2017-07-24 local         
    ##  withr             1.0.2   2016-06-20 CRAN (R 3.4.1)
    ##  xml2              1.1.1   2017-01-24 CRAN (R 3.4.1)
    ##  yaml              2.1.14  2016-11-12 CRAN (R 3.4.1)
