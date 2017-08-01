Analysis of split plot design using MCMCglmm
================

Notes on This Analysis Before Starting
--------------------------------------

If you have not already looked at the [01-Probability\_distribution\_checks](01-Probability_distribution_checks.md), please see that first to understand why MCMCglmm was chosen for this analysis due to the nature of the data.

The analyses are semi-automated. When this file is knit, it automatically loads the *ProjectTemplate* package and calls the `load.project()` function. This loads the data, cleans and munges it creating the `AUDPS` data frame used for the analysis below. The scripts used to clean and munge the data are located in the [munge](./munge) folder and numbered in the sequential order in which they are run.

#### Interpreting these analyses

For a quick overview, see this thread on Stack Overflow that walks through the output of `summary.MCMCglmm`, which is used below. [R: Making sense of the output of a MCMCglmm](http://stackoverflow.com/questions/20993643/r-making-sense-of-the-output-of-a-mcmcglmm)

The MCMCglmm documentation is more in-depth but should still be referenced.

``` r
library(MCMCglmm)
vignette("Overview")
vignette("CourseNotes")
```

I found this CrossValidated topic to be useful in interpreting the outputs from MCMCglmm as it explains the interpretation of the effects. While it is about output from a `lmer`, the output is read in the same way, [How to interpret 2-way and 3-way interaction in lmer?](https://stats.stackexchange.com/questions/87412/how-to-interpret-2-way-and-3-way-interaction-in-lmer#87415).

The base levels for this analysis are:

-   2015 `NRTE:0`

-   2015 `WMGT:FLD`

-   2016 `NRTE:60`

-   2016 `WMGT:FLD`

#### Changes between 2015 and 2016

Before the analysis, note that due to changes between the years, the analysis must be carried out on each year separately. The 2015 data and 2016 data cannot be combined due to changes in inoculation methods; there are other changes too, but the main one is this. Therefore, the analyses will be conducted separately such that comparisons will only be observational and cannot be statistically compared.

#### MCMCglmm model fitting notes

Fitting the models with the default iterations resulted in some less than acceptable models. Because of this, the number of iterations has been increased, `nitt = 5e+05` and `burnin = 5000` and `thin = 100`. This results in much smaller errors for random effects and better models.

Setup
-----

Use `set.seed()` for reproducibility.

``` r
set.seed(27)
```

The `AUDPS` object is created when the project is loaded, in the file [munge/03\_preprocess\_data.R](./munge/03_preprocess_data.R). However, because it is a `tibble` and the treatments exist in a single column for graphing the raw data, this object needs a few minor changes to be usable for the analysis.

First, separate the TRT column into the two treatments for analysis.

``` r
AUDPS <- separate(data = AUDPS, col = TRT, sep = "_", into = c("WMGT", "NRTE"))
AUDPS <- mutate_at(.tbl = AUDPS, .funs = factor, .vars = c("WMGT", "NRTE"))
```

Now create individual data frames for the analysis.

``` r
AUDPS_2015 <- as.data.frame(AUDPS[AUDPS$YEAR == 2015, ])
AUDPS_2015 <- droplevels(AUDPS_2015)

# relevel factors for easier interpretation of  analysis
AUDPS_2015 <- within(AUDPS_2015, NRTE <- relevel(NRTE, ref = "N0"))
AUDPS_2015 <- within(AUDPS_2015, WMGT <- relevel(WMGT, ref = "FLD"))

AUDPS_2016 <- as.data.frame(AUDPS[AUDPS$YEAR == 2016, ])
AUDPS_2016 <- droplevels(AUDPS_2016)

# relevel factors for easier interpretation of  analysis
AUDPS_2016 <- within(AUDPS_2016, NRTE <- relevel(NRTE, ref = "N60"))
AUDPS_2016 <- within(AUDPS_2016, WMGT <- relevel(WMGT, ref = "FLD"))
```

Now that the `AUDPS_2015` and `AUDPS_2016` `data.frames` exist, we can start the analysis.

2015
----

### 2015 Tiller Sheath Blight Incidence Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_incidence_lmm_2015 <- MCMCglmm(TShB_inc_AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = AUDPS_2015,
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
    ##  DIC: 330.8031 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     1.163 0.0004146    3.817     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units    0.8547   0.6412     1.09     5119
    ## 
    ##  Location effects: TShB_inc_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC  
    ## (Intercept)         1.2794   0.2924   2.2933     4950 0.0299 *
    ## WMGTAWD            -0.2237  -0.8321   0.3334     4950 0.4541  
    ## NRTEN100            0.4059  -0.1851   0.9565     5001 0.1701  
    ## NRTEN120            0.7249   0.1594   1.3113     4950 0.0158 *
    ## WMGTAWD:NRTEN100    0.4546  -0.3599   1.2732     4950 0.2780  
    ## WMGTAWD:NRTEN120    0.5783  -0.2334   1.4339     4950 0.1798  
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
                          data = AUDPS_2015,
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
    ##  DIC: 980.4397 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     38.87 1.724e-09    128.6     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     193.2    144.1    245.9     5154
    ## 
    ##  Location effects: TShB_percent_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## (Intercept)         6.5997  -2.1128  14.3480     4950 0.1022    
    ## WMGTAWD             4.2805  -4.2085  13.1591     4950 0.3358    
    ## NRTEN100           10.3177   2.2527  19.2712     4733 0.0214 *  
    ## NRTEN120           17.3244   9.1247  26.2496     5028 <2e-04 ***
    ## WMGTAWD:NRTEN100   13.4025   0.7935  25.4218     4950 0.0335 *  
    ## WMGTAWD:NRTEN120   -1.1310 -12.8795  11.1221     5245 0.8416    
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
                          data = AUDPS_2015,
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
    ##  DIC: 549.6104 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     10.15   0.2621    33.89     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units      5.32    3.909    6.758     4950
    ## 
    ##  Location effects: LShB_percent_AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp   pMCMC    
    ## (Intercept)         1.8750  -1.4277   4.9223     4383 0.16323    
    ## WMGTAWD            -1.2026  -2.6530   0.1500     4950 0.08646 .  
    ## NRTEN100            0.8474  -0.6564   2.1968     4950 0.23717    
    ## NRTEN120            1.9060   0.5327   3.3704     4950 0.00848 ** 
    ## WMGTAWD:NRTEN100    4.2496   2.2677   6.3005     4950 < 2e-04 ***
    ## WMGTAWD:NRTEN120    2.4312   0.5480   4.4611     4518 0.01980 *  
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
                          data = AUDPS_2016,
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
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC    
    ## (Intercept)       10.37733  8.23492 12.46388     4950 <2e-04 ***
    ## WMGTAWD            0.77021  0.04607  1.42810     4950 0.0303 *  
    ## NRTEN180           2.31183  1.61610  2.99643     4950 <2e-04 ***
    ## WMGTAWD:NRTEN180  -0.77472 -1.76741  0.19341     4950 0.1147    
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
                          data = AUDPS_2016,
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
    ##                  post.mean l-95% CI u-95% CI eff.samp   pMCMC   
    ## (Intercept)        29.8133  13.8119  44.5839     4950 0.00889 **
    ## WMGTAWD            -3.7691  -9.7168   1.9060     4950 0.20566   
    ## NRTEN180            9.1849   3.1309  15.1209     4950 0.00364 **
    ## WMGTAWD:NRTEN180   -8.9841 -16.9150  -0.2855     4950 0.03515 * 
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
                          data = AUDPS_2016,
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
    ##                  post.mean l-95% CI u-95% CI eff.samp   pMCMC   
    ## (Intercept)         1.0813   0.1083   1.9908     4950 0.03798 * 
    ## WMGTAWD             0.4898   0.1756   0.7850     4950 0.00202 **
    ## NRTEN180            0.4911   0.1921   0.8120     5330 0.00323 **
    ## WMGTAWD:NRTEN180   -0.7344  -1.1703  -0.2976     4950 0.00242 **
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

### Model Fit

The models all appear to be good fits.

None of the diagnostic plots show any signs of auto-correlation or any other obvious patterns.

The random effects all appear to be acceptable, the dotted line stays near to the solid line with no discernible patterns.

Conclusions
-----------

### Tiller Sheath Blight Incidence

In 2015 the highest N rate, N120, was significantly different from the base, N0 rate.

In 2016 both the AWD and highest N rate, N180, were significantly different from the FLD and N60 treatments.

No interactions were found to be significant in the tiller sheath blight incidence.

Tiller Sheath Blight Severity
-----------------------------

In 2015 both the N100 and N120 rates were significantly different from the N0 treatment. The interaction of the N100 rate with AWD was also significantly different than N0 and FLD treatments.

In 2016 the N180 was significantly different from the N60 rate and the interaction of the N180 and AWD was significantly different than the N60 and FLD interaction.

### Leaf Sheath Blight Severity

In 2015 the N120 was significantly different than the N0 treatment. The interactions of both the N100 and N120 with AWD were significantly different than the N0 FLD interaction.

In 2016 the N180 was significantly different from the N60 treatment. The AWD was significantly different from the FLD and the interaction of N180:AWD was significantly different from the N60:FLD treatments interaction.

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
    ##  date     2017-08-01

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
    ##  withr             2.0.0   2017-07-28 cran (@2.0.0) 
    ##  xml2              1.1.1   2017-01-24 CRAN (R 3.4.1)
    ##  yaml              2.1.14  2016-11-12 CRAN (R 3.4.1)
