Analysis of split plot design using MCMCglmm
================

Notes on this analysis before starting
--------------------------------------

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

### 2015 Tiller Sheath Blight Incidence Model

``` r
# Separate the TRT column into the two treatments for analysis
AUDPS <- separate(data = AUDPS, col = TRT, into = c("WMGT", "NRTE"))

eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_incidence_lmm_2015 <- MCMCglmm(AUDPS ~ WMGT * NRTE,
                                    random = ~REP, 
                          data = as.data.frame(AUDPS[AUDPS$YEAR == 2015, ]),
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
    ##  DIC: 120.7347 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean l-95% CI u-95% CI eff.samp
    ## REP     2.678  9.2e-08    10.69     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     6.935    2.766    12.17     5367
    ## 
    ##  Location effects: AUDPS ~ WMGT * NRTE 
    ## 
    ##                  post.mean l-95% CI u-95% CI eff.samp  pMCMC  
    ## (Intercept)         2.3177  -0.5295   5.4005     4950 0.1176  
    ## WMGTFLD             0.5983  -2.9244   4.3426     4577 0.7543  
    ## NRTEN100            2.0712  -1.7951   5.6033     4950 0.2505  
    ## NRTEN120            3.1649  -0.6522   6.5576     5159 0.0885 .
    ## WMGTFLD:NRTEN100   -1.3002  -6.7856   3.6195     4950 0.6242  
    ## WMGTFLD:NRTEN120    0.2726  -4.7023   5.6104     4950 0.9055  
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
plot_joint_random_error_dist(d = rdf,
                             title = ("2015 Random Error Distribution for Tiller ShB Incidence"))
```

![](Analysis_files/figure-markdown_github/2015_TShB_incidence-6.png)

------------------------------------------------------------------------

2016
----

### 2016 Tiller Sheath Blight Incidence Model

``` r
eprior <- list(R = list(V = 1, nu = 0.02),
               G = list(G1 = list(V = 1, nu = 0.02, alpha.V = 1000)))
TShB_incidence_lmm_2016 <- MCMCglmm(AUDPS ~ WMGT * NRTE, 
                                    random = ~REP, 
                          data = as.data.frame(AUDPS[AUDPS$YEAR == 2016, ]),
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
    ##  DIC: 80.75692 
    ## 
    ##  G-structure:  ~REP
    ## 
    ##     post.mean  l-95% CI u-95% CI eff.samp
    ## REP     35.64 0.0002254    108.2     4950
    ## 
    ##  R-structure:  ~units
    ## 
    ##       post.mean l-95% CI u-95% CI eff.samp
    ## units     6.974    1.552    14.61     4950
    ## 
    ##  Location effects: AUDPS ~ WMGT * NRTE 
    ## 
    ##                 post.mean l-95% CI u-95% CI eff.samp    pMCMC    
    ## (Intercept)       48.5942  42.6155  54.5426     5023 0.000404 ***
    ## WMGTFLD            1.7139  -2.3394   5.3820     4950 0.318384    
    ## NRTEN60           -4.9906  -8.6134  -1.1616     4850 0.011717 *  
    ## WMGTFLD:NRTEN60   -0.2142  -5.3626   5.3024     5090 0.926465    
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
plot_joint_random_error_dist(d = rdf,
                             title = ("2016 Random Error Distribution for Tiller ShB Incidence"))
```

![](Analysis_files/figure-markdown_github/2016_TShB_incidence-6.png)

Conclusions
===========

The models all appear to be good fits.

None of the diagnostic plots show any signs of autocorrelation or any other obvious patterns.

None of the treatments in either year, 2015 or 2016, were significant. The see `pMCMC` values and also posterior graphs. There is a large amount of overlap in all of the posterior graphs.

The random effects all appear to be acceptable, the dotted line stays near to the solid line with no discernible patterns.

The random effects are all fairly equally distributed except for 2016 tiller sheath blight severity where water management:replicate has a larger effect. This is not surprising given that the plot and replicate sizes were different in 2016 due to the use of two fields in the IRRI experiment station, see the [2016 plot layout plan](https://github.com/adamhsparks/AWD_ShB_Interaction/blob/master/doc/Metadata/2016%20AWD%20ShB%20Metadata/ExptLayoutAWD2016.pdf).

R Session Info
==============

``` r
sessionInfo()
```

    ## R version 3.4.0 (2017-04-21)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: OS X El Capitan 10.11.6
    ## 
    ## Matrix products: default
    ## BLAS/LAPACK: /usr/local/Cellar/openblas/0.2.19/lib/libopenblasp-r0.2.19.dylib
    ## 
    ## locale:
    ## [1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] MCMCglmm_2.24       ape_4.1             coda_0.19-1        
    ##  [4] Matrix_1.2-9        viridis_0.4.0       viridisLite_0.2.0  
    ##  [7] lubridate_1.6.0     ggthemes_3.4.0      plyr_1.8.4         
    ## [10] dplyr_0.5.0         purrr_0.2.2         readr_1.1.0        
    ## [13] tidyr_0.6.1         tibble_1.3.0        ggplot2_2.2.1      
    ## [16] tidyverse_1.1.1     plotMCMC_2.0-0      lattice_0.20-35    
    ## [19] reshape2_1.4.2      agricolae_1.2-4     car_2.1-4          
    ## [22] fitdistrplus_1.0-9  survival_2.41-3     MASS_7.3-47        
    ## [25] ProjectTemplate_0.7
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-131       bitops_1.0-6       pbkrtest_0.4-7    
    ##  [4] gmodels_2.16.2     httr_1.2.1         rprojroot_1.2     
    ##  [7] tensorA_0.36       tools_3.4.0        backports_1.0.5   
    ## [10] R6_2.2.0           KernSmooth_2.23-15 AlgDesign_1.1-7.3 
    ## [13] DBI_0.6-1          lazyeval_0.2.0     mgcv_1.8-17       
    ## [16] colorspace_1.3-2   nnet_7.3-12        sp_1.2-4          
    ## [19] gridExtra_2.2.1    mnormt_1.5-5       klaR_0.6-12       
    ## [22] compiler_3.4.0     rvest_0.3.2        quantreg_5.33     
    ## [25] SparseM_1.77       expm_0.999-2       xml2_1.1.1        
    ## [28] labeling_0.3       caTools_1.17.1     scales_0.4.1      
    ## [31] psych_1.7.3.21     stringr_1.2.0      digest_0.6.12     
    ## [34] foreign_0.8-67     minqa_1.2.4        rmarkdown_1.4     
    ## [37] htmltools_0.3.5    lme4_1.1-13        readxl_1.0.0      
    ## [40] combinat_0.0-8     jsonlite_1.4       gtools_3.5.0      
    ## [43] spdep_0.6-12       magrittr_1.5       Rcpp_0.12.10      
    ## [46] munsell_0.4.3      stringi_1.1.5      yaml_2.1.14       
    ## [49] gplots_3.0.1       grid_3.4.0         parallel_3.4.0    
    ## [52] gdata_2.17.0       forcats_0.2.0      deldir_0.1-14     
    ## [55] haven_1.0.0        splines_3.4.0      hms_0.3           
    ## [58] knitr_1.15.1       cubature_1.3-6     boot_1.3-19       
    ## [61] corpcor_1.6.9      LearnBayes_2.15    packrat_0.4.8-1   
    ## [64] evaluate_0.10      modelr_0.1.0       nloptr_1.0.4      
    ## [67] MatrixModels_0.4-1 cellranger_1.1.0   gtable_0.2.0      
    ## [70] assertthat_0.2.0   broom_0.4.2        cluster_2.0.6
