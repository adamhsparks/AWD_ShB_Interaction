Analysis of split plot design using lme4
================

Notes on This Analysis Before Starting
--------------------------------------

The base levels for this analysis are:

-   2015 `NRTE:0`

-   2015 `WMGT:FLD`

-   2016 `NRTE:60`

-   2016 `WMGT:FLD`

#### Changes between 2015 and 2016

Before the analysis, note that due to changes between the years, the analysis must be carried out on each year separately. The 2015 data and 2016 data cannot be combined due to changes in inoculation methods; there are other changes too, but the main one is this. Therefore, the analyses will be conducted separately such that comparisons will only be observational and cannot be statistically compared.

Setup
-----

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
library(lme4)
library(lsmeans)
```

    ## Loading required package: estimability

``` r
TShB_inc_AUDPS <- lmer(TShB_inc_AUDPS ~ WMGT + NRTE + (1|REP), data = AUDPS_2015)
print(summary(TShB_inc_AUDPS), correlation = FALSE)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_inc_AUDPS ~ WMGT + NRTE + (1 | REP)
    ##    Data: AUDPS_2015
    ## 
    ## REML criterion at convergence: 329.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.19625 -0.67683 -0.06657  0.82647  1.78033 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 0.1473   0.3838  
    ##  Residual             0.8425   0.9179  
    ## Number of obs: 120, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   1.1192     0.2548   4.393
    ## WMGTAWD       0.1192     0.1676   0.711
    ## NRTEN100      0.6312     0.2052   3.076
    ## NRTEN120      1.0112     0.2052   4.927

``` r
plot(TShB_inc_AUDPS)
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-1.png)

``` r
qqnorm(residuals(TShB_inc_AUDPS))
qqline(residuals(TShB_inc_AUDPS))
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence-2.png)

``` r
print(lsmeans(TShB_inc_AUDPS, list(pairwise ~ NRTE)), adjust = c("tukey"))
```

    ## Loading required namespace: lmerTest

    ## $`lsmeans of NRTE`
    ##  NRTE  lsmean        SE   df  lower.CL upper.CL
    ##  N0   1.17875 0.2406108 5.21 0.3473807 2.010119
    ##  N100 1.81000 0.2406108 5.21 0.9786307 2.641369
    ##  N120 2.19000 0.2406108 5.21 1.3586307 3.021369
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE  df t.ratio p.value
    ##  N0 - N100   -0.63125 0.2052429 113  -3.076  0.0074
    ##  N0 - N120   -1.01125 0.2052429 113  -4.927  <.0001
    ##  N100 - N120 -0.38000 0.2052429 113  -1.851  0.1578
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
print(lsmeans(TShB_inc_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean      SE   df  lower.CL upper.CL
    ##  FLD  1.666667 0.22555 4.03 0.8848847 2.448449
    ##  AWD  1.785833 0.22555 4.03 1.0040513 2.567615
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE  df t.ratio p.value
    ##  FLD - AWD -0.1191667 0.1675801 113  -0.711  0.4785
    ## 
    ## Results are averaged over the levels of: NRTE

### 2015 Tiller Sheath Blight Severity Model

``` r
TShB_sev_AUDPS <- lmer(TShB_percent_AUDPS ~ WMGT * NRTE + (1|REP), data = AUDPS_2015)
print(summary(TShB_sev_AUDPS), correlation = FALSE)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_percent_AUDPS ~ WMGT * NRTE + (1 | REP)
    ##    Data: AUDPS_2015
    ## 
    ## REML criterion at convergence: 941.8
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.19204 -0.61005  0.01013  0.81810  2.03823 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept)  10.11    3.18   
    ##  Residual             188.79   13.74   
    ## Number of obs: 120, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)         6.573      3.459   1.900
    ## WMGTAWD             4.310      4.345   0.992
    ## NRTEN100           10.360      4.345   2.384
    ## NRTEN120           17.352      4.345   3.994
    ## WMGTAWD:NRTEN100   13.288      6.145   2.162
    ## WMGTAWD:NRTEN120   -1.237      6.145  -0.201

``` r
plot(TShB_sev_AUDPS)
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-1.png)

``` r
qqnorm(resid(TShB_sev_AUDPS))
qqline(resid(TShB_sev_AUDPS))
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2015_TShB_severity-2.png)

``` r
print(lsmeans(TShB_sev_AUDPS, list(pairwise ~ NRTE)), adjust = c("tukey"))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`lsmeans of NRTE`
    ##  NRTE   lsmean       SE   df   lower.CL upper.CL
    ##  N0    8.72750 2.692054 9.22  0.9004082 16.55459
    ##  N100 25.73125 2.692054 9.22 17.9041582 33.55834
    ##  N120 25.46125 2.692054 9.22 17.6341582 33.28834
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast     estimate       SE  df t.ratio p.value
    ##  N0 - N100   -17.00375 3.072351 111  -5.534  <.0001
    ##  N0 - N120   -16.73375 3.072351 111  -5.447  <.0001
    ##  N100 - N120   0.27000 3.072351 111   0.088  0.9958
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
print(lsmeans(TShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean       SE   df  lower.CL upper.CL
    ##  FLD  15.81000 2.382001 5.72  8.653931 22.96607
    ##  AWD  24.13667 2.382001 5.72 16.980598 31.29274
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast   estimate       SE  df t.ratio p.value
    ##  FLD - AWD -8.326667 2.508564 111  -3.319  0.0012
    ## 
    ## Results are averaged over the levels of: NRTE

### 2015 Leaf Sheath Blight Severity Model

``` r
LShB_sev_AUDPS <- lmer(LShB_percent_AUDPS ~ WMGT + NRTE + (1|REP), data = AUDPS_2015)
print(summary(LShB_sev_AUDPS), correlation = FALSE)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: LShB_percent_AUDPS ~ WMGT + NRTE + (1 | REP)
    ##    Data: AUDPS_2015
    ## 
    ## REML criterion at convergence: 557.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.61615 -0.77158 -0.09009  0.57730  1.96828 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 2.168    1.473   
    ##  Residual             5.930    2.435   
    ## Number of obs: 120, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   0.7762     0.8601   0.903
    ## WMGTAWD       1.0275     0.4446   2.311
    ## NRTEN100      2.9838     0.5445   5.479
    ## NRTEN120      3.1250     0.5445   5.739

``` r
plot(LShB_sev_AUDPS)
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-1.png)

``` r
qqnorm(resid(LShB_sev_AUDPS))
qqline(resid(LShB_sev_AUDPS))
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2015_LShB_severity-2.png)

``` r
print(lsmeans(LShB_sev_AUDPS, list(pairwise ~ NRTE)), adjust = c("tukey"))
```

    ## $`lsmeans of NRTE`
    ##  NRTE  lsmean        SE   df  lower.CL upper.CL
    ##  N0   1.29000 0.8308572 4.08 -1.945382 4.525382
    ##  N100 4.27375 0.8308572 4.08  1.038368 7.509132
    ##  N120 4.41500 0.8308572 4.08  1.179618 7.650382
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE  df t.ratio p.value
    ##  N0 - N100   -2.98375 0.5445356 113  -5.479  <.0001
    ##  N0 - N120   -3.12500 0.5445356 113  -5.739  <.0001
    ##  N100 - N120 -0.14125 0.5445356 113  -0.259  0.9636
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
print(lsmeans(LShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT lsmean        SE   df   lower.CL upper.CL
    ##  FLD  2.8125 0.8005647 3.52 -0.1781747 5.803175
    ##  AWD  3.8400 0.8005647 3.52  0.8493253 6.830675
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast  estimate        SE  df t.ratio p.value
    ##  FLD - AWD  -1.0275 0.4446115 113  -2.311  0.0226
    ## 
    ## Results are averaged over the levels of: NRTE

------------------------------------------------------------------------

2016
----

### 2016 Tiller Sheath Blight Incidence Model

``` r
TShB_inc_AUDPS <- lmer(TShB_inc_AUDPS ~ WMGT + NRTE + (1|REP), data = AUDPS_2016)
print(summary(TShB_inc_AUDPS), correlation = FALSE)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_inc_AUDPS ~ WMGT + NRTE + (1 | REP)
    ##    Data: AUDPS_2016
    ## 
    ## REML criterion at convergence: 189.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.56438 -0.74359 -0.04506  0.39341  1.95282 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 0.8560   0.9252  
    ##  Residual             0.9705   0.9852  
    ## Number of obs: 64, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  10.5525     0.5094  20.715
    ## WMGTAWD       0.3850     0.2463   1.563
    ## NRTEN180      1.9250     0.2463   7.816

``` r
plot(TShB_inc_AUDPS)
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-1.png)

``` r
qqnorm(resid(TShB_inc_AUDPS))
qqline(resid(TShB_inc_AUDPS))
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_incidence-2.png)

``` r
print(lsmeans(TShB_inc_AUDPS, list(pairwise ~ NRTE)), adjust = c("tukey"))
```

    ## $`lsmeans of NRTE`
    ##  NRTE lsmean        SE   df  lower.CL upper.CL
    ##  N60  10.745 0.4943093 3.41  8.861432 12.62857
    ##  N180 12.670 0.4943093 3.41 10.786432 14.55357
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast   estimate        SE df t.ratio p.value
    ##  N60 - N180   -1.925 0.2462897 58  -7.816  <.0001
    ## 
    ## Results are averaged over the levels of: WMGT

``` r
print(lsmeans(TShB_inc_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT lsmean        SE   df  lower.CL upper.CL
    ##  FLD  11.515 0.4943093 3.41  9.631432 13.39857
    ##  AWD  11.900 0.4943093 3.41 10.016432 13.78357
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast  estimate        SE df t.ratio p.value
    ##  FLD - AWD   -0.385 0.2462897 58  -1.563  0.1234
    ## 
    ## Results are averaged over the levels of: NRTE

### 2016 Tiller Sheath Blight Severity Model

``` r
TShB_sev_AUDPS <- lmer(TShB_percent_AUDPS ~ WMGT + NRTE + (1|REP), data = AUDPS_2016)
print(summary(TShB_sev_AUDPS), correlation = FALSE)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_percent_AUDPS ~ WMGT + NRTE + (1 | REP)
    ##    Data: AUDPS_2016
    ## 
    ## REML criterion at convergence: 454.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4109 -0.6544 -0.1604  0.5292  2.3016 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 85.57    9.250   
    ##  Residual             74.74    8.645   
    ## Number of obs: 64, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   32.008      4.989   6.415
    ## WMGTAWD       -8.225      2.161  -3.806
    ## NRTEN180       4.725      2.161   2.186

``` r
plot(TShB_sev_AUDPS)
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-1.png)

``` r
qqnorm(resid(TShB_sev_AUDPS))
qqline(resid(TShB_sev_AUDPS))
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2016_TShB_severity-2.png)

``` r
print(lsmeans(TShB_sev_AUDPS, list(pairwise ~ NRTE)), adjust = c("tukey"))
```

    ## $`lsmeans of NRTE`
    ##  NRTE lsmean       SE   df  lower.CL upper.CL
    ##  N60  27.895 4.871043 3.32  9.010953 46.77905
    ##  N180 32.620 4.871043 3.32 13.735953 51.50405
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast   estimate       SE df t.ratio p.value
    ##  N60 - N180   -4.725 2.161262 58  -2.186  0.0328
    ## 
    ## Results are averaged over the levels of: WMGT

``` r
print(lsmeans(TShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT lsmean       SE   df  lower.CL upper.CL
    ##  FLD  34.370 4.871043 3.32 15.485953 53.25405
    ##  AWD  26.145 4.871043 3.32  7.260953 45.02905
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast  estimate       SE df t.ratio p.value
    ##  FLD - AWD    8.225 2.161262 58   3.806  0.0003
    ## 
    ## Results are averaged over the levels of: NRTE

### 2016 Leaf Sheath Blight Severity Model

``` r
LShB_sev_AUDPS <- lmer(LShB_percent_AUDPS ~ WMGT + NRTE + (1|REP), data = AUDPS_2016)
print(summary(LShB_sev_AUDPS), correlation = FALSE)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: LShB_percent_AUDPS ~ WMGT + NRTE + (1 | REP)
    ##    Data: AUDPS_2016
    ## 
    ## REML criterion at convergence: 99
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9351 -0.2866  0.2756  0.5318  1.6369 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 0.1291   0.3593  
    ##  Residual             0.2259   0.4753  
    ## Number of obs: 64, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   1.2688     0.2070   6.128
    ## WMGTAWD       0.1225     0.1188   1.031
    ## NRTEN180      0.1225     0.1188   1.031

``` r
plot(LShB_sev_AUDPS)
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-1.png)

``` r
qqnorm(resid(LShB_sev_AUDPS))
qqline(resid(LShB_sev_AUDPS))
```

![](lme4_analysis_files/figure-markdown_github-ascii_identifiers/2016_LShB_severity-2.png)

``` r
print(lsmeans(LShB_sev_AUDPS, list(pairwise ~ NRTE)), adjust = c("tukey"))
```

    ## $`lsmeans of NRTE`
    ##  NRTE lsmean        SE   df  lower.CL upper.CL
    ##  N60  1.3300 0.1983319 3.62 0.6008858 2.059114
    ##  N180 1.4525 0.1983319 3.62 0.7233858 2.181614
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast   estimate        SE df t.ratio p.value
    ##  N60 - N180  -0.1225 0.1188242 58  -1.031  0.3069
    ## 
    ## Results are averaged over the levels of: WMGT

``` r
print(lsmeans(LShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT lsmean        SE   df  lower.CL upper.CL
    ##  FLD  1.3300 0.1983319 3.62 0.6008858 2.059114
    ##  AWD  1.4525 0.1983319 3.62 0.7233858 2.181614
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast  estimate        SE df t.ratio p.value
    ##  FLD - AWD  -0.1225 0.1188242 58  -1.031  0.3069
    ## 
    ## Results are averaged over the levels of: NRTE

Conclusions
-----------

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
    ##  date     2017-08-04

    ## Packages -----------------------------------------------------------------

    ##  package         * version    date       source                          
    ##  acepack           1.4.1      2016-10-29 CRAN (R 3.4.1)                  
    ##  agricolae       * 1.2-5      2017-07-26 cran (@1.2-5)                   
    ##  AlgDesign         1.1-7.3    2014-10-15 CRAN (R 3.4.1)                  
    ##  ape             * 4.1        2017-02-14 CRAN (R 3.4.1)                  
    ##  assertthat        0.2.0      2017-04-11 CRAN (R 3.4.1)                  
    ##  backports         1.1.0      2017-05-22 CRAN (R 3.4.1)                  
    ##  base            * 3.4.1      2017-07-25 local                           
    ##  base64enc         0.1-3      2015-07-28 CRAN (R 3.4.1)                  
    ##  bindr             0.1        2016-11-13 CRAN (R 3.4.1)                  
    ##  bindrcpp        * 0.2        2017-06-17 CRAN (R 3.4.1)                  
    ##  bitops            1.0-6      2013-08-17 CRAN (R 3.4.1)                  
    ##  boot              1.3-19     2017-02-11 CRAN (R 3.4.1)                  
    ##  broom             0.4.2      2017-02-13 CRAN (R 3.4.1)                  
    ##  car             * 2.1-5      2017-07-04 CRAN (R 3.4.1)                  
    ##  caTools           1.17.1     2014-09-10 CRAN (R 3.4.1)                  
    ##  cellranger        1.1.0      2016-07-27 CRAN (R 3.4.1)                  
    ##  checkmate         1.8.3      2017-07-03 CRAN (R 3.4.1)                  
    ##  cluster           2.0.6      2017-03-10 CRAN (R 3.4.1)                  
    ##  coda            * 0.19-1     2016-12-08 CRAN (R 3.4.1)                  
    ##  codetools         0.2-15     2016-10-05 CRAN (R 3.4.1)                  
    ##  colorspace        1.3-2      2016-12-14 CRAN (R 3.4.1)                  
    ##  combinat          0.0-8      2012-10-29 CRAN (R 3.4.1)                  
    ##  compiler          3.4.1      2017-07-25 local                           
    ##  corpcor           1.6.9      2017-04-01 CRAN (R 3.4.1)                  
    ##  cubature          1.3-11     2017-07-19 CRAN (R 3.4.1)                  
    ##  data.table        1.10.4     2017-02-01 CRAN (R 3.4.1)                  
    ##  datasets        * 3.4.1      2017-07-25 local                           
    ##  deldir            0.1-14     2017-04-22 CRAN (R 3.4.1)                  
    ##  devtools          1.13.3     2017-08-02 cran (@1.13.3)                  
    ##  digest            0.6.12     2017-01-27 CRAN (R 3.4.1)                  
    ##  dplyr           * 0.7.2      2017-07-20 CRAN (R 3.4.1)                  
    ##  estimability    * 1.2        2016-11-19 CRAN (R 3.4.1)                  
    ##  evaluate          0.10.1     2017-06-24 CRAN (R 3.4.1)                  
    ##  expm              0.999-2    2017-03-29 CRAN (R 3.4.1)                  
    ##  fitdistrplus    * 1.0-9      2017-03-24 CRAN (R 3.4.1)                  
    ##  forcats           0.2.0      2017-01-23 CRAN (R 3.4.1)                  
    ##  foreign           0.8-69     2017-06-22 CRAN (R 3.4.1)                  
    ##  Formula           1.2-2      2017-07-10 CRAN (R 3.4.1)                  
    ##  gdata             2.18.0     2017-06-06 CRAN (R 3.4.1)                  
    ##  ggplot2         * 2.2.1      2016-12-30 CRAN (R 3.4.1)                  
    ##  glue              1.1.1      2017-06-21 CRAN (R 3.4.1)                  
    ##  gmodels           2.16.2     2015-07-22 CRAN (R 3.4.1)                  
    ##  gplots            3.0.1      2016-03-30 CRAN (R 3.4.1)                  
    ##  graphics        * 3.4.1      2017-07-25 local                           
    ##  grDevices       * 3.4.1      2017-07-25 local                           
    ##  grid              3.4.1      2017-07-25 local                           
    ##  gridExtra         2.2.1      2016-02-29 CRAN (R 3.4.1)                  
    ##  gtable            0.2.0      2016-02-26 CRAN (R 3.4.1)                  
    ##  gtools            3.5.0      2015-05-29 CRAN (R 3.4.1)                  
    ##  haven             1.1.0      2017-07-09 CRAN (R 3.4.1)                  
    ##  Hmisc             4.0-3      2017-05-02 CRAN (R 3.4.1)                  
    ##  hms               0.3        2016-11-22 CRAN (R 3.4.1)                  
    ##  htmlTable         1.9        2017-01-26 CRAN (R 3.4.1)                  
    ##  htmltools         0.3.6      2017-04-28 CRAN (R 3.4.1)                  
    ##  htmlwidgets       0.9        2017-07-10 CRAN (R 3.4.1)                  
    ##  httr              1.2.1      2016-07-03 CRAN (R 3.4.1)                  
    ##  jsonlite          1.5        2017-06-01 CRAN (R 3.4.1)                  
    ##  KernSmooth        2.23-15    2015-06-29 CRAN (R 3.4.1)                  
    ##  klaR              0.6-12     2014-08-06 CRAN (R 3.4.1)                  
    ##  knitr             1.16       2017-05-18 CRAN (R 3.4.1)                  
    ##  labeling          0.3        2014-08-23 CRAN (R 3.4.1)                  
    ##  lattice         * 0.20-35    2017-03-25 CRAN (R 3.4.1)                  
    ##  latticeExtra      0.6-28     2016-02-09 CRAN (R 3.4.1)                  
    ##  lazyeval          0.2.0      2016-06-12 CRAN (R 3.4.1)                  
    ##  LearnBayes        2.15       2014-05-29 CRAN (R 3.4.1)                  
    ##  lme4            * 1.1-13     2017-04-19 CRAN (R 3.4.1)                  
    ##  lmerTest          2.0-33     2016-12-03 CRAN (R 3.4.1)                  
    ##  lsmeans         * 2.26-3     2017-05-09 CRAN (R 3.4.1)                  
    ##  lubridate       * 1.6.0      2016-09-13 CRAN (R 3.4.1)                  
    ##  magrittr          1.5        2014-11-22 CRAN (R 3.4.1)                  
    ##  MASS            * 7.3-47     2017-02-26 CRAN (R 3.4.1)                  
    ##  Matrix          * 1.2-10     2017-05-03 CRAN (R 3.4.1)                  
    ##  MatrixModels      0.4-1      2015-08-22 CRAN (R 3.4.1)                  
    ##  MCMCglmm        * 2.24       2016-11-14 CRAN (R 3.4.1)                  
    ##  memoise           1.1.0      2017-04-21 CRAN (R 3.4.1)                  
    ##  methods         * 3.4.1      2017-07-25 local                           
    ##  mgcv              1.8-17     2017-02-08 CRAN (R 3.4.1)                  
    ##  minqa             1.2.4      2014-10-09 CRAN (R 3.4.1)                  
    ##  mnormt            1.5-5      2016-10-15 CRAN (R 3.4.1)                  
    ##  modelr            0.1.1      2017-07-24 CRAN (R 3.4.1)                  
    ##  multcomp          1.4-6      2016-07-14 CRAN (R 3.4.1)                  
    ##  munsell           0.4.3      2016-02-13 CRAN (R 3.4.1)                  
    ##  mvtnorm           1.0-6      2017-03-02 CRAN (R 3.4.1)                  
    ##  nlme              3.1-131    2017-02-06 CRAN (R 3.4.1)                  
    ##  nloptr            1.0.4      2014-08-04 CRAN (R 3.4.1)                  
    ##  nnet              7.3-12     2016-02-02 CRAN (R 3.4.1)                  
    ##  parallel          3.4.1      2017-07-25 local                           
    ##  pbkrtest          0.4-7      2017-03-15 CRAN (R 3.4.1)                  
    ##  pkgconfig         2.0.1      2017-03-21 CRAN (R 3.4.1)                  
    ##  plotMCMC        * 2.0-0      2014-03-12 CRAN (R 3.4.1)                  
    ##  plyr              1.8.4      2016-06-08 CRAN (R 3.4.1)                  
    ##  ProjectTemplate * 0.7        2016-08-11 CRAN (R 3.4.1)                  
    ##  psych             1.7.5      2017-05-03 CRAN (R 3.4.1)                  
    ##  purrr           * 0.2.3      2017-08-02 cran (@0.2.3)                   
    ##  quantreg          5.33       2017-04-18 CRAN (R 3.4.1)                  
    ##  R6                2.2.2      2017-06-17 CRAN (R 3.4.1)                  
    ##  RColorBrewer      1.1-2      2014-12-07 CRAN (R 3.4.1)                  
    ##  Rcpp              0.12.12    2017-07-15 CRAN (R 3.4.1)                  
    ##  readr           * 1.1.1      2017-05-16 CRAN (R 3.4.1)                  
    ##  readxl            1.0.0      2017-04-18 CRAN (R 3.4.1)                  
    ##  reshape2        * 1.4.2      2016-10-22 CRAN (R 3.4.1)                  
    ##  rlang             0.1.1.9000 2017-08-04 Github (tidyverse/rlang@c07ca73)
    ##  rmarkdown         1.6        2017-06-15 CRAN (R 3.4.1)                  
    ##  rpart             4.1-11     2017-03-13 CRAN (R 3.4.1)                  
    ##  rprojroot         1.2        2017-01-16 CRAN (R 3.4.1)                  
    ##  rvest             0.3.2      2016-06-17 CRAN (R 3.4.1)                  
    ##  sandwich          2.4-0      2017-07-26 cran (@2.4-0)                   
    ##  scales            0.4.1      2016-11-09 CRAN (R 3.4.1)                  
    ##  sp                1.2-5      2017-06-29 CRAN (R 3.4.1)                  
    ##  SparseM           1.77       2017-04-23 CRAN (R 3.4.1)                  
    ##  spdep             0.6-13     2017-04-25 CRAN (R 3.4.1)                  
    ##  splines           3.4.1      2017-07-25 local                           
    ##  stats           * 3.4.1      2017-07-25 local                           
    ##  stringi           1.1.5      2017-04-07 CRAN (R 3.4.1)                  
    ##  stringr           1.2.0      2017-02-18 CRAN (R 3.4.1)                  
    ##  survival        * 2.41-3     2017-04-04 CRAN (R 3.4.1)                  
    ##  tensorA           0.36       2010-12-01 CRAN (R 3.4.1)                  
    ##  TH.data           1.0-8      2017-01-23 CRAN (R 3.4.1)                  
    ##  tibble          * 1.3.3      2017-05-28 CRAN (R 3.4.1)                  
    ##  tidyr           * 0.6.3      2017-05-15 CRAN (R 3.4.1)                  
    ##  tidyverse       * 1.1.1      2017-01-27 CRAN (R 3.4.1)                  
    ##  tools             3.4.1      2017-07-25 local                           
    ##  utils           * 3.4.1      2017-07-25 local                           
    ##  withr             2.0.0      2017-07-28 cran (@2.0.0)                   
    ##  xml2              1.1.1      2017-01-24 CRAN (R 3.4.1)                  
    ##  xtable            1.8-2      2016-02-05 CRAN (R 3.4.1)                  
    ##  yaml              2.1.14     2016-11-12 CRAN (R 3.4.1)                  
    ##  zoo               1.8-0      2017-04-12 CRAN (R 3.4.1)
