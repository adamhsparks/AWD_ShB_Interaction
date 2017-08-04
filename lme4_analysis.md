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
    ## REML criterion at convergence: 332.7
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.14147 -0.65649 -0.06548  0.77720  1.79059 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 0.1403   0.3746  
    ##  Residual             0.8691   0.9323  
    ## Number of obs: 120, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   1.1446     0.2531   4.523
    ## WMGTAWD       0.1245     0.1702   0.731
    ## NRTEN100      0.6234     0.2085   2.990
    ## NRTEN120      0.9998     0.2085   4.796

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
    ##  NRTE   lsmean        SE   df  lower.CL upper.CL
    ##  N0   1.206870 0.2383566 5.39 0.3942512 2.019489
    ##  N100 1.830222 0.2383566 5.39 1.0176029 2.642841
    ##  N120 2.206678 0.2383566 5.39 1.3940586 3.019297
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast      estimate        SE  df t.ratio p.value
    ##  N0 - N100   -0.6233517 0.2084604 113  -2.990  0.0095
    ##  N0 - N120   -0.9998074 0.2084604 113  -4.796  <.0001
    ##  N100 - N120 -0.3764557 0.2084604 113  -1.806  0.1723
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
print(lsmeans(TShB_inc_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean       SE   df  lower.CL upper.CL
    ##  FLD  1.685683 0.222646 4.11 0.9211629 2.450204
    ##  AWD  1.810163 0.222646 4.11 1.0456425 2.574683
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE  df t.ratio p.value
    ##  FLD - AWD -0.1244796 0.1702072 113  -0.731  0.4661
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
    ## REML criterion at convergence: 941.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.19092 -0.61056  0.00989  0.82450  2.03307 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept)  10.14    3.184  
    ##  Residual             188.40   13.726  
    ## Number of obs: 120, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)         6.578      3.458   1.903
    ## WMGTAWD             4.293      4.341   0.989
    ## NRTEN100           10.372      4.341   2.389
    ## NRTEN120           17.292      4.341   3.984
    ## WMGTAWD:NRTEN100   13.290      6.138   2.165
    ## WMGTAWD:NRTEN120   -1.130      6.138  -0.184

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
    ##  NRTE    lsmean      SE  df   lower.CL upper.CL
    ##  N0    8.724826 2.69155 9.2  0.8951325 16.55452
    ##  N100 25.741319 2.69155 9.2 17.9116255 33.57101
    ##  N120 25.451389 2.69155 9.2 17.6216950 33.28108
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast       estimate       SE  df t.ratio p.value
    ##  N0 - N100   -17.0164931 3.069239 111  -5.544  <.0001
    ##  N0 - N120   -16.7265625 3.069239 111  -5.450  <.0001
    ##  N100 - N120   0.2899306 3.069239 111   0.094  0.9951
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
print(lsmeans(TShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean       SE   df  lower.CL upper.CL
    ##  FLD  15.79919 2.382101 5.71  8.639071 22.95931
    ##  AWD  24.14583 2.382101 5.71 16.985715 31.30595
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast   estimate       SE  df t.ratio p.value
    ##  FLD - AWD -8.346644 2.506023 111  -3.331  0.0012
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
    ## REML criterion at convergence: 559.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.61537 -0.77454 -0.08498  0.56493  1.97071 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 2.138    1.462   
    ##  Residual             6.030    2.456   
    ## Number of obs: 120, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   0.7744     0.8576   0.903
    ## WMGTAWD       1.0254     0.4483   2.287
    ## NRTEN100      3.0214     0.5491   5.502
    ## NRTEN120      3.1416     0.5491   5.721

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
    ##  NRTE   lsmean        SE   df  lower.CL upper.CL
    ##  N0   1.287037 0.8278187 4.12 -1.921804 4.495879
    ##  N100 4.308449 0.8278187 4.12  1.099608 7.517291
    ##  N120 4.428675 0.8278187 4.12  1.219833 7.637516
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 3 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast      estimate        SE  df t.ratio p.value
    ##  N0 - N100   -3.0214120 0.5490981 113  -5.502  <.0001
    ##  N0 - N120   -3.1416377 0.5490981 113  -5.721  <.0001
    ##  N100 - N120 -0.1202257 0.5490981 113  -0.219  0.9739
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## P value adjustment: tukey method for comparing a family of 3 estimates

``` r
print(lsmeans(LShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean        SE   df   lower.CL upper.CL
    ##  FLD  2.828704 0.7968892 3.54 -0.1404613 5.797869
    ##  AWD  3.854070 0.7968892 3.54  0.8849052 6.823235
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast   estimate        SE  df t.ratio p.value
    ##  FLD - AWD -1.025367 0.4483367 113  -2.287  0.0241
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
    ## REML criterion at convergence: 187.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.55989 -0.75563 -0.06021  0.36365  1.93224 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 0.8702   0.9328  
    ##  Residual             0.9448   0.9720  
    ## Number of obs: 64, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  10.5396     0.5117  20.597
    ## WMGTAWD       0.3705     0.2430   1.525
    ## NRTEN180      1.9599     0.2430   8.065

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
    ##  NRTE   lsmean        SE   df  lower.CL upper.CL
    ##  N60  10.72486 0.4970647 3.39  8.824955 12.62476
    ##  N180 12.68479 0.4970647 3.39 10.784891 14.58469
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE df t.ratio p.value
    ##  N60 - N180 -1.959936 0.2430079 58  -8.065  <.0001
    ## 
    ## Results are averaged over the levels of: WMGT

``` r
print(lsmeans(TShB_inc_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean        SE   df lower.CL upper.CL
    ##  FLD  11.51958 0.4970647 3.39 9.619678 13.41948
    ##  AWD  11.89007 0.4970647 3.39 9.990168 13.78997
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE df t.ratio p.value
    ##  FLD - AWD -0.3704899 0.2430079 58  -1.525  0.1328
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
    ## -1.4025 -0.6570 -0.1617  0.5317  2.2969 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 85.31    9.237   
    ##  Residual             74.87    8.653   
    ## Number of obs: 64, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   32.023      4.984   6.425
    ## WMGTAWD       -8.240      2.163  -3.809
    ## NRTEN180       4.740      2.163   2.191

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
    ##  NRTE   lsmean       SE   df  lower.CL upper.CL
    ##  N60  27.90278 4.865025 3.32  9.047491 46.75806
    ##  N180 32.64236 4.865025 3.32 13.787074 51.49765
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate       SE df t.ratio p.value
    ##  N60 - N180 -4.739583 2.163256 58  -2.191  0.0325
    ## 
    ## Results are averaged over the levels of: WMGT

``` r
print(lsmeans(TShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean       SE   df  lower.CL upper.CL
    ##  FLD  34.39236 4.865025 3.32 15.537074 53.24765
    ##  AWD  26.15278 4.865025 3.32  7.297491 45.00806
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast  estimate       SE df t.ratio p.value
    ##  FLD - AWD 8.239583 2.163256 58   3.809  0.0003
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
    ## REML criterion at convergence: 103.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9490 -0.3602  0.2950  0.5796  1.6382 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  REP      (Intercept) 0.1354   0.3680  
    ##  Residual             0.2435   0.4934  
    ## Number of obs: 64, groups:  REP, 4
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)   1.2426     0.2128   5.840
    ## WMGTAWD       0.1317     0.1234   1.067
    ## NRTEN180      0.1560     0.1234   1.264

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
    ##  NRTE   lsmean        SE   df  lower.CL upper.CL
    ##  N60  1.308449 0.2036391 3.63 0.5617131 2.055185
    ##  N180 1.464410 0.2036391 3.63 0.7176738 2.211146
    ## 
    ## Results are averaged over the levels of: WMGT 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast     estimate        SE df t.ratio p.value
    ##  N60 - N180 -0.1559606 0.1233561 58  -1.264  0.2112
    ## 
    ## Results are averaged over the levels of: WMGT

``` r
print(lsmeans(LShB_sev_AUDPS, list(pairwise ~ WMGT)), adjust = c("tukey"))
```

    ## $`lsmeans of WMGT`
    ##  WMGT   lsmean        SE   df  lower.CL upper.CL
    ##  FLD  1.320602 0.2036391 3.63 0.5738659 2.067338
    ##  AWD  1.452257 0.2036391 3.63 0.7055210 2.198993
    ## 
    ## Results are averaged over the levels of: NRTE 
    ## Degrees-of-freedom method: satterthwaite 
    ## Confidence level used: 0.95 
    ## Conf-level adjustment: sidak method for 2 estimates 
    ## 
    ## $`pairwise differences of contrast`
    ##  contrast    estimate        SE df t.ratio p.value
    ##  FLD - AWD -0.1316551 0.1233561 58  -1.067  0.2903
    ## 
    ## Results are averaged over the levels of: NRTE

Conclusions
-----------

R Session Info
--------------

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.4.1 (2017-06-30)
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_AU.UTF-8                 
    ##  tz       Australia/Brisbane          
    ##  date     2017-08-05

    ## Packages -----------------------------------------------------------------

    ##  package         * version    date       source                       
    ##  acepack           1.4.1      2016-10-29 CRAN (R 3.4.0)               
    ##  agricolae       * 1.2-5      2017-07-26 CRAN (R 3.4.1)               
    ##  AlgDesign         1.1-7.3    2014-10-15 CRAN (R 3.4.0)               
    ##  ape             * 4.1        2017-02-14 CRAN (R 3.4.0)               
    ##  assertthat        0.2.0      2017-04-11 CRAN (R 3.4.1)               
    ##  backports         1.1.0      2017-05-22 CRAN (R 3.4.1)               
    ##  base            * 3.4.1      2017-07-07 local                        
    ##  base64enc         0.1-3      2015-07-28 CRAN (R 3.4.1)               
    ##  bindr             0.1        2016-11-13 CRAN (R 3.4.1)               
    ##  bindrcpp        * 0.2        2017-06-17 CRAN (R 3.4.1)               
    ##  bitops            1.0-6      2013-08-17 CRAN (R 3.4.1)               
    ##  boot              1.3-19     2017-04-21 CRAN (R 3.4.1)               
    ##  broom             0.4.2      2017-02-13 CRAN (R 3.4.1)               
    ##  car             * 2.1-5      2017-07-04 CRAN (R 3.4.1)               
    ##  caTools           1.17.1     2014-09-10 CRAN (R 3.4.1)               
    ##  cellranger        1.1.0      2016-07-27 CRAN (R 3.4.1)               
    ##  checkmate         1.8.3      2017-07-03 CRAN (R 3.4.1)               
    ##  cluster           2.0.6      2017-03-10 CRAN (R 3.4.1)               
    ##  coda            * 0.19-1     2016-12-08 CRAN (R 3.4.0)               
    ##  codetools         0.2-15     2016-10-05 CRAN (R 3.4.1)               
    ##  colorspace        1.3-2      2016-12-14 CRAN (R 3.4.1)               
    ##  combinat          0.0-8      2012-10-29 CRAN (R 3.4.0)               
    ##  compiler          3.4.1      2017-07-07 local                        
    ##  corpcor           1.6.9      2017-04-01 CRAN (R 3.4.0)               
    ##  cubature          1.3-11     2017-07-19 CRAN (R 3.4.1)               
    ##  data.table        1.10.4     2017-02-01 CRAN (R 3.4.0)               
    ##  datasets        * 3.4.1      2017-07-07 local                        
    ##  deldir            0.1-14     2017-04-22 CRAN (R 3.4.0)               
    ##  devtools          1.13.3     2017-08-02 cran (@1.13.3)               
    ##  digest            0.6.12     2017-01-27 CRAN (R 3.4.1)               
    ##  dplyr           * 0.7.2      2017-07-20 CRAN (R 3.4.1)               
    ##  estimability    * 1.2        2016-11-19 CRAN (R 3.4.0)               
    ##  evaluate          0.10.1     2017-06-24 CRAN (R 3.4.1)               
    ##  expm              0.999-2    2017-03-29 CRAN (R 3.4.0)               
    ##  fitdistrplus    * 1.0-9      2017-03-24 CRAN (R 3.4.0)               
    ##  forcats           0.2.0      2017-01-23 CRAN (R 3.4.1)               
    ##  foreign           0.8-69     2017-06-21 CRAN (R 3.4.1)               
    ##  Formula           1.2-2      2017-07-10 CRAN (R 3.4.1)               
    ##  gdata             2.18.0     2017-06-06 CRAN (R 3.4.0)               
    ##  ggplot2         * 2.2.1      2016-12-30 CRAN (R 3.4.1)               
    ##  glue              1.1.1      2017-06-21 CRAN (R 3.4.1)               
    ##  gmodels           2.16.2     2015-07-22 CRAN (R 3.4.0)               
    ##  gplots            3.0.1      2016-03-30 CRAN (R 3.4.0)               
    ##  graphics        * 3.4.1      2017-07-07 local                        
    ##  grDevices       * 3.4.1      2017-07-07 local                        
    ##  grid              3.4.1      2017-07-07 local                        
    ##  gridExtra         2.2.1      2016-02-29 CRAN (R 3.4.1)               
    ##  gtable            0.2.0      2016-02-26 CRAN (R 3.4.1)               
    ##  gtools            3.5.0      2015-05-29 CRAN (R 3.4.0)               
    ##  haven             1.1.0      2017-07-09 CRAN (R 3.4.1)               
    ##  Hmisc             4.0-3      2017-05-02 CRAN (R 3.4.0)               
    ##  hms               0.3        2016-11-22 CRAN (R 3.4.1)               
    ##  htmlTable         1.9        2017-01-26 CRAN (R 3.4.0)               
    ##  htmltools         0.3.6      2017-04-28 CRAN (R 3.4.1)               
    ##  htmlwidgets       0.9        2017-07-10 CRAN (R 3.4.1)               
    ##  httr              1.2.1      2016-07-03 CRAN (R 3.4.1)               
    ##  jsonlite          1.5        2017-06-01 CRAN (R 3.4.1)               
    ##  KernSmooth        2.23-15    2015-06-29 CRAN (R 3.4.1)               
    ##  klaR              0.6-12     2014-08-06 CRAN (R 3.4.0)               
    ##  knitr             1.16       2017-05-18 CRAN (R 3.4.1)               
    ##  labeling          0.3        2014-08-23 CRAN (R 3.4.1)               
    ##  lattice         * 0.20-35    2017-03-25 CRAN (R 3.4.1)               
    ##  latticeExtra      0.6-28     2016-02-09 CRAN (R 3.4.0)               
    ##  lazyeval          0.2.0      2016-06-12 CRAN (R 3.4.1)               
    ##  LearnBayes        2.15       2014-05-29 CRAN (R 3.4.0)               
    ##  lme4            * 1.1-13     2017-04-19 CRAN (R 3.4.0)               
    ##  lmerTest          2.0-33     2016-12-03 CRAN (R 3.4.0)               
    ##  lsmeans         * 2.26-3     2017-05-09 CRAN (R 3.4.0)               
    ##  lubridate       * 1.6.0      2016-09-13 CRAN (R 3.4.1)               
    ##  magrittr          1.5        2014-11-22 CRAN (R 3.4.1)               
    ##  MASS            * 7.3-47     2017-04-21 CRAN (R 3.4.1)               
    ##  Matrix          * 1.2-10     2017-05-03 CRAN (R 3.4.1)               
    ##  MatrixModels      0.4-1      2015-08-22 CRAN (R 3.4.0)               
    ##  MCMCglmm        * 2.24       2016-11-14 CRAN (R 3.4.0)               
    ##  memoise           1.1.0      2017-04-21 CRAN (R 3.4.1)               
    ##  methods         * 3.4.1      2017-07-07 local                        
    ##  mgcv              1.8-17     2017-02-08 CRAN (R 3.4.0)               
    ##  minqa             1.2.4      2014-10-09 CRAN (R 3.4.0)               
    ##  mnormt            1.5-5      2016-10-15 CRAN (R 3.4.0)               
    ##  modelr            0.1.1      2017-07-24 CRAN (R 3.4.1)               
    ##  multcomp          1.4-6      2016-07-14 CRAN (R 3.4.0)               
    ##  munsell           0.4.3      2016-02-13 CRAN (R 3.4.1)               
    ##  mvtnorm           1.0-6      2017-03-02 CRAN (R 3.4.0)               
    ##  nlme              3.1-131    2017-02-06 CRAN (R 3.4.0)               
    ##  nloptr            1.0.4      2014-08-04 CRAN (R 3.4.0)               
    ##  nnet              7.3-12     2016-02-02 CRAN (R 3.4.1)               
    ##  parallel          3.4.1      2017-07-07 local                        
    ##  pbkrtest          0.4-7      2017-03-15 CRAN (R 3.4.0)               
    ##  pkgconfig         2.0.1      2017-03-21 CRAN (R 3.4.1)               
    ##  plotMCMC        * 2.0-0      2014-03-12 CRAN (R 3.4.0)               
    ##  plyr              1.8.4      2016-06-08 CRAN (R 3.4.1)               
    ##  ProjectTemplate * 0.7        2016-08-11 CRAN (R 3.4.0)               
    ##  psych             1.7.5      2017-05-03 CRAN (R 3.4.1)               
    ##  purrr           * 0.2.3      2017-08-02 cran (@0.2.3)                
    ##  quantreg          5.33       2017-04-18 CRAN (R 3.4.0)               
    ##  R6                2.2.2      2017-06-17 CRAN (R 3.4.1)               
    ##  RColorBrewer      1.1-2      2014-12-07 CRAN (R 3.4.1)               
    ##  Rcpp              0.12.12    2017-07-15 CRAN (R 3.4.1)               
    ##  readr           * 1.1.1      2017-05-16 CRAN (R 3.4.1)               
    ##  readxl            1.0.0      2017-04-18 CRAN (R 3.4.1)               
    ##  reshape2        * 1.4.2      2016-10-22 CRAN (R 3.4.1)               
    ##  rlang             0.1.1.9000 2017-08-03 Github (hadley/rlang@c07ca73)
    ##  rmarkdown         1.6        2017-06-15 CRAN (R 3.4.1)               
    ##  rpart             4.1-11     2017-04-21 CRAN (R 3.4.1)               
    ##  rprojroot         1.2        2017-01-16 CRAN (R 3.4.1)               
    ##  rvest             0.3.2      2016-06-17 CRAN (R 3.4.1)               
    ##  sandwich          2.4-0      2017-07-26 CRAN (R 3.4.1)               
    ##  scales            0.4.1      2016-11-09 CRAN (R 3.4.1)               
    ##  sp                1.2-5      2017-06-29 CRAN (R 3.4.1)               
    ##  SparseM           1.77       2017-04-23 CRAN (R 3.4.0)               
    ##  spdep             0.6-13     2017-04-25 CRAN (R 3.4.0)               
    ##  splines           3.4.1      2017-07-07 local                        
    ##  stats           * 3.4.1      2017-07-07 local                        
    ##  stringi           1.1.5      2017-04-07 CRAN (R 3.4.1)               
    ##  stringr           1.2.0      2017-02-18 CRAN (R 3.4.1)               
    ##  survival        * 2.41-3     2017-04-04 CRAN (R 3.4.1)               
    ##  tensorA           0.36       2010-12-01 CRAN (R 3.4.0)               
    ##  TH.data           1.0-8      2017-01-23 CRAN (R 3.4.0)               
    ##  tibble          * 1.3.3      2017-05-28 CRAN (R 3.4.1)               
    ##  tidyr           * 0.6.3      2017-05-15 CRAN (R 3.4.1)               
    ##  tidyverse       * 1.1.1      2017-01-27 CRAN (R 3.4.1)               
    ##  tools             3.4.1      2017-07-07 local                        
    ##  utils           * 3.4.1      2017-07-07 local                        
    ##  withr             2.0.0      2017-07-28 cran (@2.0.0)                
    ##  xml2              1.1.1      2017-01-24 CRAN (R 3.4.1)               
    ##  xtable            1.8-2      2016-02-05 CRAN (R 3.4.1)               
    ##  yaml              2.1.14     2016-11-12 CRAN (R 3.4.1)               
    ##  zoo               1.8-0      2017-04-12 CRAN (R 3.4.0)
