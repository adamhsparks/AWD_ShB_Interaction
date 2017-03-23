2015 Analysis
================

2015
====

Check probability distribution fit
----------------------------------

``` r
AUDPS_2015$LShB_rating_t <- AUDPS_2015$LShB_rating + 1

qqp(AUDPS_2015$LShB_rating_t, "norm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
qqp(AUDPS_2015$LShB_rating_t, "lnorm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
poisson <- fitdistr(AUDPS_2015$LShB_rating_t, "Poisson")
qqp(AUDPS_2015$LShB_rating_t, "pois", poisson$estimate)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
gamma <- fitdistr(AUDPS_2015$LShB_rating_t, "gamma")
qqp(AUDPS_2015$LShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-1-4.png)

Check probability distribution fit for tiller sheath blight
-----------------------------------------------------------

``` r
AUDPS_2015$TShB_rating_t <- AUDPS_2015$TShB_rating + 1

qqp(AUDPS_2015$TShB_rating_t, "norm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
qqp(AUDPS_2015$TShB_rating_t, "lnorm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
poisson <- fitdistr(AUDPS_2015$TShB_rating_t, "Poisson")
qqp(AUDPS_2015$TShB_rating_t, "pois", poisson$estimate)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
gamma <- fitdistr(AUDPS_2015$TShB_rating_t, "gamma")
qqp(AUDPS_2015$TShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-4.png)

Leaf Sheath Blight Severity Model
---------------------------------

``` r
(LShB_lmm_2015 <- lmer(LShB_rating ~ WMGT * NRTE + (1 | REP/WMGT),
                       data = AUDPS_2015))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: LShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2015
    ## REML criterion at convergence: 26.3713
    ## Random effects:
    ##  Groups   Name        Std.Dev.
    ##  WMGT:REP (Intercept) 0.00000 
    ##  REP      (Intercept) 0.07698 
    ##  Residual             0.39269 
    ## Number of obs: 24, groups:  WMGT:REP, 8; REP, 4
    ## Fixed Effects:
    ##     (Intercept)          WMGTFLD          NRTE120          NRTE150  
    ##          0.1800           0.0150           0.3575           0.2925  
    ## WMGTFLD:NRTE120  WMGTFLD:NRTE150  
    ##         -0.3425          -0.1950

``` r
summary(LShB_lmm_2015)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: LShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2015
    ## 
    ## REML criterion at convergence: 26.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3315 -0.5788 -0.2199  0.5834  1.8644 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept) 0.000000 0.00000 
    ##  REP      (Intercept) 0.005926 0.07698 
    ##  Residual             0.154203 0.39269 
    ## Number of obs: 24, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)       0.1800     0.2001   0.900
    ## WMGTFLD           0.0150     0.2777   0.054
    ## NRTE120           0.3575     0.2777   1.288
    ## NRTE150           0.2925     0.2777   1.053
    ## WMGTFLD:NRTE120  -0.3425     0.3927  -0.872
    ## WMGTFLD:NRTE150  -0.1950     0.3927  -0.497
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) WMGTFLD NRTE12 NRTE15 WMGTFLD:NRTE12
    ## WMGTFLD        -0.694                                     
    ## NRTE120        -0.694  0.500                              
    ## NRTE150        -0.694  0.500   0.500                      
    ## WMGTFLD:NRTE12  0.491 -0.707  -0.707 -0.354               
    ## WMGTFLD:NRTE15  0.491 -0.707  -0.354 -0.707  0.500

``` r
plot(LShB_lmm_2015)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
qqnorm(resid(LShB_lmm_2015))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-2.png)

Tiller Sheath Blight Severity Model
-----------------------------------

``` r
(TShB_lmm_2015 <- lmer(TShB_rating ~ WMGT * NRTE + (1 | REP/WMGT),
                       data = AUDPS_2015))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2015
    ## REML criterion at convergence: 92.6044
    ## Random effects:
    ##  Groups   Name        Std.Dev.
    ##  WMGT:REP (Intercept) 0.000   
    ##  REP      (Intercept) 0.000   
    ##  Residual             2.515   
    ## Number of obs: 24, groups:  WMGT:REP, 8; REP, 4
    ## Fixed Effects:
    ##     (Intercept)          WMGTFLD          NRTE120          NRTE150  
    ##          1.5775          -0.5250           1.6525           1.9625  
    ## WMGTFLD:NRTE120  WMGTFLD:NRTE150  
    ##         -0.8425           0.2975

``` r
summary(TShB_lmm_2015)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2015
    ## 
    ## REML criterion at convergence: 92.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1132 -0.6739 -0.1879  0.4838  2.3964 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept) 0.000    0.000   
    ##  REP      (Intercept) 0.000    0.000   
    ##  Residual             6.326    2.515   
    ## Number of obs: 24, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)       1.5775     1.2576   1.254
    ## WMGTFLD          -0.5250     1.7785  -0.295
    ## NRTE120           1.6525     1.7785   0.929
    ## NRTE150           1.9625     1.7785   1.103
    ## WMGTFLD:NRTE120  -0.8425     2.5152  -0.335
    ## WMGTFLD:NRTE150   0.2975     2.5152   0.118
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) WMGTFLD NRTE12 NRTE15 WMGTFLD:NRTE12
    ## WMGTFLD        -0.707                                     
    ## NRTE120        -0.707  0.500                              
    ## NRTE150        -0.707  0.500   0.500                      
    ## WMGTFLD:NRTE12  0.500 -0.707  -0.707 -0.354               
    ## WMGTFLD:NRTE15  0.500 -0.707  -0.354 -0.707  0.500

``` r
plot(TShB_lmm_2015)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
qqnorm(resid(TShB_lmm_2015))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-2.png)

------------------------------------------------------------------------

2016
====

Check probability distribution fit for leaf sheath blight
---------------------------------------------------------

``` r
AUDPS_2016$LShB_rating_t <- AUDPS_2016$LShB_rating + 1

qqp(AUDPS_2016$LShB_rating_t, "norm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
qqp(AUDPS_2016$LShB_rating_t, "lnorm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-5-2.png)

``` r
poisson <- fitdistr(AUDPS_2016$LShB_rating_t, "Poisson")
qqp(AUDPS_2016$LShB_rating_t, "pois", poisson$estimate)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
gamma <- fitdistr(AUDPS_2016$LShB_rating_t, "gamma")
qqp(AUDPS_2016$LShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-5-4.png)

Check probability distribution fit for tiller sheath blight
-----------------------------------------------------------

``` r
AUDPS_2016$TShB_rating_t <- AUDPS_2016$TShB_rating + 1

qqp(AUDPS_2016$TShB_rating_t, "norm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
qqp(AUDPS_2016$TShB_rating_t, "lnorm")
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-6-2.png)

``` r
poisson <- fitdistr(AUDPS_2016$TShB_rating_t, "Poisson")
qqp(AUDPS_2016$TShB_rating_t, "pois", poisson$estimate)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-6-3.png)

``` r
gamma <- fitdistr(AUDPS_2016$TShB_rating_t, "gamma")
qqp(AUDPS_2016$TShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-6-4.png)

Leaf Sheath Blight Severity Model
---------------------------------

``` r
(LShB_lmm_2016 <- lmer(LShB_rating ~ WMGT * NRTE + (1 | REP/WMGT),
                       data = AUDPS_2016))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: LShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2016
    ## REML criterion at convergence: 25.5824
    ## Random effects:
    ##  Groups   Name        Std.Dev.
    ##  WMGT:REP (Intercept) 0.0000  
    ##  REP      (Intercept) 0.2220  
    ##  Residual             0.5208  
    ## Number of obs: 16, groups:  WMGT:REP, 8; REP, 4
    ## Fixed Effects:
    ##    (Intercept)         WMGTFLD          NRTE60  WMGTFLD:NRTE60  
    ##          1.295           0.245          -0.070          -0.455

``` r
summary(LShB_lmm_2016)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: LShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2016
    ## 
    ## REML criterion at convergence: 25.6
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.31929 -0.70651 -0.00723  0.58557  1.36870 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept) 0.00000  0.0000  
    ##  REP      (Intercept) 0.04927  0.2220  
    ##  Residual             0.27127  0.5208  
    ## Number of obs: 16, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)      1.2950     0.2831   4.575
    ## WMGTFLD          0.2450     0.3683   0.665
    ## NRTE60          -0.0700     0.3683  -0.190
    ## WMGTFLD:NRTE60  -0.4550     0.5208  -0.874
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) WMGTFLD NRTE60
    ## WMGTFLD     -0.650               
    ## NRTE60      -0.650  0.500        
    ## WMGTFLD:NRT  0.460 -0.707  -0.707

``` r
plot(LShB_lmm_2016)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
qqnorm(resid(LShB_lmm_2016))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-7-2.png)

Tiller Sheath Blight Severity Model
-----------------------------------

``` r
(TShB_lmm_2016 <- lmer(TShB_rating ~ WMGT * NRTE + (1 | REP/WMGT),
                       data = AUDPS_2016))
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2016
    ## REML criterion at convergence: 74.1454
    ## Random effects:
    ##  Groups   Name        Std.Dev.
    ##  WMGT:REP (Intercept) 3.122   
    ##  REP      (Intercept) 3.199   
    ##  Residual             2.722   
    ## Number of obs: 16, groups:  WMGT:REP, 8; REP, 4
    ## Fixed Effects:
    ##    (Intercept)         WMGTFLD          NRTE60  WMGTFLD:NRTE60  
    ##         19.845           4.410          -0.035          -3.710

``` r
summary(TShB_lmm_2016)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: TShB_rating ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2016
    ## 
    ## REML criterion at convergence: 74.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.21343 -0.46588  0.02339  0.41610  0.98141 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept)  9.744   3.122   
    ##  REP      (Intercept) 10.235   3.199   
    ##  Residual              7.411   2.722   
    ## Number of obs: 16, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)      19.845      2.617   7.584
    ## WMGTFLD           4.410      2.929   1.506
    ## NRTE60           -0.035      1.925  -0.018
    ## WMGTFLD:NRTE60   -3.710      2.722  -1.363
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) WMGTFLD NRTE60
    ## WMGTFLD     -0.560               
    ## NRTE60      -0.368  0.329        
    ## WMGTFLD:NRT  0.260 -0.465  -0.707

``` r
plot(TShB_lmm_2016)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
qqnorm(resid(TShB_lmm_2016))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-8-2.png)
