Analysis
================

2015
====

Leaf Sheath Blight Severity Model
---------------------------------

``` r
# 2015 model goes here
```

Tiller Sheath Blight Severity Model
-----------------------------------

``` r
TShB_lmm_2015 <- lmer(TShB_AUDPS ~ WMGT * NRTE + (1 | REP/WMGT),
                      data = AUDPS_2015,
                      REML = FALSE)
summary(TShB_lmm_2015)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: TShB_AUDPS ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2015
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    123.5    134.1    -52.7    105.5       15 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2854 -0.7782 -0.2169  0.5586  2.7671 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept) 0.000    0.000   
    ##  REP      (Intercept) 0.000    0.000   
    ##  Residual             4.745    2.178   
    ## Number of obs: 24, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)       1.5775     1.0891   1.448
    ## WMGTFLD          -0.5250     1.5402  -0.341
    ## NRTE100           1.6525     1.5402   1.073
    ## NRTE120           1.9625     1.5402   1.274
    ## WMGTFLD:NRTE100  -0.8425     2.1782  -0.387
    ## WMGTFLD:NRTE120   0.2975     2.1782   0.137
    ## 
    ## Correlation of Fixed Effects:
    ##                (Intr) WMGTFLD NRTE10 NRTE12 WMGTFLD:NRTE10
    ## WMGTFLD        -0.707                                     
    ## NRTE100        -0.707  0.500                              
    ## NRTE120        -0.707  0.500   0.500                      
    ## WMGTFLD:NRTE10  0.500 -0.707  -0.707 -0.354               
    ## WMGTFLD:NRTE12  0.500 -0.707  -0.354 -0.707  0.500

``` r
plot(TShB_lmm_2015)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
qqnorm(resid(TShB_lmm_2015))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-2-2.png)

------------------------------------------------------------------------

2016
====

Leaf Sheath Blight Severity Model
---------------------------------

``` r
LShB_lmm_2016 <- lmer(LShB_AUDPS ~ WMGT * NRTE + (1 | REP/WMGT),
                      data = AUDPS_2016,
                      REML = FALSE)
summary(LShB_lmm_2016)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: LShB_AUDPS ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2016
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     36.1     41.5    -11.1     22.1        9 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.52339 -0.81581 -0.00834  0.67616  1.58044 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept) 0.00000  0.0000  
    ##  REP      (Intercept) 0.03695  0.1922  
    ##  Residual             0.20345  0.4511  
    ## Number of obs: 16, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)      1.2950     0.2452   5.282
    ## WMGTFLD          0.2450     0.3190   0.768
    ## NRTE60          -0.0700     0.3190  -0.219
    ## WMGTFLD:NRTE60  -0.4550     0.4511  -1.009
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) WMGTFLD NRTE60
    ## WMGTFLD     -0.650               
    ## NRTE60      -0.650  0.500        
    ## WMGTFLD:NRT  0.460 -0.707  -0.707

``` r
plot(LShB_lmm_2016)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
qqnorm(resid(LShB_lmm_2016))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-3-2.png)

Tiller Sheath Blight Severity Model
-----------------------------------

``` r
TShB_lmm_2016 <- lmer(TShB_AUDPS ~ WMGT * NRTE + (1 | REP/WMGT),
                      data = AUDPS_2016,
                      REML = FALSE)
summary(TShB_lmm_2016)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: TShB_AUDPS ~ WMGT * NRTE + (1 | REP/WMGT)
    ##    Data: AUDPS_2016
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    100.9    106.3    -43.4     86.9        9 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.40115 -0.53795  0.02701  0.48047  1.13323 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  WMGT:REP (Intercept) 7.308    2.703   
    ##  REP      (Intercept) 7.676    2.771   
    ##  Residual             5.558    2.358   
    ## Number of obs: 16, groups:  WMGT:REP, 8; REP, 4
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)      19.845      2.266   8.757
    ## WMGTFLD           4.410      2.536   1.739
    ## NRTE60           -0.035      1.667  -0.021
    ## WMGTFLD:NRTE60   -3.710      2.358  -1.574
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) WMGTFLD NRTE60
    ## WMGTFLD     -0.560               
    ## NRTE60      -0.368  0.329        
    ## WMGTFLD:NRT  0.260 -0.465  -0.707

``` r
plot(TShB_lmm_2016)
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
qqnorm(resid(TShB_lmm_2016))
```

![](Analysis_files/figure-markdown_github/unnamed-chunk-4-2.png)
