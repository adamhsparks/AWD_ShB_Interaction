Probability Distribution Checks
================

2015
====

Check probability distribution fit for leaf sheath blight
---------------------------------------------------------

``` r
AUDPS_2015$LShB_rating_t <- AUDPS_2015$LShB_rating + 1

qqp(AUDPS_2015$LShB_rating_t, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
qqp(AUDPS_2015$LShB_rating_t, "lnorm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
poisson <- fitdistr(AUDPS_2015$LShB_rating_t, "Poisson")
qqp(AUDPS_2015$LShB_rating_t, "pois", poisson$estimate)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
gamma <- fitdistr(AUDPS_2015$LShB_rating_t, "gamma")
qqp(AUDPS_2015$LShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-4.png)

Check probability distribution fit for tiller sheath blight
-----------------------------------------------------------

``` r
AUDPS_2015$TShB_rating_t <- AUDPS_2015$TShB_rating + 1

qqp(AUDPS_2015$TShB_rating_t, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
qqp(AUDPS_2015$TShB_rating_t, "lnorm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
poisson <- fitdistr(AUDPS_2015$TShB_rating_t, "Poisson")
qqp(AUDPS_2015$TShB_rating_t, "pois", poisson$estimate)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
gamma <- fitdistr(AUDPS_2015$TShB_rating_t, "gamma")
qqp(AUDPS_2015$TShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-4.png)

------------------------------------------------------------------------

2016
====

Check probability distribution fit for leaf sheath blight
---------------------------------------------------------

``` r
AUDPS_2016$LShB_rating_t <- AUDPS_2016$LShB_rating + 1

qqp(AUDPS_2016$LShB_rating_t, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
qqp(AUDPS_2016$LShB_rating_t, "lnorm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
poisson <- fitdistr(AUDPS_2016$LShB_rating_t, "Poisson")
qqp(AUDPS_2016$LShB_rating_t, "pois", poisson$estimate)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
gamma <- fitdistr(AUDPS_2016$LShB_rating_t, "gamma")
qqp(AUDPS_2016$LShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-3-4.png)

Check probability distribution fit for tiller sheath blight
-----------------------------------------------------------

``` r
AUDPS_2016$TShB_rating_t <- AUDPS_2016$TShB_rating + 1

qqp(AUDPS_2016$TShB_rating_t, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
qqp(AUDPS_2016$TShB_rating_t, "lnorm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
poisson <- fitdistr(AUDPS_2016$TShB_rating_t, "Poisson")
qqp(AUDPS_2016$TShB_rating_t, "pois", poisson$estimate)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
gamma <- fitdistr(AUDPS_2016$TShB_rating_t, "gamma")
qqp(AUDPS_2016$TShB_rating_t, "gamma",
    shape = gamma$estimate[[1]], 
    rate = gamma$estimate[[2]])
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-4-4.png)
