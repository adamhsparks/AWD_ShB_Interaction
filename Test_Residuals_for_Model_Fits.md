Test Residuals for Model Fits
================

``` r
require(car)
require(MASS)

# This is so that distributions that must be non-zero can make sense of my
# data
AUDPS_15 <- subset(AUDPS, YEAR == 2015)
AUDPS_16 <- subset(AUDPS, YEAR == 2016)

qqp(AUDPS_15$LShB_percent_AUDPS, "norm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

``` r
qqp(AUDPS_15$LShB_percent_AUDPS, "lnorm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-2.png)

``` r
qqp(AUDPS_15$LShB_percent_AUDPS, "exp")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-3.png)

``` r
qqp(AUDPS_15$LShB_percent_AUDPS, "chisq", df = 2)
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-4.png)

``` r
qqp(AUDPS_16$LShB_percent_AUDPS, "norm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-5.png)

``` r
qqp(AUDPS_16$LShB_percent_AUDPS, "lnorm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-6.png)

``` r
qqp(AUDPS_16$LShB_percent_AUDPS, "exp")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-7.png)

``` r
qqp(AUDPS_16$LShB_percent_AUDPS, "chisq", df = 2)
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-8.png)

``` r
qqp(AUDPS_15$TShB_percent_AUDPS, "norm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-9.png)

``` r
qqp(AUDPS_15$TShB_percent_AUDPS, "lnorm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-10.png)

``` r
qqp(AUDPS_15$TShB_percent_AUDPS, "exp")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-11.png)

``` r
qqp(AUDPS_15$TShB_percent_AUDPS, "chisq", df = 2)
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-12.png)

``` r
qqp(AUDPS_16$TShB_percent_AUDPS, "norm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-13.png)

``` r
qqp(AUDPS_16$TShB_percent_AUDPS, "lnorm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-14.png)

``` r
qqp(AUDPS_16$TShB_percent_AUDPS, "exp")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-15.png)

``` r
qqp(AUDPS_16$TShB_percent_AUDPS, "chisq", df = 2)
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-16.png)

``` r
qqp(AUDPS_15$TShB_inc_AUDPS, "norm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-17.png)

``` r
qqp(AUDPS_15$TShB_inc_AUDPS, "lnorm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-18.png)

``` r
qqp(AUDPS_15$TShB_inc_AUDPS, "exp")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-19.png)

``` r
qqp(AUDPS_15$TShB_inc_AUDPS, "chisq", df = 2)
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-20.png)

``` r
qqp(AUDPS_16$TShB_inc_AUDPS, "norm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-21.png)

``` r
qqp(AUDPS_16$TShB_inc_AUDPS, "lnorm")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-22.png)

``` r
qqp(AUDPS_16$TShB_inc_AUDPS, "exp")
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-23.png)

``` r
qqp(AUDPS_16$TShB_inc_AUDPS, "chisq", df = 2)
```

![](Test_Residuals_for_Model_Fits_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-24.png)
