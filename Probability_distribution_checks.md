Probability Distribution Checks
================

Here we check the probability distribution that best fits these data. I'm following a method from <http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html>. As you see in that guide, there are several distributions that can be checked. However, in these data only normal and exponential are necessary.

2015
----

### Check probability distribution fit for tiller sheath blight incidence

#### Tiller Incidence Normal

``` r
AUDPS15 <- subset(AUDPS, YEAR == "2015")
f1 <- fitdist(AUDPS15$TShB_inc_AUDPS, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_inc_norm-1.png)

``` r
car::qqp(AUDPS15$TShB_inc_AUDPS, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_inc_norm-2.png)

#### Tiller Incidence Exponential

``` r
f1 <- fitdist(AUDPS15$TShB_inc_AUDPS, "exp")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence_exp-1.png)

``` r
car::qqp(AUDPS15$TShB_inc_AUDPS, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_incidence_exp-2.png)

#### Tiller Severity Normal

``` r
f1 <- fitdist(AUDPS15$TShB_percent_AUDPS, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_percent_norm-1.png)

``` r
car::qqp(AUDPS15$TShB_percent_AUDPS, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_percent_norm-2.png)

#### Tiller Severity Exponential

``` r
f1 <- fitdist(AUDPS15$TShB_percent_AUDPS, "exp")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_percent_exp-1.png)

``` r
car::qqp(AUDPS15$TShB_percent_AUDPS, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_TShB_percent_exp-2.png)

#### Leaf Severity Normal

``` r
f1 <- fitdist(AUDPS15$LShB_percent_AUDPS, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_LShB_percent_norm-1.png)

``` r
car::qqp(AUDPS15$LShB_percent_AUDPS, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_LShB_percent_norm-2.png)

#### Leaf Severity Exponential

``` r
f1 <- fitdist(AUDPS15$LShB_percent_AUDPS, "exp")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_LShB_percent_exp-1.png)

``` r
car::qqp(AUDPS15$LShB_percent_AUDPS, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2015_LShB_percent_exp-2.png)

------------------------------------------------------------------------

2016
----

### Check probability distribution fit for tiller sheath blight incidence

#### Tiller Incidence Normal

``` r
AUDPS16 <- subset(AUDPS, YEAR == "2016")
f1 <- fitdist(AUDPS16$TShB_inc_AUDPS, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_inc_norm-1.png)

``` r
car::qqp(AUDPS16$TShB_inc_AUDPS, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_inc_norm-2.png)

#### Tiller Incidence Exponential

``` r
f1 <- fitdist(AUDPS16$TShB_inc_AUDPS, "exp")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_inc_exp-1.png)

``` r
car::qqp(AUDPS16$TShB_inc_AUDPS, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_inc_exp-2.png)

#### Tiller Severity Normal

``` r
AUDPS16 <- subset(AUDPS, YEAR == "2016")
f1 <- fitdist(AUDPS16$TShB_percent_AUDPS, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_percent_norm-1.png)

``` r
car::qqp(AUDPS16$TShB_percent_AUDPS, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_percent_norm-2.png)

#### Tiller Severity Exponential

``` r
f1 <- fitdist(AUDPS16$TShB_percent_AUDPS, "exp")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_percent_exp-1.png)

``` r
car::qqp(AUDPS16$TShB_percent_AUDPS, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_TShB_percent_exp-2.png)

#### Leaf Severity Normal

``` r
AUDPS16 <- subset(AUDPS, YEAR == "2016")
f1 <- fitdist(AUDPS16$LShB_percent_AUDPS, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_LShB_percent_norm-1.png)

``` r
car::qqp(AUDPS16$LShB_percent_AUDPS, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_LShB_percent_norm-2.png)

#### Leaf Severity Exponential

``` r
f1 <- fitdist(AUDPS16$LShB_percent_AUDPS, "exp")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_LShB_percent_exp-1.png)

``` r
car::qqp(AUDPS16$LShB_percent_AUDPS, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github-ascii_identifiers/2016_LShB_percent_exp-2.png)

Conclusions
-----------

To read the graphs, we want to select a distribution where most of the points fall between the dotted red lines. On the y-axis are the observations and on the x-axis the quantiles modelled by the distribution. A perfect distribution fit is represented by the solid red line and the dashed red lines are the confidence intervals of the perfect distribution fit.

### Tiller Incidence Distributions

The 2015 data fit an exponential distribution while the 2016 data fit a normal distribution.

### Leaf Sheath Blight Severity Distributions

The 2015 data contain negative values, so an exponential distribution will not fit. The 2016 data fit a normal distribution better than exponential.

### Tiller Sheath Blight Severity Distributions

The 2015 data contain negative values, so an exponential distribution will not fit. The 2016 data fit a normal distribution better than exponential.
