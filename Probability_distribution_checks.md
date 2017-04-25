Probability Distribution Checks
================

Here we check the probability distribution that best fits these data. I'm following a method from <http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html>. As you see in that guide, there are several distributions that can be checked.

2015 Check probability distribution fit for tiller sheath blight incidence
--------------------------------------------------------------------------

``` r
f1 <- fitdist(DS2015$TShB_incidence, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
car::qqp(DS2015$TShB_incidence, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
f2 <- fitdist(DS2015$TShB_incidence, "exp")
plot(f2)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
car::qqp(DS2015$TShB_incidence, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-1-4.png)

------------------------------------------------------------------------

2016 Check probability distribution fit for tiller sheath blight incidence
--------------------------------------------------------------------------

``` r
f1 <- fitdist(DS2016$TShB_incidence, "norm")
plot(f1)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
car::qqp(DS2016$TShB_incidence, "norm")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
f2 <- fitdist(DS2016$TShB_incidence, "exp")
plot(f2)
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
car::qqp(DS2016$TShB_incidence, "exp")
```

![](Probability_distribution_checks_files/figure-markdown_github/unnamed-chunk-2-4.png)

Conclusions
-----------

To read the graphs, we want to select a distribution where most of the points fall between the dotted red lines. On the y-axis are the observations and on the x-axis the quantiles modelled by the distribution. A perfect distribution fit is represented by the solid red line and the dashed red lines are the confidence intervals of the perfect distribution fit. In this case, for the 2015 data, the leaf data fit a lognormal distribution best or exponential, for all others a normal distribution appears to be fine. Because of the non-normal residuals of the 2015 leaf sheath blight data we'll look at using something that can handle these data like `MCMCglmm`.
