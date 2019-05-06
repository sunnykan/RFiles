Linear Regression
================
KS
2019-03-10

``` r
library("MASS")
library("ISLR")
library("tidyverse")
library("broom")
library("ggfortify")
library("car")
```

### Ch3: Linear Regression

#### Applied Exercises

##### Q8: Use of simple linear regression on the Auto data set

###### (a)

``` r
auto <- as_tibble(Auto)
glimpse(auto)
```

    ## Observations: 392
    ## Variables: 9
    ## $ mpg          <dbl> 18, 15, 18, 16, 17, 15, 14, 14, 14, 15, 15, 14, 15,…
    ## $ cylinders    <dbl> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 6, 6, …
    ## $ displacement <dbl> 307, 350, 318, 304, 302, 429, 454, 440, 455, 390, 3…
    ## $ horsepower   <dbl> 130, 165, 150, 150, 140, 198, 220, 215, 225, 190, 1…
    ## $ weight       <dbl> 3504, 3693, 3436, 3433, 3449, 4341, 4354, 4312, 442…
    ## $ acceleration <dbl> 12.0, 11.5, 11.0, 12.0, 10.5, 10.0, 9.0, 8.5, 10.0,…
    ## $ year         <dbl> 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70,…
    ## $ origin       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, …
    ## $ name         <fct> chevrolet chevelle malibu, buick skylark 320, plymo…

``` r
lm_fit <- lm(mpg ~ horsepower, data = auto)
summary(lm_fit)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ horsepower, data = auto)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.5710  -3.2592  -0.3435   2.7630  16.9240 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
    ## horsepower  -0.157845   0.006446  -24.49   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.906 on 390 degrees of freedom
    ## Multiple R-squared:  0.6059, Adjusted R-squared:  0.6049 
    ## F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

``` r
glance(lm_fit)
```

    ## # A tibble: 1 x 11
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
    ## 1     0.606         0.605  4.91      600. 7.03e-81     2 -1179. 2363. 2375.
    ## # … with 2 more variables: deviance <dbl>, df.residual <int>

``` r
tidy(lm_fit)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   39.9     0.717        55.7 1.22e-187
    ## 2 horsepower    -0.158   0.00645     -24.5 7.03e- 81

There is a negative association between horsepower and mpg. The
coefficient on the predictor is highly significant and negative. The RSE
is 4.91 which means that actual mpg for each car deviates from the true
regression line by approximately 4.91, on average. The mean mpg for all
cars is 23.44592 and so the percentage error is 4.91/23.44592 = 21%.

The \(R^2\) value indicates that about 60% of the variance in mpg can be
explained by horsepower.

The confidence and prediction intervals when horsepower = 98 are:

``` r
predict(lm_fit, data.frame(horsepower = 98), interval = "confidence")
```

    ##        fit      lwr      upr
    ## 1 24.46708 23.97308 24.96108

``` r
predict(lm_fit, data.frame(horsepower = 98), interval = "prediction")
```

    ##        fit     lwr      upr
    ## 1 24.46708 14.8094 34.12476

The predicted mpg associated with horsepower of 98 is 24.46708. The
confidence and prediction intervals are (23.97308, 24.96108) and
(14.8094 34.12476). As expected, the latter is wider than the former.

###### (b)

A plot of the response and the predictor with a least squares regression
line.

``` r
ggplot(data = auto, aes(x = horsepower, y = mpg)) +
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 0.3) + 
    geom_point(alpha = 0.3)
```

![](ch3-linear-regression_files/figure-gfm/plot-1.png)<!-- -->

###### (c)

Diagnostic plots of the least squares regression fit.

``` r
diag_plots <- autoplot(lm_fit, which = 1:6, colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots
```

![](ch3-linear-regression_files/figure-gfm/diagnostic-plots-1-1.png)<!-- -->

The residuals versus fitted plot indicates a curvilinear relationship
between mpg and horsepower. Observation 116 stands out as having both
high leverage and a high studentized residual suggesting that it is an
outlier with high leverage.

##### Q9: Use of multiple linear regression on the Auto data set

###### (a)

A scatterplot matrix of all the variables

``` r
pairs(auto)
```

![](ch3-linear-regression_files/figure-gfm/scatteplotmat-1.png)<!-- -->

###### (b)

A scatterplot matrix of all the variables

``` r
cor(auto[1:8])
```

    ##                     mpg  cylinders displacement horsepower     weight
    ## mpg           1.0000000 -0.7776175   -0.8051269 -0.7784268 -0.8322442
    ## cylinders    -0.7776175  1.0000000    0.9508233  0.8429834  0.8975273
    ## displacement -0.8051269  0.9508233    1.0000000  0.8972570  0.9329944
    ## horsepower   -0.7784268  0.8429834    0.8972570  1.0000000  0.8645377
    ## weight       -0.8322442  0.8975273    0.9329944  0.8645377  1.0000000
    ## acceleration  0.4233285 -0.5046834   -0.5438005 -0.6891955 -0.4168392
    ## year          0.5805410 -0.3456474   -0.3698552 -0.4163615 -0.3091199
    ## origin        0.5652088 -0.5689316   -0.6145351 -0.4551715 -0.5850054
    ##              acceleration       year     origin
    ## mpg             0.4233285  0.5805410  0.5652088
    ## cylinders      -0.5046834 -0.3456474 -0.5689316
    ## displacement   -0.5438005 -0.3698552 -0.6145351
    ## horsepower     -0.6891955 -0.4163615 -0.4551715
    ## weight         -0.4168392 -0.3091199 -0.5850054
    ## acceleration    1.0000000  0.2903161  0.2127458
    ## year            0.2903161  1.0000000  0.1815277
    ## origin          0.2127458  0.1815277  1.0000000

###### (c)

Fit a multiple linear regression model.

``` r
lm_fit_full <- lm(mpg ~. -name, data = auto) 
summary(lm_fit_full)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ . - name, data = auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5903 -2.1565 -0.1169  1.8690 13.0604 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
    ## cylinders     -0.493376   0.323282  -1.526  0.12780    
    ## displacement   0.019896   0.007515   2.647  0.00844 ** 
    ## horsepower    -0.016951   0.013787  -1.230  0.21963    
    ## weight        -0.006474   0.000652  -9.929  < 2e-16 ***
    ## acceleration   0.080576   0.098845   0.815  0.41548    
    ## year           0.750773   0.050973  14.729  < 2e-16 ***
    ## origin         1.426141   0.278136   5.127 4.67e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.328 on 384 degrees of freedom
    ## Multiple R-squared:  0.8215, Adjusted R-squared:  0.8182 
    ## F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16

``` r
tidy(lm_fit_full)
```

    ## # A tibble: 8 x 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  -17.2      4.64        -3.71  2.40e- 4
    ## 2 cylinders     -0.493    0.323       -1.53  1.28e- 1
    ## 3 displacement   0.0199   0.00752      2.65  8.44e- 3
    ## 4 horsepower    -0.0170   0.0138      -1.23  2.20e- 1
    ## 5 weight        -0.00647  0.000652    -9.93  7.87e-21
    ## 6 acceleration   0.0806   0.0988       0.815 4.15e- 1
    ## 7 year           0.751    0.0510      14.7   3.06e-39
    ## 8 origin         1.43     0.278        5.13  4.67e- 7

``` r
glance(lm_fit_full)
```

    ## # A tibble: 1 x 11
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <int>  <dbl> <dbl>
    ## 1     0.821         0.818  3.33      252. 2.04e-139     8 -1023. 2065.
    ## # … with 3 more variables: BIC <dbl>, deviance <dbl>, df.residual <int>

The p-value on the F-statistic is extremely small which indicates that
there is a relationship between the predictors and the response.

displacement, weight, year and origin appear to have statistically
significant relationship to the response.

The coefficient on the year variable is highly significant and positive
which suggest that mpg has improved over time.

###### (d)

``` r
diag_plots <- autoplot(lm_fit_full, which = c(1:4, 6), colour = "dodgerblue3",
                       smooth.colour = "red", smooth.linetype = "dashed",
                       ad.colour = "blue",
                       label.size = 3, label.n = 5, label.colour = "black",
                       ncol = 2, alpha = 0.3)
diag_plots
```

![](ch3-linear-regression_files/figure-gfm/diagnostic-plots-2-1.png)<!-- -->

There doesn’t appear to be any unusually large outlier. Observation 14
seems to have unusually high leverage. We can fit the model again
without that observation and inspect the results.
