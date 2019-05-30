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

##### Q10: Use of multiple linear regression on the Carseats data set

###### carseats data

``` r
carseats <- as_tibble(Carseats)
names(carseats) <- tolower(names(carseats))
names(carseats)
```

    ##  [1] "sales"       "compprice"   "income"      "advertising" "population" 
    ##  [6] "price"       "shelveloc"   "age"         "education"   "urban"      
    ## [11] "us"

``` r
glimpse(carseats)
```

    ## Observations: 400
    ## Variables: 11
    ## $ sales       <dbl> 9.50, 11.22, 10.06, 7.40, 4.15, 10.81, 6.63, 11.85, …
    ## $ compprice   <dbl> 138, 111, 113, 117, 141, 124, 115, 136, 132, 132, 12…
    ## $ income      <dbl> 73, 48, 35, 100, 64, 113, 105, 81, 110, 113, 78, 94,…
    ## $ advertising <dbl> 11, 16, 10, 4, 3, 13, 0, 15, 0, 0, 9, 4, 2, 11, 11, …
    ## $ population  <dbl> 276, 260, 269, 466, 340, 501, 45, 425, 108, 131, 150…
    ## $ price       <dbl> 120, 83, 80, 97, 128, 72, 108, 120, 124, 124, 100, 9…
    ## $ shelveloc   <fct> Bad, Good, Medium, Medium, Bad, Bad, Medium, Good, M…
    ## $ age         <dbl> 42, 65, 59, 55, 38, 78, 71, 67, 76, 76, 26, 50, 62, …
    ## $ education   <dbl> 17, 10, 12, 14, 13, 16, 15, 10, 10, 17, 10, 13, 18, …
    ## $ urban       <fct> Yes, Yes, Yes, Yes, Yes, No, Yes, Yes, No, No, No, Y…
    ## $ us          <fct> Yes, Yes, Yes, Yes, No, Yes, No, Yes, No, Yes, Yes, …

###### (a) Fit a multiple regression model to predict Sales using Price, Urban and US.

``` r
m1 <- lm(sales ~ price + urban + us, data = carseats)
tidy(m1)
```

    ## # A tibble: 4 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  13.0      0.651     20.0    3.63e-62
    ## 2 price        -0.0545   0.00524  -10.4    1.61e-22
    ## 3 urbanYes     -0.0219   0.272     -0.0807 9.36e- 1
    ## 4 usYes         1.20     0.259      4.63   4.86e- 6

``` r
glance(m1)
```

    ## # A tibble: 1 x 11
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
    ## 1     0.239         0.234  2.47      41.5 2.39e-23     4  -928. 1865. 1885.
    ## # … with 2 more variables: deviance <dbl>, df.residual <int>

``` r
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = sales ~ price + urban + us, data = carseats)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.9206 -1.6220 -0.0564  1.5786  7.0581 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 13.043469   0.651012  20.036  < 2e-16 ***
    ## price       -0.054459   0.005242 -10.389  < 2e-16 ***
    ## urbanYes    -0.021916   0.271650  -0.081    0.936    
    ## usYes        1.200573   0.259042   4.635 4.86e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.472 on 396 degrees of freedom
    ## Multiple R-squared:  0.2393, Adjusted R-squared:  0.2335 
    ## F-statistic: 41.52 on 3 and 396 DF,  p-value: < 2.2e-16

The price company charges for car seats at each site (price) and whether
the store is in the US or not (usYes) are statistically significant at
the 0.001 level. The former has a negative effect on sales while the
latter has a negative effect. Specifically, a one unit change in price
is associated with a decrease in sales of 0.054 units. If the store is
located in the US, sales increase by 1.2 units. However, whether the
store is located in an urban or rural location (urbanYes) is not
statistically significant.

###### (e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome. Fit model with price and us.

``` r
m2 <- lm(sales ~ price + us, data = carseats)
tidy(m2)
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  13.0      0.631       20.7  7.00e-65
    ## 2 price        -0.0545   0.00523    -10.4  1.27e-22
    ## 3 usYes         1.20     0.258        4.64 4.71e- 6

``` r
glance(m2)
```

    ## # A tibble: 1 x 11
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
    ## 1     0.239         0.235  2.47      62.4 2.66e-24     3  -928. 1863. 1879.
    ## # … with 2 more variables: deviance <dbl>, df.residual <int>

``` r
summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = sales ~ price + us, data = carseats)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.9269 -1.6286 -0.0574  1.5766  7.0515 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 13.03079    0.63098  20.652  < 2e-16 ***
    ## price       -0.05448    0.00523 -10.416  < 2e-16 ***
    ## usYes        1.19964    0.25846   4.641 4.71e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.469 on 397 degrees of freedom
    ## Multiple R-squared:  0.2393, Adjusted R-squared:  0.2354 
    ## F-statistic: 62.43 on 2 and 397 DF,  p-value: < 2.2e-16

The R Square and RSE values in both models are identical. In this case,
we prefer the simpler model in m2 with fewer predictors.

###### (g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).

``` r
upper <- tidy(m2)["estimate"] + qt(.975, 397) * tidy(m2)["std.error"]
lower <- tidy(m2)["estimate"] - qt(.975, 397) * tidy(m2)["std.error"]
cbind(upper, lower)
```

    ##      estimate    estimate
    ## 1 14.27126531 11.79032020
    ## 2 -0.04419543 -0.06475984
    ## 3  1.70776632  0.69151957

``` r
confint(m2)
```

    ##                   2.5 %      97.5 %
    ## (Intercept) 11.79032020 14.27126531
    ## price       -0.06475984 -0.04419543
    ## usYes        0.69151957  1.70776632

###### (h) Is there evidence of outliers or high leverage observations in the model from (e)? We can examine some diagnostic plots.

We can look at plots of studentized residuals versus fitted values.

``` r
aug_m2 <- augment(m2)
y.fitted <- flatten_dbl(aug_m2[".fitted"])
stu.resids <- rstudent(m2)
stres_plt <- ggplot(data = NULL, mapping = aes(x = y.fitted, y = stu.resids)) +
    geom_point(color = "dodgerblue") +
    geom_hline(yintercept = 0, color = "grey") +
    xlab("Fitted Values") +
    ylab("Studentized Residuals")
stres_plt
```

![](ch3-linear-regression_files/figure-gfm/diagplot1-q10-g-1.png)<!-- -->

There are no observations greater then 3 in absolute value. Hence, there
are no outliers.

To identify high leverage observations we can plot studentized residuals
versus leverage.

``` r
hats <- flatten_dbl(aug_m2[".hat"])
lev_plt <- ggplot(data = carseats, mapping = aes(x = hats, y = stu.resids, 
                  label =      rownames(carseats))) +
    geom_text(color = "dodgerblue") +
    geom_hline(yintercept = 0, color = "grey") +
    xlab("Leverage") +
    ylab("Studentized Residuals")
lev_plt
```

![](ch3-linear-regression_files/figure-gfm/diagplot2-q10-g-1.png)<!-- -->

Observation 43 is possibly a high leverage point. Removing it and
estimating the model again shows a very slight reduction in the
R-squared and the RSE values.

##### Q11. In this problem we will investigate the t-statistic for the null hypoth- esis H0 : β = 0 in simple linear regression without an intercept.

generate a predictor x and a response y:

``` r
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
```

###### (a) Perform a simple linear regression of y onto x, without an intercept.

``` r
lm.fit_yx <- lm(y ~ x + 0)
tidy(lm.fit_yx)
```

    ## # A tibble: 1 x 5
    ##   term  estimate std.error statistic  p.value
    ##   <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 x         1.99     0.106      18.7 2.64e-34

The coefficient is highly statistically significant (\(beta\) = 1.99, SE
= 0.106, t-statistic = 18.7, p-value = 2.64e-34). We can reject the
null.

###### (b) Perform a simple linear regression of x onto y without an intercept.

``` r
lm.fit_xy <- lm(x ~ y + 0)
tidy(lm.fit_xy)
```

    ## # A tibble: 1 x 5
    ##   term  estimate std.error statistic  p.value
    ##   <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 y        0.391    0.0209      18.7 2.64e-34

The coefficient is highly statistically significant (\(beta\) = 0.391,
SE = 0.0209, t-statistic = 18.7, p-value = 2.64e-34). We can reject the
null.

The t-statistics are identical in a and b.

###### (f) In R, show that when regression is performed with an intercept, the t-statistic for H0 : β1 = 0 is the same for the regression of y onto x as it is for the regression of x onto y.

``` r
lm.fit_yx <- lm(y ~ x)
tidy(lm.fit_yx)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  -0.0377    0.0970    -0.389 6.98e- 1
    ## 2 x             2.00      0.108     18.6   7.72e-34

``` r
lm.fit_xy <- lm(x ~ y)
tidy(lm.fit_xy)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   0.0388    0.0427     0.910 3.65e- 1
    ## 2 y             0.389     0.0210    18.6   7.72e-34

The values for the t-statistics are identical (18.6) in both
regressions.

##### (12) This problem involves simple linear regression without an intercept.

###### (a) Under what circumstance is the coefficient estimate for the regression of X onto Y the same as the coefficient estimate for the regression of Y onto X?

The coefficient estimate for the regression of X onto Y is same as the
coefficient estimate for the regression of Y onto X when the Sum of
squares of X = Sum of squares of Y.

###### (b) Generate an example in R with n = 100 observations in which the coefficient estimate for the regression of X onto Y is different from the coefficient estimate for the regression of Y onto X.

``` r
set.seed(1)
obs = 100
x = rnorm(obs)
sum(x^2)
```

    ## [1] 81.05509

``` r
y = 3*x + rnorm(obs, sd = 0.2)
sum(y^2)
```

    ## [1] 732.5393

``` r
yonx <- lm(y ~ x + 0)
tidy(yonx)
```

    ## # A tibble: 1 x 5
    ##   term  estimate std.error statistic   p.value
    ##   <chr>    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 x         3.00    0.0213      141. 7.31e-116

``` r
xony <- lm(x ~ y + 0)
tidy(xony)
```

    ## # A tibble: 1 x 5
    ##   term  estimate std.error statistic   p.value
    ##   <chr>    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 y        0.332   0.00236      141. 7.31e-116
