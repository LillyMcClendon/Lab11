Lab 11 - Grading the professor, Pt. 2
================
Lilly McClendon
04.18.2025

## Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
view(evals)
```

# Part 1: Simple Linear Regression

## Exercise 1

``` r
m_bty <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ bty_avg, data = evals) %>% 
tidy()
view(m_bty)

library(tidymodels)
library(broom)

R_m_bty <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ bty_avg, data = evals)

broom::glance(R_m_bty$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0350        0.0329 0.535      16.7 0.0000508     1  -366.  738.  751.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

$\hat{\text{Score}}_i$ = 3.88 + 0.07 x bty_avg$_i$. The R squared value
is 0.04 and the adjusted R squared value is 0.03.

# Part 2: Multiple Linear Regression

## Exercise 2

``` r
m_bty_gen <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ bty_avg + gender, data = evals)
tidy(m_bty_gen)
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   3.75      0.0847     44.3  6.23e-168
    ## 2 bty_avg       0.0742    0.0163      4.56 6.48e-  6
    ## 3 gendermale    0.172     0.0502      3.43 6.52e-  4

``` r
broom::glance(m_bty_gen$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic     p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>       <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0591        0.0550 0.529      14.5 0.000000818     2  -360.  729.  745.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

$\hat{\text{Score}}_i$ = 3.75 + 0.07 x bty_avg$_i$ + .17 x gender. The R
squared is 0.06 and the adjusted R squared is 0.06.

## Additional Exercises

*Repeat the format above for additional exercises.*
