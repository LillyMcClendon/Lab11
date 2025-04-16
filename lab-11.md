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
contrasts(evals$gender)
```

    ##        male
    ## female    0
    ## male      1

``` r
broom::glance(m_bty_gen$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic     p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>       <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0591        0.0550 0.529      14.5 0.000000818     2  -360.  729.  745.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
tidy(m_bty_gen) %>% 
  mutate(exp_estimate = exp(estimate)) %>% 
  select(term, estimate, exp_estimate)
```

    ## # A tibble: 3 × 3
    ##   term        estimate exp_estimate
    ##   <chr>          <dbl>        <dbl>
    ## 1 (Intercept)   3.75          42.4 
    ## 2 bty_avg       0.0742         1.08
    ## 3 gendermale    0.172          1.19

$\hat{\text{Score}}_i$ = 3.75 + 0.07 x bty_avg$_i$ + 0.17 x gender$_i$.
The R squared is 0.06 and the adjusted R squared is 0.06.

## Exercise 3

females: Score = 3.75 + (0.07 x bty_avg) + (0.17 x 0) Score = 3.75 +
0.07 x bty_avg

males: Score = 3.75 + (0.07 x bty_avg) + (0.17 x 1) Score = 3.75 + 0.07
x bty_avg + 0.17

Scores for female professors that have an average beauty rating of 0,are
predicted on average to have a score of 42.41.

When everything else is held constant, scores for a male professor are
predicted on average to be higher by a factor of 1.19 compared to scores
for female professors.

When everything else is held constant, for each additional point in
average beauty, the score of a professor is expected to be on average
higher by a factor of 1.08.

Slope-bty_avg: all else held constant, for each additional beauty
average point, we would expect the score to be higher on average by 0.07
points.

Slope-gender: all else held constant, males on average have 0.17 more
points of a score than females.

Intercept: males with 0 beauty average points are expected to have a
score of 3.75 on average.

## Exercise 4

6% of the variability in score is explained by gender and average beauty
score.

## Exercise 5

The equation of the line corresponding to just male professors is
$\hat{\text{Score}}_i$ = 3.75 + 0.07 x bty_avg$_i$ + 0.17

## Exercise 6

For two professors who received the same beauty rating, males tend to
have the higher course evaluation score.

## Exercise 7

``` r
m_int_bty_gen <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ bty_avg * gender, data = evals)
tidy(m_int_bty_gen)
```

    ## # A tibble: 4 × 5
    ##   term               estimate std.error statistic   p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          3.95      0.118      33.5  2.92e-125
    ## 2 bty_avg              0.0306    0.0240      1.28 2.02e-  1
    ## 3 gendermale          -0.184     0.153      -1.20 2.32e-  1
    ## 4 bty_avg:gendermale   0.0796    0.0325      2.45 1.46e-  2

score = 3.95 + (0.03 x bty_avg) - (0.18 x gender) + (0.08 x
bty_avg\*gender)

females: score = 3.95 + (0.03 x bty_avg) - (0.18 x 0) + (0.08 x
bty_avg\*0)

score = 3.95 + (0.03 x bty_avg)

males: 3.95 + (0.03 x bty_avg) - (0.18 x 1) + (0.08 x bty_avg\*1)

males: 3.77 + (0.03 x bty_avg) + (0.08 x bty_avg\*1)

males: 3.77 + (0.11 x bty_avg)

The rate of change in score as the beauty average score increases does
vary between the male and female professors (different slopes). Some
scores are higher for male professors, but some are not (different
intercept).

## Exercise 8

The adjusted R squared value of m_bty is 0.03 and the adjusted R squared
value for m_bty_gen is 0.06. When we include the interaction, the
adjusted R squared value is greater, so we should use the model that
includes the interactions. The adjusted R squared value is a measure of
how much variability is explained for by the model while accounting for
the number of predictors. Because the adjusted R squared value increased
when we added the interaction, it indicates that the interaction
improves how well the model predicts score. Therefore, I would say that
gender is useful in explaining the variability in evaluation scores when
we already have information on the beauty score of the professor, and
the interaction between beauty score and gender further improves the
model.

## Exercise 9

In the m_bty model, the slope for bty_avg is 0.07. In the m_bty_gen
model, the slope for bty_avg is 0.03. By adding gender to the model, the
slope for bty_avg decreased. This possibly could be because of
multicollinearity as there could be a correlation between beauty score
and gender and when adding in gender, some of the variance explained by
bty_avg in the m_bty model may have been shifted to the variance
explained by gender.

## Exercise 10

``` r
m_bty_rank <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ bty_avg + rank, data = evals)
tidy(m_bty_rank)
```

    ## # A tibble: 4 × 5
    ##   term             estimate std.error statistic   p.value
    ##   <chr>               <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)        3.98      0.0908     43.9  2.92e-166
    ## 2 bty_avg            0.0678    0.0165      4.10 4.92e-  5
    ## 3 ranktenure track  -0.161     0.0740     -2.17 3.03e-  2
    ## 4 ranktenured       -0.126     0.0627     -2.01 4.45e-  2

``` r
contrasts(evals$rank)
```

    ##              tenure track tenured
    ## teaching                0       0
    ## tenure track            1       0
    ## tenured                 0       1

``` r
broom::glance(m_bty_rank$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0465        0.0403 0.533      7.46 0.0000688     3  -363.  737.  758.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
tidy(m_bty_rank) %>% 
  mutate(exp_estimate = exp(estimate)) %>% 
  select(term, estimate, exp_estimate)
```

    ## # A tibble: 4 × 3
    ##   term             estimate exp_estimate
    ##   <chr>               <dbl>        <dbl>
    ## 1 (Intercept)        3.98         53.6  
    ## 2 bty_avg            0.0678        1.07 
    ## 3 ranktenure track  -0.161         0.852
    ## 4 ranktenured       -0.126         0.881

score = 3.98 + (0.07 \* bty_avg) - (0.16 \* tenure track) -
(0.13\*tenured)

teaching: = 3.98 - (0.16 \* 0) - (0.13*0) = 3.98 + (0.07 * bty_avg

tenure track: = 3.98 - (0.16 \* 1) - (0.13*0) = 3.82 + (0.07 * bty_avg

tenured: = 3.98 - (0.16 \* 0) - (0.13*1) = 3.85 + (0.07 * bty_avg)

Different intercept, so scores for teaching professors are consistently
higher than scores for tenure track or tenured professors. There is the
same slope, so the rate of change in score as bty_avg increases does not
vary between teaching, tenure track, and tenured professors.

``` r
m_int_bty_rank <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ bty_avg * rank, data = evals)
tidy(m_int_bty_rank)
```

    ## # A tibble: 6 × 5
    ##   term                     estimate std.error statistic  p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)                4.10      0.150    27.4    1.80e-98
    ## 2 bty_avg                    0.0417    0.0314    1.33   1.84e- 1
    ## 3 ranktenure track          -0.0188    0.230    -0.0818 9.35e- 1
    ## 4 ranktenured               -0.409     0.182    -2.25   2.52e- 2
    ## 5 bty_avg:ranktenure track  -0.0264    0.0463   -0.570  5.69e- 1
    ## 6 bty_avg:ranktenured        0.0659    0.0392    1.68   9.38e- 2

``` r
contrasts(evals$rank)
```

    ##              tenure track tenured
    ## teaching                0       0
    ## tenure track            1       0
    ## tenured                 0       1

``` r
broom::glance(m_int_bty_rank$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1    0.0587        0.0484 0.531      5.70 0.0000409     5  -360.  735.  764.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
tidy(m_int_bty_rank) %>% 
  mutate(exp_estimate = exp(estimate)) %>% 
  select(term, estimate, exp_estimate)
```

    ## # A tibble: 6 × 3
    ##   term                     estimate exp_estimate
    ##   <chr>                       <dbl>        <dbl>
    ## 1 (Intercept)                4.10         60.2  
    ## 2 bty_avg                    0.0417        1.04 
    ## 3 ranktenure track          -0.0188        0.981
    ## 4 ranktenured               -0.409         0.664
    ## 5 bty_avg:ranktenure track  -0.0264        0.974
    ## 6 bty_avg:ranktenured        0.0659        1.07

# Part 3: The search for the best model

## Exercise 11

On its own, I would expect the worst predictor of evaluation score to be
cls_credits.

## Exercise 12

``` r
m_cls_did_eval <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ cls_did_eval, data = evals) %>% 
tidy()
view(m_cls_did_eval)

library(tidymodels)
library(broom)

R_m_cls_did_eval <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ cls_did_eval, data = evals)

broom::glance(R_m_cls_did_eval$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1   0.00395       0.00179 0.543      1.83   0.177     1  -374.  753.  766.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

I expected that the variable on its own that would be the worst
predictor of evaluation scores would be cls_credits. The model has an R
squared value of 0.004 which means that 0.4% of the variance in
evaluation scores is explained by cls_credits.

## Exercise 13

If I was going to fit a full model with variables like cls_perc_eval and
cls_students, the variable I should not include as an additional
predictor would be cls_did_eval. Because the number of students in class
who completed evaluation can be calculated from the cls_perc_eval and
the cls_students. I would not add cls_did_eval because it is tied to the
cls_perc_eval and cls_students and therefore could lead to
multicollinearity which may make it more difficulty to understand how
each predictor explains some of the variability in the model.

## Exercise 14

``` r
m_full_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals) %>% 
tidy()
view(m_full_model)

library(tidymodels)
library(broom)

R_m_full_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)

broom::glance(R_m_full_model$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.164         0.141 0.504      7.33 2.41e-12    12  -333.  694.  752.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

## Exercise 15

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following objects are masked from 'package:openintro':
    ## 
    ##     housing, mammals

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
lm_full_model <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)

best_model <- stepAIC(lm_full_model, direction = "backward", trace = 1)
```

    ## Start:  AIC=-621.66
    ## score ~ rank + ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_level + cls_profs + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - rank           2    0.4325 114.74 -623.91
    ## - cls_profs      1    0.0071 114.31 -623.63
    ## - cls_level      1    0.0288 114.34 -623.54
    ## - language       1    0.3501 114.66 -622.24
    ## - cls_students   1    0.3923 114.70 -622.07
    ## <none>                       114.31 -621.66
    ## - age            1    1.1818 115.49 -618.90
    ## - ethnicity      1    1.4771 115.78 -617.71
    ## - gender         1    3.0515 117.36 -611.46
    ## - cls_perc_eval  1    3.4284 117.74 -609.98
    ## - bty_avg        1    3.4287 117.74 -609.97
    ## - cls_credits    1    4.8017 119.11 -604.61
    ## 
    ## Step:  AIC=-623.91
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_level + cls_profs + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_profs      1    0.0103 114.75 -625.87
    ## - cls_level      1    0.0173 114.76 -625.84
    ## - cls_students   1    0.3645 115.11 -624.44
    ## <none>                       114.74 -623.91
    ## - language       1    0.5568 115.30 -623.67
    ## - age            1    0.8918 115.63 -622.32
    ## - ethnicity      1    1.7046 116.44 -619.08
    ## - gender         1    3.1469 117.89 -613.38
    ## - cls_perc_eval  1    3.5245 118.27 -611.90
    ## - bty_avg        1    3.5642 118.31 -611.75
    ## - cls_credits    1    5.6754 120.42 -603.56
    ## 
    ## Step:  AIC=-625.87
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_level + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_level      1    0.0162 114.77 -627.80
    ## - cls_students   1    0.3731 115.12 -626.36
    ## <none>                       114.75 -625.87
    ## - language       1    0.5552 115.31 -625.63
    ## - age            1    0.8964 115.65 -624.27
    ## - ethnicity      1    1.8229 116.57 -620.57
    ## - gender         1    3.1375 117.89 -615.38
    ## - cls_perc_eval  1    3.5166 118.27 -613.89
    ## - bty_avg        1    3.5547 118.31 -613.74
    ## - cls_credits    1    5.8278 120.58 -604.93
    ## 
    ## Step:  AIC=-627.8
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_students + cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - cls_students   1    0.3569 115.12 -628.36
    ## <none>                       114.77 -627.80
    ## - language       1    0.5390 115.31 -627.63
    ## - age            1    0.8828 115.65 -626.25
    ## - ethnicity      1    1.8948 116.66 -622.22
    ## - gender         1    3.1222 117.89 -617.37
    ## - cls_perc_eval  1    3.5266 118.29 -615.79
    ## - bty_avg        1    3.5461 118.31 -615.71
    ## - cls_credits    1    6.2703 121.04 -605.17
    ## 
    ## Step:  AIC=-628.36
    ## score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## <none>                       115.12 -628.36
    ## - language       1    0.6192 115.74 -627.88
    ## - age            1    0.9342 116.06 -626.62
    ## - ethnicity      1    1.8997 117.02 -622.79
    ## - cls_perc_eval  1    3.1769 118.30 -617.76
    ## - gender         1    3.4709 118.59 -616.61
    ## - bty_avg        1    4.0096 119.13 -614.51
    ## - cls_credits    1    6.1046 121.23 -606.44

``` r
summary(best_model)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

``` r
best_model_only_best <- stepAIC(lm_full_model, direction = "backward", trace = 0)
summary(best_model_only_best)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9067 -0.3103  0.0849  0.3712  1.0611 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.446967   0.203191  16.964  < 2e-16 ***
    ## ethnicitynot minority  0.204710   0.074710   2.740 0.006384 ** 
    ## gendermale             0.184780   0.049889   3.704 0.000238 ***
    ## languagenon-english   -0.161463   0.103213  -1.564 0.118427    
    ## age                   -0.005008   0.002606  -1.922 0.055289 .  
    ## cls_perc_eval          0.005094   0.001438   3.543 0.000436 ***
    ## cls_creditsone credit  0.515065   0.104860   4.912 1.26e-06 ***
    ## bty_avg                0.064996   0.016327   3.981 7.99e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.503 on 455 degrees of freedom
    ## Multiple R-squared:  0.1576, Adjusted R-squared:  0.1446 
    ## F-statistic: 12.16 on 7 and 455 DF,  p-value: 2.879e-14

score = 3.45 + (0.20 \* ethnicity) + (0.18 \* gender) +
(0.01*cls_perc_eval) + (0.52*cls_credits) + (0.06\* bty_avg)

## Exercise 16

Evaluation scores for males are expected on average to be 0.18 higher
than evaluation scores for females.

When all else is held constant, the slope of cls_perc_eval indicates
that for each additional percentage point that cls_perc_eval is higher,
the evaluation score is expected to be higher on average by 0.01.

## Exercise 17

Based on the final model, the characteristics of a professor and a
course at University of Texas at Austin that would be associated with a
high evaluation score would be a course that is one-credit where a
higher percentage of students complete the evaluation with a professor
who is male, not a minority, and has a higher beauty average.

``` r
m_best_full_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~  ethnicity + gender + cls_perc_eval + cls_credits + bty_avg, data = evals) %>% 
tidy()
view(m_best_full_model)

library(tidymodels)
library(broom)

R_m_best_full_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(score ~ ethnicity + gender + cls_perc_eval + cls_credits + bty_avg, data = evals)

broom::glance(R_m_best_full_model$fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.146         0.137 0.505      15.6 3.34e-14     5  -338.  690.  719.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
contrasts(evals$gender)
```

    ##        male
    ## female    0
    ## male      1

``` r
contrasts(evals$ethnicity)
```

    ##              not minority
    ## minority                0
    ## not minority            1

``` r
contrasts(evals$cls_credits)
```

    ##              one credit
    ## multi credit          0
    ## one credit            1

## Exercise 18

I would not be comfortable generalizing my conclusions to apply to
professors generally because the adjusted r square of the best model is
0.14 which indicates that only 14% of the variance in evaluation scores
is due to the predictors. Because less than a fifth of the variance is
explained, I would take great caution in generalizing my conclusions.
