Exploring Std. Error of Rotated Components
================

Std. Error of Rotated Components
--------------------------------

While fitting a linear regression model using rotated components in principal component analysis, I noticed that the standard error of several rotated components was identical. I have created this document as a means of pinpointing the algorithm which is causing this behavior, varimax.

Loading the Data
----------------

For this analysis, I have used the HBAT dataset.

princomp using correlation matrix
---------------------------------

First, notice that the princomp function does not have this issue.

``` r
p1 = princomp(hbatReduced, cor=T)
recastData_p1 = as.data.frame(p1$scores[,1:4])
recastData_p1$Satis = hbatNumeric$x19
p1_fit = lm(Satis ~ ., data=recastData_p1)
summary(p1_fit)
```

    ## 
    ## Call:
    ## lm(formula = Satis ~ ., data = recastData_p1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6308 -0.4996  0.1372  0.4623  1.5228 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.91800    0.07089  97.589  < 2e-16 ***
    ## Comp.1      -0.46888    0.03829 -12.244  < 2e-16 ***
    ## Comp.2      -0.03940    0.04438  -0.888    0.377    
    ## Comp.3       0.03042    0.05451   0.558    0.578    
    ## Comp.4       0.39550    0.06801   5.816 8.09e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7089 on 95 degrees of freedom
    ## Multiple R-squared:  0.6605, Adjusted R-squared:  0.6462 
    ## F-statistic: 46.21 on 4 and 95 DF,  p-value: < 2.2e-16

princomp using covariance matrix
--------------------------------

Using correlation and covariance matrices does not produce identical standard errors, so we can feel certain that this is not a simple scaling issue.

``` r
p2 = princomp(hbatReduced, cor=F)
recastData_p2 = as.data.frame(p2$scores[,1:4])
recastData_p2$Satis = hbatNumeric$x19
p2_fit = lm(Satis ~ ., data=recastData_p2)
summary(p2_fit)
```

    ## 
    ## Call:
    ## lm(formula = Satis ~ ., data = recastData_p2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.59498 -0.42194  0.06637  0.39147  1.57012 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.91800    0.06860 100.840  < 2e-16 ***
    ## Comp.1       0.30044    0.03146   9.549 1.52e-15 ***
    ## Comp.2      -0.35238    0.03921  -8.987 2.42e-14 ***
    ## Comp.3       0.10218    0.04371   2.338   0.0215 *  
    ## Comp.4       0.29125    0.05669   5.137 1.48e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.686 on 95 degrees of freedom
    ## Multiple R-squared:  0.6821, Adjusted R-squared:  0.6687 
    ## F-statistic: 50.95 on 4 and 95 DF,  p-value: < 2.2e-16

psych::principal using correlation matrix
-----------------------------------------

When we switch to the principal function in psych, our issue appears.

``` r
p3 = psych::principal(hbatReduced, rotate="varimax", nfactors=4, scores=TRUE, covar=FALSE)
recastData_p3 = as.data.frame(p3$scores)
recastData_p3$Satis = hbatNumeric$x19
p3_fit = lm(Satis ~ ., data=recastData_p3)
summary(p3_fit)
```

    ## 
    ## Call:
    ## lm(formula = Satis ~ ., data = recastData_p3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6308 -0.4996  0.1372  0.4623  1.5228 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.91800    0.07089  97.589  < 2e-16 ***
    ## RC1          0.61805    0.07125   8.675 1.12e-13 ***
    ## RC2          0.50973    0.07125   7.155 1.74e-10 ***
    ## RC3          0.06714    0.07125   0.942    0.348    
    ## RC4          0.54032    0.07125   7.584 2.24e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7089 on 95 degrees of freedom
    ## Multiple R-squared:  0.6605, Adjusted R-squared:  0.6462 
    ## F-statistic: 46.21 on 4 and 95 DF,  p-value: < 2.2e-16

psych::principal using covariance matrix
----------------------------------------

Again, correlation vs. covariance matrix does not have any effect - in this case, both produce identical standard error in the regression model.

``` r
p4 = psych::principal(hbatReduced, rotate="varimax", nfactors=4, scores=TRUE, covar=TRUE)
recastData_p4 = as.data.frame(p4$scores)
recastData_p4$Satis = hbatNumeric$x19
p4_fit = lm(Satis ~ ., data=recastData_p4)
summary(p4_fit)
```

    ## 
    ## Call:
    ## lm(formula = Satis ~ ., data = recastData_p4)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.59498 -0.42194  0.06637  0.39147  1.57012 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.91800    0.06860 100.840  < 2e-16 ***
    ## RC1          0.53974    0.06895   7.828 6.92e-12 ***
    ## RC2          0.62742    0.06895   9.100 1.39e-14 ***
    ## RC3          0.07794    0.06895   1.130    0.261    
    ## RC4          0.52706    0.06895   7.644 1.68e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.686 on 95 degrees of freedom
    ## Multiple R-squared:  0.6821, Adjusted R-squared:  0.6687 
    ## F-statistic: 50.95 on 4 and 95 DF,  p-value: < 2.2e-16

psych::principal using promax instead of varimax
------------------------------------------------

Finally, when switching to the promax algorithm, our standard errors are no longer identical, leading us to conclude that it is the varimax algorithm which is producing these results.

``` r
p5 = psych::principal(hbatReduced, rotate="promax", nfactors=4, scores=TRUE, covar=TRUE)
recastData_p5 = as.data.frame(p5$scores)
recastData_p5$Satis = hbatNumeric$x19
p5_fit = lm(Satis ~ ., data=recastData_p5)
summary(p5_fit)
```

    ## 
    ## Call:
    ## lm(formula = Satis ~ ., data = recastData_p5)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.59498 -0.42194  0.06637  0.39147  1.57012 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.91800    0.06860 100.840  < 2e-16 ***
    ## RC1          0.54067    0.07881   6.860 6.95e-10 ***
    ## RC2          0.53213    0.07522   7.074 2.54e-10 ***
    ## RC3         -0.03046    0.07222  -0.422    0.674    
    ## RC4          0.59071    0.07407   7.975 3.39e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.686 on 95 degrees of freedom
    ## Multiple R-squared:  0.6821, Adjusted R-squared:  0.6687 
    ## F-statistic: 50.95 on 4 and 95 DF,  p-value: < 2.2e-16

Conclusion
----------

Research into this issue has been conducted by Zhang and Preacher, with their results published in the Journal of Educational and Behavioral Statistics: <http://www.quantpsy.org/pubs/zhang_preacher_2015.pdf>. In a review of factor rotation and standard errors, Zhang and Preacher conclude that, "When the factor structure has an independent cluster pattern, all rotation methods provide similar results. When the factor structure is less clear, different rotations can produce different asymptotic standard errors."
