---
title: "Basic Regression"
author: "Corina Geier"
date: "2/21/2021"
output: pdf_document
---





## Question 1

## Cases per capita with overall GHSI score


```r
summary(lm(casepc ~ overall, data = sixmonth_data))$coef
```

```
##               Estimate Std. Error   t value   Pr(>|t|)
## (Intercept) 1.34705619 1.35949572 0.9908499 0.32311148
## overall     0.06208331 0.03085892 2.0118435 0.04575308
```

```r
par(mfrow=c(2,2),mar=c(5,4,2,1))

plot(lm(casepc ~ overall, data = sixmonth_data))
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

Coefficient estimate: 0.0621

pvalue = 0.046

## Deaths per capita with overall GHSI score



```r
summary(lm(deathpc ~ overall, data = sixmonth_data))$coef
```

```
##                 Estimate   Std. Error   t value     Pr(>|t|)
## (Intercept) -0.084278158 0.0415788685 -2.026947 4.416819e-02
## overall      0.004615341 0.0009437902  4.890219 2.250551e-06
```

```r
par(mfrow=c(2,2),mar=c(5,4,2,1))

plot(lm(deathpc ~ overall, data = sixmonth_data))
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

Coefficient estimate: 0.0046

pvalue < .0001

## Case Fatality Ratio with overall GHSI score 



```r
summary(lm(cfratio ~ overall, data = sixmonth_data))$coef
```

```
##                Estimate Std. Error    t value     Pr(>|t|)
## (Intercept) -0.33252295 0.57139214 -0.5819523 5.613393e-01
## overall      0.07363439 0.01296991  5.6773234 5.509572e-08
```

```r
par(mfrow=c(2,2),mar=c(5,4,2,1))

plot(lm(cfratio ~ overall, data = sixmonth_data))
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

Coefficient estimate: 0.074

pvalue < .0001


## Question 2

## Cases per capita with each subcomponent


```r
subcomponents <- c("prev_emergence_pathogens", "early_detection", "rapid_response", "robust_health_sector", "commitments", "risk_environment")

for(i in 1:length(subcomponents)) {
  predictor_i <- (subcomponents)[i]
  print(predictor_i)
  print(summary(lm(casepc ~ ., data = sixmonth_data[,c("casepc",predictor_i)]))$coef)
  par(mfrow=c(2,2),mar=c(5,4,2,1))
  plot(lm(casepc ~ ., data = sixmonth_data[,c("casepc",predictor_i)]))
}
```

```
## [1] "prev_emergence_pathogens"
##                            Estimate Std. Error  t value   Pr(>|t|)
## (Intercept)              2.03616007 1.05282360 1.933999 0.05470819
## prev_emergence_pathogens 0.05222066 0.02632479 1.983706 0.04883468
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```
## [1] "early_detection"
##                   Estimate Std. Error  t value    Pr(>|t|)
## (Intercept)     2.98123290 0.96009453 3.105145 0.002215175
## early_detection 0.02148599 0.01917076 1.120768 0.263904789
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-4-2.pdf)<!-- --> 

```
## [1] "rapid_response"
##                 Estimate Std. Error  t value  Pr(>|t|)
## (Intercept)    1.3121490 1.24044803 1.057802 0.2915870
## rapid_response 0.0660832 0.02924656 2.259521 0.0250703
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-4-3.pdf)<!-- --> 

```
## [1] "robust_health_sector"
##                        Estimate Std. Error  t value   Pr(>|t|)
## (Intercept)          2.13989642 0.83099480 2.575102 0.01083826
## robust_health_sector 0.06467009 0.02554554 2.531561 0.01222599
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-4-4.pdf)<!-- --> 

```
## [1] "commitments"
##                Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)  7.10083850 1.81375263  3.914998 0.0001288596
## commitments -0.06348428 0.03536646 -1.795042 0.0743531599
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-4-5.pdf)<!-- --> 

```
## [1] "risk_environment"
##                    Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)      -1.5801667 1.47958117 -1.067982 0.2869828581
## risk_environment  0.0993726 0.02553893  3.891025 0.0001411628
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-4-6.pdf)<!-- --> 



## Deaths per capita with each subcomponent


```r
subcomponents <- c("prev_emergence_pathogens", "early_detection", "rapid_response", "robust_health_sector", "commitments", "risk_environment")

for(i in 1:length(subcomponents)) {
  predictor_i <- (subcomponents)[i]
  print(predictor_i)
  print(summary(lm(deathpc ~ ., data = sixmonth_data[,c("deathpc",predictor_i)]))$coef)
  par(mfrow=c(2,2),mar=c(5,4,2,1))
  plot(lm(deathpc ~ ., data = sixmonth_data[,c("deathpc",predictor_i)]))
}
```

```
## [1] "prev_emergence_pathogens"
##                              Estimate   Std. Error   t value     Pr(>|t|)
## (Intercept)              -0.033147977 0.0322456879 -1.027982 3.053612e-01
## prev_emergence_pathogens  0.003884845 0.0008062709  4.818287 3.099873e-06
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

```
## [1] "early_detection"
##                    Estimate   Std. Error   t value     Pr(>|t|)
## (Intercept)     0.005811494 0.0298091995 0.1949564 0.8456506927
## early_detection 0.002301133 0.0005952175 3.8660372 0.0001551698
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-5-2.pdf)<!-- --> 

```
## [1] "rapid_response"
##                    Estimate   Std. Error    t value     Pr(>|t|)
## (Intercept)    -0.031463871 0.0389541543 -0.8077154 0.4203385866
## rapid_response  0.003519134 0.0009184382  3.8316499 0.0001766153
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-5-3.pdf)<!-- --> 

```
## [1] "robust_health_sector"
##                         Estimate   Std. Error   t value     Pr(>|t|)
## (Intercept)          0.005398486 0.0257055625 0.2100124 8.338996e-01
## robust_health_sector 0.003703231 0.0007902124 4.6863743 5.528390e-06
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-5-4.pdf)<!-- --> 

```
## [1] "commitments"
##                Estimate Std. Error   t value  Pr(>|t|)
## (Intercept) 0.016526439 0.05853463 0.2823361 0.7780157
## commitments 0.001846263 0.00114137 1.6175856 0.1075326
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-5-5.pdf)<!-- --> 

```
## [1] "risk_environment"
##                     Estimate   Std. Error   t value     Pr(>|t|)
## (Intercept)      -0.11767540 0.0463950265 -2.536380 1.206497e-02
## risk_environment  0.00407107 0.0008008207  5.083623 9.362623e-07
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-5-6.pdf)<!-- --> 

## Case Fatality Ratio with each subcomponent


```r
subcomponents <- c("prev_emergence_pathogens", "early_detection", "rapid_response", "robust_health_sector", "commitments", "risk_environment")

for(i in 1:length(subcomponents)) {
  predictor_i <- (subcomponents)[i]
  print(predictor_i)
  print(summary(lm(cfratio ~ ., data = sixmonth_data[,c("cfratio",predictor_i)]))$coef)
  par(mfrow=c(2,2),mar=c(5,4,2,1))
  plot(lm(cfratio ~ ., data = sixmonth_data[,c("cfratio",predictor_i)]))
}
```

```
## [1] "prev_emergence_pathogens"
##                            Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)              0.41630850 0.44104052 0.9439235 3.464954e-01
## prev_emergence_pathogens 0.06381554 0.01102777 5.7868016 3.196661e-08
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

```
## [1] "early_detection"
##                  Estimate  Std. Error  t value     Pr(>|t|)
## (Intercept)     1.0189258 0.410319789 2.483248 1.394981e-02
## early_detection 0.0386378 0.008193093 4.715900 4.861683e-06
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-6-2.pdf)<!-- --> 

```
## [1] "rapid_response"
##                  Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)    0.45649178 0.53813891 0.8482787 3.974285e-01
## rapid_response 0.05749323 0.01268792 4.5313348 1.075448e-05
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-6-3.pdf)<!-- --> 

```
## [1] "robust_health_sector"
##                        Estimate Std. Error  t value     Pr(>|t|)
## (Intercept)          1.11897822 0.35459144 3.155683 1.882132e-03
## robust_health_sector 0.05833582 0.01090046 5.351683 2.673197e-07
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-6-4.pdf)<!-- --> 

```
## [1] "commitments"
##                Estimate Std. Error    t value    Pr(>|t|)
## (Intercept) -0.26959584 0.79337580 -0.3398085 0.734403638
## commitments  0.06049012 0.01547008  3.9101371 0.000131269
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-6-5.pdf)<!-- --> 

```
## [1] "risk_environment"
##                    Estimate Std. Error  t value    Pr(>|t|)
## (Intercept)      0.76031876 0.67896796 1.119815 0.264309685
## risk_environment 0.03568434 0.01171961 3.044840 0.002683559
```

![](Basic-Regression_files/figure-latex/unnamed-chunk-6-6.pdf)<!-- --> 

