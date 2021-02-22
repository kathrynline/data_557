---
title: "Reporting systems sub-analysis"
author: "Emily Linebarger"
date: "2/17/2021"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(data.table)

dt = fread("C:/Users/eklin/Documents/repos/data_557/prepped_data/sixmonth.csv")
```

First, run a simple scatter to see what the correlation between the overall score and the "early detection" sub-component looks like. 
```{r exploratory_analysis}
p = plot(dt$overall, dt$early_detection, main="Correlation check", xlab="Overall score", ylab="Early detection sub-component score")
print(p)
```

Ouch they are really highly correlated. 
Do a little bit of outlier screening
```{r outlier_screening} 
range = IQR(dt$casepc, na.rm=TRUE)
mean = mean(dt$casepc, na.rm=TRUE)
subset1 = dt[casepc<mean-range | casepc>mean+range]

range = IQR(dt$deathpc, na.rm=TRUE)
mean = mean(dt$deathpc, na.rm=TRUE)
subset2 = dt[deathpc<mean-range | deathpc>mean+range]

range = IQR(dt$cfratio, na.rm=TRUE)
mean = mean(dt$cfratio, na.rm=TRUE)
subset3 = dt[cfratio<mean-range | cfratio>mean+range]

dt = dt[!country_code %in% subset1$country_code]



```


```{r reg1}
#attach(dt)
summary(lm(casepc ~ overall))
plot(lm(casepc ~ overall))
```