library(scales)

#Data - 6 months
sixmonth = read.csv("C:\\Users\\riggi\\OneDrive\\Documents\\MSDS\\DATA 557\\Final Project\\prepped_data\\sixmonth.csv")
sixmonth = na.omit(sixmonth) #omits 13 NA's
colnames(sixmonth)

#current overall weights
current.prevent = sixmonth$prev_emergence_pathogens
current.detect = sixmonth$early_detection
current.rapid = sixmonth$rapid_response
current.hs = sixmonth$robust_health_sector
current.compliance = sixmonth$commitments
current.risk = sixmonth$risk_environment

current.x = data.frame(current.prevent, current.detect, current.rapid, current.hs, current.compliance, current.risk)
#no intercept, current.coefs sums to 100

################################

#Cases per capita multilinear regression
cases.multi = lm(casepc ~ prev_emergence_pathogens + early_detection + rapid_response + robust_health_sector 
                  + commitments + risk_environment, data = sixmonth)
summary(cases.multi)$coef
summary(cases.multi)$r.squared

par(mfrow=c(2,2),mar=c(5,4,2,1))
plot(cases.multi)

####

cases.intercept = summary(cases.multi)$coef[1,1]
cases.prevent = summary(cases.multi)$coef[2,1]
cases.detect = summary(cases.multi)$coef[3,1]
cases.rapid = summary(cases.multi)$coef[4,1]
cases.hs = summary(cases.multi)$coef[5,1]
cases.compliance = summary(cases.multi)$coef[6,1]
cases.risk = summary(cases.multi)$coef[7,1]

cases.coefs = data.frame(cases.intercept, cases.prevent, cases.detect, cases.rapid, cases.hs, cases.compliance, cases.risk)

#cases.x = sixmonth$casepc
#cases.x = rescale(cases.x, to = c(0,100)) #normalize??? need to check this
#cases.x = format(cases.x, scientific = F) #get rid of scientific notation

cases.overall = cases.intercept + current.prevent*cases.prevent + current.detect*cases.detect + current.rapid*cases.rapid + 
  current.hs*cases.hs + current.compliance*cases.compliance + current.risk*cases.risk

par(mfrow=c(1,1))
plot(sixmonth$casepc ~ cases.overall)
abline(lm(sixmonth$casepc ~ cases.overall))
summary(lm(sixmonth$casepc ~ cases.overall))

################################

#Deaths per capita multilinear regression
deaths.multi = lm(deathpc ~ prev_emergence_pathogens + early_detection + rapid_response + robust_health_sector 
                 + commitments + risk_environment, data = sixmonth)
summary(deaths.multi)$coef
summary(deaths.multi)$r.squared

par(mfrow=c(2,2),mar=c(5,4,2,1))
plot(deaths.multi)

####

deaths.intercept = summary(deaths.multi)$coef[1,1]
deaths.prevent = summary(deaths.multi)$coef[2,1]
deaths.detect = summary(deaths.multi)$coef[3,1]
deaths.rapid = summary(deaths.multi)$coef[4,1]
deaths.hs = summary(deaths.multi)$coef[5,1]
deaths.compliance = summary(deaths.multi)$coef[6,1]
deaths.risk = summary(deaths.multi)$coef[7,1]

deaths.coefs = data.frame(deaths.intercept, deaths.prevent, deaths.detect, deaths.rapid, deaths.hs, deaths.compliance, deaths.risk)

deaths.overall = deaths.intercept + current.prevent*deaths.prevent + current.detect*deaths.detect + current.rapid*deaths.rapid + 
  current.hs*deaths.hs + current.compliance*deaths.compliance + current.risk*deaths.risk

par(mfrow=c(1,1))
plot(sixmonth$deathpc ~ deaths.overall)
abline(lm(sixmonth$deathpc ~ deaths.overall))
summary(lm(sixmonth$deathpc ~ deaths.overall))

################################

#Case fatality ratio multilinear regression
cfr.multi = lm(cfratio ~ prev_emergence_pathogens + early_detection + rapid_response + robust_health_sector 
                 + commitments + risk_environment, data = sixmonth)
summary(cfr.multi)$coef
summary(cfr.multi)$r.squared

par(mfrow=c(2,2),mar=c(5,4,2,1))
plot(cfr.multi)

####

cfr.intercept = summary(cfr.multi)$coef[1,1]
cfr.prevent = summary(cfr.multi)$coef[2,1]
cfr.detect = summary(cfr.multi)$coef[3,1]
cfr.rapid = summary(cfr.multi)$coef[4,1]
cfr.hs = summary(cfr.multi)$coef[5,1]
cfr.compliance = summary(cfr.multi)$coef[6,1]
cfr.risk = summary(cfr.multi)$coef[7,1]

cfr.coefs = data.frame(cfr.intercept, cfr.prevent, cfr.detect, cfr.rapid, cfr.hs, cfr.compliance, cfr.risk)

cfr.overall = cfr.intercept + current.prevent*cfr.prevent + current.detect*cfr.detect + current.rapid*cfr.rapid + 
  current.hs*cfr.hs + current.compliance*cfr.compliance + current.risk*cfr.risk

par(mfrow=c(1,1))
plot(sixmonth$cfratio ~ cfr.overall)
abline(lm(sixmonth$cfratio ~ cfr.overall))
summary(lm(sixmonth$cfratio ~ cfr.overall))
