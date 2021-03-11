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
current.gdp = sixmonth$gdp_pc
current.island = sixmonth$is_island
current.democracy = sixmonth$democracy_index

current.x = data.frame(current.prevent, current.detect, current.rapid, current.hs, current.compliance, current.risk)
#no intercept, current.coefs sums to 100

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

cfr.overall = current.prevent*cfr.prevent + current.detect*cfr.detect + current.rapid*cfr.rapid + 
  current.hs*cfr.hs + current.compliance*cfr.compliance + current.risk*cfr.risk #no intercept ???

### normalization of multilinear model
cfr.overall = rescale(cfr.overall, c(100,0))
#cfr.overall = rescale(cfr.overall, mean = 0, sd = 1)

#### NEW CFR POST NORMALIZATION ####
cfr.new = lm(sixmonth$cfratio ~ cfr.overall)
summary(cfr.new)$coef
summary(cfr.new)$r.squared

par(mfrow=c(1,1))
plot(sixmonth$cfratio ~ cfr.overall, ylab = "Case Fatality Percentage", 
     xlab = "Re-weighted overall score", pch = 20, col = "black")
abline(cfr.new)
summary(cfr.new)


####################
### ADD IN CONFOUNDING VARIABLES ###
cfr.redone = lm(cfratio ~ prev_emergence_pathogens + early_detection + rapid_response + robust_health_sector 
                + commitments + risk_environment + gdp_pc + is_island + democracy_index, data = sixmonth)
summary(cfr.redone)$coef
summary(cfr.redone)$r.squared

par(mfrow=c(2,2),mar=c(5,4,2,1))
plot(cfr.redone)

cfr2.intercept = summary(cfr.redone)$coef[1,1]
cfr2.prevent = summary(cfr.redone)$coef[2,1]
cfr2.detect = summary(cfr.redone)$coef[3,1]
cfr2.rapid = summary(cfr.redone)$coef[4,1]
cfr2.hs = summary(cfr.redone)$coef[5,1]
cfr2.compliance = summary(cfr.redone)$coef[6,1]
cfr2.risk = summary(cfr.redone)$coef[7,1]
cfr2.gdp = summary(cfr.redone)$coef[8,1]
cfr2.island = summary(cfr.redone)$coef[9,1]
cfr2.democracy = summary(cfr.redone)$coef[10,1]

cfr2.coefs = data.frame(cfr2.intercept, cfr2.prevent, cfr2.detect, cfr2.rapid, cfr2.hs, 
                        cfr2.compliance, cfr2.risk, cfr2.gdp, cfr2.island, cfr2.democracy)

cfr2.overall = current.prevent*cfr2.prevent + current.detect*cfr2.detect + current.rapid*cfr2.rapid + 
  current.hs*cfr2.hs + current.compliance*cfr2.compliance + current.risk*cfr2.risk +
  current.gdp*cfr2.gdp + current.island*cfr2.island + current.democracy*cfr2.democracy#no intercept ???

cfr2.overall = rescale(cfr2.overall, c(100,0)) #is it valid to just flip this?????

cfr2.new = lm(sixmonth$cfratio ~ cfr2.overall)
summary(cfr2.new)$coef
summary(cfr2.new)$r.squared

par(mfrow=c(1,1))
plot(sixmonth$cfratio ~ cfr2.overall, ylab = "Case Fatality Ratio", xlab = "Re-weighted overall score", 
     main = "Case fatality ratio plotted against new overall GHSI Score + outliers", col = "blue")
abline(cfr2.new)
summary(cfr2.new)

####################


###### ORIGINAL - simple linear regression model ######

cfr.simple = lm(cfratio ~ overall, data = sixmonth)
summary(cfr.simple)$coef
summary(cfr.simple)$r.squared

par(mfrow=c(1,1))
plot(cfratio ~ overall, data = sixmonth, ylab = "Case Fatality Ratio", xlab = "Overall score", 
     main = "Case fatality ratio plotted against overall GHSI Score", col = "blue")
abline(cfr.simple)
summary(cfr.simple)
