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
  current.hs*cfr.hs + current.compliance*cfr.compliance + current.risk*cfr.risk #removed intercept for now

#### NEW CFR POST NORMALIZATION ####
cfr.new = lm(sixmonth$cfratio ~ cfr.overall)
summary(cfr.new)$coef
summary(cfr.new)$r.squared

####
par(mfrow=c(1,1))
plot(sixmonth$cfratio ~ cfr.overall, xlab = "Case Fatality Ratio", ylab = "Re-weighted overall score", 
     main = "Case fatality ratio plotted against new overall GHSI Score", col = "blue")
abline(cfr.new)
summary(cfr.new)
