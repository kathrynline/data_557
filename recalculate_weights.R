# Attempt to recalculate weights 

library(data.table) 

dt = fread("C:/Users/eklin/Documents/repos/data_557/prepped_data/ghsi.csv")

# Pull in expert panel weights from report. 
# The report can be found here: https://www.ghsindex.org/wp-content/uploads/2020/04/2019-Global-Health-Security-Index.pdf
# And the table of weights is on p. 77. 
expert_weights = data.table('prev_emergence_wt'= .163,
                            'early_detection_wt'=.192,
                            'rapid_response_wt'=.192,
                            'robust_hs_wt'=.167,
                            'commitments_wt'=.158,
                            'risk_environment_wt'=.128)

dt = cbind(dt, expert_weights)

# Recalculate weights 
dt[, prev_emergence_new:=prev_emergence_pathogens*prev_emergence_wt]
dt[, early_detection_new:=early_detection*early_detection_wt]
dt[, rapid_response_new:=rapid_response*rapid_response_wt]
dt[, robust_hs_new:=robust_health_sector*robust_hs_wt]
dt[, commitments_new:=commitments*commitments_wt]
dt[, risk_environment_new:=risk_environment*risk_environment_wt]

dt[, overall_new:=prev_emergence_new + early_detection_new + rapid_response_new + robust_hs_new + commitments_new + risk_environment_new]
dt[, overall_new:=round(overall_new, digits=1)]

# Can get pretty close! Just a little bit off. 
# This shows no errors. 
print(dt[abs(overall-overall_new)>.2, .(overall, overall_new)])

# This shows a few rounding errors. 
print(dt[overall!=overall_new, .(overall, overall_new)])

