# Plot each of the six sub-components against each of the outcome scores 
library(data.table)
library(ggplot2)

dt = fread("C:/Users/eklin/Documents/repos/data_557/prepped_data/sixmonth.csv")

# Drop NAs in overall score and/or cfratio - this should happen earlier in the prep step. 
dt = dt[!(is.na(overall) | is.na(cfratio))]

dt = dt[, .(deathpc, casepc, cfratio, prev_emergence_pathogens, early_detection, rapid_response, robust_health_sector, 
            commitments, risk_environment)]
dt = melt(dt, id.vars=c('deathpc', 'casepc', 'cfratio'), variable.name="subcomponent", value.name='score')

pdf("C:/Users/eklin/Documents/repos/data_557/subcomponent_plots.pdf")
ggplot(data=dt, aes(x=score, y=deathpc, color=subcomponent)) + 
  geom_smooth(se=F) + 
  theme_bw() + 
  labs(title = "Deaths-per-capita", x="Subcomponent score (0-100)", y="Deaths-per-capita")

ggplot(data=dt, aes(x=score, y=casepc, color=subcomponent)) + 
  geom_smooth(se=F) + 
  theme_bw() + 
  labs(title = "Cases-per-capita", x="Subcomponent score (0-100)", y="Cases-per-capita")

ggplot(data=dt, aes(x=score, y=cfratio, color=subcomponent)) + 
  geom_smooth(se=F) + 
  theme_bw() + 
  labs(title = "Case-fatality ratio", x="Subcomponent score (0-100)", y="Case-fatality ratio")


dev.off()
