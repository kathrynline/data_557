# Emily Linebarger
# Plots for outlier screening section of final report 

library(data.table)

# Set working directory to root of repository 
stopifnot(grepl("data_557", getwd()))
dt = fread("prepped_data/sixmonth.csv")
outlier_screened = fread("prepped_data/six_month_outlier_screened.csv")
setwd("final_report_pngs")

png("outlier_initial_hist.png")
p = hist(dt$cfratio, main="Histogram of case-fatality ratio")
dev.off()

png("outlier_initial_boxplot.png")
boxplot(dt$cfratio, ylab="Case-fatality ratio")
dev.off()

png("regression_1_qq_plot.png")
plot(lm(cfratio ~ overall, data=dt), which=2)
dev.off()

png("regression_1_qq_plot_post_outliers.png")
plot(lm(cfratio ~ overall, data=outlier_screened), which=2)
dev.off()