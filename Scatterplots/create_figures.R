library(ggpubr)

six_month_screened=na.omit(read.csv("C:\\Users\\edwar\\OneDrive - UW\\DATA 557\\Final Project\\data_557\\prepped_data\\six_month_outlier_screened.csv", check.names = FALSE))

ggscatter(six_month_screened,x='overall',y='cfratio',
          add='reg.line',cor.coef=FALSE,cor.method='pearson',
          xlab='Overall GHSI Score',ylab='Case Fatality Percentage',) + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1))

ggscatter(six_month_screened,x='prev_emergence_pathogens',y='cfratio',
          add='reg.line',cor.coef=FALSE,cor.method='pearson',
          xlab='Prevention',ylab='Case Fatality Percentage',) + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1))


ggscatter(six_month_screened,x='gdp_pc',y='cfratio',
          add='reg.line',cor.coef=TRUE,cor.method='pearson',
          xlab='GDP per-capita',ylab='Case Fatality Percentage (Deaths/Cases)',) + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0))

ggscatter(six_month_screened,x='gdp_pc',y='early_detection',
          add='reg.line',cor.coef=TRUE,cor.method='pearson',
          xlab='GDP per-capita',ylab='Early Detection',) + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0))


round(summary(lm(cfratio ~ gdp_pc, data = six_month_screened))$coefficients,6)