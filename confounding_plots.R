# Read in data source
data <- read.csv(file = './prepped_data/six_month_outlier_screened.csv')

# Generate island nation plot
png('figures/confounding_island.png')
plot(data$is_island, data$cfratio, pch=20, xlab="Island nation", ylab="Case fatality percentage")
abline(lm(data$cfratio ~ data$is_island))
dev.off()

# Generate gdp plot
png('figures/confounding_gdp.png')
plot(data$gdp_pc, data$cfratio, pch=20, xlab="GDP per-capita", ylab="Case fatality percentage")
abline(lm(data$cfratio ~ data$gdp_pc))
dev.off()

# Generate democracy index plot
png('figures/confounding_democracy.png')
plot(data$democracy_index, data$cfratio, pch=20, xlab="Democracy index", ylab="Case fatality percentage")
abline(lm(data$cfratio ~ data$democracy_index))
dev.off()