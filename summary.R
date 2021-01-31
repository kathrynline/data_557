# Summary of GHSI summary
data <- read.csv(file = './raw_data/ghsi_summary.csv')
print('Summary of GHSI data')

# Split data into different categories
overall <- data[c(1, 2)]
prev_emergence_pathogens <- data[c(3, 4)]
early_detection <- data[c(5, 6)]
rapid_response <- data[c(7, 8)]
robust_health_sector <- data[c(9, 10)]
commitments <- data[c(11,12)]
risk_environment <- data[c(13,14)]

dfs <-
  list(
    overall,
    prev_emergence_pathogens,
    early_detection,
    rapid_response,
    robust_health_sector,
    commitments,
    risk_environment
  )

# Join into a single data frame
for (i in (1:length(dfs))){
  colnames(dfs[[i]])[1] <- 'country'
}
df <- Reduce(function(x,y) merge(x,y,by='country',all=TRUE), dfs)

# Print summary statistics
print(paste('Country count: ', nrow(overall)))
print('Summary statistics:')
print(t(sapply(df[-c(1)], function(x) c(mean=mean(x), sd=sd(x)))))

# Extract set of countries
ghsi_countries <- data$country

# Summary of population data
data <- read.csv(file = './raw_data/world_bank_population.csv')
print('Summary of population data')
wb_countries <-data[[1]]

# Note that not every row is a country so the data cannot be summarized
print(paste('Country count: ', nrow(data)))

# Use coronavirus data to find mismatched countries
cases <- read.csv(file = './raw_data/jhu_cases_01.26.2021.csv')
deaths <- read.csv(file = './raw_data/jhu_deaths_01.26.2021.csv')

jhu_countries <- unique(cases$Country.Region)
jhu_states <- unique(cases$Province.State)

print('Countries in GHSI but not JHU')
print(ghsi_countries[!(ghsi_countries %in% jhu_countries)])
print('Countries in JHU but not GHSI')
print(jhu_countries[!(jhu_countries %in% ghsi_countries)])
print('Countries in GHSI but not WB')
print(ghsi_countries[!(ghsi_countries %in% wb_countries)])
print('Countries in WB but not GHSI')
print(wb_countries[!(wb_countries %in% ghsi_countries)])
print('Countries broken into regions (need to add these together)')
print(unique(cases$Country.Region[cases$Province.State != '']))

# Summary of merged data
data <- read.csv(file = './prepped_data/all_data_cumulative.csv')
print('Summary of merged data')

print(paste('Country count: ', nrow(data)))

print(t(sapply(data[-c(1, 2, 3
)], function(x) c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE), na=sum(is.na(x)))))
)
