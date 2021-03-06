<<<<<<< HEAD
library(tidyverse)
library(lubridate)


# reading in files

cases = read.csv(".\\intermediate_data\\jhu_cases_01.26.2021.csv", check.names = FALSE)

deaths = read.csv(".\\intermediate_data\\jhu_deaths_01.26.2021.csv", check.names = FALSE)

population = read.csv(".\\intermediate_data\\world_bank_population.csv", check.names = FALSE)

indicators <- read.csv(".\\intermediate_data\\ghsi_summary.csv", check.names = FALSE)

islands <- read.csv(".\\intermediate_data\\island_countries.csv", check.names = FALSE)

gdp <- read.csv(".\\intermediate_data\\world_bank_gdp.csv", check.names = FALSE)

democracy <- read.csv(".\\intermediate_data\\democracy_index.csv", check.names = FALSE)


#pivoting dates into rows and grouping by country code and date

cases_country_date <- cases[7:ncol(cases)]

cases_pivot <- pivot_longer(cases_country_date, cols= !contains("country_code"), names_to = "Date", values_to = "Cases")

cases_pivot_grouped <- aggregate(cases_pivot$Cases, by = list(Category = cases_pivot$country_code, cases_pivot$Date), FUN=sum)

deaths_country_date <- deaths[7:ncol(cases)]

deaths_pivot <- pivot_longer(deaths_country_date, cols= !contains("country_code"), names_to = "Date", values_to = "Deaths")

deaths_pivot_grouped <- aggregate(deaths_pivot$Deaths, by = list(Category = deaths_pivot$country_code, deaths_pivot$Date), FUN=sum)


#renaming columns

cases_pivot_grouped <- cases_pivot_grouped %>% 
  rename(
    country_code = Category,
    Date = Group.2,
    Cases = x
  )

deaths_pivot_grouped <- deaths_pivot_grouped %>% 
  rename(
    country_code = Category,
    Date = Group.2, 
    Deaths = x
  )

#merging deaths and cases

deaths_and_cases <- merge(cases_pivot_grouped, deaths_pivot_grouped, by = c("country_code"="country_code", "Date"="Date"), all=TRUE)


# nonzero cases matrix, 

nonzero_cases <- deaths_and_cases[deaths_and_cases$Cases >0, ]

nonzero_cases$clean_date <- as.Date(str_replace_all(nonzero_cases$Date, "X", ""), "%m.%d.%y")

#sorting by date
nonzero_cases_sort <- nonzero_cases %>%
  group_by(country_code) %>%
  arrange(clean_date) %>%
  mutate(day_since_first_case=row_number())


#merging nonzero cases with population data
deaths_cases_population <- merge(nonzero_cases_sort, population, by = c("country_code"="country_code"), all.x = TRUE)

#calculating deaths, cases per capita and case fatality ratio 
deaths_cases_population$casepc <- (deaths_cases_population$Cases / deaths_cases_population$pop_2019) * 1000
deaths_cases_population$deathpc <- (deaths_cases_population$Deaths / deaths_cases_population$pop_2019) * 1000
deaths_cases_population$cfratio <- (deaths_cases_population$Deaths / deaths_cases_population$Cases) * 100

# Add gdp per capita
deaths_cases_population_gdp <- merge(deaths_cases_population, gdp, by = c("country_code"="country_code"), all = TRUE)

#deaths_cases_indicators <- merge(deaths_cases_population_gdp, indicators, by =c("country_code"="country_code"), all =TRUE)

# Add democracy data

deaths_cases_indicators_democracy <- merge(deaths_cases_population_gdp, democracy, by = c("country_code"="country_code"), all = TRUE)

deaths_cases_indicators <- merge(deaths_cases_indicators_democracy, indicators, by =c("country_code"="country_code"), all =TRUE)

# Cleaning up columns

deaths_cases_indicators <- deaths_cases_indicators %>% select(country_code, Cases, Deaths, clean_date, day_since_first_case, 
                                                              pop_2019, casepc, deathpc, cfratio, overall, prev_emergence_pathogens, early_detection,
                                                              rapid_response, robust_health_sector, commitments, risk_environment, gdp_pc, X2019)

# Add the island indicator

deaths_cases_indicators$is_island = deaths_cases_indicators$country_code %in% islands$country_code

# returns filtered dataframe for num_days after first case in country
# input a dataframe and a number of days after the start of an outbreak
sum_cases <- function(df, num_days) {
  return(df[df$day_since_first_case == num_days, ])
  
}


one_month <- sum_cases(deaths_cases_indicators, 30)
two_month <- sum_cases(deaths_cases_indicators, 60)
six_month <- sum_cases(deaths_cases_indicators, 180)
twelve_month <- sum_cases(deaths_cases_indicators, 360)

write.csv(one_month,".\\prepped_data\\onemonth.csv", row.names = FALSE)
write.csv(two_month,".\\prepped_data\\twomonth.csv", row.names = FALSE)
write.csv(six_month,".\\prepped_data\\sixmonth.csv", row.names = FALSE)
write.csv(twelve_month,".\\prepped_data\\twelvemonth.csv", row.names = FALSE)

=======
library(tidyverse)
library(lubridate)


# reading in files

cases = read.csv(".\\intermediate_data\\jhu_cases_01.26.2021.csv", check.names = FALSE)

deaths = read.csv(".\\intermediate_data\\jhu_deaths_01.26.2021.csv", check.names = FALSE)

population = read.csv(".\\intermediate_data\\world_bank_population.csv", check.names = FALSE)

indicators <- read.csv(".\\intermediate_data\\ghsi_summary.csv", check.names = FALSE)

islands <- read.csv(".\\intermediate_data\\island_countries.csv", check.names = FALSE)

gdp <- read.csv(".\\intermediate_data\\world_bank_gdp.csv", check.names = FALSE)

democracy <- read.csv(".\\intermediate_data\\democracy_index.csv", check.names = FALSE)


#pivoting dates into rows and grouping by country code and date

cases_country_date <- cases[7:ncol(cases)]

cases_pivot <- pivot_longer(cases_country_date, cols= !contains("country_code"), names_to = "Date", values_to = "Cases")

cases_pivot_grouped <- aggregate(cases_pivot$Cases, by = list(Category = cases_pivot$country_code, cases_pivot$Date), FUN=sum)

deaths_country_date <- deaths[7:ncol(cases)]

deaths_pivot <- pivot_longer(deaths_country_date, cols= !contains("country_code"), names_to = "Date", values_to = "Deaths")

deaths_pivot_grouped <- aggregate(deaths_pivot$Deaths, by = list(Category = deaths_pivot$country_code, deaths_pivot$Date), FUN=sum)


#renaming columns

cases_pivot_grouped <- cases_pivot_grouped %>% 
  rename(
    country_code = Category,
    Date = Group.2,
    Cases = x
  )

deaths_pivot_grouped <- deaths_pivot_grouped %>% 
  rename(
    country_code = Category,
    Date = Group.2, 
    Deaths = x
  )

#merging deaths and cases

deaths_and_cases <- merge(cases_pivot_grouped, deaths_pivot_grouped, by = c("country_code"="country_code", "Date"="Date"), all=TRUE)


# nonzero cases matrix, 

nonzero_cases <- deaths_and_cases[deaths_and_cases$Cases >0, ]

nonzero_cases$clean_date <- as.Date(str_replace_all(nonzero_cases$Date, "X", ""), "%m.%d.%y")

#sorting by date
nonzero_cases_sort <- nonzero_cases %>%
  group_by(country_code) %>%
  arrange(clean_date) %>%
  mutate(day_since_first_case=row_number())


#merging nonzero cases with population data
deaths_cases_population <- merge(nonzero_cases_sort, population, by = c("country_code"="country_code"), all.x = TRUE)

#calculating deaths, cases per capita and case fatality ratio 
deaths_cases_population$casepc <- (deaths_cases_population$Cases / deaths_cases_population$pop_2019) * 1000
deaths_cases_population$deathpc <- (deaths_cases_population$Deaths / deaths_cases_population$pop_2019) * 1000
deaths_cases_population$cfratio <- (deaths_cases_population$Deaths / deaths_cases_population$Cases) * 100

# Add gdp per capita
deaths_cases_population_gdp <- merge(deaths_cases_population, gdp, by = c("country_code"="country_code"), all = TRUE)

#deaths_cases_indicators <- merge(deaths_cases_population_gdp, indicators, by =c("country_code"="country_code"), all =TRUE)

# Add democracy data

deaths_cases_indicators_democracy <- merge(deaths_cases_population_gdp, democracy, by = c("country_code"="country_code"), all = TRUE)

deaths_cases_indicators <- merge(deaths_cases_indicators_democracy, indicators, by =c("country_code"="country_code"), all =TRUE)

# Cleaning up columns

deaths_cases_indicators <- deaths_cases_indicators %>% select(country_code, Cases, Deaths, clean_date, day_since_first_case, 
                                   pop_2019, casepc, deathpc, cfratio, overall, prev_emergence_pathogens, early_detection,
                                   rapid_response, robust_health_sector, commitments, risk_environment, gdp_pc, X2019)

# Add the island indicator

deaths_cases_indicators$is_island = deaths_cases_indicators$country_code %in% islands$country_code

# returns filtered dataframe for num_days after first case in country
# input a dataframe and a number of days after the start of an outbreak
sum_cases <- function(df, num_days) {
  return(df[df$day_since_first_case == num_days, ])
  
}


one_month <- sum_cases(deaths_cases_indicators, 30)
two_month <- sum_cases(deaths_cases_indicators, 60)
six_month <- sum_cases(deaths_cases_indicators, 180)
twelve_month <- sum_cases(deaths_cases_indicators, 360)

write.csv(one_month,".\\prepped_data\\onemonth.csv", row.names = FALSE)
write.csv(two_month,".\\prepped_data\\twomonth.csv", row.names = FALSE)
write.csv(six_month,".\\prepped_data\\sixmonth.csv", row.names = FALSE)
write.csv(twelve_month,".\\prepped_data\\twelvemonth.csv", row.names = FALSE)


>>>>>>> origin/main
