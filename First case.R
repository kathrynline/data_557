library(tidyverse)
library(lubridate)

#Calculates start date for each country

#col_num <- apply(cases[,5:ncol(cases)], 1, function(x){first_case = min(which(x>0))})

#first_case <- names(cases[,5:ncol(cases)])[col_num]

#cases <- cbind(first_case, cases)


# reading in files

cases = read.csv("./intermediate_data/jhu_cases_01.26.2021.csv", check.names = FALSE)

deaths = read.csv("./intermediate_data/jhu_deaths_01.26.2021.csv", check.names = FALSE)

population = read.csv("./intermediate_data/world_bank_population.csv", check.names = FALSE)

indicators <- read.csv("./intermediate_data/ghsi_summary.csv", check.names = FALSE)


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
deaths_cases_population <- merge(nonzero_cases_sort, population, by = c("country_code"="country_code"))

#caluclating deaths, cases per capita and case fatality ratio 
deaths_cases_population$casepc <- (deaths_cases_population$Cases / deaths_cases_population$pop_2019) * 1000
deaths_cases_population$deathpc <- (deaths_cases_population$Deaths / deaths_cases_population$pop_2019) * 1000
deaths_cases_population$cfratio <- (deaths_cases_population$Deaths / deaths_cases_population$Cases) * 100

deaths_cases_indicators <- merge(deaths_cases_population, indicators, by =c("country_code"="country_code"), all =TRUE)

# returns fileterd dataframe for num_days after first case in country
# input a dataframe and a number of days after the start of an outbreak
sum_cases <- function(df, num_days) {
  return(df[df$day_since_first_case == num_days, ])
  
}




