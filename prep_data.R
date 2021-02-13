# AUTHOR: Emily Linebarger 
# PURPOSE: Take data with mapped country codes, and merge to create datasets for analysis. 
# DATE: 1/25/2021

rm(list=ls())
library(data.table) 
library(httr)

print("Running prep_data.R...")

#-------------------------------------------
# SET UP FILE PATHS 
#-------------------------------------------
# This is set up to run from the root of the repository
stopifnot(grepl("data_557", getwd()))
intermediate = "intermediate_data/"
prepped = "prepped_data/"


#-------------------------------------------
# FORMAT GHSI SUMMARY DATA 
#-------------------------------------------
ghsi_intermediate = fread(paste0(intermediate, "ghsi_summary.csv"), encoding = "UTF-8") #added encoding
ghsi_prepped = copy(ghsi_intermediate)
#-------------------
# Manual edits to go here
ghsi_prepped$country <- sub("St ", "Saint ", ghsi_prepped$country) #mapped St to Saint
ghsi_prepped$country <- iconv(ghsi_prepped$country, from = 'UTF-8', to = 'ASCII//TRANSLIT') #dropped accents
ghsi_prepped$country <- sub("Czech Republic", "Czechia", ghsi_prepped$country) #Czechia
ghsi_prepped$country <- sub("eSwatini \\(Swaziland\\)", "Eswatini", ghsi_prepped$country) #Eswatini
ghsi_prepped$country <- sub("Saint Vincent and The Grenadines", "Saint Vincent and the Grenadines", ghsi_prepped$country) #St Vincent
ghsi_prepped <- subset(ghsi_prepped, country != 'AVERAGE') #remove AVERAGE

ghsi_prepped$country <- NULL
write.csv(ghsi_prepped, paste0(prepped, "ghsi.csv"), row.names=F)
#-------------------------------------------
# FORMAT JHU CASES/DEATHS DATA 
#-------------------------------------------
jhu_cases = fread(paste0(intermediate, "jhu_cases_01.26.2021.csv"))
jhu_deaths = fread(paste0(intermediate, "jhu_deaths_01.26.2021.csv"))

format_jhu = function(data, col_name){
  data = data[, -c('Lat', 'Long', 'X')]
  data = setnames(data, c("Province.State"), c('state'))
  data = melt(data, id.vars=c('state', 'country', 'country_code'), variable.name="date", value.name=col_name)
  data[, date:=gsub('X', '', date)]
  return(data)
}

cases_prepped = format_jhu(jhu_cases, 'cases')
deaths_prepped = format_jhu(jhu_deaths, 'deaths')

# ---------------------
# Manual edits to go here 

cases_prepped$country <- sub("Burma", "Myanmar", cases_prepped$country) #Burma -> Myanmar
deaths_prepped$country <- sub("Burma", "Myanmar", deaths_prepped$country) #Burma -> Myanmar

cases_prepped$country <- sub("Congo \\(Kinshasa\\)", "Congo (Democratic Republic)", cases_prepped$country) #D.R.C
deaths_prepped$country <- sub("Congo \\(Kinshasa\\)", "Congo (Democratic Republic)", deaths_prepped$country) #D.R.C

cases_prepped$country <- sub("Kyrgyzstan", "Kyrgyz Republic", cases_prepped$country) #Kyrgyz Republic
deaths_prepped$country <- sub("Kyrgyzstan", "Kyrgyz Republic", deaths_prepped$country) #Kyrgyz Republic

cases_prepped$country <- sub("Korea\\, South", "South Korea", cases_prepped$country) #South Korea
deaths_prepped$country <- sub("Korea\\, South", "South Korea", deaths_prepped$country) #South Korea

cases_prepped$country <- sub("US", "United States", cases_prepped$country) #United States
deaths_prepped$country <- sub("US", "United States", deaths_prepped$country) #United States

# Drop state and country with this collapse. 
cases_prepped = cases_prepped[, .(cases=sum(cases)), by=c('country_code', 'date')]
deaths_prepped = deaths_prepped[, .(deaths=sum(deaths)), by=c('country_code', 'date')]

write.csv(cases_prepped, paste0(prepped, "jhu_cases.csv"), row.names=FALSE)
write.csv(deaths_prepped, paste0(prepped, "jhu_deaths.csv", row.names=FALSE))

all_jhu = merge(cases_prepped, deaths_prepped, by=c('country_code', 'date'), all=TRUE)
write.csv(all_jhu, paste0(prepped, "all_jhu.csv"))

#-------------------------------------------------
# RUN VALIDATION - WILL INFORM MANUAL EDITS ABOVE 
#-------------------------------------------------
print("These are the country codes in GHSI that are missing from JHU")
print(ghsi_prepped$country_code[!ghsi_prepped$country_code%in%all_jhu$country_code])

#-------------------------------------------
# CREATE COMBINED JHU-GHSI DATASET
#-------------------------------------------
all_data_with_date = merge(all_jhu, ghsi_prepped, by=c('country_code'), all=TRUE)

write.csv(all_data_with_date, paste0(prepped, "all_data_with_date.csv"))

#--------------------------------------------
# CREATE CUMULATIVE DATASET
#--------------------------------------------
# This assumes the data is cumulative, and grabs the last date of data available for 2020 by country. 
all_data_with_date[, date:=as.Date(date, format="%m.%d.%y")]
all_data_cumulative = all_data_with_date[date=="2020-12-31"] # We should make sure no countries are getting left out by this! 

# Merge on population 
population = fread(paste0(intermediate, "world_bank_population.csv"))
# The population was stored in thousands of people, so multiply by 1,000
population[, pop_2019:=pop_2019*1000] 

# -------------------------------------
# Data cleaning for population here
population$country <- sub("St. ", "Saint ", population$country) #mapped St. to Saint
population$country <- sub("Bahamas\\, The", "Bahamas", population$country)
population$country <- sub("Brunei Darussalam", "Brunei", population$country)
population$country <- sub("Congo\\, Dem\\. Rep\\.", "Congo \\(Democratic Republic\\)", population$country)
population$country <- sub("Congo\\, Rep\\.", "Congo \\(Brazzaville\\)", population$country)
population$country <- sub("Czech Republic", "Czechia", population$country)
population$country <- sub("Egypt\\, Arab Rep\\.", "Egypt", population$country)
population$country <- sub("Gambia\\, The", "Gambia", population$country)
population$country <- sub("Iran\\, Islamic Rep\\.", "Iran", population$country)
population$country <- sub("Lao PDR", "Laos", population$country)
population$country <- sub("Micronesia\\, Fed\\. Sts\\.", "Micronesia", population$country)
population$country <- sub("Korea\\, Dem\\. People\\’s Rep\\.", "North Korea", population$country)
population$country <- sub("Russian Federation", "Russia", population$country)
population$country <- sub("Slovak Republic", "Slovakia", population$country)
population$country <- sub("Korea\\, Rep\\.", "South Korea", population$country)
population$country <- sub("Syrian Arab Republic", "Syria", population$country)
population$country <- sub("Venezuela\\, RB", "Venezuela", population$country)
population$country <- sub("Yemen\\, Rep\\.", "Yemen", population$country)

# Dropping country variable for now. 
population$country <- NULL
#-------------------------------------------------
# RUN VALIDATION - WILL INFORM MANUAL EDITS ABOVE 
#-------------------------------------------------
print("These are the JHU/GHSI country codes that are missing from world bank population. ")
print(all_data_cumulative$country[!all_data_cumulative$country%in%population$country])

#-------------------------------------------
# CREATE COMBINED JHU-GHSI DATASET
#------------------------------------------

all_data_cumulative = merge(all_data_cumulative, population, by='country_code', all=TRUE)
stopifnot(nrow(all_data_cumulative[is.na('country_code')])==0)

all_data_cumulative[, cases_per_capita:=cases/pop_2019]
all_data_cumulative[, deaths_per_capita:=deaths/pop_2019]
all_data_cumulative[, case_fatality_ratio:=cases_per_capita/deaths_per_capita]
write.csv(all_data_cumulative, paste0(prepped, "all_data_cumulative.csv"))

print("Data cleaning/merge step completed.")
