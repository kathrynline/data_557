# AUTHOR: Emily Linebarger 
# PURPOSE: Initial data exploration of GHSI and COVID-19 epidemiological datasets
# DATE: 1/25/2021

rm(list=ls())
library(data.table) 
library(httr)

#-------------------------------------------
# SET UP FILE PATHS 
#-------------------------------------------
# This is set up to run from the root of the repository
stopifnot(grepl("data_557", getwd()))
raw = "raw_data/"
prepped = "prepped_data/"

#-------------------------------------------
# FORMAT GHSI SUMMARY DATA 
#-------------------------------------------
ghsi_raw = fread(paste0(raw, "ghsi_summary.csv"), encoding = "UTF-8") #added encoding
ghsi_prepped = data.table()
for (i in seq(1, ncol(ghsi_raw), by=2)){
  subset = ghsi_raw[, i:(i+1)]
  if (i==1){
    ghsi_prepped = subset
  } else{
    ghsi_prepped = merge(ghsi_prepped, subset, by='country', all=TRUE)
  }
}

#-------------------
# Manual edits to go here
ghsi_prepped$country <- sub("St ", "Saint ", ghsi_prepped$country) #mapped St to Saint
ghsi_prepped$country <- iconv(ghsi_prepped$country, from = 'UTF-8', to = 'ASCII//TRANSLIT') #dropped accents
ghsi_prepped$country <- sub("Czech Republic", "Czechia", ghsi_prepped$country) #Czechia
ghsi_prepped$country <- sub("eSwatini \\(Swaziland\\)", "Eswatini", ghsi_prepped$country) #Eswatini
ghsi_prepped$country <- sub("Saint Vincent and The Grenadines", "Saint Vincent and the Grenadines", ghsi_prepped$country) #St Vincent
ghsi_prepped <- subset(ghsi_prepped, country != 'AVERAGE') #remove AVERAGE

write.csv(ghsi_prepped, paste0(prepped, "ghsi.csv"), row.names=F)
#-------------------------------------------
# FORMAT JHU CASES/DEATHS DATA 
#-------------------------------------------
jhu_cases = fread(paste0(raw, "jhu_cases_01.26.2021.csv"))
jhu_deaths = fread(paste0(raw, "jhu_deaths_01.26.2021.csv"))

format_jhu = function(data, col_name){
  data = data[, -c('Lat', 'Long', 'V1')]
  data = setnames(data, c("Province/State", "Country/Region"), c('state', 'country'))
  data = melt(data, id.vars=c('state', 'country'), variable.name="date", value.name=col_name)
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

write.csv(cases_prepped, paste0(prepped, "jhu_cases.csv"), row.names=FALSE)
write.csv(deaths_prepped, paste0(prepped, "jhu_deaths.csv", row.names=FALSE))

all_jhu = merge(cases_prepped, deaths_prepped, by=c('country', 'state', 'date'), all=TRUE)
write.csv(all_jhu, paste0(prepped, "all_jhu.csv"))

#-------------------------------------------------
# RUN VALIDATION - WILL INFORM MANUAL EDITS ABOVE 
#-------------------------------------------------
print(ghsi_prepped$country[!ghsi_prepped$country%in%all_jhu$country])

#-------------------------------------------
# CREATE COMBINED JHU-GHSI DATASET
#-------------------------------------------
# COllapse out the state level for JHU - we only have national for GHSI. 
all_jhu = all_jhu[, .(cases=sum(cases), deaths=sum(deaths)), by=c('country', 'date')]
all_data_with_date = merge(all_jhu, ghsi_prepped, by=c('country'), all=TRUE)

write.csv(all_data_with_date, paste0(prepped, "all_data_with_date.csv"))

#--------------------------------------------
# CREATE CUMULATIVE DATASET
#--------------------------------------------
# This assumes the data is cumulative, and grabs the last date of data available for 2020 by country. 
all_data_with_date[, date:=as.Date(date, format="%m/%d/%y")]
all_data_cumulative = all_data_with_date[date=="2020-12-31"] # We should make sure no countries are getting left out by this! 

# Merge on population 
population = fread(paste0(raw, "world_bank_population.csv"))
# The population was stored in thousands of people, so multiply by 1,000 
population[, pop_2019:=pop_2019*1000]

# -------------------------------------
# Data cleaning for population here 

all_data_cumulative = merge(all_data_cumulative, population, by='country', all=TRUE)

all_data_cumulative[, cases_per_capita:=cases/pop_2019]
all_data_cumulative[, deaths_per_capita:=deaths/pop_2019]
all_data_cumulative[, case_fatality_ratio:=cases_per_capita/deaths_per_capita]
write.csv(all_data_cumulative, paste0(prepped, "all_data_cumulative.csv"))

