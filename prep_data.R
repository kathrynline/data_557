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
ghsi_raw = fread(paste0(raw, "ghsi_summary.csv"))
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

write.csv(all_data_cumulative, paste0(prepped, "all_data_cumulative.csv"))

