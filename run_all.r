# Main pipeline that runs all data prep and analysis steps. 
# You will need to be in the root of the repository for this to work! 

stopifnot(grepl("data_557", getwd()))

# First take the "raw" data, add country code, and save to "intermediates" folder. 
source('add_country_code.R')

source('First case.R')

# Then, take this intermediates data, clean and merge it, and save to "prepped" folder. 
source('prep_data.R')
