# Author: Alison Gale

add_country_codes <- function(file_name, column_name){
  # Relative path for the file to transform. The file must have a "country" column
  originalFilePath <- paste0('./raw_data/', file_name)
  
  # Relative path the the resulting file with country code added
  outputFilePath <- paste0('./intermediate_data/', file_name)
  
  # Read source file and country codes
  original <-
    read.csv(file = originalFilePath, fileEncoding = 'UTF-8-BOM')
  codes <-
    read.csv(file = './raw_data/country_codes.csv', fileEncoding = 'UTF-8-BOM')
  
  
  # Replace leading white space that appears in country codes
  codes$country_code <- trimws(codes$Alpha.3.code, which = c("left"))
  
  # Rename country column for easier access
  names(original)[names(original) == column_name] <- 'country'
  
  # Join the two tables on the country column
  output <-
    merge(original, codes[c('Country', 'country_code')],
          by.x = 'country',
          by.y = 'Country')
  
  # Calculate which countries were omitted
  out_countries <- unique(output$country)
  in_countries <- unique(original$country)
  print(paste0('Countries not matched for ', file_name, ':'))
  print(in_countries[!in_countries %in% out_countries])
  
  # Write output to file. This adds a country_codes column and removes all rows
  # that don't match.
  write.csv(output, file = outputFilePath, row.names = FALSE)
} 

print("Running add_country_code.R...")

# Call this function once for each of our input files. 
# If we add raw files, we will need to add them to this list! 
add_country_codes('ghsi_summary.csv', 'country')
add_country_codes('island_countries.csv', "country")
add_country_codes('jhu_cases_01.26.2021.csv', 'Country.Region')
add_country_codes('jhu_deaths_01.26.2021.csv', 'Country.Region')
add_country_codes('world_bank_population.csv', 'country')
add_country_codes('democracy_index.csv', 'Country')
add_country_codes('ghsi_raw.csv', 'Country')

print("Add country codes step complete.")
