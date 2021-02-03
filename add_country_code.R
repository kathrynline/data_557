# Relative path for the file to transform. The file must have a "country" column
originalFilePath <- './raw_data/jhu_deaths_01.26.2021.csv'
# Name for the country column in the source file
originalFileCountryColumn <- 'Country.Region'

# Relative path the the resulting file with country code added
outputFilePath <- './raw_data/jhu_deaths_01.26.2021_with_country_code.csv'

# Read source file and country codes
original <-
  read.csv(file = originalFilePath, fileEncoding = 'UTF-8-BOM')
codes <-
  read.csv(file = './raw_data/country_codes.csv', fileEncoding = 'UTF-8-BOM')


# Replace leading white space that appears in country codes
codes$country_code <- trimws(codes$Alpha.3.code, which = c("left"))

# Rename country column for easier access
names(original)[names(original) == originalFileCountryColumn] <- 'country'

# Join the two tables on the country column
output <-
  merge(original, codes[c('Country', 'country_code')],
        by.x = 'country',
        by.y = 'Country')

# Calculate which countries were omitted
out_countries <- unique(output$country)
in_countries <- unique(original$country)
print('Countries not matched:')
print(in_countries[!(in_countries %in% out_countries)])


# Write output to file. This adds a country_codes column and removes all rows
# that don't match.
write.csv(output, file = outputFilePath, row.names = FALSE)