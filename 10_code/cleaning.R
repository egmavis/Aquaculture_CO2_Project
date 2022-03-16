library(dplyr)
library(tidyr)
library(stringr)
library(reshape)
library(reshape2)
library(comprehenr)

"
Data loading, cleaning, wrangling, and merging
"

aquaculture <- read.csv("~/702/Aquaculture_Global_Production/00_data/aquaculture.csv")
wdi <- read.csv("~/702/Aquaculture_Global_Production/00_data/WDIData.csv")

"
Clean and wrangle aquaculture data
- remove unnecessary columns
- melt down year headers for unique country-year observations
--> sum down species columns, because I don't care about individual species farming, just total aquaculture
"

# check aqua properties

dim(aquaculture)
str(aquaculture)
summary(aquaculture)

sum(is.na(aquaculture)) # 70 missing values - let's find them:

aquaculture[which(is.na(aquaculture)),]

sum(is.na(aquaculture$Country..Name.))
sum(is.na(aquaculture$ASFIS.species..Name.))
sum(is.na(aquaculture$FAO.major.fishing.area..Name.))
sum(is.na(aquaculture$Environment..Name.))
sum(is.na(aquaculture$Unit..Name.))
sum(is.na(aquaculture$Unit))

which(is.na(aquaculture$X.1950.))
# it looks as though the last observation is a bogus labeling row from loading in the data. Let's remove it and see how that changes our missing data count
aquaculture <- aquaculture[-3518, ]
sum(is.na(aquaculture)) # now no missing values, yay!

# remove columns
aquaculture <- aquaculture %>% select(-starts_with("S"))
aquaculture <- aquaculture %>% select(-c('FAO.major.fishing.area..Name.', 'Environment..Name.', 'Unit..Name.'))
aquaculture <- aquaculture %>% select(-c('Unit', 'ASFIS.species..Name.'))

# melt year headers
aqua1 <- melt(aquaculture, id.vars = c('Country..Name.'))

# rename columns
colnames(aqua1) <- c('Country', 'Year', 'Production')

# change Year entries to be just year integer
aqua1$newYear <- str_extract(aqua1$Year, "\\d{4}")
aqua1 <- aqua1 %>% select(-c('Year'))
aqua1$Year <- strtoi(aqua1$newYear)
aqua1 <- aqua1 %>% select(-c('newYear'))

# collapse observations into unique country-year totals
aqua2 <- aggregate(aqua1$Production, by=list(Category=aqua1$Country, aqua1$Year), FUN=sum)

# make sure each observation is unique for country-year
sum(duplicated(aqua2)) # 0

colnames(aqua2) <- c('Country', 'Year', 'Production')

# remove countries that are not in the WDI data set
to_remove_from_aqua <- list('Bonaire/S.Eustatius/Saba', 'China, Hong Kong SAR', 'Cook Islands', 'Falkland Is.(Malvinas)', 'French Guiana', 'Guadeloupe',
                            'Martinique', 'Mayotte', 'Netherlands Antilles', 'Palestine', 'Réunion', 'Serbia and Montenegro', 'St. Pierre and Miquelon',
                            'Sudan (former)', 'Taiwan Province of China', 'Totals - Tonnes - live weight', 'Un. Sov. Soc. Rep.', 'Venezuela, Boliv Rep of', 
                            'Yugoslavia SFR', 'Zanzibar', 'Czechoslovakia')
aqua3 <- aqua2[!aqua2$Country %in% to_remove_from_aqua, ]


"
Clean and wrangle world census data
- remove unnecessary indicators
- melt down year headers
- remove NAs
  --> create unique country-year observations
- MAKE SURE EACH COUNTRY SPELLING MATCHES AQUA DATA
"

# check properties of data
dim(wdi)
str(wdi)
summary(wdi)

sum(is.na(wdi)) # yikes, a lot
# let's do the reshaping and paring down of indicators first, then check missing values again

wdi <- wdi %>% select(-c('Country.Code', 'Indicator.Code'))

# melt year headers down
wdi2 <- melt(wdi, id.vars = c('Country.Name', 'Indicator.Name'))

# pivot Indicators to be column headers
wdi3 <- dcast(wdi2, Country.Name + variable ~ Indicator.Name, sum)

# remove countries from WDI that are not in aquaculture
to_remove_from_wdi <- list('Africa Eastern and Southern', 'Africa Western and Central', 'Andorra', 'Arab World', 'Bermuda', 'Caribbean small states', 'Cayman Islands', 'Central Europe and the Baltics', 'Comoros', 'Curacao', 
                           'Djibouti', 'Early-demographic dividend', 'East Asia & Pacific', 'East Asia & Pacific (excluding high income)', 'East Asia & Pacific (IDA & IBRD countries)', 'Euro area', 'Europe & Central Asia', 
                           'Europe & Central Asia (excluding high income)', 'Europe & Central Asia (IDA & IBRD countries)', 'European Union', 'Fragile and conflict affected situations', 'Gibraltar', 'Greenland', 'Heavily indebted poor countries (HIPC)',
                           'High income', 'Hong Kong SAR, China', 'IBRD only', 'IDA & IBRD total', 'IDA blend', 'IDA only', 'IDA total', 'Isle of Man', 'Kosovo', 'Late-demographic dividend', 'Latin America & Caribbean', 'Latin America & Caribbean (excluding high income)',
                           'Latin America & the Caribbean (IDA & IBRD countries)', 'Least developed countries: UN classification', 'Liechtenstein', 'Low & middle income', 'Low income', 'Lower middle income', 'Luxembourg', 'Macao SAR, China',
                           'Maldives', 'Middle East & North Africa', 'Middle East & North Africa (excluding high income)', 'Middle East & North Africa (IDA & IBRD countries)', 'Middle income', 'Monaco', 'Mongolia', 'Not classified', 'OECD members', 'Other small states',
                           'Pacific island small states', 'Post-demographic dividend', 'Pre-demographic dividend', 'San Marino', 'Sao Tome and Principe', 'Sint Maarten (Dutch part)', 'Small states', 'Somalia', 'South Asia', 'South Asia (IDA & IBRD)',
                           'St. Martin (French part)', 'Sub-Saharan Africa', 'Sub-Saharan Africa (excluding high income)', 'Sub-Saharan Africa (IDA & IBRD countries)', 'Upper middle income', 'Venezuela, RB', 'World', 'Mauritania', 'North America', 'West Bank and Gaza')
wdi4 <- wdi3[!wdi3$Country.Name %in% to_remove_from_wdi, ]

# a lot of the indicators have missing values, so I will grab my variables of interest based on how much is missing
variables <- colnames(wdi4)
templist <- c()
for (i in 1:length(wdi4)) {
  if (sum(is.na(wdi4[[i]])) < 6000) {
    templist <- c(templist, colnames(wdi4)[i])
  }
  
}
templist


# these are my chosen predictors, and all have less than 6000 missing observations, which is a suitable cutoff (also including Gini Index even though it didn't make the cutoff)
wdi_to_keep <- c("Country.Name", "variable", "GDP per capita (constant 2015 US$)", "Gini index (World Bank estimate)", 
                 "CO2 emissions (kt)", "Population, total", "Population density (people per sq. km of land area)", 
                 "Urban population (% of total population)", "Primary education, duration (years)", 
                 "School enrollment, primary (gross), gender parity index (GPI)")

# subset on these chosen columns
wdi5 <- wdi4 %>% select(wdi_to_keep)

# rename columns
wdi5 <- wdi5 %>% dplyr::rename(c("Country" = "Country.Name", 
                                 "Year" = "variable", 
                                 "GDP_per_capita" = "GDP per capita (constant 2015 US$)",
                                 "Gini_index" = 'Gini index (World Bank estimate)',
                                 "CO2_emissions" = "CO2 emissions (kt)",
                                 "Population" = "Population, total",
                                 "Population_density" = "Population density (people per sq. km of land area)",
                                 "Percent_urban_population" = "Urban population (% of total population)",
                                 "Years_primary" = "Primary education, duration (years)",
                                 "Primary_GPI" = "School enrollment, primary (gross), gender parity index (GPI)"))

# change Year entries to be just year integer
wdi5$Year <- str_extract(wdi5$Year, "\\d{4}")
wdi5$Year <- strtoi(wdi5$Year)


# Now to get all the spelling of each country in both datasets to be the same
wdi5$Country <- mapvalues(wdi5$Country, from = c('Bahamas, The', 'Congo, Dem. Rep.', 'Congo, Rep.', 'Egypt, Arab Rep.', 'Gambia, The', 'Iran, Islamic Rep.', 'Korea, Dem. People\'s Rep.', 'Korea, Rep.',
                                                 'Kyrgyz Republic', 'Lao PDR', 'Micronesia, Fed. Sts.', 'St. Kitts and Nevis', 'St. Lucia', 'St. Vincent and the Grenadines', 'Virgin Islands (U.S.)', 'Yemen, Rep.'),
                                        to = c('Bahamas', 'Dem Rep of the Congo', 'Congo', 'Egypt', 'Gambia', 'Iran', 'North Korea', 'South Korea', 'Kyrgyzstan', 'Laos', 'Fed States of Micronesia', 
                                               'Saint Kitts and Nevis','Saint Lucia', 'Saint Vincent and the Grenadines', 'US Virgin Islands', 'Yemen'))

aqua3$Country <- mapvalues(aqua3$Country, from = c('Bolivia (Plurinat.State)', 'Congo, Dem. Rep. of the', 'Czechia', 'Iran (Islamic Rep. of)', 'Korea, Dem. People\'s Rep', 'Korea, Republic of',
                                                   'Lao People\'s Dem. Rep.', 'Micronesia, Fed.States of', 'Moldova, Republic of', 'Northern Mariana Is.', 'Saint Vincent/Grenadines', 'Slovakia',
                                                   'Tanzania, United Rep. of', 'Turks and Caicos Is.', 'United States of America', 'Viet Nam'),
                                          to = c('Bolivia', 'Dem Rep of the Congo', 'Czech Republic', 'Iran', 'North Korea', 'South Korea', 'Laos', 'Fed States of Micronesia', 'Moldova', 'Northern Mariana Islands',
                                                 'Saint Vincent and the Grenadines', 'Slovak Republic', 'Tanzania', 'Turks and Caicos Islands', 'United States', 'Vietnam'))

# now let's relevel both Country variables so they are comparable
aqua3 <- droplevels(aqua3)
wdi5 <- droplevels(wdi5)


"""
Drop years 1950-1959 in aqua data because wdi data doesn't start until 1960
Drop year 2020 in wdi data because aqua data doesn't go till 2020
"""

aqua4 <- aqua3[aqua3$Year >= 1960,]
wdi6 <- wdi5[wdi5$Year < 2020,]

length(unique(aqua4$Country))
length(unique(wdi6$Country))
unique(aqua4$Year)
unique(wdi6$Year)

# All the same!


"""
Merge data frames together on Country and Year!
"""
final <- merge(aqua4, wdi6, by = c("Country", "Year"), all = TRUE)
# let's check in on this missing data
summary(final)

# We will save two dataframes: one with gini index and one without

# final with gini
final_with_gini <- drop_na(final)
# final without_gini
final_without_gini <- final %>% select(-c(Gini_index))
final_without_gini <- drop_na(final_without_gini)

"""
save new data frames as csv
"""
write.csv(final_with_gini, "/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/aqua_census.csv", row.names = FALSE)
write.csv(final_without_gini, "/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/NO_GINI_aqua_census.csv", row.names = FALSE)










