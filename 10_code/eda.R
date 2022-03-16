library(ggplot2)
library(dplyr)
library(ggthemes)
library(xtable)
options(xtable.comment = FALSE)

df <- read.csv("~/702/Aquaculture_Global_Production/20_files/NO_GINI_aqua_census.csv")

str(df) 
summary(df)

str(df_no_gini)
summary(df_no_gini)

"""
Response variable
"""

# First look at the distrubtion of response variable, Production
ggplot(df, aes(x = Production)) + geom_histogram(col = "black", fill = "darkseagreen4") +
  labs(title = "Aquaculture Production") +
  theme_fivethirtyeight() # yikes gotta take a log

ggplot(df, aes(x = log(Production))) + geom_histogram(col = "black", fill = "darkseagreen4") + 
  labs(title = "Log of Aquaculture Production") +
  theme_fivethirtyeight() # way better, we'll get that new column in there

df$logProduction <- log(df$Production)

# also let's count how many original production values were 0,
#  since they will convert to -inf values
sum(df$Production == 0) # 997, about 17% of data is unusable

# check the years in which Production is 0
df[df$Production == 0,] %>%
  count('Year') # most 0 values are in the 70s and 80s

# check which countries still have 0 values after 2010
df %>% 
  filter(Production == 0) %>%
  filter(Year >= 2010) %>%
  summarise(Country)
# small island countries- upon investigation, we will see if these countries do not have an aquaculture sector
#  because of gdp or population factors

# factorize years in primary school
df$Years_primary <- factor(df$Years_primary)

"""
First look at Relationships
"""

"""
Going forward, model variables will be:
Response: logProduction
Predictors: Year, gdp per capita, CO2 emissions, population, population density, urban %, years in primary school,
            and GPI in primary school
"""

# subset on Production values greater than 0 so no -inf in log
df <- df %>% filter(Production > 0)

# Explore relationship between logProduction and each predictor




# Making extra cols for plot labelling purposes
library(ggrepel)
df$f_country <- ''
df$f_country[df$Country == 'China'] <- factor('China')
df$f_country[df$Country == 'Indonesia'] <- factor('Indonesia')

# logProduction vs Year
ggplot(df, aes(x = Year, y = logProduction, label = Country)) + geom_point(col = "darkslategray") +
  labs(title = "Log of Production of Aquaculture Over Time") +
  theme_fivethirtyeight() +
  geom_label_repel(aes(label = Country))
  
  
  #geom_text(aes(label = ifelse(logProduction > 15, as.character(Country), ''), col = f_country))
# loose, positive linear relationship

options(ggrepel.max.overlaps = 10)

ggplot(df, aes(x= Year, y = logProduction, label = Country)) + 
  geom_point(color = dplyr::case_when((df$logProduction > 15 & df$Year == 2010) ~ "darkorchid4", 
                                      (df$logProduction < 2 & df$Year == 2010) ~ "darkorchid4",
                                      TRUE ~ "darkseagreen4"), 
             size = 3, alpha = 0.8) +
  geom_label_repel(data          = subset(df, (logProduction > 15 & Year == 2010)),
                  nudge_y       = 20 - subset(df, (logProduction > 15 & Year == 2010))$logProduction,
                  size          = 4,
                  box.padding   = 0.25,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey22",
                  direction     = "x") +
  geom_label_repel(data         = subset(df, (logProduction < 2 & Year == 2010)),
                   nudge_y       = -3 - subset(df, (logProduction < 2 & Year == 2010))$logProduction,
                   size          = 4,
                   box.padding   = 0.25,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey22",
                   direction     = "x") +
  #scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  theme_fivethirtyeight() +
  labs(title = "Log of Aquaculture Production vs Year")
  #theme(plot.title = element_text(size = 20, face = 'bold'))




# logProductoin vs gdp per cap
ggplot(df, aes(x = GDP_per_capita, y = logProduction)) + geom_point() +
  labs(title = " Log of Production of Aquaculture vs GDP Per Capita") +
  theme_fivethirtyeight()
# not at all linear, probably won't be included in fnial model

# logProduction vs CO2 emissions
ggplot(df, aes(x = CO2_emissions, y = logProduction)) + geom_point() +
  labs(title = "Log of Production of Aquaculture vs CO2 emissions") +
  theme_fivethirtyeight() 
# nope

# logProduction vs population
ggplot(df, aes(x = Population, y = logProduction)) + geom_point() +
  labs(title = "Log of Production of Aquaculture vs Population") +
  theme_fivethirtyeight()
# not linear.... some countries that have around 500000000 people or more have linear trend, but most small countries don't

# logProduction vs population density
ggplot(df, aes(x = Population_density, y = logProduction)) + geom_point() +
  labs(title = "Log of Production of Aquaculture vs Population Density") +
  theme_fivethirtyeight()
# not linear

# logProduction vs urban pop %
ggplot(df, aes(x = Percent_urban_population, y = logProduction)) + geom_point() +
  labs(title = "Log of Production of Aquaculture vs Urban Population Percentage") +
  theme_fivethirtyeight() 
# total mess

# logProduction vs years in primary school
ggplot(df, aes(x = Years_primary, y = logProduction, fill = Years_primary)) + geom_boxplot() +
  labs(title = "Log of Production of Aquaculture vs Years in Primary School") +
  theme_fivethirtyeight()
# trend flip flops

# logProduction vs GPI in primary school
ggplot(df, aes(x = Primary_GPI, y = logProduction)) + geom_point() +
  labs(title = "Log of Production of Aquaculture vs GPI in Primary School") +
  theme_fivethirtyeight() 
# not linear

# CREATE FACTOR FOR GDP VARIABLE
df$gdplevel <- 1
df$gdplevel[df$GDP_per_capita > 10000 & df$GDP_per_capita <= 20000] <- 2
df$gdplevel[df$GDP_per_capita > 20000 & df$GDP_per_capita <= 40000] <- 3
df$gdplevel[df$GDP_per_capita > 40000 & df$GDP_per_capita <= 60000] <- 4
df$gdplevel[df$GDP_per_capita > 60000] <- 5
df$gdplevel <- factor(df$gdplevel)

# logProduction vd gdp level
ggplot(df, aes(x = gdplevel, y = logProduction, fill = gdplevel)) + geom_boxplot() +
  labs(title = "Log of Production vs GDP Per Capita Level") +
  theme_fivethirtyeight()
# appears to be a relationship here- this could be alternative to GDP_per_cap continuous variable


# INTERACTION EXPLORATIONS

ggplot(df, aes(x = Year, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Year by GDP Level") +
  facet_wrap(~gdplevel)
# Trend does appear to change among each level
# will consider this interaction

ggplot(df, aes(x = CO2_emissions, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs CO2 Emissions") +
  facet_wrap(~gdplevel)
# no change

ggplot(df, aes(x = Population, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Population by GDP Level") +
  facet_wrap(~gdplevel)
# no change

ggplot(df, aes(x = Population_density, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Population Density by GDP Level") +
  facet_wrap(~gdplevel)
# trend of level 1 is different from others
# will consider this interaction

ggplot(df, aes(x = Percent_urban_population, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Percent Urban Population by GDP Level") +
  facet_wrap(~gdplevel)
# trend does change
# will consider this interaction

ggplot(df, aes(x = Primary_GPI, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs GPI in Primary School by GDP Level") +
  facet_wrap(~gdplevel)
# no change

ggplot(df, aes(x = Years_primary, y = logProduction, fill = Years_primary)) + 
  geom_boxplot() + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Years in Primary School by GDP Level") +
  facet_wrap(~gdplevel)
# trend does change
# will consider this interaction



ggplot(df, aes(x = Year, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Year by Years in Primary School") +
  facet_wrap(~Years_primary)
# no change

ggplot(df, aes(x = CO2_emissions, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs CO2 Emissions by Years in Primary School") +
  facet_wrap(~Years_primary)
# no change

ggplot(df, aes(x = Population, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Population by Years in Primary School") +
  facet_wrap(~Years_primary)
# no change

ggplot(df, aes(x = Population_density, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Population Density by Years in Primary School") +
  facet_wrap(~Years_primary)
# some change in trend
# will consider this interaction

ggplot(df, aes(x = Percent_urban_population, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Percent Urban Population by Years in Primary School") +
  facet_wrap(~Years_primary)
# slight change
# will consider this interaction

ggplot(df, aes(x = Primary_GPI, y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs GPI in Primary School by Years in Primary School") +
  facet_wrap(~Years_primary)
# some change
# will consider this interaction

ggplot(df, aes(x = gdplevel, y = logProduction, fill = gdplevel)) + 
  geom_boxplot() + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs GDP Level by Years in Primary School") +
  facet_wrap(~Years_primary)
# trend does change
# will consider this interaction


# THOUGHTS: so far each relationship does not appear to have much of a linear trend, but we will put all base 
#  predictors in lm, assess assumptions, residuals, etc, and then see if additional linear model building (i.e. interactions)
#  are a path forward or if a time series model would be best. 

# INTERACTIONS TO INCLUDE IN MODELING: gdplevel interactions with: year, pop_density, urban_pop, years_primary
#                                      years in primary interactions with: primary_gpi, urban_pop, pop_density


"""
Cleaning up columns for modelling
"""
df$CO2_emissions_c <- df$CO2_emissions - mean(df$CO2_emissions)
df$Population_c <- df$Population - mean(df$Population)
df$Population_density_c <- df$Population_density - mean(df$Population_density)
df$Primary_GPI_c <- df$Primary_GPI - mean(df$Primary_GPI)


"""
save edited dataframe to use in modeling
"""

write.csv(df, "/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/to_model_aquaculture_and_census.csv", row.names = FALSE)



















