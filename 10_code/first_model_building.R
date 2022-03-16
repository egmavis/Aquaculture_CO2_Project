library(ggplot2)
library(dplyr)
library(ggthemes)
library(xtable)
library(car)
options(xtable.comment = FALSE)

df <- read.csv("/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/to_model_aquaculture_and_census.csv")
str(df)
df$Years_primary <- factor(df$Years_primary)
df$gdplevel <- factor(df$gdplevel)

"""
Model Building
"""

# BASE LINEAR MODEL (with centered variables)
base <- lm(logProduction ~ Year + CO2_emissions_c + Population_c + Population_density_c + Percent_urban_population + 
             Years_primary + Primary_GPI + gdplevel, data = df)
summary(base)
# not too bad, not super high r-sq value, but most predictors do appear to be significant

# Base model assessment

# residuals for each cont variable
ggplot(df, aes(x = Year, y = base$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Year')
# not bad, no clear pattern, random scatter

ggplot(df, aes(x = CO2_emissions_c, y = base$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs CO2_emissions_c')
# definitely some problems
# try a log transform

ggplot(df, aes(x = Population_c, y = base$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Population_c')
# def some problems
# try a log transform

ggplot(df, aes(x = Population_density_c, y = base$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Population_density_c')
# def some problems
# try a log transform

ggplot(df, aes(x = Percent_urban_population, y = base$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Percent_urban_population')
# mostly random scatter

ggplot(df, aes(x = Primary_GPI, y = base$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = "red3") +
  theme_fivethirtyeight() +
  labs(title = "Residuals vs Primary_GPI")
# def some clustering, but random distribution across y=0 line

plot(base)
# problems with linearity, normality okay, problems with equal variance, a few outliers,


# All predictors as main effects with log transformed predictors
# Use log transform on un-centered versions so not as many NaNs are produced
base2 <- lm(logProduction ~ Year + log(CO2_emissions) + log(Population) + log(Population_density) + Percent_urban_population + 
              Years_primary + Primary_GPI + gdplevel, data = df)
summary(base2)
# Woah r-sq way increased!
# some assessment:
# residuals for each cont variable
ggplot(df, aes(x = Year, y = base2$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Year')
# not bad, no clear pattern, random scatter

ggplot(df, aes(x = log(CO2_emissions), y = base2$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs CO2_emissions_c')
# way better, small hint of curve but much more random scatter
# keep this variable transformed

ggplot(df, aes(x = log(Population), y = base2$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Population_c')
# way better
# keep this variable transformed

ggplot(df, aes(x = log(Population_density), y = base2$residuals)) + geom_point() +
  geom_hline(yintercept = 0, col = 'red3') + 
  theme_fivethirtyeight() +
  labs(title = 'Residuals vs Population_density_c')
# way better
# keep this variable transformed

plot(base2)
# still a slight curve in the residuals, but much much better
# normality still good, constant variane is much better
# still some outliers but leverage values are much smaller



# formatting plots for final report
library(gtable)
library(grid)
library(egg)
library(gridExtra)

p1 <- ggplot(df, aes(x = CO2_emissions_c, y = base$residuals)) + geom_point(col = "slategray") +
  geom_hline(yintercept = 0, col = 'purple4') + 
  theme_fivethirtyeight() +
  labs(title = 'Base effects') +
  theme(plot.title = element_text(size=12))

p2 <- ggplot(df, aes(x = Population_c, y = base$residuals)) + geom_point(col = "slategray") +
  geom_hline(yintercept = 0, col = 'purple4') + 
  theme_fivethirtyeight()

p3 <- ggplot(df, aes(x = Population_density_c, y = base$residuals)) + geom_point(col = "slategray") +
  geom_hline(yintercept = 0, col = 'purple4') + 
  theme_fivethirtyeight()

p4 <- ggplot(df, aes(x = log(CO2_emissions), y = base2$residuals)) + geom_point(col = "slategray") +
  geom_hline(yintercept = 0, col = 'purple4') + 
  theme_fivethirtyeight() +
  labs(title = 'Log transformed effects') +
  theme(plot.title = element_text(size=12))

p5 <- ggplot(df, aes(x = log(Population), y = base2$residuals)) + geom_point(col = "slategray") +
  theme_fivethirtyeight() +
  geom_hline(yintercept = 0, col = 'purple4')
  
p6 <- ggplot(df, aes(x = log(Population_density), y = base2$residuals)) + geom_point(col = "slategray") +
  geom_hline(yintercept = 0, col = 'purple4') + 
  theme_fivethirtyeight()

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)




# Now for interactions: First let's plot the interactions we initially found to be significant again
#  with the log transformed pop_density to see if they are still relavant

ggplot(df, aes(x = log(Population_density), y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") +
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Log of Population Density by Years in Primary School") +
  facet_wrap(~Years_primary)
# still potential interaction
ggplot(df, aes(x = log(Population_density), y = logProduction)) + geom_point() +
  geom_smooth(method = "lm", col = "red3") +
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Log of Population Density by GDP Level") +
  facet_wrap(~gdplevel)
# still potential interaction

df$logCO2 <- log(df$CO2_emissions)
df$logPop <- log(df$Population)
df$logPop_density <- log(df$Population_density)


"""
Summary so far: 
                Response: logProduction
 Main effects predictors: Year, log(CO2_emissions), log(Population), log(Population_density), Percent_urban_population,
                           Years_primary, Primary_GPI_c, gdplevel
Interactions to Consider: gdplevel*year, gdplevel*log(pop_density), gdplevel*urban_pop, gdplevel*years_primary
                          years_primary*primary_gpi, years_primary*log(pop_density), years_primary*urban_pop
"""

base_model <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population +
                   Years_primary + Primary_GPI_c + gdplevel, data = df)

# INTERACTIONS
inter1 <- lm(logProduction ~ gdplevel*Year + logCO2 + logPop + logPop_density + Percent_urban_population + 
               Years_primary + Primary_GPI, data = df)
inter2 <- lm(logProduction ~ Year + logCO2 + logPop + gdplevel*logPop_density + Percent_urban_population + 
               Years_primary + Primary_GPI, data = df)
inter3 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + gdplevel*Percent_urban_population + 
              Years_primary + Primary_GPI, data = df)
inter4 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population + 
               gdplevel*Years_primary + Primary_GPI, data = df)
inter5 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population + 
               Years_primary*Primary_GPI + gdplevel, data = df)
inter6 <- lm(logProduction ~ Year + logCO2 + logPop + Years_primary*logPop_density + Percent_urban_population + 
               Primary_GPI + gdplevel, data = df)
inter7 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Years_primary*Percent_urban_population + 
               Primary_GPI + gdplevel, data = df)

# test each potential interaction with base model
anova(base_model, inter1) # sig
anova(base_model, inter2) # sig
anova(base_model, inter3) # sig
anova(base_model, inter4) # 
anova(base_model, inter5)
anova(base_model, inter6)
anova(base_model, inter7)
# they all appear to be significant, interesting
# let's put them all in one model and then compare to base
all_inter <- lm(logProduction ~ gdplevel*(Year + logPop_density + Percent_urban_population + Years_primary) + 
                  Years_primary*(Primary_GPI_c + logPop_density + Percent_urban_population) +
                  logPop + logCO2, data = df)
anova(base_model, all_inter) # yup pretty significant

# quick assessment of all_inter model
summary(all_inter)
# Not enough data for the gdp*years_primary interaction
plot(all_inter)
# slight curve in residuals, but mostly okay
# normality okay
# constant variance pretty good
# definitely a few outliers

# remove gdp*years_primary and assess
all_inter2 <- lm(logProduction ~ gdplevel*(Year + logPop_density + Percent_urban_population) + 
                   Years_primary*(Primary_GPI_c + logPop_density + Percent_urban_population) +
                   logPop + logCO2, data = df)
summary(all_inter2)
# still not enough data in a couple of levels in some interactions, but let's move forward with variable selection
plot(all_inter2)
# Yikes, outliers a bit more of a problem here. 

"""
Initial testing for which predictors to include
"""
null_model <- lm(logProduction ~ 1, data = df)
n <- nrow(df)

# AIC
aic_forward <- step(null_model, scope = formula(all_inter), direction = "forward", trace = 0)
aic_backward <- step(all_inter, direction = "backward", trace = 0)
aic_stepwise <- step(null_model, scope = formula(all_inter), direction = "both", trace = 0)

aic_forward$call
aic_backward$call
aic_stepwise$call

summary(aic_forward)
summary(aic_backward)
summary(aic_stepwise)

# BIC
bic_forward <- step(null_model, scope = formula(all_inter), direction = "forward", trace = 0, k = log(n))
bic_backward <- step(all_inter, direction = "backward", trace = 0, k = log(n))
bic_stepwise <- step(null_model, scope = formula(all_inter), direction = "both", trace = 0, k = log(n))

bic_forward$call
bic_backward$call
bic_stepwise$call

summary(bic_forward)
summary(bic_backward)
summary(bic_stepwise)

# unique models are: aic_forward, aic_backward, bic_forward, bic_backward

# best r-sq values: aic_forward, aic_backward, bic_backward

# MULTICOLLINEARITY of all four selection models
vif(aic_backward)

alias(aic_backward)
# looks like gdplevel:Years_primary is a problem
# remove then run vif again
aic_backward$call
tempmodel <- lm(logProduction ~ gdplevel * (Year + logPop_density + 
                                              Percent_urban_population) + Years_primary * 
                  (Primary_GPI_c + logPop_density + Percent_urban_population) + 
                  logPop + logCO2, data = df)
vif(tempmodel)
alias(tempmodel)
# years primary interactions allover are problematic
tempmodel2 <- lm(logProduction ~ gdplevel * (Year + logPop_density + Percent_urban_population) + Primary_GPI_c +  logPop + logCO2, data = df)
vif(tempmodel2)
alias(tempmodel2)

# DO NOT INCLUDE YEARS_PRIMARY INTERACTION AND RUN MODEL ASSESSMENT AND SELECTION AGAIN

write.csv(df, "/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/second_to_model_aquaculture_and_census.csv", row.names = FALSE)







