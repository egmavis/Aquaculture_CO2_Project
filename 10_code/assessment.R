library(ggplot2)
library(dplyr)
library(ggthemes)
library(xtable)
library(car)
options(xtable.comment = FALSE)


df <- read.csv("/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/assessment_aquaculture_and_census.csv")
df$Years_primary <- factor(df$Years_primary)
df$gdplevel2 <- factor(df$gdplevel2)

model <- lm(logProduction ~ gdplevel2*(logPop_density + Percent_urban_population) + Year + Years_primary + 
              Primary_GPI_c + logCO2, data = df)

summary(model)
vif(model)


# plots and things for final report


library(jtools)
library(ggstance)
library(broom.mixed)
library(broom)


plot_summs(model, scale = TRUE)

par(mfrow=c(2,2))

ggplot(df, aes(x = model$, y = model$residuals)) + geom_point(col = "gray12") +
  theme_fivethirtyeight() + 
  #geom_smooth(col = "darkorchid4") +
  labs(title = "Residuals vs Fitted Values")

plot(model, col = "gray12") + theme_fivethirtyeight()


# check residuals against each continuous predictor
ggplot(df, aes(x = Year, y = model$residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, col = 'red3') +
  theme_fivethirtyeight() +
  labs(title = 'Model Residuals vs Year')
# good random scatter

ggplot(df, aes(x = Primary_GPI_c, y = model$residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, col = 'red3') +
  theme_fivethirtyeight() +
  labs(title = 'Model Residuals vs Primary School GPI (cent)')
# some clustering around the mode of GPIm but otherwise no distinct pattern

ggplot(df, aes(x = logCO2, y = model$residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, col = 'red3') +
  theme_fivethirtyeight() +
  labs(title = 'Model Residuals vs logCO2')
# mostly random scatter of residuals, slight decreasing curve, but not too bad

ggplot(df, aes(x = logPop_density, y = model$residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, col = 'red3') +
  theme_fivethirtyeight() +
  labs(title = 'Model Residuals vs logPop_density')
# good random scatter

ggplot(df, aes(x = Percent_urban_population, y = model$residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0, col = 'red3') +
  theme_fivethirtyeight() +
  labs(title = 'Model Residuals vs Percent_urban_population')
# good random scatter



# plots for whole model
plot(model)
# residuals vs fitted has slight curve pattern bc of high values near zero,
#  but mostly good random scatter around the y=0 line
# normality is pretty good, some slight slight deviation at the tails,
#  but just a few points
# constant variance looks good
# not too terrible of outliers- no points with high influence, a few outliers 
#  with leverage in between .04 and .06, but not bad. One point (1378) has leverage
#  one, so let's investigate and potentially remove from dataset

df[1378,]
# El Salvador in 1997
# let's remove it from data and see how it effects model and assessment
tempdf <- df[-1378,]
tempmodel <- lm(logProduction ~ gdplevel2*(logPop_density + Percent_urban_population) + Year + Years_primary + 
                           Primary_GPI_c + logCO2, data = tempdf)
summary(tempmodel)
vif(tempmodel)
# no significant changes here
plot(tempmodel)
# no changes, just no point with leverage 1. I'm going to keep this observation in 
#  the dataframe because it does not effect the model in any way.

# overall all assumptions are met quite fairly.







