library(ggplot2)
library(dplyr)
library(ggthemes)
library(xtable)
library(car)
options(xtable.comment = FALSE)

df <- read.csv("/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/second_to_model_aquaculture_and_census.csv")
df$gdplevel <- factor(df$gdplevel)
df$Years_primary <- factor(df$Years_primary)
str(df)
# model defined based on initial multicollinearity issues in first round of model building
# all main effects, and interactions with only gdplevel

base_model <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population +
                   Years_primary + Primary_GPI_c + gdplevel, data = df)

inter_model <- lm(logProduction ~ gdplevel*(Year + logPop_density + Percent_urban_population) + Years_primary + 
                  Primary_GPI_c + logPop + logCO2, data = df)

summary(base_model)
summary(inter_model)
vif(base_model)
vif(inter_model)

# interaction model does have slightly higher r-sq value
# f-test
anova(base_model, inter_model) # interactions are significant

# gdplevel interactions are very colinear
# let's remove and test again

# gdplevel itself does not appear to be problematic because the base_model has decent vif values.
# let's look at each combination of gdplevel's interactions and compare

inter1 <- lm(logProduction ~ gdplevel*Year + logPop_density + Percent_urban_population + Years_primary + Primary_GPI_c + logPop + logCO2, data = df)
vif(inter1) # nope
inter2 <- lm(logProduction ~ Year + gdplevel*logPop_density + Percent_urban_population + Years_primary + Primary_GPI_c + logPop + logCO2, data = df)
vif(inter2) # nope
inter3 <- lm(logProduction ~ Year + logPop_density + gdplevel*Percent_urban_population + Years_primary + Primary_GPI_c + logPop + logCO2, data = df)
vif(inter3) # nope

# double check on the plotting from EDA to see again how strong that relationship looked
ggplot(df, aes(x = logPop_density, y = logProduction)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Log of Population Density by GDP level") +
  facet_wrap(~gdplevel)
ggplot(df, aes(x = Year, y = logProduction)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Year by GDP level") +
  facet_wrap(~gdplevel)
ggplot(df, aes(x = Percent_urban_population, y = logProduction)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Percent_urban_population by GDP level") +
  facet_wrap(~gdplevel)

# regroup gdplevel and see with more evenly distributed groupings?
hist(df$GDP_per_capita)

df$gdplevel2 <- 1
df$gdplevel2[df$GDP_per_capita > 5000 & df$GDP_per_capita <= 20000] <- 2
df$gdplevel2[df$GDP_per_capita > 20000 & df$GDP_per_capita <= 40000] <- 3
df$gdplevel2[df$GDP_per_capita > 40000] <- 4
df$gdplevel2 <- factor(df$gdplevel2)

# check EDA plots with new gdplevel2
ggplot(df, aes(x = logPop_density, y = logProduction)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Log of Population Density by GDP level") +
  facet_wrap(~gdplevel2)
ggplot(df, aes(x = Year, y = logProduction)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red3") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Year by GDP level") + 
  facet_wrap(~gdplevel2)




# modifying this plot for inclusion in final report

gdp_labs <- c(`1` = "GDP 1: < $5,000", `2` = "GDP 2: $5,000 - $20,000", `3` = "GDP 3: $20,000 - $40,000", `4` = "GDP 4: > $40,000")

country_labs <- df[c(17, 1323, 1604, 4650), ]

ggplot(df, aes(x = Percent_urban_population, y = logProduction, label = Country)) + 
  geom_point(col = "darkseagreen4") +
  
  geom_label_repel(data = country_labs,
            aes(Percent_urban_population, logProduction, label= Country),
            nudge_y = 4,
            nudge_x = -4,
            size          = 3,
            box.padding   = 0.25,
            point.padding = 0.5,
            force         = 100,
            segment.size  = 0.5,
            segment.color = "black") +
  
  geom_smooth(method = "lm", col = "purple4") + 
  theme_fivethirtyeight() + 
  labs(title = "Log of Production vs Urban Population Percentage by GDP level") +
  facet_wrap(~gdplevel2, labeller = as_labeller(gdp_labs)) +
  theme(strip.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(color = "gray10", size = 1.5, linetype = "solid"))




# 1 afghanistan 2018 
# 2 ecuador 2018
# 3 france 2018
# 4 us 2018


# with new grouping, interaction with Year no longer looks useful. Other two still matter.
# let's do modelling again with new gdplevel2

base_model2 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population +
                    Years_primary + Primary_GPI_c + gdplevel2, data = df)
summary(base_model2)
vif(base_model2)

inter_model2 <- lm(logProduction ~ gdplevel2*(logPop_density + Percent_urban_population) + Year + Years_primary + 
                    Primary_GPI_c + logPop + logCO2, data = df)
summary(inter_model2)
vif(inter_model2) # still high with both interactions
# let's do remaining two potential interactions one at a time

inter4 <- lm(logProduction ~ Year + gdplevel2*logPop_density + Percent_urban_population + Years_primary + Primary_GPI_c + logPop + logCO2, data = df)
vif(inter4) # nope
inter5 <- lm(logProduction ~ Year + logPop_density + gdplevel2*Percent_urban_population + Years_primary + Primary_GPI_c + logPop + logCO2, data = df)
vif(inter5) # nope


# okay. so multicollinearity is an issue with gdplevel interactions. But now how much do i care?
# do i keep the interactions because they lead to better modelling or do I remove bc of high multicollinearity?

# first let's see which grouping of gdplevel is best:
temp_base <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population +
                  Years_primary + Primary_GPI_c, data = df)
test1 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population +
              Years_primary + Primary_GPI_c + gdplevel, data = df)
test2 <- lm(logProduction ~ Year + logCO2 + logPop + logPop_density + Percent_urban_population +
              Years_primary + Primary_GPI_c + gdplevel2, data = df)

anova(temp_base, test1)
anova(temp_base, test2)
# will keep 2 because it is more evenly distributed with observations

# MOVING FORWARD
# I will still do model AIC/BIC model selection with the interactions model, and see what it gives
# but when it comes down to it, if the r-sq values between the main effects model and the interactions model are super close,
#  I'll stick with the main effects because the high VIF values outweigh the increase in r-sq because of highly inflated standard errors.
# These high VIF values are mostly likely due to the fact that I'm interacting with continuous variables rather than two categorical ones

# REMOVE PROBLEM VARIABLE logPop since it is colinear with Population density

base_model3 <- lm(logProduction ~ Year + logCO2 + logPop_density + Percent_urban_population +
                        Years_primary + Primary_GPI_c + gdplevel2, data = df)
vif(base_model3)
# very low vif scores for the main effects
inter_model3 <- lm(logProduction ~ gdplevel2*(logPop_density + Percent_urban_population) + Year + Years_primary + 
                         Primary_GPI_c + logCO2, data = df)
vif(inter_model3)
# even though there are high vif levels (especially with the interactions), I am not concerened because the main effects
#  were not worrisome. Interactions among categorical variables will have high VIF anyway


# MODEL SELECTION
# working with inter_model3
null_model <- lm(logProduction ~ 1, data = df)
n <- nrow(df)

# AIC
aic_forward <- step(null_model, scope = formula(inter_model3), direction = "forward", trace = 0)
aic_backward <- step(inter_model3, direction = "backward", trace = 0)
aic_stepwise <- step(null_model, scope = formula(inter_model3), direction = "both", trace = 0)

aic_forward$call # == aic_stepwise$call
aic_backward$call

summary(aic_forward)
summary(aic_backward)

# BIC
bic_forward <- step(null_model, scope = formula(inter_model3), direction = "forward", trace = 0, k = log(n))
bic_backward <- step(inter_model3, direction = "backward", trace = 0, k = log(n))
bic_stepwise <- step(null_model, scope = formula(inter_model3), direction = "both", trace = 0, k = log(n))

bic_forward$call # == bic_stepwise$call
bic_backward$call

summary(bic_forward)
summary(bic_backward)

# Wow, all metrics have landed on the same, full model.
# Done with selection!


# save dataframe with just the variables in use for model assessment
df <- df %>% select(c(Country, logProduction, logPop_density, Percent_urban_population, gdplevel2, Year, Years_primary, Primary_GPI_c, logCO2))
write.csv(df, "/Users/emeliamavis/702/Aquaculture_Global_Production/20_files/assessment_aquaculture_and_census.csv", row.names = FALSE)



















