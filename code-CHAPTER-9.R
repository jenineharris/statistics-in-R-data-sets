#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 17, 2019                                                    #
# Purpose: Review Code for Chapter 9 of Dr. Harris' Book                        #
#          Linear Regression                                                    #
#         The R Team and the Needle Exchange Examination                        #
# Packages Used: tidyverse, tableone, lmtest, broom, car, gridExtra             #
# Data Used: AMPFAR + USDA ERS                                                  #
# Last Update: Nov 18, 2019                                                     #
#################################################################################
library(tidyverse)

#Box 9.1
# read in all the data
op.dist <- read.csv(file = "[data folder location]/data/opioid_dist_to_needle_exchange_2018.csv")
hiv.prev <- read.csv(file = "[data folder location]/data/hiv_prevalence_amfar_ch9.csv")
op.scripts <- read.csv(file = "[data folder location]/data/opioid_script_rate_amfar_ch9.csv")
unins <- read.csv(file = "[data folder location]/data/percent_unins_amfar_2016_ch9.csv")
metro <- read.csv(file = "[data folder location]/data/metro_nonmetro_usda_ers_2015_ch9.csv")

# open tidyverse
library(package = "tidyverse")

# clean distance variable
op.dist <- op.dist %>%
  rename(county = COUNTY) %>%
  mutate(county = tolower(x = as.character(x = county))) %>%
  rename(dist_SSP = VALUE) %>%
  select(county, dist_SSP, STATEABBREVIATION)

# clean HIV prevalence variable
hiv.prev <- hiv.prev %>%
  filter(YEAR == 2016) %>%
  rename(county = COUNTY) %>%
  mutate(county = tolower(x = as.character(x = county))) %>%
  rename(HIVprevalence = VALUE) %>%
  select(county, HIVprevalence, STATEABBREVIATION)

# clean opioid prescriptions variable
op.scripts <- op.scripts %>%
  filter(YEAR == 2017) %>%
  rename(county = COUNTY) %>%
  mutate(county = tolower(x = as.character(x = county))) %>%
  rename(opioid_RxRate = VALUE) %>%
  select(county, opioid_RxRate, STATEABBREVIATION)

# clean percent uninsured variable
unins <- unins %>%
  filter(YEAR == 2016) %>%
  rename(county = COUNTY) %>%
  rename(pctunins = VALUE) %>%
  mutate(county = tolower(x = as.character(x = county))) %>%
  select(county, pctunins, STATEABBREVIATION)

# clean metro status variable
metro <- metro %>%
  rename(metro = Metro.nonmetro.status..2013.0.Nonmetro.1.Metro) %>%
  mutate(metro = as.factor(recode(metro, `0` = "non-metro", `1` =
                                    "metro"))) %>%
  rename(county = County_name) %>%
  rename(STATEABBREVIATION = State) %>%
  mutate(county = tolower(x = as.character(x = county))) %>%
  select(county, metro, STATEABBREVIATION)

# merge the data frames
dist.data <- op.dist %>%
  merge(x = hiv.prev) %>%
  merge(x = op.scripts) %>%
  merge(x = unins) %>%
  merge(x = metro)
# summary of data
summary(object = dist.data)

# sample 500 counties
set.seed(seed = 42)
dist.data.samp <- dist.data %>%
  drop_na(HIVprevalence) %>%
  drop_na(opioid_RxRate) %>%
  sample_n(size = 500, replace = FALSE)

# check sample
summary(object = dist.data.samp)

# save data
write.csv(x = dist.data.samp, file = "[data folder location]/data/dist_ssp_amfar_ch9.csv", row.names = FALSE)

#9.4.1 
# distance to syringe program data
dist.ssp <- read.csv(file = "[data folder location]/data/dist_ssp_amfar_ch9.csv")

# summary
summary(object = dist.ssp)

dist.ssp$county <- as.character(dist.ssp$county)
dist.ssp$STATEABBREVIATION <- as.character(dist.ssp$STATEABBREVIATION)
dist.ssp$metro <- as.character(dist.ssp$metro)
dist.ssp$opioid_RxRate <- as.numeric(dist.ssp$opioid_RxRate)


# recoding -1 to NA for HIVprevalence
dist.ssp <- dist.ssp %>%
  mutate(HIVprevalence = na_if(x = HIVprevalence, y = -1))

# check recoding
summary(object = dist.ssp)

dist.ssp$pctunins <- dist.ssp$pctunins * 100

# descriptive statistics for syringe data
tableone::CreateTableOne(data = dist.ssp,
                         vars = c('dist_SSP', 'HIVprevalence',
                                  'opioid_RxRate', 'pctunins',
                                  'metro'))

# check distribution of HIV prevalence (Figure 9.1)
dist.ssp %>%
  ggplot(aes(x = HIVprevalence)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  labs(x = "HIV cases per 100,000 people", y = "Number of counties") +
  theme_minimal()

# descriptive statistics for syringe data
syringe.desc <- tableone::CreateTableOne(data = dist.ssp,
                                         vars = c('dist_SSP',
                                                  'HIVprevalence',
                                                  'opioid_RxRate',
                                                  'pctunins',
                                                  'metro'))
print(x = syringe.desc, nonnormal = c("HIVprevalence"))

# percent without health insurance and distance to needle exchange (Figure 9.2)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(x = "Percent without health insurance", y = "Miles to syringe program") +
  scale_size_manual(values = 2, name = "")

# percent without health insurance and distance to needle exchange (Figure 9.3)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(linetype = "Linear fit line"), method = "lm", se = FALSE,
              color = "gray60") +
  theme_minimal() +
  labs(x = "Percent uninsured", y = "Miles to syringe program") +
  scale_size_manual(values = 2, name = "") +
  scale_linetype_manual(values = 1, name = "")

#Box 9.2
# percent without health insurance and distance to needle exchange
# (Figure 9.4)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(shape = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(linetype = "Linear fit line"), method = "lm", se =
                FALSE, color = "gray60") +
  theme_minimal() +
  labs(x = "Percent uninsured", y = "Miles to syringe program") +
  scale_shape_manual(values = 0, name = "") +
  scale_linetype_manual(values = 1, name = "")

# percent without health insurance and distance to needle exchange
# (Figure 9.5)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(shape = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(size = "Linear fit line"), method = "lm", se = FALSE, color = "gray60") +
  theme_minimal() +
  labs(x = "Percent uninsured", y = "Miles to syringe program") +
  scale_shape_manual(values = 17, name = "") +
  scale_size_manual(values = 1, name = "")

#9.4.4
# correlation between percent uninsured and distance
dist.ssp %>%
  summarize(cor.dist.uninsur = cor(x = dist_SSP,
                                   y = pctunins),
            samp.n = n())

#9.4.5
# bivariate relationships with distance to SSP
dist.ssp %>%
  summarize(cor.rx.rate = cor(x = dist_SSP, y = opioid_RxRate),
            cor.hiv = cor(x = dist_SSP, y = HIVprevalence, use =
                            "complete"),
            cor.unins = cor(x = dist_SSP, y = pctunins))

# bivariate relationships with distance to SSP
dist.ssp %>%
  summarize(cor.rx.rate = cor(x = dist_SSP, y = opioid_RxRate),
            cor.s.hiv = cor(x = dist_SSP, y = HIVprevalence, method =
                              "spearman", use = "complete"),
            cor.unins = cor(x = dist_SSP, y = pctunins))

# metro and distance to SSP
dist.ssp %>%
  group_by(metro) %>%
  summarize(m.dist = mean(x = dist_SSP))

#9.4.6
# metro and distance to SSP (Figure 9.6)
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_jitter(aes(color = metro), alpha = .6) +
  geom_boxplot(aes(fill = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Distance to nearest syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal()

# metro and distance to SSP (Figure 9.7)
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_jitter(aes(color = metro), alpha = .8) +
  geom_violin(aes(fill = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Distance to nearest syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal()

# metro and distance to SSP (Figure 9.8)
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_violin(aes(color = metro), fill = "white", alpha = .8) +
  geom_boxplot(aes(fill = metro, color = metro), width = .2, alpha = .3) +
  geom_jitter(aes(color = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Miles to syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  coord_flip()

#9.4.7 CHECK YOUR UNDERSTANDING
#Check distributions of Opioid Rx Rate and Pct Uninsured
dist.ssp %>%
  ggplot(aes(x = opioid_RxRate, fill = "grey", color = "white"))+
  geom_histogram() +
  theme_minimal()+
  scale_fill_manual(values = "#78A678", guide = FALSE) +
  scale_color_manual(values = "white", guide = FALSE) +
  geom_vline(xintercept = median(dist.ssp$opioid_RxRate, na.rm =T), linetype = 1)+
  annotate("text", x = median(dist.ssp$opioid_RxRate, na.rm = T)+17, y = 75, label = "Median")

dist.ssp %>%
  ggplot(aes(x = pctunins, fill = "grey", color = "white"))+
  geom_histogram() +
  theme_minimal()+
  scale_fill_manual(values = "#7463AC", guide = FALSE) +
  scale_color_manual(values = "white", guide = FALSE) +
  geom_vline(xintercept = median(dist.ssp$pctunins, na.rm =T), linetype = 1)+
  annotate("text", x = median(dist.ssp$pctunins, na.rm = T)+.05, y = 50, label = "Median")

#No neither of the variables is normally distributed. Leslie could report medians/IQRs and use
#non-parametric tests, or could try to transform the variables to approximate normal distributions

#Part 2
dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_boxplot(aes(fill = metro, color = metro), width = .2, alpha = .3) +
  geom_violin(aes(color = metro), fill = "white", alpha = .8) +
  geom_jitter(aes(color = metro), alpha = .4) +
  labs(x = "Type of county",
       y = "Miles to syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  coord_flip()

dist.ssp %>%
  ggplot(aes(x = metro, y = dist_SSP, fill = metro)) +
  geom_jitter(aes(color = metro), alpha = .4) +
  geom_boxplot(aes(fill = metro, color = metro), width = .2, alpha = .3) +
  geom_violin(aes(color = metro), fill = "white", alpha = .8) +
  labs(x = "Type of county",
       y = "Miles to syringe program") +
  scale_fill_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("#78A678", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  coord_flip()

#Depending on which layer comes first, the others may or may not look lighter/grayed out. This is because
#ggplot places layers on top of each other, so the one on bottom gets covered up by the others

#9.5
# make a vector called weeks that has the values 1 through 12 in it
weeks <- 1:12

# use the regression model to make a vector called gallons with
# weeks as the values
gallons <- 3 + 2 * weeks

# make a data frame of weeks and gallons
water <- data.frame(weeks, gallons)

# Make a plot (Figure 9.9)
water %>%
  ggplot(aes(x = weeks, y = gallons)) +
  geom_line(aes(linetype = "Linear model\ngallons=3+2*weeks"), color =
              "gray60", size = 1) +
  geom_point(aes(color = "Observation"), size = 4, alpha = .6) +
  theme_minimal() +
  labs(x = "Weeks", y = "Gallons of water needed") +
  scale_linetype_manual(values = 2, name = "") +
  scale_color_manual(values = "#7463AC", name = "")

#9.5.4 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
#Y = b0 = b1*X + error
#Income = b0 + b1*yrs_edu + error
#A person's predicted income is the coefficient b1 times their years of education, plus the 
#intercept and unmeasured error.

#9.6
# linear regression of distance to syringe program by percent uninsured
dist.by.unins <- lm(formula = dist_SSP ~ pctunins,
                    data = dist.ssp, na.action = na.exclude)
summary(object = dist.by.unins)

#9.6.7 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
#Washington County, NE: pctunins = 13
(distance <- 12.48 + 7.82 * 13)

#9.7.3
# confidence interval for regression parameters
ci.dist.by.unins <- confint(object = dist.by.unins)
ci.dist.by.unins

#9.7.4
# use predict to find predicted value of distance for 10% uninsured
pred.dist.ssp <- predict(object = dist.by.unins,
                         newdata = data.frame(pctunins = 10),
                         interval = "confidence")
pred.dist.ssp

# use predict to find predicted value for all observed x
pred.dist.ssp.all <- predict(object = dist.by.unins,
                             interval = "confidence")

# print out the first six predicted values and CI
head(x = pred.dist.ssp.all)

#9.7.5 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#first, get predicted distances for county with 2% uninsured...
pred.dist.ssp.2perc <- predict(object = dist.by.unins,
                         newdata = data.frame(pctunins = 2),
                         interval = "confidence")

#.. and a county with 12% uninsured
pred.dist.ssp.12perc <- predict(object = dist.by.unins,
                               newdata = data.frame(pctunins = 12),
                               interval = "confidence")

#The difference between the two can be derive reproducibly by substracting:
(diff.2.to.12.perc <- pred.dist.ssp.2perc[1,1] - pred.dist.ssp.12perc[1,1])

#We can also add a confidence interval around this:
(diff.2.to.12.perc.lower <- pred.dist.ssp.2perc[1,2] - pred.dist.ssp.12perc[1,2])
(diff.2.to.12.perc.upper <- pred.dist.ssp.2perc[1,3] - pred.dist.ssp.12perc[1,3])

#9.8.4 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
dist.by.rx <- lm(formula = dist_SSP ~ opioid_RxRate,
                 data = dist.ssp, na.action = na.exclude)
summary(dist.by.rx)

#9.9.5
# percent without health insurance and distance to needle exchange (Figure 9.17)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP)) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  theme_minimal() +
  labs(y = "Miles to syringe program", x = "Percent uninsured") +
  scale_color_manual(values = c("gray60", "deeppink"), name = "") +
  scale_size_manual(values = 2, name = "")

# testing for equal variance
const.var.test <- lmtest::bptest(formula = dist.by.unins)
const.var.test

# test independence of residuals
lmtest::dwtest(formula = dist.by.unins)

#9.9.7
# check residual plot of uninsured percent and distance to syringe program
# (Figure 9.19)
data.frame(dist.by.unins$residuals) %>%
  ggplot(aes(x = dist.by.unins.residuals)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Residual (distance between observed and predicted\nmiles to syringe program)",
       y = "Number of counties")

# check residual plot of uninsured percent and distance to syringe program
# (Figure 9.20)
data.frame(dist.by.unins$residuals) %>%
  ggplot(aes(sample = dist.by.unins.residuals)) +
  geom_abline(aes(intercept = mean(x = dist.by.unins.residuals),
                  slope = sd(x = dist.by.unins.residuals),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  stat_qq(aes(size = "County"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed residuals (distance between observed and\npredicted miles to syringe program)") +
  scale_size_manual(values = 1, name = "") +
  scale_linetype_manual(values = 1, name = "")

#9.9.9.1
# add standardized residuals and predicted values to data frame
dist.ssp.diag <- dist.ssp %>%
  mutate(standardres = rstandard(model = dist.by.unins)) %>%
  mutate(predicted = predict(object = dist.by.unins))

# get a subset of counties with large standardized residuals
dist.ssp.diag %>%
  filter(abs(x = standardres) > 1.96) %>%
select(county, STATEABBREVIATION, dist_SSP, pctunins, predicted,
       standardres)

#9.9.9.2
# get dfbetas and add to data frame
# there will be one new variable per parameter
dist.ssp.diag <- dist.ssp %>%
  mutate(standardres = rstandard(model = dist.by.unins)) %>%
  mutate(dfbeta.intercept = dfbeta(model = dist.by.unins)[ , 1]) %>%
  mutate(dfbeta.slope = dfbeta(model = dist.by.unins)[ , 2])%>%
  mutate(predicted = predict(object = dist.by.unins))

# get subset of states with dfbetas > 2 for intercept and slope
dist.ssp.diag %>%
  filter(abs(x = dfbeta.intercept) > 2 | abs(x = dfbeta.slope) > 2) %>%
  select(county, STATEABBREVIATION, dist_SSP, pctunins, predicted,
         dfbeta.intercept, dfbeta.slope)

#9.9.9.3
# cooks distance
# greater than 4/n is some influence
dist.ssp.diag <- dist.ssp %>%
  mutate(standardres = rstandard(model = dist.by.unins)) %>%
  mutate(dfbeta.intercept = dfbeta(model = dist.by.unins)[ , 1]) %>%
  mutate(dfbeta.slope = dfbeta(model = dist.by.unins)[ , 2]) %>%
  mutate(cooks.dist = cooks.distance(model = dist.by.unins)) %>%
  mutate(predicted = predict(object = dist.by.unins))

# find counties with some influence
dist.ssp.diag %>%
  filter(cooks.dist > 4/n()) %>%
  select(county, STATEABBREVIATION, dist_SSP, pctunins, predicted,
         cooks.dist)

#9.9.4
# leverage values
# identify those that are greater than 2p/n
dist.ssp.diag <- dist.ssp %>%
  mutate(standardres = rstandard(model = dist.by.unins)) %>%
  mutate(dfbeta.intercept = dfbeta(model = dist.by.unins)[ , 1]) %>%
  mutate(dfbeta.slope = dfbeta(model = dist.by.unins)[ , 2]) %>%
  mutate(cooks.dist = cooks.distance(model = dist.by.unins)) %>%
  mutate(lever = hatvalues(model = dist.by.unins)) %>%
  mutate(predicted = predict(object = dist.by.unins))

# 2p/n = 2*2/500
dist.ssp.diag %>%
  filter(lever > 2*2/n()) %>%
  select(county, STATEABBREVIATION, dist_SSP, pctunins, predicted,
         lever)

# sum the number of times observations were outliers/influential
dist.ssp.diag <- dist.ssp %>%
  mutate(standardres = rstandard(model = dist.by.unins)) %>%
  mutate(dfbeta.intercept = dfbeta(model = dist.by.unins)[ , 1]) %>%
  mutate(dfbeta.slope = dfbeta(model = dist.by.unins)[ , 2]) %>%
  mutate(cooks.dist = cooks.distance(model = dist.by.unins)) %>%
  mutate(lever = hatvalues(model = dist.by.unins)) %>%
  mutate(predicted = predict(object = dist.by.unins)) %>%
  mutate(outlier.infl = as.numeric(x = lever > 2*2/n()) +
           as.numeric(x = cooks.dist > 4/n()) +
           as.numeric(x = abs(x = dfbeta.intercept) > 2) +
           as.numeric(x = abs(x = dfbeta.slope) > 2) +
           as.numeric(x = abs(x = standardres) > 1.96))

#9.9.10
# create a subset of those with 2 or more measures indicating outlier/ influential
dist.ssp.diag %>%
  filter(outlier.infl >= 2)

#9.9.11 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
#create and save subset of data with influential observations
inf.obs <- dist.ssp.diag %>%
  filter(outlier.infl >= 2)

#find county where the predicted value is furthest away from observed value, and closest to observed value
#largest diff
inf.obs %>%
  mutate(obs_min_pred = dist_SSP - predicted) %>%
  filter(obs_min_pred == max(abs(obs_min_pred))) %>%
  select(county,STATEABBREVIATION, obs_min_pred)

#smallest difference
inf.obs %>%
  mutate(obs_min_pred = dist_SSP - predicted) %>%
  filter(obs_min_pred == min(abs(obs_min_pred))) %>%
  select(county, STATEABBREVIATION, obs_min_pred)

#9.10
# linear regression distance to syringe program by
# uninsured percent and metro status in 500 counties
dist.by.unins.metro <- lm(formula = dist_SSP ~ pctunins +
                            metro, data = dist.ssp)
summary(object = dist.by.unins.metro)

# graphing the regression model with percent uninsured and metro (Figure 9.21)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = dist_SSP, group = metro)) +
  geom_line(data = broom::augment(x = dist.by.unins.metro),
            aes(y = .fitted, linetype = metro)) +
  geom_point(aes(color = metro), alpha = .4, size = 2) +
  theme_minimal() +
  scale_color_manual(values = c("dodgerblue2", "deeppink"), name =
                       "County") +
  ylab("Miles to nearest syringe program") +
  xlab("County percent uninsured") +
  scale_linetype_manual(values = c(1, 2), name = "Regression line\n(predicted values)")

#9.10.2.1
# check normality of distance variable (Figure 9.22)
dist.ssp %>%
  ggplot(aes(x = dist_SSP)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Miles to syringe program",
       y = "Number of counties")

# check normality of uninsured variable (Figure 9.23)
dist.ssp %>%
  ggplot(aes(x = pctunins)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Percent uninsured",
       y = "Number of counties")

# Q-Q plot of distance variable to check normality (Figure 9.24)
dist.ssp %>%
  ggplot(aes(sample = dist_SSP)) +
stat_qq(aes(color = "County"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = dist_SSP), slope = sd(x = dist_SSP),
                  linetype = "Normally distributed"), color = "gray60",
              size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed miles to syringe program") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# Q-Q plot of uninsured variable to check normality (Figure 9.25)
dist.ssp %>%
  ggplot(aes(sample = pctunins)) +
  stat_qq(aes(color = "County"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = pctunins), slope = sd(x = pctunins),
                  linetype = "Normally distributed"),
              size = 1, color = "gray60") +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed percent uninsured") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

#9.10.2.2
# histograms of square root of dist_SSP (Figure 9.26)
# cube root
cube.root.dist <- dist.ssp %>%
ggplot(aes(x = (dist_SSP)^(1/3))) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Cube root of distance", y = "Number of counties") +
  theme_minimal()

# square root
sq.root.dist <- dist.ssp %>%
  ggplot(aes(x = sqrt(x = dist_SSP))) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Square root of distance", y = "")+
  theme_minimal()

# inverse
inverse.dist <- dist.ssp %>%
  ggplot(aes(x = 1/dist_SSP)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Inverse of distance", y = "Number of counties")+
  theme_minimal()

# log
log.dist <- dist.ssp %>%
  ggplot(aes(x = log(x = dist_SSP))) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Log of distance", y = "")+
  theme_minimal()

# view options for transformation
gridExtra::grid.arrange(cube.root.dist, sq.root.dist,
                        inverse.dist, log.dist)

# histograms of transformed HIVprevalence (Figure 9.27)
# cube root
cube.root.hiv <- dist.ssp %>%
  ggplot(aes(x = (HIVprevalence)^(1/3))) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Cube root of HIV prevalence", y = "Number of counties")+
  theme_minimal()

# square root
sq.root.hiv <- dist.ssp %>%
  ggplot(aes(x = sqrt(x = HIVprevalence))) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Square root of HIV prevalence", y = "")+
  theme_minimal()

# inverse
inverse.hiv <- dist.ssp %>%
  ggplot(aes(x = 1/HIVprevalence)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Inverse of HIV prevalence", y = "Number of counties")+
  theme_minimal()

# log
log.hiv <- dist.ssp %>%
  ggplot(aes(x = log(x = HIVprevalence))) +
  geom_histogram(fill = "#7463AC", col = "white") +
  labs(x = "Log of HIV prevalence", y = "") +
  theme_minimal()

# view options for transformation
gridExtra::grid.arrange(cube.root.hiv, sq.root.hiv,
                        inverse.hiv, log.hiv)

# linear regression of distance by percent uninsured, HIV prevalence,
# metro status
dist.full.model <- lm(formula = (dist_SSP)^(1/3) ~ pctunins +
                        log(x = HIVprevalence) + metro,
                      data = dist.ssp,
                      na.action = na.exclude)
summary(object = dist.full.model)

#9.10.3.1
# correlations among continuous variables in the full model
dist.ssp %>%
  mutate(log.HIVprev = log(x = HIVprevalence)) %>%
  drop_na(log.HIVprev) %>%
  summarize(cor.hiv.unins = cor(x = log.HIVprev, y = pctunins))

#9.10.3.2
# VIF for model with poverty
car::vif(mod = dist.full.model)

#9.10.4
# log of HIV prevalence and cube root of distance to needle exchange
# (Figure 9.28)
dist.ssp %>%
  ggplot(aes(x = log(x = HIVprevalence), y = (dist_SSP)^(1/3))) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  theme_minimal() +
  labs(y = "Cube root of miles to syringe program", x = "Log of HIV prevalence") +
  scale_color_manual(values = c("gray60", "deeppink"), name = "") +
  scale_size_manual(values = 2, name = "")

# percent uninsured and cube root of distance to needle exchange (Figure 9.29)
dist.ssp %>%
  ggplot(aes(x = pctunins, y = (dist_SSP)^(1/3))) +
  geom_point(aes(size = "County"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  theme_minimal() +
  labs(y = "Cube root of miles to syringe program", x = "Percent uninsured") +
  scale_color_manual(values = c("gray60", "deeppink"), name = "") +
  scale_size_manual(values = 2, name = "")

#9.10.5
# testing for equal variance
const.var.test.full <- lmtest::bptest(formula = dist.full.model)
const.var.test.full

#9.10.6
# test independence of residuals
lmtest::dwtest(formula = dist.full.model)

#9.10.7
# check residual plot of percent uninsured and distance to syringe program
# (Figure 9.30)
data.frame(dist.full.model$residuals) %>%
  ggplot(aes(x = dist.full.model.residuals)) +
geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Residual (difference between observed and predicted values)",
       y = "Number of counties")

# check residual plot of percent uninsured and distance to syringe program
# (Figure 9.31)
data.frame(dist.full.model$residuals) %>%
  ggplot(aes(sample = dist.full.model.residuals)) +
  geom_abline(aes(intercept = mean(x = dist.full.model.residuals),
                  slope = sd(x = dist.full.model.residuals),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  stat_qq(aes(size = "County"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(x="Theoretical normal distribution",
     y="Observed residuals") +
  scale_size_manual(values = 1, name = "") +
  scale_linetype_manual(values = 1, name = "")

#9.10.8
# partial F test for dist.by.unins and dist.by.unins.metro
anova(object = dist.by.unins, dist.by.unins.metro)

#9.10.9 ACHIEVEMENT 7: CHECK YOUR UNDERSTANDING
#For the model:
#H0: A model including log HIV prevalence and percent uninsured is not significantly better
# at predicting cubed distance to a needle exchange facility than the mean cubed distance to 
#the needle exchange facility
#HA: A model including log HIV prevalence and percent uninsured is significantly better
# at predicting cubed distance to a needle exchange facility than the mean cubed distance to 
#the needle exchange facility

#For a predictor:
#H0: The slope of the coefficient for log HIV prevalence is 0.
#HA: The slope of the coefficient for log HIV prevalence is not 0.