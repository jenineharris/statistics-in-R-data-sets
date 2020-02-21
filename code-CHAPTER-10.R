#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 18, 2019                                                    #
# Purpose: Review Code for Chapter 10 of Dr. Harris' Book                       #
#          Binary Logistic Regression                                           #
#         The R Team and the Perplexing Libraries Problem                       #
# Packages Used: tidyverse, odds.n.ends, lmtest, car, tableone                  #
# Data Used: PEW research library survey 2016                                   #
# Last Update: Nov 18, 2019                                                     #
#################################################################################

#Box 10.1
# bring in the data
libraries <- read.csv(file = "[data folder location]/data/pew_libraries_2016_ch10.csv")

# open the tidyverse
library(package = "tidyverse")

# subset library data set to variables of interest
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp)

# check the new data frame
summary(object = libraries.cleaned)

# add age data management
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98))%>%
  mutate(age = na_if(x = age, y = 99))

# check the recoding
summary(object = libraries.cleaned$age)

# recode sex, parent, disabled
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98)) %>%
  mutate(age = na_if(x = age, y = 99)) %>%
  mutate(sex = recode(.x = sex, `1` = "male", `2` = "female")) %>%
  mutate(parent = recode(.x = par, `1` = "parent", `2` = "not parent")) %>%
  mutate(disabled = recode(.x = disa, `1` = "yes", `2` = "no"))

# check the recoding
summary(object = libraries.cleaned)

# recode library use variable
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98)) %>%
  mutate(age = na_if(x = age, y = 99)) %>%
  mutate(sex = factor(recode(.x = sex,
                             `1` = "male",
                             `2` = "female"))) %>%
  mutate(parent = factor(recode(.x = par,
                                `1` = "parent",
                                `2` = "not parent"))) %>%
  mutate(disabled = factor(recode(.x = disa,
                                  `1` = "yes",
                                  `2` = "no"))) %>%
  mutate(uses.lib = factor(recode(.x = libusea,
                                  `1` = "yes",
                                  `2` = "no",
                                  `3` = "no")))

# check the recoding
summary(object = libraries.cleaned$uses.lib)

# subset library data set eight variables of interest
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98)) %>%
  mutate(age = na_if(x = age, y = 99)) %>%
  mutate(sex = factor(x = recode(.x = sex,
                                 `1` = "male",
                                 `2` = "female"))) %>%
  mutate(parent = factor(x = recode(.x = par,
                                    `1` = "parent",
                                    `2` = "not parent"))) %>%
  mutate(disabled = factor(x = recode(.x = disa,
                                      `1` = "yes",
                                      `2` = "no"))) %>%
  mutate(uses.lib = factor(x = recode(.x = libusea,
                                      `1` = "yes",
                                      `2` = "no",
                                      `3` = "no"))) %>%
  mutate(ses = factor(x = if_else(condition = hh1 == 1 & inc == 1 |
                                    hh1 == 2 & inc <= 2 |
                                    hh1 == 3 & inc <= 2 |
                                    hh1 == 4 & inc <= 3 |
                                    hh1 == 5 & inc <= 3 |
                                    hh1 == 6 & inc <= 4 |
                                    hh1 == 7 & inc <= 4 |
                                    hh1 == 8 & inc <= 5 ,
                                  true = "low",
                                  false = if_else(condition = inc == 9,
                                                  true = "high",
                                                  false = "medium"))))
# check recoding for SES
summary(object = libraries.cleaned$ses)

# add race recoding
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98)) %>%
  mutate(age = na_if(x = age, y = 99)) %>%
  mutate(sex = factor(x = recode(.x = sex,
                                 `1` = "male",
                                 `2` = "female"))) %>%
  mutate(parent = factor(x = recode(.x = par,
                                    `1` = "parent",
                                    `2` = "not parent"))) %>%
  mutate(disabled = factor(x = recode(.x = disa,
                                      `1` = "yes",
                                      `2` = "no"))) %>%
  mutate(uses.lib = factor(x = recode(.x = libusea,
                                      `1` = "yes",
                                      `2` = "no",
                                      `3` = "no"))) %>%
  mutate(ses = factor(x = if_else(condition = hh1 == 1 & inc == 1 |
                                    hh1 == 2 & inc <= 2 |
                                    hh1 == 3 & inc <= 2 |
                                    hh1 == 4 & inc <= 3 |
                                    hh1 == 5 & inc <= 3 |
                                    hh1 == 6 & inc <= 4 |
                                    hh1 == 7 & inc <= 4 |
                                    hh1 == 8 & inc <= 5 ,
                                  true = "low",
                                  false = if_else(condition = inc == 9,
                                                   true = "high",
                                                   false = "medium")))) %>%
  mutate(raceth = factor(x = if_else(condition = hisp == 2 &
                                       race3m1 == 2 &
                                       is.na(x = race3m2),
                                     true = "Non-Hispanic Black",
                                     false = if_else(condition = hisp == 2 &
                                                       race3m1 == 1 & is.na(x = race3m2),
                                                     true = "Non-Hispanic White",
                                                     false = if_else(condition = hisp == 1 |
                                                                     race3m1 == 7 |
                                                                       race3m2 == 7 |
                                                                       race3m3 == 7,
                                                                     true = "Hispanic",
                                                                     false = "Non-Hisp Other or Mixed")))))
# check recoding for raceth
summary(object = libraries.cleaned$raceth)

# recode other and mixed to NA
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98)) %>%
  mutate(age = na_if(x = age, y = 99)) %>%
  mutate(sex = factor(x = recode(.x = sex,
                                 `1` = "male",
                                 `2` = "female"))) %>%
  mutate(parent = factor(x = recode(.x = par,
                                    `1` = "parent",
                                    `2` = "not parent"))) %>%
  mutate(disabled = factor(x = recode(.x = disa,
                                      `1` = "yes",
                                      `2` = "no"))) %>%
  mutate(uses.lib = factor(x = recode(.x = libusea,
                                      `1` = "yes",
                                      `2` = "no",
                                      `3` = "no"))) %>%
  mutate(ses = factor(x = if_else(condition = hh1 == 1 & inc == 1 |
                                    hh1 == 2 & inc <= 2 |
                                    hh1 == 3 & inc <= 2 |
                                    hh1 == 4 & inc <= 3 |
                                  hh1 == 5 & inc <= 3 |
                                    hh1 == 6 & inc <= 4 |
                                    hh1 == 7 & inc <= 4 |
                                    hh1 == 8 & inc <= 5 ,
                                  true = "low",
                                  false = if_else(condition = inc == 9,
                                                   true = "high",
                                                   false = "medium")))) %>%
  mutate(raceth = factor(x = if_else(condition = hisp == 2 &
                                       race3m1 == 2 &
                                       is.na(race3m2),
                                     true = "Non-Hispanic Black",
                                     false = if_else(condition = hisp == 2 &
                                                       race3m1 == 1 & is.na(x = race3m2),
                                                     true = "Non-Hispanic White",
                                                     false = if_else(condition = hisp == 1 |
                                                                       race3m1 == 7 |
                                                                       race3m2 == 7 |
                                                                       race3m3 == 7,
                                                                     true = "Hispanic",
                                                                     false = NA_character_)))))
# check recoding for raceth
summary(object = libraries.cleaned$raceth)

# complete cleaning
libraries.cleaned <- libraries %>%
  select(libusea, age, sex, par, disa, inc, race3m1,
         race3m2, race3m3, educ2, live1, hh1, hisp) %>%
  mutate(age = na_if(x = age, y = 98)) %>%
  mutate(age = na_if(x = age, y = 99)) %>%
  mutate(sex = factor(x = recode(.x = sex,
                                 `1` = "male",
                                 `2` = "female"))) %>%
  mutate(parent = factor(x = recode(.x = par,
                                    `1` = "parent",
                                    `2` = "not parent"))) %>%
  mutate(disabled = factor(x = recode(.x = disa,
                                      `1` = "yes",
                                      `2` = "no"))) %>%
  mutate(uses.lib = factor(x = recode(.x = libusea,
                                      `1` = "yes",
                                      `2` = "no",
                                      `3` = "no"))) %>%
  mutate(ses = factor(x = if_else(condition = hh1 == 1 & inc == 1 |
                                    hh1 == 2 & inc <= 2 |
                                    hh1 == 3 & inc <= 2 |
                                    hh1 == 4 & inc <= 3 |
                                    hh1 == 5 & inc <= 3 |
                                    hh1 == 6 & inc <= 4 |
                                    hh1 == 7 & inc <= 4 |
                                    hh1 == 8 & inc <= 5 ,
                                  true = "low",
                                  false = if_else(condition = inc == 9,
                                                  true = "high",
                                                  false = "medium")))) %>%
  mutate(raceth = factor(x = if_else(condition = hisp == 2 &
                                       race3m1 == 2 &
                                       is.na(race3m2),
                                     true = "Non-Hispanic Black",
                                     false = if_else(condition = hisp == 2 &
                                                       race3m1 == 1 & is.na(x = race3m2),
                                                     true = "Non-Hispanic White",
                                                     false = if_else(condition = hisp == 1 |
                                                                       race3m1 == 7 |
                                                                       race3m2 == 7 |
                                                                       race3m3 == 7,
                                                                     true = "Hispanic",
                                                                     false = NA_character_))))) %>%
  mutate(educ = factor(x = if_else(condition = libraries$educ2 < 3,
                                   true = "< HS",
                                   false = if_else(condition = libraries$educ2 < 6,
                                                   true = "HS to 2-year degree",
                                                   false = "Four-year degree or more")))) %>%
  mutate(rurality = factor(x = if_else(condition =
                                         libraries$live1 == 1,
                                       true = "urban",
                                       false = if_else(condition = libraries$live1 == 2,
                                                       true = "suburban",
                                                       false = if_else(condition = libraries$live1 < 8,
                                                                       true = "rural",
                                                                       false = NA_character_))))) %>%
  select(- c(libusea, par, disa, inc, race3m1, race3m2,
             race3m3, educ2, live1, hh1, hisp))

# check recoding
summary(object = libraries.cleaned)

# write to a new csv file
write.csv(x = libraries.cleaned,
          file = "[data folder location]/data/pew_libraries_2016_cleaned_ch10.csv",
          row.names = FALSE)

#10.4
# import the libraries cleaned file
libraries.cleaned <- read.csv(file = "[data folder location]/data/pew_libraries_2016_cleaned_ch10.csv")

# check the data
summary(object = libraries.cleaned)

#10.4.1
# open tidyverse
library(package = "tidyverse")

# examine the distribution of age (Figure 10.2)
libraries.cleaned %>%
  ggplot(aes(x = age)) +
  geom_density(fill = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Probability density", x = "Age in years")

# open tableone package
library(package = "tableone")

# get a table of descriptive statistics
table.desc <- CreateTableOne(data = libraries.cleaned)
print(table.desc,
      nonnormal = 'age',
      showAllLevels = TRUE)

# get a table of descriptive statistics with bivariate tests
table.desc <- CreateTableOne(data = libraries.cleaned,
                             strata = 'uses.lib',
                             vars = c("age", "sex", "parent", "disabled",
                                      "ses", "raceth", "educ", "rurality"))
print(table.desc,
      nonnormal = 'age',
      showAllLevels = TRUE)

#10.4.2 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#A greater percentage of those who use libraries are parents (28.2%) in comparison to the percentage of
# individuals who are parents that do not use libraries (20.9%) (p < 0.01). COnversely, individuals with
#disabilites make up a smaller percentage of those who use libraries (13.7%) than those who do not (p = 0.024).
#Rurality was also significantly associated with library use, with individuals in urban (24.0%) and
#suburban (24.9%) making up a greter percentage of those who used libraries than those who did not use
#libraries (20.5% and 19.9%, respectively), and individuals in rural areas making up a greater
#percentage of those who did not use libraries compared ot those who did use libraries (59.7% vs 51%) (p = 0.002).
#Neither race (p = 0.110) nor socioeconomic status (p = 0.088) were significantly associated with library
#use, although Non-Hispanic Black, Non-Hispanic White, and medium SES individuals made up a higher percentage of
#library users than non-users.

#get standardized residuls for sex
sex.libuse.chi <- chisq.test(x = libraries.cleaned$sex,
                             y = libraries.cleaned$uses.lib)
sex.libuse.chi$residuals

#get standardized residuls for parent
par.libuse.chi <- chisq.test(x = libraries.cleaned$parent,
                             y = libraries.cleaned$uses.lib)
par.libuse.chi$residuals

#get standardized residuls for disability
dis.libuse.chi <- chisq.test(x = libraries.cleaned$disabled,
                             y = libraries.cleaned$uses.lib)
dis.libuse.chi$residuals

#get standardized residuls for education
edu.libuse.chi <- chisq.test(x = libraries.cleaned$educ,
                             y = libraries.cleaned$uses.lib)
edu.libuse.chi$residuals

#get standardized residuls for rurality
rur.libuse.chi <- chisq.test(x = libraries.cleaned$rurality,
                             y = libraries.cleaned$uses.lib)
rur.libuse.chi$residuals

#add standardized residuals to interpretation
#A greater percentage of those who use libraries are parents (28.2%) in comparison to the percentage of
# individuals who are parents that do not use libraries (20.9%) (p < 0.01). Based on standardized
#residuals, parents were overrepresented among library users (std resid = 2.08) and
#underrepresented among nonusers (std resid = -2.06). COnversely, individuals with
#disabilites make up a smaller percentage of those who use libraries (13.7%, std resid = -1.52) 
#than those who do not (18.0%, std resid = 1.50) (p = 0.024).
#Rurality was also significantly associated with library use, with individuals in urban (24.0%) and
#suburban (24.9%) making up a greter percentage of those who used libraries than those who did not use
#libraries (20.5% and 19.9%, respectively), and individuals in rural areas making up a greater
#percentage of those who did not use libraries compared ot those who did use libraries (59.7% vs 51%) (p = 0.002).
#Standardized residuals showed that the rural category was the strongest driver of the association between
# rurality and library use, with more rural individuals being nonusers (std resid = 1.63) and fewer
#rural individuals beign users (std resid = - 1.65) of libraries than expected.
#Neither race (p = 0.110) nor socioeconomic status (p = 0.088) were significantly associated with library
#use, although Non-Hispanic Black, Non-Hispanic White, and medium SES individuals made up a higher percentage of
#library users than non-users.

#10.6.1.2
# checking the order of the outcome variable categories
levels(x = libraries.cleaned$uses.lib)

# make no the reference group
libraries.cleaned <- libraries.cleaned %>%
  mutate(uses.lib = relevel(x = uses.lib, ref = "no"))

# check the re-ordering
levels(x = libraries.cleaned$uses.lib)

# estimate the library use model and print results
lib.model.small <- glm(formula = uses.lib ~ age,
                       data = libraries.cleaned,
                       family = binomial("logit"))
summary(object = lib.model.small)

# open odds.n.ends
library(package = "odds.n.ends")

# get model fit, model significance, odds ratios
odds.n.ends(x = lib.model.small)

#10.6.3
# checking model results again
odds.n.ends(x = lib.model.small)

#10.6.7 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
# simple logistic with sex predicting library use
lib.by.sex <- glm(formula = uses.lib ~ sex,
                  data = libraries.cleaned,
                  family = binomial("logit"))
odds.n.ends(x = lib.by.sex)

#The binary logistic regression predicting library use from sex was significantly better at
#predicting the probability of library use than the null tmodel (X2(1) = 33.866, p < 0.001).
#Based on the odds ratios, males had 44.4% lower odds of library use than females (OR 95% CI = 0.456 - 0.679).

#10.7.3 CHECK YOUR UNDERSTANDING
#Count R 2 = NCorrect/ NTotal
lib.by.sex.odds.n.ends <- odds.n.ends(x = lib.by.sex)

#contingency table
lib.by.sex.cont.tab<- lib.by.sex.odds.n.ends$`Contingency tables (model fit): frequency predicted`

#correct/total: Count R2
(lib.by.sex.countR2 <- (lib.by.sex.cont.tab[1,1] + lib.by.sex.cont.tab[2,2]) / lib.by.sex.cont.tab[3,3])

#(correct - most common )/ (total - most common): Adjusted count R2
(lib.by.sex.adjCountR2 <- (lib.by.sex.cont.tab[1,1] + lib.by.sex.cont.tab[2,2]- #add up the correct
                          max(c(lib.by.sex.cont.tab[3,1], lib.by.sex.cont.tab[3,2]))) / #subtract MAXIMUM of observed
                      (lib.by.sex.cont.tab[3,3] - 
                         max(c(lib.by.sex.cont.tab[3,1], lib.by.sex.cont.tab[3,2])))) #divide by tot-max

#difference btwn r2 and adj r2
lib.by.sex.countR2 - lib.by.sex.adjCountR2

#The binary logistic regression predicting library use from sex correctly predicted
# 57.28% of individuals' library use (Count R2 = 0.5728). Compared to the null model, there were 43.6% more
#correct predictions (Adjusted Count R2 = 0.1364)

#10.8
# estimate the library use model and print results
lib.model <- glm(formula = uses.lib ~ age + sex + educ + parent +
                   disabled + rurality + raceth + ses,
                 data = libraries.cleaned,
                 na.action = na.exclude,
                 family = binomial("logit"))
odds.n.ends(x = lib.model)

#10.8.5 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
# estimate the library use model and print results
lib.model.noSES.noPar <- glm(formula = uses.lib ~ age + sex + educ +
                   disabled + rurality + raceth,
                 data = libraries.cleaned,
                 na.action = na.exclude,
                 family = binomial("logit"))
odds.n.ends(x = lib.model.noSES.noPar)

#The binary logistic model predicting library use from age, sex, education, disability status,
#rurality, and race/ethnicity was significantly better than the null model in predicting the
#probability of library use (X2 (9) = 90.368, p < 0.001).

#10.9.5 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
#There is no statistically significant association between age and library use (OR = .56; 95% CI: .34-1.23).

#10.10.2
#make a variable of the log-odds of the predicted values
logit.use <- log(x = lib.model$fitted.values/(1-lib.model$fitted.values))

# make a small data frame with the log-odds variable and the age predictor
linearity.data <- data.frame(logit.use, age = lib.model$model$age)

# create a plot (Figure 10.9)
linearity.data %>%
  ggplot(aes(x = age, y = logit.use))+
  geom_point(aes(size = "Observation"), color = "gray60", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear")) +
  theme_minimal() +
  labs(x = "Age in years", y = "Log-odds of library use predicted probability") +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "")

#10.10.3
# compute GVIF
car::vif(mod = lib.model)

#10.10.4.1
# get standardized residuals and add to data frame
libraries.cleaned <- libraries.cleaned %>%
  mutate(standardized = rstandard(model = lib.model))

# check the residuals for large values > 2
libraries.cleaned %>%
  drop_na(standardized) %>%
  summarize(max.resid = max(abs(x = standardized)))

#10.10.4.2
# get influence statistics
influence.lib.mod <- influence.measures(model = lib.model)

# summarize data frame with dfbetas, cooks, leverage
summary(object = influence.lib.mod$infmat)

# save the data frame
influence.lib <- data.frame(influence.lib.mod$infmat)

#10.10.4.3
# observations with high Cook's D
influence.lib %>%
  filter(cook.d > 4/1427)

#10.10.4.4
# observations with high Leverage
influence.lib %>%
  filter(hat > 2*13/1427)

# observations with high leverage and Cook's D
influence.lib %>%
  filter(hat > 2*13/1427 & cook.d > 4/1427)

# make row names as a variable
influence.lib <- influence.lib %>%
  rownames_to_column()

# merge data frame with diagnostic stats
libraries.cleaned.diag <- libraries.cleaned %>%
  rownames_to_column() %>%
  merge(x = influence.lib, by = 'rowname') %>%
  mutate(pred.prob = predict(object = lib.model, type = "response"))

# find mean predicted probability
libraries.cleaned.diag %>%
  summarize(mean.predicted = mean(x = pred.prob, na.rm = TRUE))

# review influential observations
libraries.cleaned.diag %>%
  filter(hat > 2*13/1427 & cook.d > 4/1427) %>%
  select(rowname, age, sex, educ, parent, disabled,
         rurality, raceth, ses, hat, cook.d, pred.prob)

#10.10.5 ACHIEVEMENT 7: CHECK YOUR UNDERSTANDING
#Independence of observations, Linearity (of log odds predicted probability and continuous predictor)

#10.11
# make a new data frame containing the observations of interest
examp.data <- data.frame(age = c(35, 65, 68),
                         sex = c("male", "female", "male"),
                         educ = c("Four-year degree or more",
                                  "Four-year degree or more",
                                  "Four-year degree or more"),
                         disabled = c("no", "no", "no"),
                         parent = c("not parent", "parent", "parent"),
                         rurality = c("rural", "rural", "rural"),
                         raceth = c("Non-Hispanic White",
                                    "Non-Hispanic White",
                                    "Non-Hispanic White"),
                         ses = c("low", "medium", "medium"))

# use the new data frame to predict
predictions <- predict(object = lib.model, newdata = examp.data,
                       type = "response")
predictions

#10.11.1 CHECK YOUR UNDERSTANDING (answers will vary)
examp.data.2 <- data.frame(age = c(23, 27, 28),
                           sex = c("female", "male", "female"),
                           educ = c("Four-year degree or more",
                                    "Four-year degree or more",
                                    "Four-year degree or more"),
                           disabled = c("no", "no", "no"),
                           parent = c("not parent", 
                                      "not parent",
                                      "not parent"),
                           rurality = c("urban", "urban", "suburban"),
                           ses = c("medium", "high", "low"),
                           raceth = c("Non-Hispanic White",
                                      "Non-Hispanic White",
                                      "Non-Hispanic White"))

(predictions2 <- predict(object = lib.model, newdata = examp.data.2,
                        type = "response"))

#I have a 73.1% predicted probability of library use, while my brother and sister have
#predicted probabilities of 60.17% and 73.38%, respectively.

#10.12
# the relationship between parent status and library use (Figure 10.11)
libraries.cleaned %>%
  drop_na(parent) %>%
  ggplot(aes(x = parent, fill = factor(uses.lib))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Parent status", y = "Number of participants") +
  scale_fill_manual(values=c("#7463AC", "gray"),
                    name="Library use")

# library use by sex (Figure 10.12)
libraries.cleaned %>%
  drop_na(parent) %>%
  ggplot(aes(x = sex, fill = factor(uses.lib))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Sex", y = "Number of participants") +
  scale_fill_manual(values=c("#7463AC", "gray"),
                    name="Library use")

# the relationship among sex, parent status, and library use (Figure 10.13)
libraries.cleaned %>%
  drop_na(parent) %>%
  ggplot(aes(x = parent, fill = factor(uses.lib))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Parent status", y = "Number of participants") +
  scale_fill_manual(values=c("#7463AC", "gray"),
                    name="Library use") +
  facet_grid("sex")

# estimate the library use model and print results
lib.model.int <- glm(formula = uses.lib ~ age + sex + educ + parent +
                       disabled + rurality + ses + raceth + sex*parent,
                     data = libraries.cleaned,
                     family = binomial("logit"))

odds.n.ends(x = lib.model.int)

#10.12.4.2
# compute GVIF
car::vif(mod = lib.model.int)


#10.12.4.3
# make a variable of the log-odds of the outcome
logit.use.int <- log(lib.model.int$fitted.values/(1-lib.model.int$fitted.values))

# make a small data frame with the log-odds variable and the age predictor
linearity.data.int <- data.frame(logit.use.int, age.int = lib.model.int$model$age)

# create a plot (Figure 10.14)
linearity.data.int %>%
  ggplot(aes(x = age.int, y = logit.use.int))+
  geom_point(aes(size = "Observation"), color = "gray", alpha = .6) +
  geom_smooth(se = FALSE, aes(color = "Loess curve")) +
  geom_smooth(method = lm, se = FALSE, aes(color = "linear model")) +
  scale_color_manual(name="Type of fit line", values=c("dodgerblue2",
                                                       "deeppink")) +
  scale_size_manual(values = 1.5, name = "") +
  theme_minimal() +
  labs(x = "Age in years", y = "Log-odds of library use predicted probability")

#10.12.5 ACHIEVEMENT 9: CHECK YOUR UNDERSTANDING
#Assumption 1: Independence of Observations
#Assumption 2: Linearity between logit of predicted probability and all continuous predictors
#Assumption 3: No multicollinearity


#10.13.1.2
# compare simple logistic with age to
# full library use model
lmtest::lrtest(object = lib.model, lib.model.int)

#Box 10.2
# get odds ratio table from lib.model
odds.lib.mod <- data.frame(odds.n.ends(x = lib.model)[4])

# make row names a variable
odds.lib.mod$var <- row.names(x = odds.lib.mod)

# change variable names for easier use
names(x = odds.lib.mod) <- c("OR", "lower", "upper", "variable")

# forest plot of odds ratios from lib.model (Figure 10.15)
odds.lib.mod %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink",
             size = 1) +
  coord_flip() +
  labs(x = "Variable from library use model", y = "Odds ratio (95% CI)") +
  theme_minimal()

# clean variable names for graph
odds.lib.mod.cleaned <- odds.lib.mod %>%
  mutate(variable = recode(.x = variable,
                           "sexmale" = "Male",
                           "ruralityurban" = "Urban residence",
                           "ruralitysuburban" = "Suburban residence",
                           "parentparent" = "Parent",
                           "educHS to 2-year degree" = "HS to 2-year degree",
                           "educFour-year degree or more" = "Four-year degree or more",
                           "disabledyes" = "Disabled",
                           "age" = "Age",
                           "seslow" = "Low socioeconomic status",
                           "sesmedium" = "Medium socioeconomic status",
                           "racethNon-Hispanic White" = "Non-Hispanic white",
                           "racethNon-Hispanic Black" = "Non-Hispanic black",
                           "(Intercept)" = "Intercept"))

# modify graph to include clean variable names (Figure 10.16)
# change scale of y-axis (flipped) to log scale for visualization
odds.lib.mod.cleaned %>%
  ggplot(aes(x = variable, y = OR, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10),
                minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from library use model", y = "Odds ratio (95% CI)") +
  theme_minimal()


# reorder the variable names by odds ratio size (Figure 10.17)
odds.lib.mod.cleaned %>%
  ggplot(aes(x = reorder(variable, OR), y = OR, ymin = lower, ymax =
               upper)) +
  geom_pointrange(color = "#7463AC") +
  geom_hline(yintercept = 1, lty = 2, color = "deeppink", size = 1) +
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10), minor_breaks = NULL)+
  coord_flip() +
  labs(x = "Variable from library use model", y = "Odds ratio (95% CI)") +
  theme_minimal()

lib.model
#10.13.3 ACHIEVEMENT 10: CHECK YOUR UNDERSTANDING
lib.model.complete <- libraries.cleaned %>%
  drop_na(sex, raceth, parent, educ, disabled, ses, age, rurality)

lib.model.small.complete <- glm(formula = uses.lib ~ age,
                                family = binomial("logit"),
                                data = lib.model.complete)


lmtest::lrtest(object = lib.model, lib.model.small.complete)

#A likelihood ratio test was conducted to determine whether the full model was statistically
#significantly better than the model including only age at predicting library use. The full
#model was significantly better (LR X2(11) = 84.672, p < 0.001), and thus the smaller model was
#rejected in favor of the larger model.