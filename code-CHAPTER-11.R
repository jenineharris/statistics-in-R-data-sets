#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 19, 2019                                                    #
# Purpose: Review Code for Chapter 11 of Dr. Harris' Book                       #
#          Multinomial and Ordinal Logistic Regression                          #
#         The R Team and the STEM Diversity Problem                             #
# Packages Used: tidyverse, Hmisc, nnet, mlogit, MASS, ordinal                  #
# Data Used: PEW research library survey 2016                                   #
# Last Update: Nov 19, 2019                                                     #
#################################################################################
setwd("C:\\Users\\svana\\OneDrive\\Documents\\DrHarris\\data")
RNGkind(sample.kind = "Rejection")

#Box 11.1

# load the STEM career data
stem <- Hmisc::sasxport.get(file = "[data folder location]/data/stem-nsf-2017-ch11.xpt")

# open the tidyverse
library(package = "tidyverse")

# make a subset
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age)

# check the new data set
summary(object = stem.cleaned)

# Write a function to recode
RecSatis <- function(x){
  return(recode(x,
                "1" = "Very satisfied",
                "2" = "Somewhat satisfied",
                "3" = "Somewhat dissatisfied",
                "4" = "Very dissatisfied",
                "L" = NA_character_))
}

# recode and rename
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age) %>%
  mutate(job.cat = recode(.x = n2ocprmg,
                          "1" = "CS, Math, Eng",
                          "2" = "Other Sciences",
                          "3" = "Other Sciences",
                          "4" = "Other Sciences",
                          "5" = "CS, Math, Eng",
                          "6" = "CS, Math, Eng",
                          "7" = "Nonscience",
                          "8" = NA_character_)) %>%
  mutate(satis.advance = RecSatis(x = satadv)) %>%
  mutate(satis.salary = RecSatis(x = satsal)) %>%
  mutate(satis.contrib = RecSatis(x = satsoc))


#11.4
# import the STEM career data
stem <- Hmisc::sasxport.get(file = "[data folder location]/data/stem-nsf-2017-ch11.xpt")
# function to recode the satisfaction variables
RecSatis <- function(x){
  return(recode(x,
                "1" = "Very satisfied",
                "2" = "Somewhat satisfied",
                "3" = "Somewhat dissatisfied",
                "4" = "Very dissatisfied",
                "L" = NA_character_))
}

# recode and rename
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age) %>%
  mutate(job.cat = recode(.x = n2ocprmg,
                          "1" = "CS, Math, Eng",
                          "2" = "Other Sciences",
                          "3" = "Other Sciences",
                          "4" = "Other Sciences",
                          "5" = "CS, Math, Eng",
                          "6" = "CS, Math, Eng",
                          "7" = "Nonscience",
                          "8" = NA_character_)) %>%
  mutate(satis.advance = RecSatis(x = satadv)) %>%
  mutate(satis.salary = RecSatis(x = satsal)) %>%
  mutate(satis.contrib = RecSatis(x = satsoc)) %>%
  mutate(sex = recode(.x = gender, "M" = "Male", "F"= "Female")) %>%
  mutate(sex = fct_relevel(.f = sex, c("Male", "Female")))

# make sure the reordering worked
# re-order to have male first for ref group
print(levels(x = stem.cleaned$sex))

# recode and rename
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age) %>%
  mutate(job.cat = recode(.x = n2ocprmg,
                          "1" = "CS, Math, Eng",
                          "2" = "Other Sciences",
                          "3" = "Other Sciences",
                          "4" = "Other Sciences",
                          "5" = "CS, Math, Eng",
                          "6" = "CS, Math, Eng",
                          "7" = "Nonscience",
                          "8" = NA_character_)) %>%
  mutate(satis.advance = RecSatis(x = satadv)) %>%
  mutate(satis.salary = RecSatis(x = satsal)) %>%
  mutate(satis.contrib = RecSatis(x = satsoc)) %>%
  mutate(sex = recode(.x = gender, "M" = "Male", "F"= "Female")) %>%
  mutate(sex = fct_relevel(.f = sex, c("Male", "Female"))) %>%
  mutate(age = as.numeric(x = age)) %>%
  select(-n2ocprmg, -satadv, -satsal, -satsoc, -gender)


# check a summary
summary(object = stem.cleaned)

# open tidyverse
library(package = "tidyverse")

# recode and rename variables
stem.cleaned <- stem %>%
  select(n2ocprmg, satadv, satsal, satsoc, gender, age) %>%
  mutate(job.cat = recode(.x = n2ocprmg,
                          "1" = "CS, Math, Eng",
                          "2" = "Other Sciences",
                          "3" = "Other Sciences",
                          "4" = "Other Sciences",
                          "5" = "CS, Math, Eng",
                          "6" = "CS, Math, Eng",
                          "7" = "Nonscience",
                          "8" = NA_character_)) %>%
  mutate(satis.advance = RecSatis(x = satadv)) %>%
  mutate(satis.salary = RecSatis(x = satsal)) %>%
  mutate(satis.contrib = RecSatis(x = satsoc)) %>%
  mutate(sex = recode(.x = gender, "M" = "Male", "F"= "Female")) %>%
  mutate(sex = fct_relevel(.f = sex, c("Male", "Female"))) %>%
  mutate(age = as.numeric(age)) %>%
  select(-n2ocprmg, -satadv, -satsal, -satsoc, -gender)

# check a summary
summary(object = stem.cleaned)

# set a seed value to take a sample
set.seed(seed = 143)

# take a sample of 1500 cases
# 500 from each job.cat category
stem.samp <- stem.cleaned %>%
  drop_na(job.cat) %>%
  group_by(job.cat) %>%
  sample_n(size = 500)

# check work
summary(object = stem.samp)

#Box 11.2
# set a seed
set.seed(seed = 143)

# take 200 from each job.cat
stem.samp.200 <- stem.cleaned %>%
  group_by(job.cat) %>%
  sample_n(size = 200)

summary(object = stem.samp.200)

# take 200 from each job.cat
# subset first to remove NA from job.cat
set.seed(seed = 143)

stem.samp.200.noNA <- stem.cleaned %>%
  drop_na(job.cat) %>%
  group_by(job.cat) %>%
  sample_n(size = 200)

summary(object = stem.samp.200.noNA)

# sample 10% of each job.cat group
set.seed(seed = 143)
stem.samp.perc <- stem.cleaned %>%
  drop_na(job.cat) %>%
  group_by(job.cat) %>%
  sample_frac(size = .1)

summary(object = stem.samp.perc)

#11.4.1
# plotting distribution of sex within job type (Figure 11.3)
stem.samp %>%
  ggplot(aes(x = sex, group = job.cat, y = ..prop..)) +
  geom_bar(fill = "#7463AC") +
  theme_minimal() +
  labs(y = "Percent within job category", x = "Sex") +
  facet_grid(cols = vars(job.cat)) +
  scale_y_continuous(labels = scales::percent)

# plotting distribution of job type by sex (Figure 11.4)
stem.samp %>%
  ggplot(aes(x = job.cat, y = ..prop.., group = sex)) +
  geom_bar(fill = "#7463AC") +
  theme_minimal() +
  labs(y = "Percent within sex category", x = "Job category") +
  facet_grid(cols = vars(sex)) +
  scale_y_continuous(labels = scales::percent)


# plotting distribution of job type and age (Figure 11.5)
stem.samp %>%
  ggplot(aes(y = age, x = job.cat)) +
  geom_jitter(aes(color = job.cat), alpha = .6) +
  geom_boxplot(aes(fill = job.cat), alpha = .4) +
  scale_fill_manual(values = c("dodgerblue2","#7463AC", "gray40"), guide =
                      FALSE) +
  scale_color_manual(values = c("dodgerblue2","#7463AC", "gray40"), guide =
                       FALSE) +
  theme_minimal() + labs(x = "Job type", y = "Age in years")

# plotting distribution of job type, age, and sex (Figure 11.6)
stem.samp %>%
  ggplot(aes(y = age, x = job.cat, fill = sex)) +
  geom_jitter(aes(color = sex), alpha = .6) +
  geom_boxplot(aes (fill = sex), alpha = .4) +
  scale_fill_manual(values = c("gray", "#7463AC"), name = "Sex") +
  scale_color_manual(values = c("gray", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Job type", y = "Age in years")

# plotting distribution of job type, sex, and age (Figure 11.7)
stem.samp %>%
  ggplot(aes(y = age, x = job.cat)) +
  geom_jitter(aes(color = sex), alpha = .6) +
  geom_boxplot(aes (fill = sex), alpha = .4) +
  scale_fill_manual(values = c("gray", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("gray", "#7463AC"), guide = FALSE) +
  theme_minimal() + labs(x = "Job type", y = "Age in years") +
  facet_grid(cols = vars(sex))

#11.4.2
# plotting distribution of age (Figure 11.8)
stem.samp %>%
  ggplot(aes(x = age)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Age in years", y = "Number of observations") +
  facet_grid(cols = vars(job.cat))

# open tableone package
library(package = "tableone")

# make a table of statistics to examine job.cat
table.desc <- CreateTableOne(data = stem.samp, strata = 'job.cat',
                             vars = c('sex', 'age'))

print(table.desc, showAllLevels = TRUE, nonnormal = 'age')

#11.4.3 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#Since we have a significant Chi-squared test, we probably want to obtain standardized residuals
#as a follow up.
sex.jobcat.chi <- chisq.test(x = stem.samp$sex,
                             y = stem.samp$job.cat)
sex.jobcat.chi$residuals
sex.jobcat.chi

#We also have a significant Kruskal Wallis test. We may want to follow up to learn where
#the difference is, since this is also an omnibus test
age.jobcat.dunn <- dunn.test::dunn.test(x = stem.samp$age, 
                                        g = stem.samp$job.cat,
                                        method = "bonferroni")
?dunn.test

#Interpretation: A Chi-Squared test showed a significant association between sex and job category
#(Computer Science/Math/Engineering, Other Sciences,or Non-Science) (X2(2) = 32.257, p < 0.001).
# Out of 500 respondents in the Computer Science/Math/Engineering category, only 168 were female (33.6%), 
#which was much lower than the expected number of females if job category was unrelated to sex 
#(std resid = -3.37). Although the percentage and frequency of females was also lower than the percentage and
#frequency of males in other science careers (N(%) Female = 231 (46.2%) vs N(%) Male = 269 (53.8%)),
#this observed difference was not substantially different from what would be expected given the 
#overall frequencies of females and career categories in our sample (std resid = 0.90). Conversely,
#a higher frequency of females than expected was observed in Non-Science jobs (N = 254, 50.8%, std resid = 2.46).
#In concordance with the observations in females, more males than expected were in
#Computer Science/Math/Engineering careers than expected (std resid = 2.96), and fewer were in Non-science
#careers (std resid = -2.16).
#A Kruskal-Wallis test showed that age also differed between the job categories (KW(2) = 11.35, p = 0.003).
#Those in computer science, math, and engineering careers (median age = 39, IQR = 31 - 52.25) 
# were significnatly younger than those in other science (median age = 42, IQR = 32 - 56; bonferroni p <0.01)
# or non-science careers (median age = 44, IQR = 32 - 57, bonferroni p <0.01).

#11.5
# get reference groups for job.cat and sex
levels(x = stem.samp$job.cat)
levels(x = stem.samp$sex)

#11.5.1.2
# load the nnet package
library(package = "nnet")

# estimate the model and print its summary
job.type.mod <- multinom(formula = job.cat ~ age + sex + age*sex,
                         data = stem.samp,
                         model = TRUE)

summary(object = job.type.mod)

# multinomial null model
job.type.mod.null <- multinom(formula = job.cat ~ 1,
                              data = stem.samp,
                              model = TRUE)

summary(object = job.type.mod.null)

# get the job model chi-squared
job.chisq <- job.type.mod.null$deviance - job.type.mod$deviance

# get the degrees of freedom for chi-squared
job.df <- length(x = summary(object = job.type.mod)$coefficients) - 
  length(x = summary(object = job.type.mod.null)$coefficients)

# get the p-value for chi-squared
job.p <- pchisq(q = job.chisq, df = job.df, lower.tail = FALSE)

# put together into a vector and round to 3 decimal places
modelsig <- round(x = c(job.chisq, job.df, job.p), 3)

# add names to the vector
names(x = modelsig) <- c("Chi-squared", "df", "p")

# print the vector
modelsig

#11.5.2
# print first six rows of fitted values
head(x = job.type.mod$fitted.values)

# predict job type
head(x = predict(object = job.type.mod))

# observed vs. predicted category for each observation
fit.n <- table(observed = job.type.mod$model$job.cat,
               predicted = predict(object = job.type.mod))
fit.n

# observed vs. predicted category for each observation
fit.perc <- prop.table(table(observed = job.type.mod$model$job.cat,
                             predicted = predict(object = job.type.mod)),
                       margin = 1)
fit.perc

#11.5.3
# get odds ratios
exp(x = coef(object = job.type.mod))

# get odds ratios and transpose
t(x = exp(x = coef(object = job.type.mod)))

# confidence intervals for odds ratios
exp(x = confint(object = job.type.mod))

# get odds ratios for other sciences from the model object
oddsrat.other.sci <- t(x = exp(x = coef(object = job.type.mod)))[ , 1]

# get CI for other sciences
confint.other.sci <- exp(x = confint(object = job.type.mod))[ , 1:2, 1]

# put into a data frame and print
other.sci <- data.frame(OR.other = oddsrat.other.sci,
                        CI.other = confint.other.sci)
other.sci

# get odds ratios for nonscience
oddsrat.non.sci <- t(x = exp(x = coef(object = job.type.mod)))[ , 2]

# get CI for nonscience
confint.non.sci <- exp(x = confint(object = job.type.mod))[ , 1:2, 2]

# put into a data frame and print
non.sci <- data.frame(OR.non = oddsrat.non.sci,
                      CI.non = confint.non.sci)
non.sci

# all together
or.ci <- data.frame(other.sci, non.sci)
or.ci

#11.5.4 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
#The odds of a female entering a non-science career in comparison to a computer science/engineering/math
#career were 2.80 times higher than the odds of a male entering a non-science career in comparison to 
#a computer science/engineering/math career (95% CI = 1.15 - 6.76). Similarly, the odds of a female entering
#an other science career in comparison to a computer science/math/engineering career were 3.04 times higher
#than the odds of a male entering an other science career in comparison to a computer science/math/engineering
#career (95% CI = 1.26 - 7.35). Overall, this suggests that females are more likely to enter
#other science and non-science careers over computer science/engineering/math careers in comparison to males.

#11.6.2
# load the mlogit package
library(package = "mlogit")

# reshape data to use with the mlogit function
stem.samp.4mlog <- mlogit.data(data = stem.samp,
                               choice = "job.cat",
                               shape = "wide")

# estimate the model and print its summary
mlogit.job <- mlogit(formula = job.cat ~ 0 | age + sex + age*sex,
                     data = stem.samp.4mlog)

summary(object = mlogit.job)

# estimate the model with two outcome categories and print its summary
mlogit.job.alt <- mlogit(formula = job.cat ~ 0 | age + sex + age*sex,
                         data = stem.samp.4mlog,
                         alt.subset = c("CS, Math, Eng", "Nonscience"))

summary(object = mlogit.job.alt)

# hmftest
hmftest(x = mlogit.job, z = mlogit.job.alt)

#11.6.4 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
mlogit.job.no.int <- mlogit(formula = job.cat ~ 0 | age + sex,
                     data = stem.samp.4mlog)
summary(mlogit.job.no.int)

#get confidence intervals and odds ratios
mlogit.job.no.int.confint <- exp(confint(mlogit.job.no.int))
mlogit.job.no.int.or <- exp(summary(mlogit.job.no.int)$coefficients[1:6])

#put the OR and 95% CI together
mlogit.job.no.int.all <- cbind(mlogit.job.no.int.or, mlogit.job.no.int.confint)

#look at the results
mlogit.job.no.int.all

#get df for LR x2
job.df.noint <- length(x = summary(object = mlogit.job.no.int)$coefficients) - 
  length(x = summary(object = job.type.mod.null)$coefficients)

#Interpertation: 
#A multinomial logit model using the predictors age and sex was significantly better than the null
#model in predicting job choice (LR X2(4) = 49.27, p < 0.001).
#As before, female sex was associated with greater odds of a nonscience versus engineering, math, 
#computer science career relative the odds of a nonscience versus engineering, math, computer science 
#career for a male (OR = 2.15, 95% CI = 1.65 - 2.78). In comparison to male sex,
#female sex was also associated with greater odds of an 'other science' career relative to an engineering, math
#computer science career (OR = 1.78, 95% CI = 1.37 - 2.30). While the direction of these associations did not
#change with respect to the model including the interaction term, they were substantially reduced, indicating
#less disparity in career choice than was shown previously.


#11.7
# sample of 1,000 females
set.seed(seed = 143)

stem.samp.f<- stem.cleaned %>%
  drop_na(job.cat) %>%
  filter(sex == "Female") %>%
  group_by(satis.contrib) %>%
  sample_n(size = 250) %>%
  ungroup()

# check work
summary(object = stem.samp.f)

#11.7.1
# job satisfaction in job type (Figure 11.14)
stem.samp.f %>%
  ggplot(aes(x = job.cat, fill = satis.contrib)) +
  geom_bar(position = "fill") + coord_flip() +
  theme_minimal() + labs(x = "Job type", y = "Percent") +
  scale_fill_brewer(name = "How satisfied\nwith contribution\nto society?",
                    palette = "Purples",
                    direction = -1) +
  scale_y_continuous(labels = scales::percent)

# job satisfaction by age (Figure 11.15)
stem.samp.f %>%
  ggplot(aes(y = age, x = job.cat, fill = satis.contrib)) +
  geom_jitter(aes(color = satis.contrib), alpha = .6) +
  geom_boxplot(aes (fill = satis.contrib), alpha = .4) +
  scale_fill_brewer(name = "How satisfied\nwith contribution\nto society?",
                    palette = "Purples",
                    direction = -1) +
  scale_color_brewer(name = "How satisfied\nwith contribution\nto society?",
                     palette = "Purples",
                     direction = -1) +
  theme_minimal() + labs(x = "Job type", y = "Age in years")

# order variables function
OrderSatis <- function(x){
  return(ordered(x, levels = c("Very dissatisfied",
                               "Somewhat dissatisfied",
                               "Somewhat satisfied",
                               "Very satisfied")))
}

# use function to order 3 satisfaction variables
stem.samp.f.cleaned <- stem.samp.f %>%
  mutate(satis.advance = OrderSatis(x = satis.advance)) %>%
  mutate(satis.salary = OrderSatis(x = satis.salary)) %>%
  mutate(satis.contrib = OrderSatis(x = satis.contrib))

# check the data
summary(object = stem.samp.f.cleaned)

# examine contribution satisfaction by job type for females
sat.kw <- kruskal.test(formula = as.integer(x = satis.contrib) ~ job.cat,
                       data = stem.samp.f.cleaned)
sat.kw

#examine contribution satisfaction by age
sat.rho <- cor.test(formula = ~ as.integer(x = satis.contrib) + age,
                    method = "spearman",
                    data = stem.samp.f.cleaned)
sat.rho

#11.7.3 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#visualize: salary sat by job category in females
stem.samp.f.cleaned %>%
  ggplot(aes(x = job.cat, y = as.integer(satis.salary), fill = satis.salary)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal()+
  scale_fill_manual(name = "Satisfaction With Salary",
                    values = c("#edf8e9", '#bae4b3', "#74c476", "#238b45")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(x = "Job Category",
       y = "Percent of Category")

#Salary satisfaction by age
stem.samp.f.cleaned %>%
  ggplot(aes(fill = satis.salary, x = job.cat, y = age)) +
  geom_boxplot(aes(fill = satis.salary))+
  geom_jitter(aes(y = age, x = job.cat, color = satis.salary), alpha = 0.5)+
  theme_minimal()+
  scale_fill_manual(name = "Satisfaction With Salary",
                   values = c("#edf8e9", '#bae4b3', "#74c476", "#238b45")) +
  scale_color_manual(guide = FALSE,
                    values = c("#edf8e9", '#bae4b3', "#74c476", "#238b45")) +
  labs(y = "Age", x = "Job Category")

#examine salary satisfaction by job type for females
sal.sat.chi <- chisq.test(x = stem.samp.f.cleaned$satis.salary,
                          y = stem.samp.f.cleaned$job.cat)

sal.sat.chi

#examine salary satisfaction by age
sat.rho <- cor.test(formula = ~ as.integer(x = satis.salary) + age,
                    method = "spearman",
                    data = stem.samp.f.cleaned)
sat.rho

#Interpretation: There was not a statistically significant relationship between job category and salary
#satisfaction in females (X2(6) = 11.633, p = 0.071). For females, medians and
#interquartile ranges for salary satisfaction were identical between those in different
#careers: the median satisfaction level in all groups was 3 (somewhat satisfied), with an interquartile range
#of 2 (somewhat dissatisfied) to 3 (somewhat satisfied).
#Similarly, there was no significant correlation between age and salary satisfaction (rho = 0.051, p = 0.107).


# ordinal logistic regression for contribution satisfaction
library(package = "MASS")

# model contribution satisfaction based on job type and age
society.mod <- polr(formula = satis.contrib ~ job.cat + age,
                    data = stem.samp.f.cleaned)

summary(object = society.mod)

# estimate null model
society.mod.null <- polr(formula = satis.contrib ~ 1,
                         data = stem.samp.f.cleaned)

# job model chi-squared
society.chisq <- society.mod.null$deviance - society.mod$deviance

# degrees of freedom for chi-squared
society.df <- length(x = society.mod$coefficients) - length(x = society.mod.null$coefficients)

# pvalue for chi-squared
society.p <- pchisq(q = society.chisq, df = society.df, lower.tail = FALSE)

# put together and print
modelsig.society <- round(x = c(society.chisq, society.df, society.p), 3)

names(x = modelsig.society) <- c("Chi-squared", "d.f.", "p")

modelsig.society

# observed vs. predicted category for each observation
fit.n.soc <- table(observed = society.mod$model$satis.contrib,
                   predicted = predict(object = society.mod))
fit.n.soc

# observed vs. predicted category for each observation
fit.perc <- prop.table(x = table(observed = society.mod$model$satis.contrib,
                                 predicted = predict(object = society.mod)),
                       margin = 1)
fit.perc

# odds ratios and confidence intervals
or.soc <- data.frame(OR = exp(x = society.mod$coefficients),
                     CI = exp(x = confint(object = society.mod)))
or.soc

#11.8.4 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
satis.advance.mod <- polr(formula = satis.advance ~ job.cat + age,
                          data = stem.samp.f.cleaned)
summary(satis.advance.mod)

#model significance:
#get null model
satis.advance.null <- polr(formula = satis.advance ~ 1,
                           data = stem.samp.f.cleaned)
# get model chi-squared
satis.chisq <- satis.advance.null$deviance - satis.advance.mod$deviance

# degrees of freedom for chi-squared
satis.df <- length(x = satis.advance.mod$coefficients) - length(x = satis.advance.null$coefficients)

# pvalue for chi-squared
(satis.p <- pchisq(q = satis.chisq, df = satis.df, lower.tail = FALSE))

#No the model is not significant.

#11.9.2
# check proportional odds assumption
library(package = "ordinal")
nominal_test(object = clm(formula = satis.contrib ~ job.cat + age,
                          data = stem.samp.f.cleaned))

#11.9.4 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
nominal_test(object = clm(formula = satis.advance ~ job.cat + age,
                          data = stem.samp.f.cleaned))
#The proportional odds assumptions for both job category and age are met in this model


