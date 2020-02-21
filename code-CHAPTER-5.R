#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 16, 2019                                                    #
# Purpose: Review Code for Chapter 5 of Dr. Harris' Book                        #
#          Computing and Interpreting Chi-Squared                               #
#         The R Team and the Vexing Voter Fraud Problem                         #
# Packages Used: tidyverse, haven, descr, fmsb, lsr                             #
# Data Used: Pew Research Center April 19-23 2017 survey                        #
# Last Update: Nov 16, 2019                                                     #
#################################################################################

#import the April 17-23 Pew Research Center data
library(package = "haven")
library(package = "tidyverse")

#create a temporary file for downloading the data
temp <- tempfile()

#download from web address
download.file("https://www.people-press.org/wp-content/uploads/sites/4/2018/11/Apr-19-23-2017-public.zip", temp)

#unzip and read in using haven's read_sav for SPSS type data
vote <- read_sav(unz(temp, "Apr 19-23 2017 public.sav"))
unlink(temp)

#alternatively, load it in from your own computer
# import the voting data
vote <- read_sav(file = "[data folder location]/data/pew_apr_19-23_2017_weekly_ch5.sav")

# select variables of interest
vote.cleaned <- vote %>%
  select(pew1a, pew1b, race, sex, mstatus, ownhome, employ, polparty)

# check data
summary(object = vote.cleaned)

# select variables of interest and clean them
vote.cleaned <- vote %>%
  select(pew1a, pew1b, race, sex, mstatus, ownhome, employ, polparty) %>%
  zap_labels() %>%
  mutate(pew1a = recode_factor(.x = pew1a,
                               `1` = 'Register to vote',
                               `2` = 'Make easy to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(ease.vote = pew1a) %>%
  mutate(pew1b = recode_factor(.x = pew1b,
                               `1` = 'Require to vote',
                               `2` = 'Choose to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(require.vote = pew1b)

# check voting variables
summary(object = vote.cleaned)

# examine race variable before recoding
table(vote.cleaned$race)

# select variables of interest and clean them
vote.cleaned <- vote %>%
  select(pew1a, pew1b, race, sex, mstatus, ownhome, employ, polparty) %>%
  zap_labels() %>%
  mutate(pew1a = recode_factor(.x = pew1a,
                               `1` = 'Register to vote',
                               `2` = 'Make easy to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(ease.vote = pew1a) %>%
  mutate(pew1b = recode_factor(.x = pew1b,
                               `1` = 'Require to vote',
                               `2` = 'Choose to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(require.vote = pew1b) %>%
  mutate(race = recode_factor(.x = race,
                              `1` = 'White non-Hispanic',
                              `2` = 'Black non-Hispanic',
                              `3` = 'Hispanic',
                              `4` = 'Hispanic',
                              `5` = 'Hispanic',
                              `6` = 'Other',
                              `7` = 'Other',
                              `8` = 'Other',
                              `9` = 'Other',
                              `10` = 'Other',
                              `99` = NA_character_)) %>%
  mutate(sex = recode_factor(.x = sex,
                             `1` = 'Male',
                             `2` = 'Female')) %>%
  mutate(ownhome = recode_factor(.x = ownhome,
                                 `1` = 'Owned',
                                 `2` = 'Rented',
                                 `8` = NA_character_,
                                 `9` = NA_character_))
# check recoding
summary(object = vote.cleaned)

# voting ease by race-eth no spread
vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(race) %>%
  group_by(ease.vote, race) %>%
  summarize(freq.n = n())

# voting ease by race-eth with spread
vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(race) %>%
  group_by(ease.vote, race) %>%
  summarize(freq.n = n()) %>%
  spread(key = race, value = freq.n)

# voting ease by race-eth with table
table(vote.cleaned$ease.vote, vote.cleaned$race)

# table of percents voting ease by race-eth
prop.table(x = table(Voting.ease = vote.cleaned$ease.vote,
                     Race.eth = vote.cleaned$race))

# table of percents voting ease by race-eth
prop.table(x = table(Voting.ease = vote.cleaned$ease.vote,
                     Race.eth = vote.cleaned$race),
           margin = 2)

# table of percents voting required by race-eth
prop.table(x = table(Voting.requirement = vote.cleaned$require.vote,
                     Race.eth = vote.cleaned$race),
           margin = 2)

# open gridExtra to put graphs together (Figure 5.6)
library(package = "gridExtra")

# graph the relationship between registration ease and race eth
ease.graph <- vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(race) %>%
  group_by(ease.vote, race) %>%
  count() %>%
  group_by(race) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = race, y = perc, fill = ease.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), name = "Opinion on\nvoter registration") +
  labs(x = "", y = "Percent within group") +
  theme(axis.text.x = element_blank())

# graph the relationship between required voting and race eth
req.graph <- vote.cleaned %>%
  drop_na(require.vote) %>%
  drop_na(race) %>%
  group_by(require.vote, race) %>%
  count() %>%
  group_by(race) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = race, y = perc, fill = require.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), name = "Opinion on voting") +
  labs(x = "Race-ethnicity group", y = "Percent within group")

grid.arrange(ease.graph, req.graph, nrow = 2)

#5.4.4 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#Make an appropriate table or a graph to examine the relationship between sex and voting requirements.
#Explain the results.
#table
prop.table(table(vote.cleaned$sex, vote.cleaned$require.vote),1)
#graph
vote.cleaned %>%
  drop_na(require.vote) %>%
  group_by(require.vote, sex) %>%
  count() %>%
  group_by(sex) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = sex, y = perc, fill = require.vote))+
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("darkblue", "grey"), name = "Opinion on Voting")+
  labs(x = "Sex", y = "Percent Within Group")+
  ggtitle("Male and Female American Opinions\n on Voting Requirement")

#5.5.5 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
#There are more people than expected who want to make it easy to vote and are Hispanic.
#There are more people than expected who want to make it easy to vote and are Black non-Hispanic.

#5.6.3
#chi-squared statistic for ease of voting and race
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$race)

#5.6.4 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
#The way to compute a chi-squared statistic is to add up all the observed values and subtract the sum of
#all the expected values. Then, square the total. = FALSE

# When computing a chi-squared statistic, square any negative values but not the positive ones = FALSE

#5.7.5
#Which of the following is a good representation of p-value < 2.2e-4?
#p < .00022

#Which of these is a common value for an alpha in many fields?
# .05

#5.8.2
# chi-squared statistic for ease of voting
# and race
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$race)

#5.8.6 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
#Chi-squared is larger when there is a bigger difference between observed and expected.
#Chi-squared and ??2 are two ways to represent the same thing.

#Box 5.1
# frequencies of race-ethnicity from pew data
race.eth <- vote.cleaned %>%
  drop_na(race) %>%
  count(race)
race.eth

# chi-squared comparing observed race-eth to population race-eth
race.gof <- chisq.test(x = race.eth$n, p = c(.691, .121, .125, .063))
race.gof

# add percents of race-ethnicity from Pew data to make a data frame
# for graphing
race.eth.graph.data <- vote.cleaned %>%
  drop_na(race) %>%
  group_by(race) %>%
  summarize(n = n()) %>%
  mutate(sample = n/sum(n),
         population = c(.691, .121, .125, .063)) %>%
  gather(key = samp.or.pop, value = perc, sample, population)
race.eth.graph.data

# Make the graph (Figure 5.11)
race.eth.graph.data %>%
  ggplot(aes(x = race, y = 100*perc, fill = samp.or.pop)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"),
                    name = "Population\nor sample") +
  labs(y = "Percent of people", x = "Race-ethnicity group")

# standardized residuals from race.gof object
race.gof$stdres

#5.9
# open descr
library(package = "descr")
# chi-squared examining ease of voting and race
CrossTable(x = vote.cleaned$ease.vote,
           y = vote.cleaned$race,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)

#Box 5.2 Specifying Arguments in Code
# chi-squared examining ease of voting and race-eth category
# no specification of argument names
CrossTable(vote.cleaned$ease.vote,
           vote.cleaned$race)

#chi-squared examining ease of voting and race-eth category
# no specification of argument names
CrossTable(vote.cleaned$race,
           vote.cleaned$ease.vote)

# chi-squared examining ease of voting and race-eth category
# specification of argument names
CrossTable(y = vote.cleaned$race,
           x = vote.cleaned$ease.vote)

# chi-squared examining ease of voting and race-eth category
# specification of argument names
CrossTable(x = vote.cleaned$ease.vote,
           y = vote.cleaned$race)

# chi-squared examining ease of voting and race-eth category
# specification of arguments
CrossTable(x = vote.cleaned$ease.vote,
           y = vote.cleaned$race,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)

#5.9.3 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
#A standardized residual of -3.56 would indicate that the observed value is
#less than the expected value of the cell

#5.10

# compute Cramér's V for voting ease and race
# chi-squared analysis
library(package = "lsr")
cramersV(x = vote.cleaned$ease.vote,
         y = vote.cleaned$race)

# chi-squared examining ease of voting and race-ethnicity category
CrossTable(x = vote.cleaned$ease.vote,
           y = vote.cleaned$ownhome,
           expected = TRUE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           chisq = TRUE,
           sresid = TRUE)

# checking chisq.test function
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$ownhome)

# removing the Yates correction
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$ownhome,
           correct = FALSE)

# compute Cramér's V for voting ease and home owning
cramersV(x = vote.cleaned$ease.vote,
         y = vote.cleaned$ownhome)

# open fmsb
library(package = "fmsb")
# odds ratio from frequencies
oddsratio(a = 287,
          b = 112,
          c = 375,
          d = 208)

#5.10.7 ACHIEVEMENT 7: CHECK YOUR UNDERSTANDING
#Cramer's V, the phi coefficient, and odds ratio are all measures of effect size for
#the relationship between two categorical variables. Phi and the odds ratio are used just for
#two binary variables, while Cramer's V can be used for categorical variables with >2 levels


