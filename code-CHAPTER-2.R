##### Chapter 2

## section 2.4.3
# CHECK YOUR UNDERSTANDING

# Number of healthy days per month (integer/numeric) 
# Marital status (factor)
# Religious affiliation (factor)
# Smoking status (factor)
# Number of alcoholic beverages per week (integer/numeric)

## box 2.1

# open the haven package to read an xpt
library(package = "haven")

# create a temporary file to store the zipped file
# before you open it
temp <- tempfile(fileext = ".zip")

# use download.file to put the zipped file in the temp file
# this will take a couple of minutes depending on computer speed
download.file(url = "http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.zip",
              destfile = temp)

# unzip it and read it
brfss.2014 <- read_xpt(file = temp)

# open tidyverse to select variables
library(package = "tidyverse")

# select variables for analysis
# use ` around variable names starting with underscores
transgender_hc_ch2 <- brfss.2014 %>%
  select(TRNSGNDR, `_AGEG5YR`, `_RACE`, `_INCOMG`, `_EDUCAG`,
         HLTHPLN1, HADMAM, `_AGE80`, PHYSHLTH)

# export the data set to a csv file in a local folder called data
write.csv(x = transgender_hc_ch2, file = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/transgender_hc_ch2.csv",
          row.names = FALSE)

## section 2.5.1

# read the 2014 BRFSS data
brfss.trans.2014 <- read.csv(file = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/transgender_hc_ch2.csv")

# examine the data
summary(object = brfss.trans.2014)

## section 2.5.2

# frequency distribution for transgender
# participants in the 2014 BRFSS
table(brfss.trans.2014$TRNSGNDR)

## section 2.5.3

# check data type for TRNSGNDR variable
class(x = brfss.trans.2014$TRNSGNDR)

# open tidyverse for data management
library(package = "tidyverse")

# change variable from integer to factor
brfss.2014.cleaned <- brfss.trans.2014 %>%
  mutate(TRNSGNDR = as.factor(TRNSGNDR))

# check data type again
class(x = brfss.2014.cleaned$TRNSGNDR)

# cleaning the TRNSGNDR variable
brfss.2014.cleaned <- brfss.trans.2014 %>%
  mutate(TRNSGNDR = recode_factor(.x = TRNSGNDR,
                                  `1` = 'Male to female',
                                  `2` = 'Female to male',
                                  `3` = 'Gender non-conforming',
                                  `4` = 'Not transgender',
                                  `7` = 'Not sure',
                                  `9` = 'Refused'))

# table of transgender status frequencies
table(brfss.2014.cleaned$TRNSGNDR)

# use summary for frequencies
summary(object = brfss.2014.cleaned)

# trying describe for descriptive statistics
Hmisc::describe(x = brfss.2014.cleaned)

# use freq from the descr package to make a table of frequencies and percents
# suppress the bar plot that automatically prints
descr::freq(x = brfss.2014.cleaned$TRNSGNDR, plot = FALSE)

# use tidyverse to make table of frequency and percent
brfss.2014.cleaned %>%
  group_by(TRNSGNDR) %>%
  summarize(freq.trans = n()) %>%
  mutate(perc.trans = 100 * (freq.trans / sum(freq.trans)))

# use tidyverse to make table of frequency and percent
brfss.2014.cleaned %>%
  group_by(TRNSGNDR) %>%
  summarize(freq.trans = n()) %>%
  mutate(perc.trans = 100 * (freq.trans / sum(freq.trans))) %>%
  mutate(valid.perc = 100 * (freq.trans / (sum(freq.trans[na.omit(object = TRNSGNDR)]))))

## section 2.5.4 
# CHECK YOUR UNDERSTANDING
brfss.2014.cleaned <- brfss.trans.2014 %>%
  mutate(TRNSGNDR = recode_factor(.x = TRNSGNDR,
                                  `1` = 'Male to female',
                                  `2` = 'Female to male',
                                  `3` = 'Gender non-conforming',
                                  `4` = 'Not transgender',
                                  `7` = 'Not sure',
                                  `9` = 'Refused')) %>%
  mutate(HADMAM = recode_factor(.x = HADMAM,
                                `1` = 'Yes',
                                `2` = 'No',
                                `7` = 'Not sure',
                                `9` = 'Refused'))
descr::freq(x = brfss.2014.cleaned$HADMAM, plot = FALSE)

## section 2.6.1

# table with frequencies from the age variable
table(brfss.2014.cleaned$X_AGE80)

## section 2.6.2.1

# create salaries vector and find its mean
salaries <- c(25000, 62000, 41000, 96000, 41000)
mean(x = salaries)

# add Bill Gates
salaries.gates <- c(salaries, 11500000000)

# find the mean of the vector with gates
mean(x = salaries.gates)

# median salary without Bill Gates
median(x = salaries)

# median salary with Bill Gates
median(x = salaries.gates)

# skewness of salaries variable
semTools::skew(object = salaries.gates)

## section 2.6.2.3 

# table showing salaries frequencies
table(salaries)

# table showing salaries frequencies
sort(x = table(salaries), decreasing = TRUE)

# table showing salaries frequencies
names(x = sort(x = table(salaries), decreasing = TRUE))

# mean, median, and mode of salaries
mean(x = salaries)
median(x = salaries)
names(x = sort(x = table(salaries), decreasing = TRUE))[1]

## section 2.6.3

# pipe in the original data frame
# recode the TRNSGNDR factor so it's easy to read
# recode 77, 88, 99 on PHYSHLTH
brfss.2014.cleaned <- brfss.trans.2014 %>%
  mutate(TRNSGNDR = recode_factor(.x = TRNSGNDR,
                                  `1` = 'Male to female',
                                  `2` = 'Female to male',
                                  `3` = 'Gender non-conforming',
                                  `4` = 'Not transgender',
                                  `7` = 'Not sure',
                                  `9` = 'Refused')) %>%
  mutate(PHYSHLTH = na_if(x = PHYSHLTH, y = 77)) %>%
  mutate(PHYSHLTH = na_if(x = PHYSHLTH, y = 99)) %>%
  mutate(PHYSHLTH = as.numeric(recode(.x = PHYSHLTH, `88` = 0L)))

# examine PHYSHLTH to check data management
summary(object = brfss.2014.cleaned$PHYSHLTH)

# no mode function so find the mode using a table
# and sort values in decreasing order
# so the most common value comes first
names(x = sort(x = table(salaries), decreasing = TRUE))[1]

# make a histogram
brfss.2014.cleaned %>%
  ggplot(aes(x = PHYSHLTH)) +
  geom_histogram()

# get mean, median, mode
mean(x = brfss.2014.cleaned$PHYSHLTH)
median(x = brfss.2014.cleaned$PHYSHLTH)
names(x = sort(x = table(brfss.2014.cleaned$PHYSHLTH), decreasing = TRUE))[1]

# get mean, median, mode
mean(x = brfss.2014.cleaned$PHYSHLTH, na.rm = TRUE)
median(x = brfss.2014.cleaned$PHYSHLTH, na.rm = TRUE)
names(x = sort(table(brfss.2014.cleaned$PHYSHLTH), decreasing = TRUE))[1]

# get mean, median, mode
brfss.2014.cleaned %>%
  summarize(mean.days = mean(x = PHYSHLTH,
                             na.rm = TRUE),
            med.days = median(x = PHYSHLTH,
                              na.rm = TRUE),
            mode.days = names(x = sort(table(PHYSHLTH),
                                       decreasing = TRUE))[1])

# skewness for PHYSHLTH
semTools::skew(object = brfss.2014.cleaned$PHYSHLTH)

## section 2.6.4.1

# variance of unhealthy days
var(x = brfss.2014.cleaned$PHYSHLTH, na.rm = TRUE)

# get mean, median, mode, and spread
brfss.2014.cleaned %>%
summarise(mean.days = mean(x = PHYSHLTH, na.rm = TRUE),
          sd.days = sd(x = PHYSHLTH, na.rm = TRUE),
          var.days = var(x = PHYSHLTH, na.rm = TRUE),
          med.days = median(x = PHYSHLTH, na.rm = TRUE),
          mode.days = names(x = sort(x = table(PHYSHLTH),
                                     decreasing = TRUE))[1])

## section 2.6.4.2

# range of days of physical health
range(brfss.2014.cleaned$PHYSHLTH, na.rm = TRUE)

# get descriptive statistics for PHYSHLTH
brfss.2014.cleaned %>%
  summarize(mean.days = mean(x = PHYSHLTH, na.rm = TRUE),
            sd.days = sd(x = PHYSHLTH, na.rm = TRUE),
            var.days = var(x = PHYSHLTH, na.rm = TRUE),
            med.days = median(x = PHYSHLTH, na.rm = TRUE),
            iqr.days = IQR(x = PHYSHLTH, na.rm = TRUE),
            mode.days = names(x = sort(x = table(PHYSHLTH),
                                       decreasing = TRUE))[1])

# get descriptive statistics
brfss.2014.cleaned %>%
  drop_na(PHYSHLTH) %>%
  summarize(mean.days = mean(x = PHYSHLTH),
            sd.days = sd(x = PHYSHLTH),
            var.days = var(x = PHYSHLTH),
            med.days = median(x = PHYSHLTH),
            iqr.days = IQR(x = PHYSHLTH),
            mode.days = names(x = sort(x = table(PHYSHLTH),
                                       decreasing = TRUE))[1])

## section 2.6.4.3

# B index of TRNSGNDR variable
qualvar::B(x = table(brfss.2014.cleaned$TRNSGNDR))

## section 2.6.5
# CHECK YOUR UNDERSTANDING 

# examine the age variable
summary(object = brfss.2014.cleaned$X_AGE80)

# get descriptive statistics
brfss.2014.cleaned %>%
  drop_na(X_AGE80) %>%
  summarize(mean.age = mean(x = X_AGE80),
            sd.age = sd(x = X_AGE80),
            var.age = var(x = X_AGE80),
            med.age = median(x = X_AGE80),
            iqr.age = IQR(x = X_AGE80),
            mode.age = names(x = sort(x = table(X_AGE80),
                                       decreasing = TRUE))[1])

## section 2.7.1.1

# create a subset of the data set to keep
# transgender status of MtF OR FtM OR Gender non-conforming
# age group higher than group 4 and lower than group 12
# was asked mammogram question
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
           TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM))

# check the new data frame
summary(object = brfss.2014.small)

# create a subset of observations and variables
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
           TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM)) %>%
  select(TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, X_EDUCAG, HLTHPLN1, HADMAM)

# summary statistics for the new data frame
summary(object = brfss.2014.small)

# change variables to factor data types
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
         TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM)) %>%
  select(TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, 
         X_EDUCAG, HLTHPLN1, HADMAM) %>%
  mutate_all(as.factor)

# summary statistics for the new data frame
summary(object = brfss.2014.small)

## section 2.7.1.2

# add labels to factor variables
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
           TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM)) %>%
  select(TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, X_EDUCAG, HLTHPLN1, HADMAM) %>%
  mutate_all(as.factor) %>%
  mutate(X_AGEG5YR = recode_factor(.x = X_AGEG5YR,
                                   `5` = '40-44',
                                   `6` = '45-49',
                                   `7` = '50-54',
                                   `8` = '55-59',
                                   `9` = '60-64',
                                   `10` = '65-69',
                                   `11` = '70-74')) %>%
  mutate(X_INCOMG = recode_factor(.x = X_INCOMG,
                                  `1` = 'Less than $15,000',
                                  `2` = '$15,000 to less than $25,000',
                                  `3` = '$25,000 to less than $35,000',
                                  `4` = '$35,000 to less than $50,000',
                                  `5` = '$50,000 or more',
                                  `9` = 'Don\'t know/not sure/missing')) %>%
  mutate(X_EDUCAG = recode_factor(.x = X_EDUCAG,
                                  `1` = 'Did not graduate high school',
                                  `2` = 'Graduated high school',
                                  `3` = 'Attended college/technical school',
                                  `4` = 'Graduated from college/technical school',
                                  `9` = NA_character_)) %>%
  mutate(HLTHPLN1 = recode_factor(.x = HLTHPLN1,
                                  `1` = 'Yes',
                                  `2` = 'No',
                                  `7` = 'Don\'t know/not sure/missing',
                                  `9` = 'Refused'))

#check the work so far
summary(object = brfss.2014.small)

# add labels to factor variables
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
           TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM)) %>%
  select(TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, X_EDUCAG, HLTHPLN1, HADMAM) %>%
  mutate_all(as.factor) %>%
  mutate(X_AGEG5YR = recode_factor(.x = X_AGEG5YR,
                                   `5` = '40-44',
                                   `6` = '45-49',
                                   `7` = '50-54',
                                   `8` = '55-59',
                                   `9` = '60-64',
                                   `10` = '65-69',
                                   `11` = '70-74')) %>%
  mutate(X_INCOMG = recode_factor(.x = X_INCOMG,
                                  `1` = 'Less than $15,000',
                                  `2` = '$15,000 to less than $25,000',
                                  `3` = '$25,000 to less than $35,000',
                                  `4` = '$35,000 to less than $50,000',
                                  `5` = '$50,000 or more',
                                  `9` = 'Don\'t know/not sure/missing')) %>%
  mutate(X_EDUCAG = recode_factor(.x = X_EDUCAG,
                                  `1` = 'Did not graduate high school',
                                  `2` = 'Graduated high school',
                                  `3` = 'Attended college/technical school',
                                  `4` = 'Graduated from college/technical school',
                                  `9` = NA_character_)) %>%
  mutate(HLTHPLN1 = recode_factor(.x = HLTHPLN1,
                                  `1` = 'Yes',
                                  `2` = 'No',
                                  `7` = 'Don\'t know/not sure/missing',
                                  `9` = 'Refused')) %>%
  mutate(X_RACE = recode_factor(.x = X_RACE,
                                `1` = 'White',
                                `2` = 'Black',
                                `3` = 'Native American',
                                `4` = 'Asian/Pacific Islander',
                                `5` = 'Other',
                                `6` = 'Other',
                                `7` = 'Other',
                                `8` = 'Other',
                                `9` = 'Other')) %>%
  mutate(HADMAM = recode_factor(.x = HADMAM,
                                `1` = 'Yes',
                                `2` = 'No',
                                `7` = 'Don\'t know/not sure/missing',
                                `9` = 'Refused'))
#check the work so far
summary(object = brfss.2014.small)

## section 2.7.1.3

# get percents for TRNSGNDR
prop.table(x = table(brfss.2014.small$TRNSGNDR))

# complete data management code
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
           TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM)) %>%
  mutate(TRNSGNDR = if_else(condition = HADMAM != 9,
                            true = TRNSGNDR,
                            false = factor(NA))) %>%
  select(TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, X_EDUCAG, HLTHPLN1) %>%
  mutate_all(as.factor) %>%
  mutate(X_AGEG5YR = recode_factor(.x = X_AGEG5YR,
                                   `5` = '40-44',
                                   `6` = '45-49',
                                   `7` = '50-54',
                                   `8` = '55-59',
                                   `9` = '60-64',
                                   `10` = '65-69',
                                   `11` = '70-74')) %>%
  mutate(X_INCOMG = recode_factor(.x = X_INCOMG,
                                  `1` = 'Less than $15,000',
                                  `2` = '$15,000 to less than $25,000',
                                  `3` = '$25,000 to less than $35,000',
                                  `4` = '$35,000 to less than $50,000',
                                  `5` = '$50,000 or more',
                                  `9` = 'Don\'t know/not sure/missing')) %>%
  mutate(X_EDUCAG = recode_factor(.x = X_EDUCAG,
                                  `1` = 'Did not graduate high school',
                                  `2` = 'Graduated high school',
                                  `3` = 'Attended college/technical school',
                                  `4` = 'Graduated from college/technical school',
                                  `9` = NA_character_)) %>%
  mutate(HLTHPLN1 = recode_factor(.x = HLTHPLN1,
                                  `1` = 'Yes',
                                  `2` = 'No',
                                  `7` = 'Don\'t know/not sure/missing',
                                  `9` = 'Refused')) %>%
  mutate(X_RACE = recode_factor(.x = X_RACE,
                                `1` = 'White',
                                `2` = 'Black',
                                `3` = 'Native American',
                                `4` = 'Asian/Pacific Islander',
                                `5` = 'Other',
                                `6` = 'Other',
                                `7` = 'Other',
                                `8` = 'Other',
                                `9` = 'Other')) %>%
  droplevels()

#check the work
prop.table(x = table(brfss.2014.small$TRNSGNDR))

## section 2.7.1.4

# open tableone
library(package = "tableone")

# create a basic table
CreateTableOne(data = brfss.2014.small)

# check the labels for the data frame
str(object = brfss.2014.small)

# add variable labels to print in table
labelled::var_label(x = brfss.2014.small) <- c("Transition status (n = 220)",
                                               "Age category",
                                               "Race/ethnicity",
                                               "Income category",
                                               "Education category",
                                               "Health insurance?")
# check data frame for labels
str(object = brfss.2014.small)

# create a basic table as an object
trans.hc.table <- CreateTableOne(data = brfss.2014.small)

# use print to show table with labels
print(x = trans.hc.table, varLabels = TRUE)

# use print to show table with labels and percent
print(x = trans.hc.table,
      varLabels = TRUE,
      format = "p",
      explain = FALSE)

# complete data management code
brfss.2014.small <- brfss.2014.cleaned %>%
  filter(TRNSGNDR == 'Male to female'|
           TRNSGNDR == 'Female to male'|
           TRNSGNDR == 'Gender non-conforming') %>%
  filter(X_AGEG5YR > 4 & X_AGEG5YR < 12) %>%
  filter(!is.na(HADMAM)) %>%
  mutate(TRNSGNDR = if_else(HADMAM != 9, TRNSGNDR, factor(NA))) %>%
  select(TRNSGNDR, X_AGEG5YR, X_RACE, X_INCOMG, X_EDUCAG, HLTHPLN1, PHYSHLTH) %>%
  mutate(X_AGEG5YR = recode_factor(.x = X_AGEG5YR,
                                   `5` = '40-44',
                                   `6` = '45-49',
                                   `7` = '50-54',
                                   `8` = '55-59',
                                   `9` = '60-64',
                                   `10` = '65-69',
                                   `11` = '70-74')) %>%
  mutate(X_INCOMG = recode_factor(.x = X_INCOMG,
                                  `1` = 'Less than $15,000',
                                  `2` = '$15,000 to less than $25,000',
                                  `3` = '$25,000 to less than $35,000',
                                  `4` = '$35,000 to less than $50,000',
                                  `5` = '$50,000 or more',
                                  `9` = 'Don\'t know/not sure/missing')) %>%
  mutate(X_EDUCAG = recode_factor(.x = X_EDUCAG,
                                  `1` = 'Did not graduate high school',
                                  `2` = 'Graduated high school',
                                  `3` = 'Attended college/technical school',
                                  `4` = 'Graduated from college/technical school',
                                  `9` = NA_character_)) %>%
  mutate(HLTHPLN1 = recode_factor(.x = HLTHPLN1,
                                  `1` = 'Yes',
                                  `2` = 'No',
                                  `7` = 'Don\'t know/not sure/missing',
                                  `9` = 'Refused')) %>%
  mutate(X_RACE = recode_factor(.x = X_RACE,
                                `1` = 'White',
                                `2` = 'Black',
                                `3` = 'Native American',
                                `4` = 'Asian/Pacific Islande',
                                `5` = 'Other',
                                `6` = 'Other',
                                `7` = 'Other',
                                `8` = 'Other',
                                `9` = 'Other')) %>%
  droplevels()
#check the work
prop.table(x = table(brfss.2014.small$TRNSGNDR))

# add variable labels to print in table
labelled::var_label(x = brfss.2014.small) <- c("Transition status (n = 220)",
                                               "Age category",
                                               "Race/ethnicity",
                                               "Income category",
                                               "Education category",
                                               "Health insurance?",
                                               "Days/month poor physical health")
# check data frame for labels
str(object = brfss.2014.small)

# create a basic table as an object
trans.hc.table <- CreateTableOne(data = brfss.2014.small)

# use print to show table with labels
print(x = trans.hc.table, varLabels = TRUE)

# make a histogram of PHYSHLTH
brfss.2014.small %>%
  ggplot(aes(x = PHYSHLTH)) +
  geom_histogram()

# use print to show table
print(x = trans.hc.table,
      varLabels = TRUE,
      nonnormal = 'PHYSHLTH')

## section 2.7.1.5

# get percents for TRNSGNDR
( trans.p <- prop.table(x = table(brfss.2014.small$TRNSGNDR)) )

# get percents for TRNSGNDR
( trans.perc <- round(x = 100 * prop.table(x = table(brfss.2014.small$TRNSGNDR)), 1) )


# get percents and assign a name for trans and race
# turn into data frames for easier merging
( trans.perc <- data.frame(round(x = prop.table(x = table(brfss.2014.small$TRNSGNDR)) * 100, 1)) )
( race.perc <- data.frame(round(x = prop.table(x = table(brfss.2014.small
                                                         $X_RACE)) * 100, 1)) )
# merge together into one data frame
( table.perc <- rbind(trans.perc, race.perc) )

# finding percents and rounding to one decimal place
# putting all percents in one column with cbind
TableFun <- function(x){
  data.frame(round(x = prop.table(x = table(x)) * 100, 1))
}

# using the TableFun function for the TRNSGNDR variable
TableFun(x = brfss.2014.small$TRNSGNDR)

## box 2.2 

#vectors for each variable
hotdogs.2016 <- c(6, 2, 0, 3, 9, 1)
hotdogs.2017 <- c(8, 3, 0, 2, 6, 2)
cousins <- c("Therese", "Geoff", "Nick", "John", "Jim", "Karen")

#make a data frame from vectors
#use cousins vector as row name rather than variable
cuz.hot.dogs <- data.frame(hotdogs.2016, hotdogs.2017)
row.names(x = cuz.hot.dogs) <- cousins
cuz.hot.dogs

# mean by observation
apply(X = cuz.hot.dogs, MARGIN = 1, FUN = mean)

# mean by variable
apply(X = cuz.hot.dogs, MARGIN = 2, FUN = mean)

# make a list
cuz.list <- list(pet = c('cat', 'dog', 'dog', 'cat', 'bird', 'cat'),
                 ice.cream = c('vanilla', 'chocolate', 
                               'chocolate', 'chocolate',
                               'strawberry', 'strawberry'))
# print the list
cuz.list

# make a table for each
# variable in cuzList list
lapply(X = cuz.list, FUN = table)

## section 2.7.3.4 continued 

# use lapply to apply the TableFun function to
# all the variables in the data frame
# use the do.call function to call the rbind function
# to combine the list items into rows
( table.data <- do.call(rbind, (lapply(X = brfss.2014.small[ , -7], FUN = TableFun))) )

# remove health care No category
table.data <- data.frame(table.data[c(1:26), ])

# label the columns
colnames(x = table.data) <- c("Survey participant demographics (n = 220)", "Percent")

## box 2.3

# basic example of a for loop
squared.numbers <- NULL # initialize an empty vector that will contain the output
for (i in 1:10) {
  squared.numbers[i] <- i^2 # body of the for loop
}
# print out the result
print(squared.numbers)

# initialize an empty data frame
table.data <- data.frame()
# make the for loop
for (i in 1:ncol(brfss.2014.small)) {
  # get the percents for a variable and put them in table.each
  table.each <-
    data.frame(round(x = prop.table(x = table(brfss.2014.small[, i])) * 100, 1))
  # combine table.each with whatever is in table.data already
  table.data <- rbind(table.data, table.each)
}
# print table.data
table.data

# initialize an empty data frame
table.data <- data.frame()
# write the for loop
for (i in 1:(ncol(brfss.2014.small) - 1)) {
  # first, get the table
  table.each <-
    data.frame(round(x = prop.table(x = table(brfss.2014.small[, i])) * 100, 1))
  # store the column name of that iteration for labels
  c.name <- colnames(brfss.2014.small[i])
  # make a new data frame that just contains the labels
  label.names <- data.frame(Variable = rep(c.name, times = nrow(table.each)))
  # combine the label.names data frame and table.each data frame via columns
  table.each.labelled <- cbind(label.names, table.each)
  # combine this with the table.data via rbind
  table.data <- rbind(table.data, table.each.labelled)
}
# remove the extra variable and print the new data frame
table.data

# subset and add labels
table.data <- table.data[c(1:26), c(2:3)]
colnames(table.data) <- c("Survey participant demographics (n = 220)",
                          "Percent")
# print the new data frame
table.data

# open libraries
library(package = "knitr")
library(package = "kableExtra")

# send the table.data to kable and add a title
table.data %>%
  kable(format = "html", 
        caption = "Transgender Survey Participant Demographics")%>%
  kable_styling()

# add the section names
table.data %>%
  kable(format = "html", caption = "Transgender Survey Participant Demographics",
        row.names = FALSE) %>%
  kableExtra::group_rows(group_label = "Transition status",
                         start_row = 1, end_row = 3) %>%
  kableExtra::group_rows(group_label = "Age category",
                         start_row = 4, end_row = 10) %>%
  kableExtra::group_rows(group_label = "Race/ethnicity",
                         start_row = 11, end_row = 15) %>%
  kableExtra::group_rows(group_label = "Income category",
                         start_row = 16, end_row = 21) %>%
  kableExtra::group_rows(group_label = "Education category",
                         start_row = 22, end_row = 25) %>%
  kableExtra::group_rows(group_label = "Health insurance?",
                         start_row = 26, end_row = 26)

# revise the TableFun function to compute both
# frequencies and percentages
TableFreqPerc<- function(x){
  data.frame(table(x), round(x = prop.table(x = table(x)) * 100, 1))
}
# apply new function to brfss.2014.small data frame
bigger.table <- do.call(rbind, (lapply(X = brfss.2014.small, FUN = TableFreqPerc)))
# click on the bigger.table object in the Environment
# pane to see the resulting table
# note that the categories show up twice in the data frame
# delete the second occurrence by making a subset of the data
bigger.table <- bigger.table[-3]
# remove Health insurance No category and PHYSHLTH numbers
bigger.table <- data.frame(bigger.table[c(1:26), ])
#add variable names
names(x = bigger.table) <- c("Survey participant demographics (n = 220)", "Frequency", "Percent")

# Table 2.4
bigger.table %>%
  kable(format = "html", caption = "Transgender Survey Participant Demographics",
        row.names = FALSE) %>%
  kableExtra::group_rows(group_label = "Transition status",
                         start_row = 1, end_row = 3) %>%
  kableExtra::group_rows(group_label = "Age category",
                         start_row = 4, end_row = 10) %>%
  kableExtra::group_rows(group_label = "Race/ethnicity",
                         start_row = 11, end_row = 15) %>%
  kableExtra::group_rows(group_label = "Income category",
                         start_row = 16, end_row = 21) %>%
  kableExtra::group_rows(group_label = "Education category",
                         start_row = 22, end_row = 25) %>%
  kableExtra::group_rows(group_label = "Health insurance?",
                         start_row = 26, end_row = 26) 


# descriptive statistics for PHYSHLTH
median(x = brfss.2014.small$PHYSHLTH, na.rm = TRUE)
IQR(x = brfss.2014.small$PHYSHLTH, na.rm = TRUE)

# revise the TableFreqPerc function to compute
# frequencies and percents for factors
# median and IQR for numeric data types
TableFreqPerc<- function(x){
  if(is.factor(x))
    data.frame(table(x), round(x = prop.table(x = table(x)) * 100, 1))
}
# apply new function to brfss.2014.small data frame
bigger.table <- do.call(rbind, (lapply(X = brfss.2014.small, FUN = TableFreqPerc)))
# note that the categories show up twice in the data frame
# delete the second occurrence by making a subset of the data
bigger.table <- bigger.table[-3]
# remove Health insurance No category
bigger.table <- data.frame(bigger.table[c(1:26), ])
# add the age summary data
# make a small data frame for the age information
bigger.table <- rbind(bigger.table, data.frame(x="Poor health (days/mo)",
                                               Freq = median(x = brfss.2014.small$PHYSHLTH,
                                                             na.rm = TRUE),
                                               Freq.1 = IQR(x = brfss.2014.small$PHYSHLTH,
                                                            na.rm = TRUE)))
# add variable names
names(bigger.table) <- c("Survey participant demographics", "Frequency", "Percent")

# Table 2.5
bigger.table %>%
  kable(format = "html", caption = "Transgender Survey Participant Demographics",
        row.names = FALSE) %>%
  kableExtra::group_rows(group_label = "Transition status (n = 220)",
                         start_row = 1, end_row = 3) %>%
  kableExtra::group_rows(group_label = "Age category",
                         start_row = 4, end_row = 10) %>%
  kableExtra::group_rows(group_label = "Race/ethnicity",
                         start_row = 11, end_row = 15) %>%
  kableExtra::group_rows(group_label = "Income category",
                         start_row = 16, end_row = 21) %>%
  kableExtra::group_rows(group_label = "Education category",
                         start_row = 22, end_row = 25) %>%
  kableExtra::group_rows(group_label = "Health insurance?",
                         start_row = 26, end_row = 26) %>%
  kableExtra::group_rows(group_label = "Days poor physical health per month (median, IQR)",
                       start_row = 27, end_row = 27)

