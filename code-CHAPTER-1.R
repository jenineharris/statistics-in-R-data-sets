##### CHAPTER 1 #####

### box 1.3
2+2
(4+6)/2
10^2
a <- 3
a

## section 1.5.2
states <- 29
states
2 + states

## section 1.5.3
# CHECK YOUR UNDERSTANDING 
myname <- 48
5 + myname 

## section 1.6.1

# create an object with the number of states with
# legal medical marijuana in 2017
states <- 29

# print the value of the states object
states

# determine how many states there would be if 2
# more passed this policy
2 + states

## section 1.6.1

###########################################
# Project: R-Team meeting one
# Purpose: Code examples for meeting one
# Author: Leslie
# Edit date: April 19, 2019
# No external data files used
###########################################

# create an object with the number of states with
# legal medical marijuana in 2017
states <- 29

# print the value of the states object
states

# determine how many states there would be if 2
# more passed this policy
2 + states

## section 1.6.3

# make a new object with well-formatted name
kStates <- 29

# assign the existing states object a new name
kStates <- states

# remove the states object
rm(states)

## section 1.6.4
# CHECK YOUR UNDERSTANDING 

###########################################
# Project: R-Team meeting one
# Purpose: Code examples for meeting one
# Author: Leslie
# Edit date: October 24, 2019
# No external data files used
###########################################

kIllegalNum <- 21
killegalNum - 2

## section 1.7

# identify data type for states object
class(x = kStates)

## section 1.7.1

# assign Rhode Island limit for medical marijuana
# in ounces per person
kOuncesRhode <- 2.5

# identify the data type for kOuncesRhode
class(x = kOuncesRhode)

## section 1.7.2 

# assign the value of 4 to a constant called kTestInteger
# make sure it is an integer
kTestInteger <- as.integer(x = 4)

# use class() to determine the data type of kTestInteger
class(x = kTestInteger)

# use as.integer() to truncate the constant kOuncesRhode
as.integer(x = kOuncesRhode)

# multiply the kTestInteger and kOuncesRhode constants
kTestInteger * kOuncesRhode

# multiply kTestInteger and integer kOuncesRhode constants
kTestInteger * as.integer(x = kOuncesRhode)

# type the object name to see what is currently saved
# in the object
kOuncesRhode

## section 1.7.3

# create the constant
kTestLogical <- TRUE

# print the value of the constant
kTestLogical

# check the constant data type
class(x = kTestLogical)

# store the result of 6 > 8 in a constant called kSixEight
kSixEight <- 6 > 8

# print kSixEight
kSixEight

# determine the data type of kSixEight
class(x = kSixEight)

## section 1.7.4

# make constants
kFirstName <- "Corina"
kLastName <- "Hughes"

# check the data type
class(x = kFirstName)

# create a zip code constant
# check the data type
kZipCode <- "97405"
class(x = kZipCode)

## section 1.7.6
# CHECK YOUR UNDERSTANDING

# check the data type for kIllegalNum
class(x = kIllegalNum)

## section 1.8.1

# creates character vector char.vector
char.vector <- c('Oregon', 'Vermont', 'Maine')

# prints vector char.vector
char.vector

# creates numeric vector nums.1.to.4
nums.1.to.4 <- c(1, 2, 3, 4)

# prints vector nums.1.to.4
nums.1.to.4

# creates logical vector logic.vector
logic.vector <- c(TRUE, FALSE, FALSE, TRUE)

# prints vector logic.vector
logic.vector

# create and print vectors
( char.vector <- c('Oregon', 'Vermont', 'Maine') )

( nums.1.to.4 <- c(1, 2, 3, 4) )

( logic.vector <- c(TRUE, FALSE, FALSE, TRUE) )

# add 3 to each element in the nums.1.to.4 vector
nums.1.to.4 + 3

# add 1 to the 1st element of nums.1.to.4, 2 to the 2nd element, etc
nums.1.to.4 + c(1, 2, 3, 4)

# multiply each element of nums.1.to.4 by 5
nums.1.to.4 * 5

# subtract 1 from each element and then divide by 5
(nums.1.to.4 - 1) / 5

# make a subset of the vector including numbers > 2
nums.1.to.4[nums.1.to.4 > 2]

# add 3 to number vector and save
# as new vector
( nums.1.to.4.plus.3 <- nums.1.to.4 + 3 )

# divide vector by 10 and save
# as new vector
( nums.1.to.4.div.10 <- nums.1.to.4 / 10 )

# add 3 and divide by 10 for each vector member
( nums.1.to.4.new <- (nums.1.to.4 + 3) / 10 )

## section 1.8.2

# create and print a matrix
( policies <- matrix(data = c(1, 2, 3, 4, 5, 6), #data in the matrix
                     nrow = 2, # number of rows
                     ncol = 3, # number of columns
                     byrow = TRUE) ) # fill the matrix by rows

# add names to the rows and columns of the matrix
dimnames(x = policies) <- list(
  c("2013", "2014"), # row names
  c("medical", "recreational", "both") # column names
)
# print the policies matrix
policies

# vector of policy types
policy.2013.and.2014 <- c('medical', 'medical', 'both', 'recreational',
                          'medical', 'both', 'both')
# data type
class(x = policy.2013.and.2014)

# change the data type to factor
policy.2013.and.2014 <- as.factor(x = policy.2013.and.2014)

# check the data type
class(x = policy.2013.and.2014)

## section 1.8.3

# state, year enacted, personal oz limit medical marijuana
# create vectors
state <- c('Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado')
year.legal <- c('1998', '2010', '2016', '1996', '2000')
ounce.lim <- c(1, 2.5, 3, 8, 2)

# combine vectors into a data frame
# name the data frame pot.legal
pot.legal <- data.frame(state, year.legal, ounce.lim)

# change state variable from pot.legal data frame
# to a character variable
pot.legal$state <- as.character(x = pot.legal$state)

# check the variable type
class(x = pot.legal$state)

# summarize the data frame
summary(object = pot.legal)

## section 1.8.5 

# read the GSS 2016 data
gss.2016 <- read.csv(file = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/legal_weed_age_GSS2016_ch1.csv")

# examine the contents of the file
summary(object = gss.2016)

## box 1.6

install.packages(pkgs = "tidyverse")
library(package = "tidyverse")

# pipe the data set into ggplot
# in the aesthetics provide the variable names for the x and y axes
# choose a geom for graph type
# add axis labels with labs
# choose a theme for the overall graph look
USArrests %>%
  ggplot(aes(x = UrbanPop, y = Assault)) +
  geom_point() +
  labs(x = "Percent of state population living in urban areas",
       y = "Number of reported assaults per 100,000 annually") +
  theme_minimal()

# bring in GSS 2016 data
gss.2016 <- data.table::fread(input = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/legal_weed_age_GSS2016_ch1.csv")

# examine the contents of the file
summary(object = gss.2016)

# load Hmisc and tidyverse
library(package = "Hmisc")
library(package = "tidyverse")

# use the summarize function
gss.2016 %>%
  summarize(length.age = length(x = age))

# check for conflicts
conflicts()

# use summarize from dplyr
gss.2016 %>%
  dplyr::summarize(length.age = length(x = age))

# check source package for summarize
environment(fun = summarize)

# detach Hmisc
detach(name = package:Hmisc)

# try summarize
gss.2016 %>%
  summarize(length.age = length(x = age))

## section 1.8.6

# first six observations in the gss.2016 data frame
head(x = gss.2016)

# change grass variable to a factor
gss.2016$grass <- as.factor(x = gss.2016$grass)

# recode the 89 OR OLDER category to 89
gss.2016$age[gss.2016$age == "89 OR OLDER"] <- "89"

# bring in GSS 2016 data
gss.2016 <- data.table::fread(input = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/legal_weed_age_GSS2016_ch1.csv")

# change the variable type for the grass variable
gss.2016$grass <- as.factor(x = gss.2016$grass)

# recode "89 OR OLDER" into just "89"
gss.2016$age[gss.2016$age == "89 OR OLDER"] <- "89"

# change the variable type for the age variable
gss.2016$age <- as.numeric(x = gss.2016$age)

# examine the variable types and summary to
# check the work
class(x = gss.2016$grass)
class(x = gss.2016$age)
summary(object = gss.2016)

## section 1.8.7 

# CHECK YOUR UNDERSTANDING
gss.2016 <- data.table::fread(input = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/legal_weed_age_GSS2016_ch1.csv")
# THERE ARE 2867 OBSERVATIONS AND 2 VARIABLES IN THE DATA FRAME

## section 1.9.1

# start over by bringing in the data again
gss.2016 <- data.table::fread(input = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/legal_weed_age_GSS2016_ch1.csv")

# use tidyverse pipe to change DK to NA
gss.2016.cleaned <- gss.2016 %>%
  mutate(grass = as.factor(x = grass)) %>%
  mutate(grass = na_if(x = grass, y = "DK"))

# check the summary, there should be 110 + 3 in the NA category
summary(object = gss.2016.cleaned)

# use tidyverse pipe to change DK and IAP to NA
gss.2016.cleaned <- gss.2016 %>%
  mutate(grass = as.factor(x = grass)) %>%
  mutate(grass = na_if(x = grass, y = "DK")) %>%
  mutate(grass = na_if(x = grass, y = "IAP"))

# check the summary, there should now be 110 + 911 + 3 in the NA category
summary(object = gss.2016.cleaned).

# use tidyverse pipe to change DK and IAP to NA
gss.2016.cleaned <- gss.2016 %>%
  mutate(grass = as.factor(x = grass)) %>%
  mutate(grass = na_if(x = grass, y = "DK")) %>%
  mutate(grass = na_if(x = grass, y = "IAP")) %>%
  mutate(grass = droplevels(x = grass))

# check the summary
summary(object = gss.2016.cleaned)

# use tidyverse to change data types and recode
gss.2016.cleaned <- gss.2016 %>%
  mutate(age = recode(.x = age, "89 OR OLDER" = "89")) %>%
  mutate(age = as.numeric(x = age)) %>%
  mutate(grass = as.factor(x = grass)) %>%
  mutate(grass = na_if(x = grass, y = "DK")) %>%
  mutate(grass = na_if(x = grass, y = "IAP")) %>%
  mutate(grass = droplevels(x = grass)) %>%
  mutate(age.cat = cut(x = age,
                       breaks = c(-Inf, 29, 59, 74, Inf),
                       labels = c("< 30", "30 - 59", "60 - 74", "75+" )))
summary(object = gss.2016.cleaned)

##########################################################
# Project: First R-team meeting
# Purpose: Clean GSS 2016 data
# Author: Leslie
# Edit date: April 20, 2019
# Data: GSS 2016 subset of age and marijuana use variables
##########################################################
# bring in GSS 2016 data from the web and examine it
library(package = "data.table")
gss.2016 <- fread(file = "C:/Users/jenine/Box/research/gamification book/book-for-Sage-070119/PROOFS first ones/data/legal_weed_age_GSS2016_ch1.csv")

# use tidyverse to clean the data
library(package = "tidyverse")
gss.2016.cleaned <- gss.2016 %>%
  mutate(age = recode(.x = age, "89 OR OLDER" = "89")) %>%
  mutate(age = as.numeric(x = age)) %>%
  mutate(grass = as.factor(x = grass)) %>%
  mutate(grass = na_if(x = grass, y = "DK")) %>%
  mutate(grass = na_if(x = grass, y = "IAP")) %>%
  mutate(grass = droplevels(x = grass)) %>%
  mutate(age.cat = cut(x = age,
                       breaks = c(-Inf, 29, 59, 74, Inf),
                       labels = c("< 30", "30 - 59", "60 - 74", "75+" )))
# check the summary
summary(object = gss.2016.cleaned)

## section 1.9.2
# CHECK YOUR UNDERSTANDING

# Mutate is used to recode variables or create new variables, na_if() changes values of a variable to missing (NA), and %>% is the pipe that sends information through the processes 

## section 1.10

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  ggplot(aes(x = grass)) +
  geom_bar()

# show the chart
legalize.bar

## section 1.10.1

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass)) +
  geom_bar()

# show the chart
legalize.bar

## section 1.10.2 

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass, fill = grass)) +
  geom_bar()

# show the chart
legalize.bar

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass, fill = grass)) +
  geom_bar() +
  scale_fill_manual(values = c("#78A678", "#7463AC"),
                    guide = FALSE)
# show the chart
legalize.bar

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass, fill = grass)) +
  geom_bar() +
  scale_fill_manual(values = c("green", 'purple'),
                    guide = FALSE)
# show the chart
legalize.bar

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass, fill = grass)) +
  geom_bar() +
  scale_fill_manual(values = c("#78A678", '#7463AC'),
                    guide = FALSE) +
  theme_minimal() +
  labs(x = "Should marijuana be legal?",
       y = "Number of responses")

# show the chart
legalize.bar

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  ggplot(aes(x = grass,
             y = 100 * (..count..) / sum(..count..),
             fill = grass)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("#78A678", '#7463AC'),
                    guide = FALSE) +
  labs(x = "Should marijuana be legal?",
       y = "Percent of responses")
# show the chart
legalize.bar

# make a bar chart for grass variable
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  mutate(grass = recode_factor(.x = grass,
                               `LEGAL` = "Yes",
                               `NOT LEGAL` = "No")) %>%
  ggplot(aes(x = grass,
             y = 100 * (..count..) / sum(..count..),
             fill = grass)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("#78A678", '#7463AC'),
                    guide = FALSE) +
  labs(x = "Should marijuana be legal?",
       y = "Percent of responses")
# show the chart
legalize.bar

# adding dodge to show bars side by side, remove legend
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  drop_na(age) %>%
  mutate(grass = recode_factor(.x = grass,
                               `LEGAL` = "Yes",
                               `NOT LEGAL` = "No")) %>%
  ggplot(aes(x = age.cat,
             y = 100*(..count..)/sum(..count..),
             fill = grass)) +
  geom_bar(position = 'dodge') +
  theme_minimal() +
  scale_fill_manual(values = c("#78A678", '#7463AC'), name = "Should marijuana\nbe legal?") +
  labs(x = "Age category",
       y = "Percent of total responses")
legalize.bar

# code to create Figure 1.2
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  drop_na(age) %>%
  mutate(grass = recode_factor(.x = grass,
                               `LEGAL` = "Yes",
                               `NOT LEGAL` = "No")) %>%
  group_by(grass, age.cat) %>%
  count() %>%
  group_by(age.cat) %>%
  mutate(perc.grass = 100*n/sum(n)) %>%
  ggplot(aes(x = age.cat, fill = grass,
             y = perc.grass)) +
  geom_col(position = 'dodge') +
  theme_minimal() +
  scale_fill_manual(values = c("#78A678", '#7463AC'),
                    name = "Should marijuana\nbe legal?") +
  labs(x = "Age group (in years)",
       y = "Percent of responses in age group")
legalize.bar

## section 1.10.4
# CHECK YOUR UNDERSTANDING 

# With 2,867 observations and 1,836 complete observations, there would be 3 observations missing both, 7 missing only age, and 1,021 missing only grass: 2,867 - 1,836 - 3 - 7 - 1,021 = 0

# code to create Figure 1.2
legalize.bar <- gss.2016.cleaned %>%
  drop_na(grass) %>%
  drop_na(age) %>%
  mutate(grass = recode_factor(.x = grass,
                               `LEGAL` = "Yes",
                               `NOT LEGAL` = "No")) %>%
  group_by(grass, age.cat) %>%
  count() %>%
  group_by(age.cat) %>%
  mutate(perc.grass = 100*n/sum(n)) %>%
  ggplot(aes(x = age.cat, fill = grass,
             y = perc.grass)) +
  geom_col(position = 'dodge') +
  theme_classic() +
  scale_fill_manual(values = c("#88398a", 'gray40'),
                    name = "Should marijuana\nbe legal?") +
  labs(x = "Age group (in years)",
       y = "Percent of responses in age group")
legalize.bar
