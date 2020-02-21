#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 15, 2019                                                    #
# Purpose: Review Code for Chapter 4 of Dr. Harris' Book                        #
#          Probability Distributions and Inference                              #
#         The R Team and the Opioid Overdose Problem                            #
# Packages Used: tidyverse                                                      #
# Data Used: County Health Rankings 2014, 2015, 2015, and 2017,                 #
#            PDMP 2017 from KFF,  Distance to Opioid Facilitiy 2017, amFAR      #
# Last Update: Nov 15, 2019                                                     #
#################################################################################

#BOX 4.1

#load data skip first row
library(package = "tidyverse")

counties.2017.demo <- read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv", skip = 1)

# check the first few observations
head(counties.2017.demo)

# load data skip first row after header
counties.2014.demo <- read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/analytic_data2014.csv")[-1, ]

# check the first few observations
head(x = counties.2014.demo)

#import 2014
# examine the data at http://www.countyhealthrankings.org/sites/default/files/2014%20CHR%20analytic%20data.csv
# the variable names are on the first row
# get the variable names first to add them to the data

colNames2014 <- names(x = read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2014%20CHR%20analytic%20data.csv",
                                   n_max = 0))
# then get the data
# add the variable names using col_names
counties2014 <- read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2014%20CHR%20analytic%20data.csv",
                         col_names = colNames2014, skip=2)

# 2017 has variable names in the second row
# data location http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv
# get the names and save them, skip one row to get the names from second row

colNames2017 <- names(x = read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv",
                                   skip=1, n_max = 0))
# then get the data
# add the variable names using col_names
counties2017 <- read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv",
                         col_names = colNames2017, skip=2)

# 2015 & 2016 have no var names
# URL: http://www.countyhealthrankings.org/sites/default/files/2015%20CHR%20Analytic%20Data.csv
counties2015 <- read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2015%20CHR%20Analytic%20Data.csv")

# URL: http://www.countyhealthrankings.org/sites/default/files/2016CHR_CSV_Analytic_Data_v2.csv
counties2016 <- read_csv(file = "http://www.countyhealthrankings.org/sites/default/files/2016CHR_CSV_Analytic_Data_v2.csv")

# the columns for 2015 and 2016 are named with the labels rather than the variable names
# rename these columns to be consistent with 2014 and 2017
# overdose numerator column should be renamed measure_138_numerator
# overdose denominator column should be renamed measure_138_denominator

counties2015 <- rename(.data = counties2015,
                       measure_138_numerator = "Drug poisoning deaths Numerator")
counties2015 <- rename(.data = counties2015,
                       measure_138_denominator = "Drug poisoning deaths Denominator")

# rename the 2016 columns
counties2016 <- rename(.data = counties2016,
                       measure_138_numerator = "Drug Overdose Deaths Numerator")
counties2016 <- rename(.data = counties2016,
                       measure_138_denominator = "Drug Overdose Deaths Denominator")

# overdose rates function
OverdoseFunc <- function(x) {
  (sum(x$measure_138_numerator, na.rm = TRUE)/sum(x$measure_138_denominator, na.rm = TRUE)) * 100000
}

# overdose rates vector
overdose.per.100k <- c(OverdoseFunc(x = counties2014),
                       OverdoseFunc(x = counties2015),
                       OverdoseFunc(x = counties2016),
                       OverdoseFunc(x = counties2017))
# year vector
year <- c(2014, 2015, 2016, 2017)

# combine vectors into small data frame
od.rates.year <- data.frame(overdose.per.100k, year)

# make the graph (Figure 4.1)
od.rates.year %>%
  ggplot(aes(x = year, y = overdose.per.100k)) +
  geom_line(color = '#7463AC', size = 1) +
  geom_point(color = '#7463AC', size = 3) +
  theme_minimal() +
  ylim(0, 18) +
  labs(y = "Overdose deaths per 100,000 people", x = "Year")

#4.4.3 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#The binomial distribution is a probability distribution for **DICHOTOMOUS (BINARY) CATEGORICAL (Specifically, a binomial random variable)** variables, 
#while the normal distribution is a probability distribution for  **CONTINUOUS** variables.

#4.5.2
# where successes = 10, n = 20, prob = .51
dbinom(x = 10, size = 20, prob = .51)

# 5 successes from a sample of 20 with 51% probability of success
dbinom(x = 5, size = 20, prob = .51)

# 5 successes from 20 selections with 75% probability of success
dbinom(x = 5, size = 20, prob = .75)

# 5 or fewer successes from 20 selections
# with 51% probability of success
pbinom(q = 5, size = 20, prob = .51)

# 10 or more successes from 20 selections
# with 51% probability of success
# get right side of graph
pbinom(q = 10, size = 20, prob = .51, lower.tail = FALSE)

#10 or more successes from 20 selections
# with 51% probability of success
pbinom(q = 9, size = 20, prob = .51, lower.tail = FALSE)

# bring in the opioid policy 2018 data and check it out
opioid.policy.kff <- read.csv(file = "[data folder location]/data/pdmp_2017_kff_ch4.csv")

# check the data frame
summary(object = opioid.policy.kff)

# 15 or more successes from 25 selections
# with 63% probability of success
# pbinom computes left tail, so use lower.tail = FALSE
pbinom(q = 14, size = 25, prob = .63, lower.tail = FALSE)

# sample 25 states at random and get some frequencies
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# sample another 25 states at random using the same code
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# set seed value
set.seed(seed = 35)
# sample 25 states at random
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

set.seed(seed = 35)
# sample 25 states at random
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# reproduce R results from analyses prior
# to R version 3.6.0
RNGkind(sample.kind = "Rounding")
set.seed(seed = 35)

# sample 25 states at random
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# reproduce R results from analyses from
# R version 3.6.0 and later
RNGkind(sample.kind = "Rejection")
set.seed(seed = 35)
# sample 25 states at random
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# reinstate default RNG settings of
# version of R being used
RNGversion(vstr = getRversion())

# set a starting value for sampling
set.seed(seed = 3)
# sample 25 states and check file
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# set a starting value for sampling
set.seed(seed = 10)
# sample 25 states and check file
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

# set a starting value for sampling
set.seed(seed = 999)
# sample 25 states and check file
opioid.policy.kff %>%
  select(Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  sample_n(size = 25) %>%
  summary()

#4.5.4 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
#About 15% probability that in your sample exactly 30 would have quantity policies
#Confirm: dbinom(x = 30, size = 30, prob = .94)

#Section 4.6.1
# distance to substance abuse facility with medication-assisted treatment
dist.mat <- read.csv(file = "[data folder location]/data/opioid_dist_to_facility_2017_ch4.csv")

# review the data
summary(object = dist.mat)

# open tidyverse
library(package = "tidyverse")

# graph the distance variable (Figure 4.13)
dist.mat %>%
  ggplot(aes(x = VALUE)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Miles to nearest substance abuse facility", y = "Number of counties")

#transforming the variable
dist.mat.cleaned <- dist.mat %>%
  mutate(miles.cube.root = VALUE^(1/3)) %>%
  mutate(miles.log = log(x = VALUE)) %>%
  mutate(miles.inverse = 1/VALUE) %>%
  mutate(miles.sqrt = sqrt(x = VALUE))

# graph the transformations (Figure 4.13)
cuberoot <- dist.mat.cleaned %>%
  ggplot(aes(x = miles.cube.root)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Cube root of miles to nearest facility", y = "Number of counties")

logged <- dist.mat.cleaned %>%
  ggplot(aes(x = miles.log)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Log of miles to nearest facility", y = "")

inversed <- dist.mat.cleaned %>%
  ggplot(aes(x = miles.inverse)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() + xlim(0, 1) +
  labs(x = "Inverse of miles to nearest facility", y = "Number of counties")

squareroot <- dist.mat.cleaned %>%
  ggplot(aes(x = miles.sqrt)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Square root of miles to nearest facility", y = "")
gridExtra::grid.arrange(cuberoot, logged, inversed, squareroot)

# mean and standard deviation for cube root of miles
dist.mat.cleaned %>%
  drop_na(miles.cube.root) %>%
  summarize(mean.tran.dist = mean(x = miles.cube.root),
            sd.tran.dist = sd(x = miles.cube.root))

#4.6.2
#shaded area under normal curve > 4
# when curve has mean of 2.66 and sd of .79
pnorm(q = 4, mean = 2.66, sd = .79)

# shaded area under normal curve
# when curve has mean of 2.66 and sd of .79
pnorm(q = 4, mean = 2.66, sd = .79, lower.tail = FALSE)

#4.6.3 Check your understanding
#Estimate: About 20% of counties fall in the shaded area, having to travel < 8 miles to nearest facility (8 = 2^3)
pnorm(q = 2, mean = 2.66, sd = .79)

#4.7.3 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#Use Equation (4.5) to calculate the z-score for a county where you have to drive 15 mi to a facility
#(15^(1/3) - 2.66)/0.79
# z = -0.2453012
# The transformed distance of a facility 15 miles away is .24 standard deviations LOWER than the mean transformed distance

#Section 4.8
# rename variable
dist.mat.cleaned <- dist.mat %>%
  rename('distance' = VALUE)

# get mean and sd from cleaned data
dist.mat.cleaned %>%
  drop_na(distance)%>%
  summarize(mean.dist = mean(x = distance),
            sd.dist = sd(x = distance),
            n = n())

# set a starting value for sampling
set.seed(seed = 1945)

# sample 500 counties at random
counties.500 <- dist.mat.cleaned %>%
  drop_na(distance) %>%
  sample_n(size = 500, replace = TRUE)

# compute the mean distance in the sample
counties.500 %>%
  summarize(mean.s1 = mean(x = distance))

# set a different starting value for sampling
set.seed(seed = 48)

# sample 500 counties at random
counties.500.2 <- dist.mat.cleaned %>%
  drop_na(distance) %>%
  sample_n(size = 500, replace = TRUE)

# compute the mean distance in the sample
counties.500.2 %>%
  summarize(mean.s2 = mean(x = distance))

# get 20 samples
# each sample has 500 counties
# put samples in a data frame with each sample having
# a unique id called sample_num
set.seed(seed = 111)
samples.20 <- bind_rows(replicate(n = 20, dist.mat.cleaned %>%
                                    sample_n(size = 500, replace = TRUE),
                                  simplify = FALSE), .id = "sample_num")

# find the mean for each sample
sample.20.means <- samples.20 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = distance, na.rm = TRUE))
sample.20.means

# find the mean of the 20 sample means
sample.20.means %>%
  summarize(mean.20.means = mean(x = mean.distance))

# get 100 samples
set.seed(seed = 143)
samples.100 <- bind_rows(replicate(n = 100, dist.mat.cleaned %>%
                                     drop_na(distance) %>%
                                     sample_n(size = 500, replace = TRUE),
                                   simplify = FALSE), .id = "sample_num")

# find the mean for each sample
sample.100.means <- samples.100 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = distance))

# find the mean of the 100 sample means
sample.100.means %>%
  summarize(mean.100.means = mean(mean.distance))

# get 1000 samples
set.seed(seed = 159)
samples.1000 <- bind_rows(replicate(n = 1000, dist.mat.cleaned %>%
                                      drop_na(distance) %>%
                                      sample_n(size = 500, replace = TRUE),
                                    simplify = FALSE), .id = "sample_num")

# find the mean for each sample
sample.1000.means <- samples.1000 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = distance))

# find the mean of the sample means
sample.1000.means %>%
  summarize(mean.1000.means = mean(x = mean.distance))

#histogram of the 1000 means (Figure 4.17)
sample.1000.means %>%
  ggplot(aes(x = mean.distance)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  labs(x = "Mean distance to facility with MAT",
       y = "Number of samples") +
  theme_minimal()

#4.8.5
# compute estimated standard dev of sampling dist
dist.mat.cleaned %>%
  drop_na(distance) %>%
  summarize(n = n(),
            pop.var = var(x = distance)*((n - 1)/n),
            pop.s = sqrt(x = pop.var),
            s.samp.dist.est = pop.s/sqrt(x = 500))

# computing the samp dist standard devation
# directly from the 1000 sample means
sd(x = sample.1000.means$mean.distance, na.rm = T)

# mean, sd, se for first sample of 500 counties
counties.500 %>%
  drop_na(distance) %>%
  summarize(mean.dist = mean(x = distance),
            sd.dist = sd(x = distance),
            se.dist = sd(x = distance)/sqrt(x = length(x = distance)))

# mean, sd, se for second sample
counties.500.2 %>%
  drop_na(distance) %>%
  summarize(mean.dist = mean(x = distance),
            sd.dist = sd(x = distance),
            se.dist = sd(x = distance)/sqrt(x = length(x = distance)))

#4.8.8 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
#set seed for reproducibility
set.seed(50)
# sample 200 counties at random
counties.200 <- dist.mat.cleaned %>%
  drop_na(distance) %>%
  sample_n(size = 200, replace = TRUE) %>%
  summarise(mean.dist = mean(distance),
            sd.dist = sd(distance),
            se.dist = sd(distance)/sqrt(length(distance)))
counties.200
#The mean estimated distance to a facility based on this sample of 200 counties was 23.78 miles. We would estimate that 95% of
# the time, if we were to take a different random sample of counties, the mean distance would fall between 23.78 - (2*1.40) and 23.78 + (2*1.40)
# or 20.98 to 26.58 miles away

#4.9
#add CI to summary statistics
summ.500.counties <- counties.500 %>%
  summarize(mean.s1 = mean(x = distance),
            sd.s1 = sd(x = distance),
            se.s1 = sd(x = distance)/sqrt(x = length(x = distance)),
            lower.ci.s1 = mean.s1 - 1.96 * se.s1,
            upper.ci.s1 = mean.s1 + 1.96 * se.s1)
summ.500.counties

# add CI to summary statistics other sample
counties.500.2 %>%
  summarize(mean = mean(x = distance),
            sd = sd(x = distance),
            se = sd(x = distance)/sqrt(x = length(x = distance)),
            lower.ci = mean - 1.96 * se,
            upper.ci = mean + 1.96 * se)

# add CI to summary statistics other sample
samp.20.stats <- samples.20 %>%
  drop_na(distance) %>%
  group_by(sample_num) %>%
  summarize(means = mean(x = distance),
            sd = sd(x = distance),
            se = sd(x = distance)/sqrt(x = length(x = distance)),
            lower.ci = means - 1.96 * se,
            upper.ci = means + 1.96 * se)
samp.20.stats

# graph means and CI for 20 samples (Figure 4.22)
samp.20.stats %>%
  ggplot(aes(y = means, x = sample_num)) +
  geom_errorbar(aes(ymin = lower.ci,
                    ymax = upper.ci,
                    linetype = "95% CI of\nsample mean"), color = "#7463AC") +
  geom_point(stat = "identity", aes(color = "Sample mean")) +
  geom_hline(aes(yintercept = 24.04, alpha = "Population mean"), color =
               "deeppink") +
  labs(y = "Mean distance to treatment facility (95% CI)",
       x = "Sample") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = c(1, 1), name = "") +
  scale_alpha_manual(values = 1, name = "") +
  theme_minimal()

# get sample statistics
samp.100.stats <- samples.100 %>%
  group_by(sample_num) %>%
  summarize(means = mean(x = distance),
            sd = sd(x = distance),
            se = sd(x = distance)/sqrt(x = length(x = distance)),
            lower.ci = means - 1.96*se,
            upper.ci = means + 1.96*se)

# graph means and CI for 100 samples (Figure 4.23)
samp.100.stats %>%
  ggplot(aes(y = means, x = sample_num)) +
  geom_errorbar(aes(ymin = lower.ci,
                    ymax = upper.ci,
                    linetype = "95% CI of\nsample mean"), color = "#7463AC") +
  geom_point(stat = "identity", aes(color = "Sample mean")) +
  geom_hline(aes(yintercept = 24.04, alpha = "Population mean"), color =
               "deeppink") +
  labs(y = "Mean distance to treatment facility (95% CI)",
       x = "Sample") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = c(1, 1), name = "") +
  scale_alpha_manual(values = 1, name = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# get sample statistics
samp.100.stats <- samples.100 %>%
  group_by(sample_num) %>%
  summarize(means = mean(x = distance),
            sd = sd(x = distance),
            se = sd(x = distance)/sqrt(x = length(x = distance)),
            lower.ci = means - 1.96*se,
            upper.ci = means + 1.96*se,
            differs = lower.ci > 24.04 | upper.ci < 24.04)

# graph means and CI for 100 samples (Figure 4.24)
samp.100.stats %>%
  ggplot(aes(y = means, x = sample_num)) +
  geom_errorbar(aes(ymin = lower.ci,
                    ymax = upper.ci,
                    color = differs)) +
  geom_point(stat = "identity", aes(fill = "Sample mean"), color = "#7463AC") +
  geom_hline(aes(yintercept = 24.04, linetype = "Population mean"), color =
               "dodgerblue2") +
  labs(y = "Mean distance to treatment facility with MAT (95% CI)",
       x = "Sample") +
  scale_fill_manual(values = "#7463AC", name = "") +
  scale_color_manual(values = c("#7463AC", "deeppink"), name = "",
                     labels = c("95% CI of sample mean", "95% CI of sample mean")) +
  scale_linetype_manual(values = c(1, 1), name = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


#Section 4.9.2
#do you drink coffee?
coffee <- c(1, 0, 1, 1, 0,
            0, 0, 1, 1, 1)
mean(x = coffee)
  
# open state opioid program data
state.opioid.pgm.2018 <- read.csv(file = "[data folder location]/data/pdmp_2017_kff_ch4.csv")

# recode Yes to 1 and No to 0
# change long name to pdmp
state.opioid.pgm.2018.cleaned <- state.opioid.pgm.2018 %>%
  rename(pdmp = Required.Use.of.Prescription.Drug.Monitoring.Programs) %>%
  mutate(pdmp = as.numeric(x = pdmp) - 1)

# find the mean of pdmp
state.opioid.pgm.2018.cleaned %>%
  summarize(p = mean(x = pdmp))

# get 100 samples
# each sample has 30 states
# put samples in a data frame with each sample having
# a unique id called sample_num
set.seed(seed = 143)
samples.30.states <- bind_rows(replicate(n = 100, state.opioid.pgm.2018.cleaned %>%
                                           sample_n(size = 30, replace = TRUE),
                                         simplify = FALSE), .id = "sample_num")
# find the mean for each sample
sample.30.means.states <- samples.30.states %>%
  group_by(sample_num) %>%
  summarize(p.pdmp = mean(x = pdmp))
sample.30.means.states

#4.9.4 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
#Describe what a confidence interval is in your own words.
#A confidence interval is are a way to assess where the population measure likely falls, given
# repeated sampling. If you take repeated samples, for instance, the population value would fall into the 95% Confidence interval
#of 95% of those samples

