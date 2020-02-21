#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 16, 2019                                                    #
# Purpose: Review Code for Chapter 6 of Dr. Harris' Book                        #
#          Conducting and Interpreting T-Tests                                  #
#         The R Team and the Blood Pressure Predicament                         #
# Packages Used: tidyverse, RNHANES, lsr, car, BSDA, rcompanion                 #
# Data Used: NHANES 2015-2016                                 ,                 #
# Last Update: Nov 16, 2019                                                     #
#################################################################################
library(package = "RNHANES")

#Box 6.1
# import nhanes 2013-2014 BPX file with demographics
nhanes.2013 <- RNHANES::nhanes_load_data(file_name = "BPX",
                                         year = "2013-2014",
                                         demographics = TRUE)

# installing new RNHANES package from GitHub
devtools::install_github("silentspringinstitute/RNHANES")

# import nhanes 2015-2016 BPX file with demographics
nhanes.2016 <- RNHANES::nhanes_load_data(file_name = "BPX",
                                         year = "2015-2016",
                                         demographics = TRUE)

#6.4
# import nhanes 2015-2016
nhanes.2016 <- read.csv(file = "[data folder location]/data/nhanes_2015-2016_ch6.csv")

# open tidyverse for graphing with ggplot2
library(package = "tidyverse")

# graph systolic blood pressure variable BPXSY1 (Figure 6.1)
sbp.histo <- nhanes.2016 %>%
  ggplot(aes(x = BPXSY1)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "NHANES participants")
sbp.histo

# graph systolic bp BPXSY1 (Figure 6.2)
sbp.histo <- nhanes.2016 %>%
  ggplot(aes(x = BPXSY1, fill = BPXSY1 > 120)) +
  geom_histogram(color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("#7463AC","gray"),
                    labels = c("Normal range", "At-risk or high"),
                    name = "Systolic\nblood pressure") +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "Number of NHANES participants")
sbp.histo

# graph diastolic bp BPXDI1 (Figure 6.3)
nhanes.2016 %>%
  ggplot(aes(x = BPXDI1, fill = BPXDI1 > 80)) +
  geom_histogram(color = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("#7463AC", "gray"),
                    labels = c("Normal range", "At-risk or high"),
                    name = "Blood pressure") +
  labs(x = "Diastolic blood pressure (mmHg)",
       y = "Number of NHANES participants")

# mean and sd of systolic blood pressure
nhanes.2016 %>%
  drop_na(BPXSY1) %>%
  summarize(m.sbp = mean(x = BPXSY1),
            sd.sbp = sd(x = BPXSY1))


#6.4.1 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#A density plot and boxplot could also be used to check the normality of the blood pressure data

#boxplot of variable to see median, 25%, and 75% plus outliers
nhanes.2016 %>%
  ggplot(aes(y = BPXDI1)) +
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Blood Pressure") +
  coord_flip()

#density plot
nhanes.2016 %>%
  ggplot(aes(x = BPXDI1)) +
  geom_density(bw = 1.5, fill = "gray")+
  theme_minimal()+
  labs(x = "Blood Pressure",
       y = "Probability Density")

#6.5
# mean and sd of systolic blood pressure
nhanes.2016 %>%
  drop_na(BPXSY1) %>%
  summarize(m.sbp = mean(x = BPXSY1),
            sd.sbp = sd(x = BPXSY1),
            n.spb = n())

# comparing mean of BPXSY1 to 120
t.test(x = nhanes.2016$BPXSY1, mu = 120)

#6.5.5 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
#create a subset of the data frame of people 65+ years old
nhanes.2016.65plus <- nhanes.2016 %>%
  filter(RIDAGEYR >= 65)

# comparing mean of BPXSY1 to 120
t.test(x = nhanes.2016.65plus$BPXSY1, mu = 120)

sd(nhanes.2016.65plus$BPXSY1, na.rm = T)

#The mean systolic blood pressure in a sample of 1233 NHANES participants who
#were age 65 and above was 136.60 (sd = 19.93). The mean systolic blood pressure was found to be
#statistically significantly different from the hypothesized mean of 120 via a 
#one sample t-test (t(1232) = 29.238, p < 0.001). The true mean systolic blood pressure
#in the population of adults 65 and older is likely not equal to 120.

#6.6
# compare means of BPXSY1 across groups
# sex variable is RIAGENDR
nhanes.2016 %>%
  drop_na(BPXSY1) %>%
  group_by(RIAGENDR) %>%
  summarize(m.sbp = mean(x = BPXSY1))

# add labels to sex and rename variables
nhanes.2016.cleaned <- nhanes.2016 %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR,
                                  `1` = 'Male',
                                  `2` = 'Female')) %>%
  rename(sex = RIAGENDR) %>%
  rename(systolic = BPXSY1)

# compare means of systolic by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(x = systolic))
  
# density plot of systolic by sex (Figure 6.8)
dens.sex.bp <- nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic,
             fill = sex)) +
  geom_density(alpha = .8) +
  theme_minimal() +
  labs(x = "Systolic blood pressure", y = "Probability density") +
  scale_fill_manual(values = c('gray', '#7463AC'),
                    name = "Sex")
dens.sex.bp

# compare means of systolic by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(x = systolic),
            var.sbp = var(x = systolic),
            samp.size = n())

# compare systolic blood pressure for males and females
twosampt <- t.test(formula = nhanes.2016.cleaned$systolic ~ nhanes.2016.cleaned$sex)
twosampt

#6.6.5 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
#Run a two-sample t test to check if systolic BP
#differs btwn males and females age 65+
nhanes.2016.65plus.clean<- 
  nhanes.2016.65plus %>%
  drop_na(BPXSY1) %>%
  mutate(sex = recode_factor(.x = RIAGENDR,
                             `1` = "Male",
                             `2` = "Female")) %>%
  rename(systolic = BPXSY1)

nhanes.2016.65plus.clean %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(x = systolic),
            var.sbp = var(x = systolic),
            sd.sbp = sd(x = systolic),
            n = n())

# compare systolic blood pressure for males and females age 65+
twosampt65 <- t.test(formula = nhanes.2016.65plus.clean$systolic ~ nhanes.2016.65plus.clean$sex)
twosampt65

#There is a statistically significant difference in the mean blood pressure of
#males (n = 613) and females (n = 620) over the age of 65 participating in the 2016 NHANES
#(t(1231) = -2.01, p = 0.044). The mean blood pressure for males in the sample was
#135 (sd = 19.8), while the mean for females was 138 (sd = 20.0). Thus, in our sample
#the mean difference in systolic blood pressure between males and females was -3.0 points,
#whereas the estimated difference in the population of US males and females over age 65
# is between -0.059 points and -4.508 points.

#6.7
# rename second systolic measure and create diff variable for
# difference between measure 1 and 2 for systolic BP
nhanes.2016.cleaned <- nhanes.2016 %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR,
                                  `1` = 'Male',
                                  `2` = 'Female')) %>%
  rename(sex = RIAGENDR) %>%
  rename(systolic = BPXSY1) %>%
  rename(systolic2 = BPXSY2) %>%
  mutate(diff.syst = systolic - systolic2)

# mean of the differences
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  summarize(m.diff = mean(x = diff.syst))

# histogram of the differences between first and second (Figure 6.10)
# blood pressure measures
nhanes.2016.cleaned %>%
  ggplot(aes(x = diff.syst)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Difference between SBP Measures 1 and 2",
       y = "Number of NHANES participants")

#6.7.2
# mean, var, and sample size of the difference variable
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  summarize(m.sbp = mean(x = diff.syst),
            var.sbp = var(x = diff.syst),
            n = n())

# dependent-samples t-test for systolic measures 1 and 2
t.test(x = nhanes.2016.cleaned$systolic,
       y = nhanes.2016.cleaned$systolic2,
       paired = TRUE)

#6.7.5 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#one sample: Compares a single mean to a population or hypothesized value
#two sample: Compares means of two unrelated groups
#paired sample: Compares two related means
       
#6.8.1
# Cohen's d effect size for one-sample t
lsr::cohensD(x = nhanes.2016.cleaned$systolic, mu = 120)

#6.8.2
# compare means of systolic by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(systolic),
            var.sbp = var(systolic))

# cohen's d effect size for indep sample t
lsr::cohensD(x = systolic ~ sex,
             data = nhanes.2016.cleaned,
             method = "unequal")

# var and sample size of the difference variable
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  summarize(m.sbp = mean(x = diff.syst),
            sd.sbp = sd(x = diff.syst))

#cohen's d effect size for indep sample t
lsr::cohensD(x = nhanes.2016.cleaned$systolic,
             y = nhanes.2016.cleaned$systolic2,
             method = "paired")

#6.9
# graph systolic bp (Figure 6.11)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "NHANES participants")

# skewness of systolic bp
semTools::skew(object = nhanes.2016.cleaned$systolic)

# graph systolic bp by sex (Figure 6.13)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic)) +
  geom_histogram(fill = "#7463AC", col = "grey") +
  facet_grid(cols = vars(sex)) +
  theme_minimal() +
  labs(x="Systolic blood pressure (mmHg)",
       y="NHANES participants")

#graph systolic bp (Figure 6.14)
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  ggplot(aes(sample = systolic)) +
  stat_qq(aes(color = "NHANES participant"), alpha = .6) +
  facet_grid(cols = vars(sex)) +
  geom_abline(aes(intercept = mean(x = systolic),
                  slope = sd(x = systolic), linetype = "Normally distributed"),
              color = "gray", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed systolic blood pressure (mmHg)")+
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# statistical test of normality for systolic bp by sex
nhanes.2016.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(z.skew = semTools::skew(object = systolic)[3])

# graph systolic difference between systolic and systolic2 (Figure 6.15)
nhanes.2016.cleaned %>%
  ggplot(aes(x = diff.syst)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Difference between measures of systolic blood pressure (mmHg)",
       y = "NHANES participants")

# Q-Q plot difference between systolic and systolic2 (Figure 6.16)
nhanes.2016.cleaned %>%
  drop_na(diff.syst) %>%
  ggplot(aes(sample = diff.syst)) +
  stat_qq(aes(color = "NHANES participant"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = diff.syst),
                  slope = sd(x = diff.syst), linetype = "Normally distributed"),
              color = "gray", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed differences between SBP measures")+
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# equal variances for systolic by sex
car::leveneTest(y = systolic ~ sex, data = nhanes.2016.cleaned)

#6.9.1 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
#noramlity assumption is violated

#6.10
# examine median for systolic variable
median(x = nhanes.2016.cleaned$systolic, na.rm = TRUE)

#6.10.1.2
# compare observed median SBP to 120
BSDA::SIGN.test(x = nhanes.2016.cleaned$systolic, md = 120)

#6.10.2
# test the distribution of SBP by time period
wilcox.test(x = nhanes.2016.cleaned$systolic,
            y = nhanes.2016.cleaned$systolic2,
            paired = TRUE)

#6.10.3
# test the distribution of systolic by sex
u.syst.by.sex <- wilcox.test(formula = nhanes.2016.cleaned$systolic ~
  nhanes.2016.cleaned$sex, paired = FALSE)
u.syst.by.sex

#6.10.4
# use qnorm to find z from p-value
qnorm(p = u.syst.by.sex$p.value)

# new data frame with no NA
nhanes.2016.cleaned.noNA <- nhanes.2016.cleaned %>%
  drop_na(systolic)

# use new data frame to get r
rcompanion::wilcoxonR(x = nhanes.2016.cleaned.noNA$systolic,
                      g = nhanes.2016.cleaned.noNA$sex)

#6.10.5
# get vectors for male and female systolic
males.systolic <- nhanes.2016.cleaned %>%
  filter(sex == "Male") %>%
  pull(var = systolic)

females.systolic <- nhanes.2016.cleaned %>%
  filter(sex == "Female") %>%
  pull(var = systolic)

# conduct the test
ks.test(x = males.systolic,
        y = females.systolic)

# ECDF for male and female SBP (Figure 6.19)
nhanes.2016.cleaned %>%
  ggplot(aes(x = systolic, color = sex)) +
  stat_ecdf(size = 1) +
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "Cumulative probability") +
  scale_color_manual(values = c("Male" = "gray", "Female" = "#7463AC"),
                     name = "Sex")
