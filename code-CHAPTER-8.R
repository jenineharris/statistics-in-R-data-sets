#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 17, 2019                                                    #
# Purpose: Review Code for Chapter 8 of Dr. Harris' Book                        #
#          Correlation Coefficients                                             #
#         The R Team and the Clean Water Conundrum                              #
# Packages Used: tidyverse, readxl, lmtest, rcompanion, ppcor                   #
# Data Used: WHO and UNESCO 2015                                                #
# Last Update: Nov 17, 2019                                                     #
#################################################################################

#Box 8.1
# import water data from WHO website
water <- read_csv("http://apps.who.int/gho/athena/data/GHO/WSH_WATER_SAFELY_MANAGED,WSH_WATER_BASIC?filter=COUNTRY:*;RESIDENCEAREATYPE:*&x-sideaxis=COUNTRY&x-topaxis=YEAR;GHO;RESIDENCEAREATYPE&profile=crosstable&format=csv")

# download the water data from WHO website
# skip first two rows
water <- read_csv("http://apps.who.int/gho/athena/data/GHO/WSH_WATER_SAFELY_MANAGED,WSH_WATER_BASIC?filter=COUNTRY:*;RESIDENCEAREATYPE:*&x-sideaxis=COUNTRY&x-topaxis=YEAR;GHO;RESIDENCEAREATYPE&profile=crosstable&format=csv",
                  skip = 2)

# limit data to 2015 basic and safe water
library(package = "tidyverse")
water.cleaned <- water %>%
  select(Country, Total, Total_1) %>%
  rename(country = 'Country', perc.basic2015water = 'Total', perc.safe2015water = 'Total_1')

# get sanitation data
sanitation <- read_csv("http://apps.who.int/gho/athena/data/GHO/WSH_SANITATION_SAFELY_MANAGED,WSH_SANITATION_BASIC?filter=COUNTRY:*;RESIDENCEAREATYPE:*&x-sideaxis=COUNTRY&x-topaxis=YEAR;GHO;RESIDENCEAREATYPE&profile=crosstable&format=csv", skip = 2)

# limit to 2015
# name the variables consistent with water data
sanitation.cleaned <- sanitation %>%
  select(Country, Total, Total_1) %>%
  rename(country = 'Country', perc.basic2015sani = 'Total', perc.safe2015sani = 'Total_1')

# get population data
pop <- read_csv("[data folder location]/data/2015-who-income-datach8.csv", skip = 1)

# add variable names and recode
# change to numeric
pop.cleaned <- pop %>%
  rename(country = "Country", med.age = "2013", perc.1dollar =
           "2007-2013") %>%
  mutate(perc.1dollar = as.numeric(recode(perc.1dollar, `&lt;2.0` =
                                            "1")))
# bring in education data
educ <- readxl::read_excel(path = "[data folder location]/data/2015-outOfSchoolRate-primarySecondary-ch8.xlsx", skip = 4)

# examine the education data
View(x = educ)

# remove second column and rename the variables
educ.cleaned <- educ %>%
  select(-...2) %>%
  slice(1:280) %>%
  rename(country = "Country", perc.in.school = "...3",
         female.in.school = "...4", male.in.school = "...5")

# change variable types and recode
educ.cleaned <- educ %>%
  select(-...2) %>%
  slice(1:280) %>%
  rename(country = "Country", perc.in.school = "...3",
         female.in.school = "...4", male.in.school = "...5") %>%
  na_if("..") %>%
  mutate(perc.in.school = 100 - as.numeric(perc.in.school)) %>%
  mutate(female.in.school = 100 - as.numeric(female.in.school)) %>%
  mutate(male.in.school = 100 - as.numeric(male.in.school))

# review data
summary(object = educ.cleaned)

# merge population, sanitation, water data frames by country
# merge the data frames
water.educ <- educ.cleaned %>%
  merge(pop.cleaned) %>%
  merge(sanitation.cleaned) %>%
  merge(water.cleaned)

# remove observations with na for school variables
water.educ <- water.educ %>%
  drop_na(perc.in.school) %>%
  drop_na(female.in.school) %>%
  drop_na(male.in.school)

# save as a csv
write.csv(x = water.educ, file = "[data folder location]/data/water_educ.csv", row.names = FALSE)

#8.4
# import the water data
water.educ <- read.csv(file = "[data folder location]/data/water_educ_2015_who_unesco_ch8.csv")

# examine the data
summary(object = water.educ)

# open the tidyverse
library(package = "tidyverse")

# descriptive statistics for females in school and water access
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  summarize(m.f.educ = mean(x = female.in.school),
            sd.f.educ = sd(x = female.in.school),
            m.bas.water = mean(x = perc.basic2015water),
            sd.bas.water = sd(x = perc.basic2015water))

#8.4.1
# explore plot of female education and water access (Figure 8.3)
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_point(aes(color = "Country"), size = 2, alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic water access") +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

#8.4.2 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#A positive correlation indicates: one variable increases when the other increases.

#8.5
# covariance of females in school, poverty, and
# percentage with basic access to drinking water
water.educ %>%
  summarize(cov.females.water = cov(x = perc.basic2015water,
                                    y = female.in.school,
                                    use = "complete"),
            cov.females.pov = cov(x = perc.1dollar,
                                  y = female.in.school,
                                  use = "complete"))

# covariance of females in school, poverty, and
# percentage with basic access to drinking water
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  drop_na(perc.1dollar) %>%
  summarize(cov.females.water = cov(x = perc.basic2015water,
                                    y = female.in.school),
            cov.females.pov = cov(x = perc.1dollar,
                                  y = female.in.school))

# covariance of females in school and
# percentage with basic access to drinking water
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  summarize(cov.females.water = cov(x = perc.basic2015water,
                                    y = female.in.school))

# covariance of poverty and
# percentage of females in school
water.educ %>%
  drop_na(perc.1dollar) %>%
  drop_na(female.in.school) %>%
  summarize(cov.females.pov = cov(x = perc.1dollar,
                                  y = female.in.school))

#8.5.3
# explore plot of female education and water access (Figure 8.7)
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = "Fit line"), color
              = "gray60") +
  geom_point(size = 2, aes(color = "Country"), alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")

# correlation between water access and female education
water.educ %>%
  summarize(cor.females.water = cor(x = perc.basic2015water,
                                    y = female.in.school,
                                    use = "complete"))

#Box 8.2
# explore plot of female education and water access (Figure 8.8)
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray60") +
  geom_point(size = 2, color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# explore plot of female education and water (Figure 8.9)
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Linear fit line")) +
  geom_point(aes(size = "Country"), color = "#7463AC", alpha = .6)+
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = "gray60", name = "") +
  scale_size_manual(values = 2, name = "")

# explore plot of female education and water (Figure 8.10)
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Linear fit line")) +
  geom_point(aes(color = "Country"), size = 2, alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#7463AC", "gray60"), name = "")

#8.5.4
# correlations between water access, poverty, and female education
water.educ %>%
  summarize(cor.females.water = cor(x = perc.basic2015water,
                                    y = female.in.school,
                                    use = "complete"),
            cor.females.pov = cor(x = perc.1dollar,
                                  y = female.in.school,
                                  use = "complete"))

#8.5.5 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
water.educ %>%
  summarize(cor.females.sani = cor(x = perc.basic2015sani,
                                   y = female.in.school,
                                   use = "complete"))

#graph
water.educ %>%
  ggplot(aes(x = perc.basic2015sani/100, y = female.in.school/100)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Linear fit line")) +
  geom_point(aes(color = "Country"), size = 2, alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic sanitation access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#7463AC", "gray60"), name = "")

#There is a strongly positive association between access to basic sanitation
#and percent of females enrolled in school (r = 0.83). As the percent of the population with
#access to basic sanitation increases, so to does the percent of females enrolled in school

#8.6
# correlation between water access and female education
water.educ %>%
  drop_na(perc.basic2015water) %>%
  drop_na(female.in.school) %>%
  summarize(cor.females.water = cor(x = perc.basic2015water,
                                    y = female.in.school),
            samp.n = n())

# test for correlation coefficient
cor.test(x = water.educ$perc.basic2015water,
         y = water.educ$female.in.school)

#8.6.5 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
cor.test(x = water.educ$perc.1dollar,
         y = water.educ$female.in.school)
# a. Negative, statistically significant

#8.7
# conduct the correlation analysis
# assign the results to an object
cor.Fem.Educ.Water <- cor.test(x = water.educ$perc.basic2015water,
                               y = water.educ$female.in.school)
# explore the object
str(object = cor.Fem.Educ.Water)

# square the correlation coefficient
r.squared <- cor.Fem.Educ.Water$estimate^2
r.squared

#Box 8.3.1
# correlation of sanitation and water access
cor.test(x = water.educ$perc.basic2015sani,
         y = water.educ$perc.basic2015water)

# compute coefficient of determination
.89^2

# correlation of sanitation and water access
cor.sani.water <- cor.test(x = water.educ$perc.basic2015sani,
                           y = water.educ$perc.basic2015water)
cor.sani.water

#coefficient of determination for
# sanitation and water access
cod <- cor.sani.water$estimate^2
cod

#8.7.3 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#conduct cor.test
cor.females.sani = cor.test(x = water.educ$perc.basic2015sani,
                       y = water.educ$female.in.school,
                       use = "complete")
(cod.females.sani <- cor.females.sani$estimate^2)
#a. 0.69

#8.8.1
# check normality of female.in.school variable (Figure 8.14)
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(x = female.in.school)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Percent of school-aged females in school",
       y = "Number of countries")

# Q-Q plot of female.in.school variable to check normality (Figure 8.15)
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(sample = female.in.school)) +
  stat_qq(aes(color = "Country"), alpha = .6) +
  geom_abline(aes(intercept = mean(female.in.school),
                  slope = sd(female.in.school),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent of\nschool-aged females in school") +
  ylim(0,100) +
  scale_linetype_manual(values = 1, name = "") +
  scale_color_manual(values = "#7463AC", name = "")

# check normality of water access variable (Figure 8.16)
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(x = perc.basic2015water)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Percent with basic water access",
       y = "Number of countries")

# Q-Q plot of water access variable to check normality (Figure 8.17)
water.educ %>%
  drop_na(female.in.school) %>%
  drop_na(perc.basic2015water) %>%
  ggplot(aes(sample = perc.basic2015water)) +
  stat_qq(aes(color = "Country"), alpha = .6) +
  geom_abline(aes(intercept = mean(x = perc.basic2015water),
                  slope = sd(x = perc.basic2015water),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent of people\nwith basic water access") +
  ylim(0,100) +
  scale_linetype_manual(values = 1, name = "") +
  scale_color_manual(values = "#7463AC", name = "")


#8.8.2
# female education and water graph with linear fit line and Loess curve (Figure 8.18)
water.educ %>%
  ggplot(aes(y = female.in.school/100, x = perc.basic2015water/100)) +
  geom_point(aes(size = "Country"), color = "#7463AC", alpha = .6) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent with basic access to water") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("gray60", "deeppink"), name= "") +
  scale_size_manual(values = 2, name = "")

# Breusch-Pagan test for equal variance
testVar <- lmtest::bptest(formula = water.educ$female.in.school ~ water.educ$perc.basic2015water)
testVar

#8.8.5 ACHIEVEMENT 5: CHECK YOUR UNDERSTANDING
#a) Observations are independent: No nearby countries are likely to be more similar
#than those that are further away, which could violate this assumption
#b) Both variables are continuous: Yes (again with caveat that it is a percentage, so there are ceiling and
#floor effects). It's "quasi-continuous"
#c) Variables are normally distributed: No
#d) Relationship between variables is linear: Yes
#e) Homoscedasticity:
# Breusch-Pagan test for equal variance: Not met
testVar2 <- lmtest::bptest(formula = water.educ$female.in.school ~ water.educ$perc.basic2015sani)
testVar2


#8.9
# create new variables
water.educ.new <- water.educ %>%
  mutate(logit.female.school = log(x = (female.in.school/100)/(1-female.in.school/100))) %>%
  mutate(logit.perc.basic.water = log(x = (perc.basic2015water/100)/(1-
                                                                       perc.basic2015water/100))) %>%
  mutate(arcsin.female.school = asin(x = sqrt(female.in.school/100))) %>%
  mutate(arcsin.perc.basic.water = asin(x = sqrt(perc.basic2015water/100)))
# check the data
summary(water.educ.new)

# use Tukey transformation to get power for transforming
# female in school variable to more normal distribution
p.female <- rcompanion::transformTukey(x = water.educ$female.in.school,
                                       plotit = FALSE,
                                       quiet = TRUE,
                                       returnLambda = TRUE)
p.female

# use Tukey transformation to get power for transforming
# basic 2015 water variable to more normal distribution
p.water <- rcompanion::transformTukey(x = water.educ$perc.basic2015water,
                                      plotit = FALSE,
                                      quiet = TRUE,
                                      returnLambda = TRUE)
p.water


# create new transformation variables
water.educ.new <- water.educ %>%
  mutate(arcsin.female.school = asin(x = sqrt(female.in.school/100))) %>%
  mutate(arcsin.perc.basic.water = asin(x = sqrt(perc.basic2015water/100)))%>%
  mutate(folded.p.female.school = (female.in.school/100)^(1/p.female) -
           (1-female.in.school/100)^(1/p.female)) %>%
  mutate(folded.p.basic.water = (perc.basic2015water/100)^(1/p.water) - (1-perc.basic2015water/100)^(1/p.water))

# check the data
summary(water.educ.new)

# histogram of arcsin females in school (Figure 8.21)
water.educ.new %>%
  ggplot(aes(x = arcsin.female.school)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Arcsine transformation of females in school", y = "Number of countries")

# histogram of folded power transf females in school (Figure 8.22)
water.educ.new %>%
  ggplot(aes(x = folded.p.female.school)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Folded power transformation of females in school", y = "Number of countries")

# histogram of arcsine of water variable (Figure 8.23)
water.educ.new %>%
  ggplot(aes(x = arcsin.perc.basic.water)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Arcsine transformed basic water access", y = "Number of countries")

# histogram of folded power transformed water variable (Figure 8.24)
water.educ.new %>%
  ggplot(aes(x = folded.p.basic.water)) +
  geom_histogram(fill = "#7463AC", color = "white") +
  theme_minimal() +
  labs(x = "Folded power transformed basic water access", y = "Number of countries")

#8.9.2
# correlation test for transformed variables
cor.test(water.educ.new$folded.p.female.school,
         water.educ.new$folded.p.basic.water)

# explore plot of transformed females in school and basic water
# with linear fit line and Loess curve (Figure 8.25)
water.educ.new %>%
  ggplot(aes(y = folded.p.female.school, x = folded.p.basic.water)) +
  geom_smooth(aes(color = "linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  geom_point(aes(size = "Country"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Power transformed percent of females in school",
       x = "Power transformed percent with basic water access") +
  scale_color_manual(name="Type of fit line", values=c("gray60",
                                                       "deeppink")) +
  scale_size_manual(values = 2)

# testing for homoscedasticity
bp.test.trans <- lmtest::bptest(formula = water.educ.new$folded.p.female.school ~ 
                                  water.educ.new$folded.p.basic.water)
bp.test.trans

#8.9.6 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING

# use Tukey transformation to get power for transforming perc.1dollar
p.pov <- rcompanion::transformTukey(x = water.educ$perc.1dollar,
                                      plotit = FALSE,
                                      quiet = TRUE,
                                      returnLambda = TRUE)
p.pov


# create new transformation variables
water.educ.new <- water.educ %>%
  mutate(arcsin.female.school = asin(x = sqrt(female.in.school/100))) %>%
  mutate(arcsin.perc.basic.water = asin(x = sqrt(perc.basic2015water/100)))%>%
  mutate(folded.p.female.school = (female.in.school/100)^(1/p.female) -
           (1-female.in.school/100)^(1/p.female)) %>%
  mutate(folded.p.basic.water = (perc.basic2015water/100)^(1/p.water) - (1-perc.basic2015water/100)^(1/p.water)) %>%
  mutate(folded.p.perc.1dollar = (perc.1dollar/100)^(1/p.pov) - (1- perc.1dollar/100)^(1/p.pov))

water.educ.new %>%
  ggplot(aes(x = folded.p.perc.1dollar))+
  geom_histogram(fill = "purple", color = "white") +
  theme_minimal() +
  labs(x = "Power Transformed Percent in Poverty (<$1/day")+
  scale_fill_manual(values = "#7463AC", guide = FALSE)

#No, the transofrmed variable is not normally distributed

#8.10.3
# spearman correlation females in school and water access
spear.fem.water <- cor.test(x = water.educ$perc.basic2015water,
                            y = water.educ$female.in.school,
                            method = "spearman")
spear.fem.water

# compute the sample size
# drop rows with NA
water.educ.new %>%
  drop_na(perc.basic2015water) %>%
  drop_na(female.in.school) %>%
  summarize(n = n(),
            t.spear = spear.fem.water$estimate*sqrt((n()-2)/(1-spear.fem.water$estimate^2)))

# print the list from the Spearman's analysis
summary(object = spear.fem.water)

# access the statistic
spear.fem.water$statistic

#8.11.1
# create a data frame with only females in school
# poverty and water access
water.educ.small <- water.educ.new %>%
  select(female.in.school, perc.basic2015water, perc.1dollar) %>%
  drop_na()

# check the new data
summary(water.educ.small)

# examine the bivariate correlations
water.educ.small %>%
  summarize(corr.fem.water = cor(x = perc.basic2015water, y = female.in.school),
            corr.fem.pov = cor(x = perc.1dollar, y = female.in.school),
            corr.water.pov = cor(x = perc.basic2015water, y =
                                   perc.1dollar))

# conduct partial Pearson correlation
educ.water.poverty <- ppcor::pcor(x = water.educ.small, method = "pearson")
educ.water.poverty

# conduct partial correlation with Spearman
educ.water.poverty.spear <- ppcor::pcor(x = water.educ.small, method =
                                          "spearman")
educ.water.poverty.spear

#8.11.4
# check monotonic of plot of females in school and poverty (Figure 8.30)
water.educ.small %>%
  ggplot(aes(y = female.in.school/100, x = perc.1dollar/100)) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  geom_point(aes(size = "Country"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs(y = "Percent of school-aged females in school",
       x = "Percent living on < $1 per day") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name="", values=c("gray60", "deeppink")) +
  scale_size_manual(name="", values = 2)

# check monotonic assumption for water access and poverty (Figure 8.31)
water.educ.small %>%
  ggplot(aes(x = perc.1dollar/100, y = perc.basic2015water/100)) +
  geom_smooth(aes(color = "Linear fit line"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess curve"), se = FALSE) +
  geom_point(aes(size = "Country"), color = "#7463AC", alpha = .6) +
  theme_minimal() +
  labs( x = "Percent living on < $1 per day",
        y = "Percent with basic water access") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = "", values = c("gray60", "deeppink")) +
  scale_size_manual(name = "", values = 2)
