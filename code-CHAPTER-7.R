#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 17, 2019                                                    #
# Purpose: Review Code for Chapter 7 of Dr. Harris' Book                        #
#          Analysis of Variance                                                 #
#         The R Team and the Technical Difficulties Problem                     #
# Packages Used: tidyverse, car, dunn.test                                      #
# Data Used: General Social Survey 2018                                         #
# Last Update: Nov 17, 2019                                                     #
#################################################################################

setwd("C:/Users/svana/OneDrive/Documents/DrHarris/data")

# load GSS rda file
load(file = "gss2018.rda")

# assign GSS to gss.2018
gss.2018 <- GSS

# remove GSS
rm(GSS)

#7.4.1

# examine the variables
summary(object = gss.2018)

# recode USETECH to valid range
library(package = "tidyverse")
gss.2018.cleaned <- gss.2018 %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999))

# check recoding
summary(object = gss.2018.cleaned$USETECH)

# recode USETECH and AGE to valid ranges
gss.2018.cleaned <- gss.2018 %>%
  select(HAPPY, SEX, DEGREE, USETECH, AGE) %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(AGE = na_if(x = AGE, y = 98)) %>%
  mutate(AGE = na_if(x = AGE, y = 99))

# check recoding
summary(object = gss.2018.cleaned)

#recode variables of interest to valid ranges
gss.2018.cleaned <- gss.2018 %>%
  select(HAPPY, SEX, DEGREE, USETECH, AGE) %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(AGE = na_if(x = AGE, y = 98)) %>%
  mutate(AGE = na_if(x = AGE, y = 99)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 8)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 8)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 0))

# check recoding
summary(object = gss.2018.cleaned)

# recode variables of interest to valid ranges
gss.2018.cleaned <- gss.2018 %>%
  select(HAPPY, SEX, DEGREE, USETECH, AGE) %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(AGE = na_if(x = AGE, y = 98)) %>%
  mutate(AGE = na_if(x = AGE, y = 99)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 8)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 8)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 0)) %>%
  mutate(SEX = factor(x = SEX, labels = c("male","female"))) %>%
  mutate(DEGREE = factor(x = DEGREE, labels = c("< high school",
                                                "high school", "junior college",
                                                "bachelor", "graduate"))) %>%
  mutate(HAPPY = factor(x = HAPPY, labels = c("very happy",
                                              "pretty happy",
                                              "not too happy")))
# check recoding
summary(object = gss.2018.cleaned)

#7.4.2

# mean and sd of age by group
use.stats <- gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(DEGREE) %>%
  summarize(m.techuse = mean(x = USETECH),
            sd.techuse = sd(x = USETECH))
use.stats

# graph usetech (Figure 7.4)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  scale_color_brewer(palette = "Spectral", guide = FALSE) +
  theme_minimal() +
  labs(x = "Highest educational attainment", y = "Percent of time spent using technology")

#7.4.3 ACHIEVEMENT 1: CHECK YOUR UNDERSTANDING
#The upper and lower parts of each box in the boxplot represent the 75th and 25th percentile,
# respectively, for each group. The middle line represents the median, and tails represent
# the maximum and minimum values

#7.5.1
# mean tech use percent by degree groups
techuse.by.deg <- oneway.test(formula = USETECH ~ DEGREE,
                              data = gss.2018.cleaned,
                              var.equal = TRUE)
techuse.by.deg

#7.5.1.1
# print the results of the ANOVA
techuse.by.deg

#7.5.2 ACHIEVEMENT 2: CHECK YOUR UNDERSTANDING
#analysis of variance is useful for comparing means of three or more groups

#7.6.1.1
# find differences in mean tech use by degree groups
bonf.tech.by.deg <- pairwise.t.test(x = gss.2018.cleaned$USETECH,
                                    g = gss.2018.cleaned$DEGREE,
                                    p.adj = "bonf")
bonf.tech.by.deg

# mean age by groups
use.stats

#7.6.1.2
# Tukey's post hoc test for tech.by.deg
tukey.tech.by.deg <- TukeyHSD(x = aov(formula = USETECH ~ DEGREE,
                                      data = gss.2018.cleaned))
tukey.tech.by.deg

#Box 7.2
# run the ANOVA and get a new object
anova.for.Tukey <- aov(formula = USETECH ~ DEGREE,
                       data = gss.2018.cleaned)

# use the newly created ANOVA object in TukeyHSD
tukey.tech.by.deg <- TukeyHSD(x = anova.for.Tukey)
tukey.tech.by.deg

#7.6.2
# put the contrast in a vector
contrast1 <- c(0, 3, -1, -1, -1)

# link the contrast to the categorical variable using contrasts()
contrasts(x = gss.2018.cleaned$DEGREE) <- contrast1

# view the structure of the DEGREE variable with contrast
str(object = gss.2018.cleaned$DEGREE)

# re-run the model using aov()
techuse.by.deg.aov <- aov(formula = USETECH ~ DEGREE,
                          data = gss.2018.cleaned)

# apply the contrasts to the anova object techuse.by.deg.aov
# give the contrast a good descriptive name of "high school vs. all college"
tech.by.deg.contr <- summary.aov(object = techuse.by.deg.aov, 
                                 split = list(DEGREE = list("high school vs. all college" = 1)))
tech.by.deg.contr

# recode and compute the means for high school and college groups
gss.2018.cleaned %>%
  mutate(DEGREE = factor(x = DEGREE,
                         labels = c("< high school",
                                  "high school", "all college", "all college", "all college"),
                         levels = c("< high school",
                                    "high school", "all college", "all college", "all college")
                        )) %>%
  group_by(DEGREE) %>%
  summarize(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))

#Sarah Added
###############################################################
gss.2018.cleaned %>%
  #use base R version of ifelse because dplyr won't allow NA assignment
  #recode degree into a different degree factor where 3 old levels of college are 'all college'
  mutate(DEGREE = if_else(DEGREE == "< high school", "< high school",
                         ifelse(DEGREE == "high school", "high school",
                                ifelse(DEGREE %in% c("junior college",
                                                     "bachelor",
                                                     "graduate"),
                                       "all college", NA)))) %>%
  #to achieve ordinal leveling, recast degree back into a factor and specify desired order
  mutate(DEGREE = factor(x = DEGREE,
                        labels = c("< high school",
                                   "high school",
                                   "all college"),
                        levels = c("< high school",
                                   "high school",
                                   "all college"),
                        ordered = TRUE)) %>%
  group_by(DEGREE) %>%
  summarize(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))
#####################################################################
# add filter and ggplot (Figure 7.8)
gss.2018.cleaned %>%
  mutate(DEGREE = factor(x = DEGREE, labels = c("< high school",
                                                "high school", "all college",
                                                "all college", "all college"))) %>%
  filter(DEGREE == "high school" | DEGREE == "all college") %>%
  ggplot(aes(y = USETECH, x = DEGREE, fill = DEGREE, color = DEGREE)) +
  geom_boxplot(alpha = .4) +
  geom_jitter(alpha = .6) +
  scale_fill_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology")

#Sarah Added
################################################################
# add filter and ggplot (Figure 7.8)
gss.2018.cleaned %>%
  #use base R version of ifelse because dplyr won't allow NA assignment
  #recode degree into a different degree factor where 3 old levels of college are 'all college'
  mutate(DEGREE = if_else(DEGREE == "< high school", "< high school",
                          ifelse(DEGREE == "high school", "high school",
                                 ifelse(DEGREE %in% c("junior college",
                                                      "bachelor",
                                                      "graduate"),
                                        "all college", NA)))) %>%
  #to achieve ordinal leveling, recast degree back into a factor and specify desired order
  mutate(DEGREE = factor(x = DEGREE,
                         labels = c("< high school",
                                    "high school",
                                    "all college"),
                         levels = c("< high school",
                                    "high school",
                                    "all college"),
                         ordered = TRUE)) %>%
  filter(DEGREE == "high school" | DEGREE == "all college") %>%
  ggplot(aes(y = USETECH, x = DEGREE, fill = DEGREE, color = DEGREE)) +
  geom_boxplot(alpha = .4) +
  geom_jitter(alpha = .6) +
  scale_fill_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology")
#############################################################################
# less than HS v. all college contrast
contrast2 <- c(3, 0, -1, -1, -1)

# bind the two contrasts together into a matrix
cons <- cbind(contrast1, contrast2)

# connect the matrix with the factor variable
contrasts(x = gss.2018.cleaned$DEGREE) <- cons

# estimate the ANOVA with contrasts
tech.by.deg.contr <-
  summary.aov(object = techuse.by.deg.aov,
              split = list(DEGREE = 
                             list("high school vs. all college" = 1,
                            "<high school vs. all college" = 2)))
tech.by.deg.contr

# recode to group the college groups
gss.2018.cleaned %>%
  mutate(DEGREE = factor(x = DEGREE,
                         labels = c("< high school",
                                    "high school", "all college",
                                    "all college", "all college"))) %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  scale_fill_manual(values = c("gray70","#7463AC","dodgerblue2"),
                    guide = FALSE) +
  scale_color_manual(values = c("gray70","#7463AC","dodgerblue2"),
                     guide = FALSE) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology")

#Sarah Added
################################################################
# recode to group the college groups
gss.2018.cleaned %>%
  #use base R version of ifelse because dplyr won't allow NA assignment
  #recode degree into a different degree factor where 3 old levels of college are 'all college'
  mutate(DEGREE = if_else(DEGREE == "< high school", "< high school",
                          ifelse(DEGREE == "high school", "high school",
                                 ifelse(DEGREE %in% c("junior college",
                                                      "bachelor",
                                                      "graduate"),
                                        "all college", NA)))) %>%
  #to achieve ordinal leveling, recast degree back into a factor and specify desired order
  mutate(DEGREE = factor(x = DEGREE,
                         labels = c("< high school",
                                    "high school",
                                    "all college"),
                         levels = c("< high school",
                                    "high school",
                                    "all college"),
                         ordered = TRUE)) %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  scale_fill_manual(values = c("gray70","#7463AC","dodgerblue2"),
                    guide = FALSE) +
  scale_color_manual(values = c("gray70","#7463AC","dodgerblue2"),
                     guide = FALSE) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology")

# contrasts for ANOVA of tech time by degree
c1 <- c(2, -1, -1, 0, 0)
c2 <- c(0, 3, -1, -1, -1)
c3 <- c(0, 0, 2, -1, -1)
c4 <- c(0, 0, 0, -1, 1)

# bind the contrasts into a matrix
conts <- cbind(c1, c2, c3, c4)
conts

#connect the matrix with the factor variable
contrasts(x = gss.2018.cleaned$DEGREE) <- conts

# estimate the ANOVA with 4 independent contrasts
tech.by.deg.contr <-
  summary.aov(object = techuse.by.deg.aov,
              split =
                list(DEGREE =
                       list("< high school vs. high school & jr college" = 1,
                            "high school vs. all college" = 2,
                            "jr college vs. bach or grad degree" = 3,
                            "bachelor's vs. graduate degree" = 4)))
tech.by.deg.contr

# adjust p-values for multiple comparisons
adj.p.vals <- p.adjust(p = tech.by.deg.contr[[1]]$`Pr(>F)`, method =
                         "bonferroni")
adj.p.vals

#contrast 1 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = factor(DEGREE, labels = c("< high school",
                                            "high school & jr coll", "high school & jr coll",
                                            NA, NA))) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))

#Sarah added
##################################################
# contrast 1 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = ifelse(DEGREE == "< high school", "< high school",
                         ifelse(DEGREE %in% c("high school", "junior college"),"high school & jr. college",
                                NA))) %>%
  mutate(DEGREE = factor(DEGREE, levels = c("< high school",
                                            "high school & jr. college"),
                         ordered = T)) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))
###########################################################
# contrast 2 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = factor(DEGREE, labels = c(NA,
                                            "high school", "all college",
                                            "all college", "all college"))) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))

#Sarah added
###########################################################
# contrast 2 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = ifelse(DEGREE == "< high school", NA,
                         ifelse(DEGREE == "high school", "high school",
                                "all college"))) %>%
  mutate(DEGREE = factor(DEGREE, levels = c("high school",
                                            "all college"),
                         ordered = T)) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))
#################################################################
# contrast 3 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = factor(DEGREE, labels = c(NA,
                                            NA, "jr college",
                                            "bach or grad degree", "bach or grad degree"))) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))

#Sarah added
############################################################
gss.2018.cleaned %>%
  mutate(DEGREE = ifelse(DEGREE %in% c("< high school", "high school"), NA,
                         ifelse(DEGREE == "junior college", "jr college",
                                ifelse(DEGREE %in% c("bachelor", "graduate"),
                                       "bach or grad degree", NA)))) %>%
  mutate(DEGREE = factor(DEGREE, levels = c("jr college", "bach or grad degree", ordered = T))) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))
###################################################################
# contrast 4 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = factor(DEGREE, labels = c(NA,
                                            NA, NA,
                                            "bachelor", "graduate"))) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))


#Sarah Added
#################################################
# contrast 4 statistics
gss.2018.cleaned %>%
  mutate(DEGREE = ifelse(DEGREE == "bachelor", "bachelor",
                         ifelse(DEGREE == "graduate", "graduate", NA))) %>%
  mutate(DEGREE = factor(DEGREE, levels = c("bachelor", "graduate", ordered = T))) %>%
  group_by(DEGREE) %>%
  summarise(m.techuse = mean(x = USETECH, na.rm = T),
            sd.techuse = sd(x = USETECH, na.rm = T))
#################################################

#7.6.3 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
#A planned contrast or post hoc test is useful after a significant ANOVA because
#Post hoc and planned comparison tests help identify which groups' means are different from
#each other.

#7.7
# ANOVA model from earlier
summary(object = techuse.by.deg.aov)

#Box 7.3
#create a summary object
summ.tech.anova <- summary(object = techuse.by.deg.aov)

# access first item in the summ.tech.anova list
summ.tech.anova[[1]]

# determine the class of the first element in the summ.tech.anova list
class(x = summ.tech.anova[[1]])

# access the entry in the first row and 4th column
# of the first item in the summ.tech.anova list
summ.tech.anova[[1]][1, 4]

# compute omega using R code
k.om <- summ.tech.anova[[1]][1, 1] + 1
n.om <- summ.tech.anova[[1]][2, 1] + summ.tech.anova[[1]][1, 1] + 1
omega.sq <- (summ.tech.anova[[1]][1, 4])/(summ.tech.anova[[1]][1, 4]
                                          + (n.om - k.om + 1)/(k.om - 1))
omega.sq

#7.7.1 ACHIEVEMENT 4: CHECK YOUR UNDERSTANDING
#The difference between the statistical significance of a relationship and the effect size of 
#a relationship is that statistical significance indicates how likely (the probability) that the
# you would get the result you got if the null hypothesis were true. The effect size is the
#magnitude of the relationship, and is often more meaningful for clinical or practical significance.

#7.8.1
# graph tech use by degree (Figure 7.12)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(x = USETECH)) +
  geom_density(aes(fill = DEGREE)) +
  facet_wrap(facets = vars(DEGREE), nrow = 2) +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  theme_minimal() +
  labs(x = "Percent of time using tech",
       y = "Probability density")

# graph tech use by degree (Figure 7.13)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(sample = USETECH)) +
  geom_abline(aes(intercept = mean(USETECH), slope = sd(USETECH), linetype =
                    "Normally distributed"),
              color = "gray60", size = 1) +
  stat_qq(aes(color = DEGREE)) +
  scale_color_brewer(palette = "Spectral", guide = FALSE) +
  scale_linetype_manual(values = 1, name = "") +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent time using tech") +
  theme_minimal() +
  facet_wrap(facets = vars(DEGREE), nrow = 2)

# statistical test of normality for groups
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(DEGREE) %>%
  summarize(shapiro.pval = shapiro.test(x = USETECH)$p.value)


#7.8.2
# equal variances for systolic by sex
car::leveneTest(y = USETECH ~ DEGREE, data = gss.2018.cleaned)

#7.8.4
# add new variable to data management
gss.2018.cleaned <- gss.2018 %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(AGE = na_if(x = AGE, y = 98)) %>%
  mutate(AGE = na_if(x = AGE, y = 99)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 8)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 8)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 0)) %>%
  mutate(SEX = factor(SEX, labels = c("male","female"))) %>%
  mutate(DEGREE = factor(x = DEGREE, labels = c("< high school",
                                                "high school", "junior college",
                                                "bachelor", "graduate"))) %>%
  mutate(HAPPY = factor(x = HAPPY, labels = c("very happy",
                                              "pretty happy",
                                              "not too happy"))) %>%
  mutate(usetech.tran = abs(USETECH - median(USETECH, na.rm = TRUE)))

# check new variable
summary(object = gss.2018.cleaned$usetech.tran)

# brown-forsythe anova
usetech.t.by.degree <- oneway.test(formula = usetech.tran ~ DEGREE,
                                   data = gss.2018.cleaned)
usetech.t.by.degree

# means of transformed variable
usetech.t.stats <- gss.2018.cleaned %>%
  drop_na(usetech.tran) %>%
  group_by(DEGREE) %>%
  summarise(m.usetech.tran = mean(x = usetech.tran),
            sd.usetech.tran = sd(x = usetech.tran))
usetech.t.stats

# graph transformed USETECH variable (Figure 7.15)
gss.2018.cleaned %>%
  drop_na(usetech.tran) %>%
  ggplot(aes(y = usetech.tran, x = DEGREE)) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  scale_color_brewer(palette = "Spectral", guide = FALSE) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Distance to median of tech use time for group")

#7.9.1.6
# welch test for unequal variances
welch.usetech.by.degree <- oneway.test(formula = USETECH ~ DEGREE,
                                       data = gss.2018.cleaned,
                                       var.equal = FALSE)
welch.usetech.by.degree

#7.9.2
#compare usetech by degree
kw.usetech.by.degree <- kruskal.test(formula = USETECH ~ DEGREE,
                                     data = gss.2018.cleaned)
kw.usetech.by.degree

# post hoc test for usetech by degree
dunn.usetech.by.degree <- dunn.test::dunn.test(x =gss.2018.cleaned$USETECH,
                                               g = gss.2018.cleaned$DEGREE,
                                               method = "bonferroni")

# add new variable to data management
gss.2018.cleaned <- gss.2018 %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(AGE = na_if(x = AGE, y = 98)) %>%
  mutate(AGE = na_if(x = AGE, y = 99)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 8)) %>%
  mutate(DEGREE = na_if(x = DEGREE, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 8)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 9)) %>%
  mutate(HAPPY = na_if(x = HAPPY, y = 0)) %>%
  mutate(SEX = factor(SEX, labels = c("male","female"))) %>%
  mutate(DEGREE = factor(x = DEGREE, labels = c("< high school",
                                                "high school", "junior college",
                                                "bachelor", "graduate"))) %>%
  mutate(HAPPY = factor(x = HAPPY, labels = c("very happy",
                                              "pretty happy",
                                              "not too happy"))) %>%
  mutate(usetech.t = abs(x = USETECH - median(USETECH, na.rm = TRUE))) %>%
  mutate(usetech.rank = rank(x = USETECH, na.last = "keep"))

# check new variable
summary(object = gss.2018.cleaned$usetech.rank)

# graph the ranks (Figure 7.17)
gss.2018.cleaned %>%
  ggplot(aes(y = usetech.rank, x = DEGREE)) +
  geom_jitter(aes(color = DEGREE), alpha = .6) +
  geom_boxplot(aes(fill = DEGREE), alpha = .4) +
  scale_fill_brewer(palette = "Spectral", guide = FALSE) +
  scale_color_brewer(palette = "Spectral", guide = FALSE) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Ranks of technology use time")

#7.9.3 ACHIEVEMENT 6: CHECK YOUR UNDERSTANDING
#Brown- Forsythe: Used when equality of variance not met
#Welch's Correction: Used when equality of variance not met
#Kruskal- Wallis: Used when normality within groups is not met

#7.10.1
# graph usetech by sex (Figure 7.18)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = SEX)) +
  geom_jitter(aes(color = SEX), alpha = .4) +
  geom_boxplot(aes(fill = SEX), alpha = .6) +
  scale_fill_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  scale_color_manual(values = c("gray70", "#7463AC"), guide = FALSE) +
  theme_minimal() +
  labs(x = "Sex", y = "Percent of time spent using technology")

# graph usetech by degree and sex (Figure 7.19)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = DEGREE)) +
  geom_boxplot(aes(fill = SEX), alpha = .4) +
  scale_fill_manual(values = c("gray70", "#7463AC")) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology")

# means plots graph (Figure 7.20)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = DEGREE, color = SEX)) +
  stat_summary(fun.y = mean, geom="point", size = 3) +
  stat_summary(fun.y = mean, geom="line", aes(group = SEX), size = 1) +
  scale_color_manual(values = c("gray70", "#7463AC")) +
  theme_minimal() +
  labs(x = "Educational attainment", y = "Percent of time spent using technology") +
  ylim(0, 100)

# means by degree and sex
use.stats.2 <- gss.2018.cleaned %>%
  group_by(DEGREE, SEX) %>%
  drop_na(USETECH) %>%
  summarize(m.techuse = mean(USETECH),
            sd.techuse = sd(USETECH))
use.stats.2


#7.10.2
# two-way ANOVA technology use by degree and sex
techuse.by.deg.sex <- aov(formula = USETECH ~ DEGREE * SEX, data =
                            gss.2018.cleaned)
summary(techuse.by.deg.sex)

#Tukey's HSD post hoc test
TukeyHSD(x = techuse.by.deg.sex)

#7.10.4.1
# statistical test of normality for groups
shapiro.test(x = techuse.by.deg.sex$residuals)

# make a data frame
tech.deg.sex <- data.frame(techuse.by.deg.sex$residuals)
# plot the residuals (Figure 7.21)
tech.deg.sex %>%
  ggplot(aes(x = techuse.by.deg.sex.residuals)) +
  geom_histogram(fill = "#7463AC", col = "white") +
  theme_minimal() +
  labs(x = "Residuals", y = "Number of observations")

#7.10.4.2
# Levene test for ANOVA
car::leveneTest(y = USETECH ~ DEGREE*SEX, center = mean,
                data = gss.2018.cleaned)

# Friedman two-way ANOVA for ranked data
# R command requires summary data
agg.gss.2018 <- gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(DEGREE, SEX) %>%
  summarize(m.usetech = mean(x = USETECH))
agg.gss.2018

# Friedman test
tech.by.deg.sex.f <- friedman.test(formula = m.usetech ~ DEGREE | SEX,
                                   data = agg.gss.2018)
tech.by.deg.sex.f

# two-way ANOVA technology use by degree and sex
techuse.by.deg.sex.t <- aov(formula = usetech.rank ~ DEGREE * SEX,
                            data = gss.2018.cleaned)
summary(object = techuse.by.deg.sex.t)

#7.10.6 ACHIEVEMENT 7: CHECK YOUR UNDERSTANDING
#List ANOVA assumptions and one way to test each of them
#1) Independent observations: known through study design, no statistical test
#2) Normality of DV within groups (or normality of residuals), can be tested using
# Shapiro-Wilks test, and looked at visually with histograms or qqplots
#3) Equality of variance between groups: use Levene's test
#4) One continuous variable (outcome) amd one categorical variable with >2 categories (predictor):
# again, no statistical test, just based on study design and prior knowledge.
