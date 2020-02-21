#################################################################################
# Name: Sarah C. Van Alsten                                                     #
# Date Created: Nov 15, 2019                                                    #
# Purpose: Review Code for Chapter 3 of Dr. Harris' Book                        #
#          Data Visualization + The Tricky Trigger Problem                      #
# Packages Used: tidyverse, ggmosaic, waffle, gridExtra, readxl, ggrepel  ,     #
#                scales, httr, data.table, RNHANES                              #
# Data Used: NHANES 2011, FBI deaths 2016, Gun Publication Funds 2004-2015      #
# Last Update: Nov 22, 2019                                                     #
#################################################################################
#Box 3.1

# install and then load the readxl
# and httr packages
library(package = "readxl")
library(package = "httr")

#Example 3.1.1 Excel Data

# create a variable that contains the web
# URL for the data set
url1 <- "https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/tables/expanded-homicide-data-table-4.xls/output.xls"

# use the GET function in httr to put the URL
# in a temporary file and specify the .xls file extension
#temporary file is named tf in this example
GET(url = url1, write_disk(tf <- tempfile(fileext = ".xls")))

# use the read_excel function in readxl
# to get the temporary file tf
# skip the first 3 rows of the file because they
# are header rows and do not contain data
# specify the number of observations using n_max
fbi.deaths <- data.frame(read_excel(path = tf,
                                    sheet = 1,
                                    skip = 3,
                                    n_max = 18))

# option to save the file to a folder called "data"
write.csv(x = fbi.deaths, file = "[data folder location]/data/fbi_deaths_2016_ch3.csv",
          row.names = FALSE)

#3.1.2 NHANES Data

# open package
library(package = "RNHANES")

# download audiology data (AUQ_G)
# with demographics
nhanes.2012 <- nhanes_load_data(file_name = "AUQ_G", year = "2011-2012",
                                demographics = TRUE)

# option to save the data to a "data" folder
write.csv(x = nhanes.2012, file = "[data folder location]/data/nhanes_2011_2012_ch3.csv ",
          row.names = FALSE)

# examine gun use variable (AUQ300)
summary(object = nhanes.2012$AUQ300)

#3.4.2 Bar Charts

# import the data
nhanes.2012 <- read.csv(file = "[data folder location]/data/nhanes_2011_2012_ch3.csv")

# check the import
head(x = nhanes.2012)

# check the data
summary(object = nhanes.2012$AUQ300)

# open tidyverse
library(package = "tidyverse")
# recode gun use variable
nhanes.2012.clean <- nhanes.2012 %>%
  mutate(AUQ300 = recode_factor(.x = AUQ300,
                                `1` = 'Yes',
                                `2` = 'No',
                                `7` = 'Refused',
                                `9` = 'Don\'t know'))

# check the recoding
summary(object = nhanes.2012.clean$AUQ300)

# recode gun use variable
nhanes.2012.clean <- nhanes.2012 %>%
  mutate(AUQ300 = na_if(x = AUQ300, y = 7)) %>%
  mutate(AUQ300 = recode_factor(.x = AUQ300,
                                `1` = 'Yes',
                                `2` = 'No')) %>%
  rename(gun.use = AUQ300)

# check recoding
summary(object = nhanes.2012.clean$gun.use)

# plot gun use in US 2011-2012 (Figure 3.10)
nhanes.2012.clean %>%
  ggplot(aes(x = gun.use)) +
  geom_bar()

# omit NA category from gun.use plot and add axis labels (Figure 3.11)
nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use)) +
  geom_bar() +
  labs(x = "Gun use", y = "Number of participants")

#fill bars inside aes
fill.aes <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use)) +
  geom_bar(aes(fill = gun.use)) +
  labs(x = "Gun use", y = "Number of participants", subtitle = "Filled inside the aes()") +
  scale_fill_manual(values = c("#7463AC", "gray"), guide = FALSE) +
  theme_minimal()

# fill bars outside aes
fill.outside <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use)) +
  geom_bar(fill = "#7463AC") +
  labs(x = "Gun use", y = "Number of participants", subtitle = "Filled outside the aes()") +
  theme_minimal()

# arrange the two plots side by side (Figure 3.12)
gridExtra::grid.arrange(fill.aes, fill.outside, ncol = 2)


# fill inside aes for ggplot layer
fill.aes <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use, fill = gun.use)) +
  geom_bar() +
  labs(x = "Gun use", y = "Number of participants", subtitle = "Filled in ggplot layer") +
  scale_fill_manual(values = c("#7463AC", "gray"), guide = FALSE) +
  theme_minimal()

# fill inside aes for geom_bar layer
fill.outside <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use)) +
  geom_bar(aes(fill = gun.use)) +
  labs(x = "Gun use", y = "Number of participants", subtitle = "Filled in geom_bar layer") +
  scale_fill_manual(values = c("#7463AC", "gray"), guide = FALSE) +
  theme_minimal()

# arrange the two plots side by side (Figure 3.13)
gridExtra::grid.arrange(fill.aes, fill.outside, ncol = 2)

# formatted bar chart of gun use (Figure 3.15)
nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use,
             y = 100*(..count..)/sum(..count..))) +
  geom_bar(aes(fill = gun.use)) +
  labs(x = "Gun use", y = "Percent of participants") +
  scale_fill_manual(values = c("#7463AC", "gray"), guide = FALSE) +
  theme_minimal()

# formatted bar chart of gun use (Figure 3.16)
nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use,
             y = 100*(..count..)/sum(..count..))) +
  geom_bar(aes(fill = gun.use)) +
  labs(x = "Gun use", y = "Percent of participants") +
  scale_fill_manual(values = c("#7463AC", "gray"), guide = FALSE) +
  theme_minimal() +
  ylim(0, 100)


#3.4.3 Waffle Charts
# recode gun use variable
nhanes.2012.clean <- nhanes.2012 %>%
  mutate(AUQ300 = na_if(x = AUQ300, y = 7)) %>%
  mutate(AUQ300 = recode_factor(.x = AUQ300,
                                `1` = 'Yes',
                                `2` = 'No')) %>%
  rename(gun.use = AUQ300) %>%
  mutate(AUQ310 = recode_factor(.x = AUQ310,
                                `1` = "1 to less than 100",
                                `2` = "100 to less than 1000",
                                `3` = "1000 to less than 10k",
                                `4` = "10k to less than 50k",
                                `5` = "50k or more",
                                `7` = "Refused",
                                `9` = "Don't know")) %>%
  rename(fired = AUQ310)

#check recoding
summary(object = nhanes.2012.clean$fired)

#open the waffle library
library(package = "waffle")

# make a table of rounds fired data
rounds <- table(nhanes.2012.clean$fired)

# each square is 25 people (Figure 3.17)
# 5 rows of squares
waffle(parts = rounds / 25, rows = 5)

# change the colors (Figure 3.18)
waffle(parts = rounds / 25, rows = 5,
       colors = c("1 to less than 100" = "#7463AC",
                  "100 to less than 1000" = "gray60",
                  "1000 to less than 10k" = "gray90",
                  "10k to less than 50k" = "black",
                  "50k or more" = "gray40",
                  "Don't know" = "gray80"))

#Achievement 3.4.4 Check Your Understanding
#Create Bar chart for Gender (clean data first!)
nhanes.2012.clean <-
  nhanes.2012 %>%
  mutate(gender = ifelse(RIAGENDR == 2, "Female",
                         ifelse(RIAGENDR == 1, "Male", NA))) %>%
  select(gender) %>%
  drop_na() %>%
  ggplot(aes (x = gender)) + 
  geom_bar(aes(fill = gender)) +
  theme_minimal()+
  scale_fill_discrete(guide = F)


#Section 3.5.1
#bring in the data
research.funding <- read.csv(file = "[data folder location]/data/gun_publications_funds_2004_2015_ch3.csv")

# check out the data
summary(object = research.funding)

# make a histogram of funding (Figure 3.19)
histo.funding <- research.funding %>%
  ggplot(aes(x = Funding)) +
  geom_histogram()

histo.funding

# make a histogram of funding (Figure 3.20)
histo.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_histogram()

histo.funding

# make a histogram of funding (Figure 3.21)
# adjust the number of bins to 10
histo.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_histogram(bins = 10)

histo.funding

# make a histogram of funding (Figure 3.22)
# adjust the number of bins to 50
histo.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_histogram(bins = 50)

histo.funding

# make a histogram of funding (Figure 3.23)
# adjust the number of bins to 10
histo.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_histogram(bins = 10) +
  labs(x = "Research funding for 2004-2015 (billions of dollars)",
       y = "Number of causes") +
  theme_minimal()
histo.funding

# make a histogram of funding (Figure 3.24)
# formatting options
histo.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_histogram(bins = 10, fill = "white", color = "gray") +
  labs(x = "Research funding for 2004-2015 (billions of dollars)",
       y = "Number of causes") +
  theme_minimal()
histo.funding

# density plot of research funding (Figure 3.25)
dens.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_density() +
  labs(x = "Research funding for 2004-2015 (billions of dollars)",
       y = "Probability density") +
  theme_minimal()
dens.funding

# density plot of research funding (Figure 3.26)
# bw = .5
dens.funding <- research.funding %>%
ggplot(aes(x = Funding/1000000000)) +
  geom_density(bw = .5, fill = "#7463AC") +
  labs(x = "Research funding for 2004-2015 (billions of dollars)", y =
         "Probability density") +
  theme_minimal()
dens.funding

# density plot of research funding (Figure 3.27)
# bw = 1.5
dens.funding <- research.funding %>%
  ggplot(aes(x = Funding/1000000000)) +
  geom_density(bw = 1.5, fill = "#7463AC") +
  labs(x = "Research funding for 2004-2015 (billions of dollars)", y =
         "Probability density") +
  theme_minimal()
dens.funding

# boxplot of research funding (Figure 3.28)
box.funding <- research.funding %>%
  ggplot(aes(y = Funding/1000000000)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = "Research funding for 2004-2015 (billions of dollars)")
box.funding

# boxplot of research funding (Figure 3.29)
box.funding <- research.funding %>%
ggplot(aes(y = Funding/1000000000)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = "Research funding for 2004-2015 (billions of dollars)") +
  coord_flip()
box.funding

# plot all three options together (Figure 3.30)
gridExtra::grid.arrange(histo.funding,
                        dens.funding,
                        box.funding,
                        nrow = 3)

#Achievement 3.5.4- Check your Understanding
#boxplot of NHANES RIDAGEYR
nhanes.2012 %>%
  select(RIDAGEYR) %>%
  drop_na()%>%
  ggplot(aes(y = RIDAGEYR)) +
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Age (Yrs)")

#histogram
nhanes.2012 %>%
  select(RIDAGEYR) %>%
  drop_na()%>%
  ggplot(aes(x = RIDAGEYR)) +
  geom_histogram()+
  theme_minimal()+
  labs(x = "Age (Yrs)")

#density plot
nhanes.2012 %>%
  select(RIDAGEYR) %>%
  drop_na()%>%
  ggplot(aes(x = RIDAGEYR)) +
  geom_density(bw = 1.5, fill = "green")+
  theme_minimal()+
  labs(x = "Age (Yrs)")

#3.6.1
# check coding of RIAGENDR
table(nhanes.2012$RIAGENDR)

# recode sex variable
nhanes.2012.clean <- nhanes.2012 %>%
  mutate(AUQ300 = na_if(x = AUQ300, y = 7)) %>%
  mutate(AUQ300 = recode_factor(.x = AUQ300,
                                `1` = 'Yes',
                                `2` = 'No')) %>%
  rename(gun.use = AUQ300) %>%
  mutate(AUQ310 = recode_factor(.x = AUQ310,
                                `1` = "1 to less than 100",
                                `2` = "100 to less than 1000",
                                `3` = "1000 to less than 10k",
                                `4` = "10k to less than 50k",
                                `5` = "50k or more",
                                `7` = "Refused",
                                `9` = "Don't know")) %>%
  rename(fired = AUQ310) %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR,
                                  `1` = 'Male',
                                  `2` = 'Female')) %>%
  rename(sex = RIAGENDR)

#check recoding
summary(object = nhanes.2012.clean$sex)

# open library
library(package = "ggmosaic")

# mosaic plot of gun use by sex (Figure 3.31)
mosaic.gun.use.sex <- nhanes.2012.clean %>%
  mutate(gun.use, gun.use = na_if(x = gun.use, y = 7)) %>%
  ggplot() +
  geom_mosaic(aes(x = product(gun.use, sex), fill = gun.use))
mosaic.gun.use.sex

# formatted mosaic plot of sex and gun use (Figure 3.32)
# mosaic gun use by sex
mosaic.gun.use.sex <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot() +
  geom_mosaic(aes(x = product(gun.use, sex), fill = gun.use)) +
  labs(x = "Participant sex", y = "Ever used firearm") +
  scale_fill_manual(values=c("#7463AC", "gray80"),
                    guide = FALSE) +
  theme_minimal()
mosaic.gun.use.sex

#3.6.2

# stacked bar chart (Figure 3.33)
stack.gun.use.sex <- nhanes.2012.clean %>%
  ggplot(aes(x = sex, fill = gun.use)) +
  geom_bar()
stack.gun.use.sex

# formatted stacked bar chart (Figure 3.34)
stack.gun.use.sex <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = sex, fill = gun.use)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Participant sex", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    name = "Firearm use")
stack.gun.use.sex

# formatted grouped bar chart (Figure 3.35)
group.gun.use.sex <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = sex, fill = gun.use)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Participant sex", y = "Number of participants") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    name = "Firearm use")
group.gun.use.sex


# formatted grouped bar chart with percents (Figure 3.36)
group.gun.use.sex <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = sex, fill = gun.use,
             y = 100*(..count..)/sum(..count..))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Participant sex", y = "Percent of total participants") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    name = "Firearm use")
group.gun.use.sex

# formatted grouped bar chart with percents (Figure 3.37)
group.gun.use.sex <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  group_by(gun.use, sex) %>% # make groups of gun.use by sex
  count() %>% # count how many are in each group
  group_by(sex) %>% # pick the variable that will add to 100%
  mutate(percent = 100*(n/sum(n))) %>% # compute percents within chosen variable
ggplot(aes(x = sex, fill = gun.use,
           y = percent)) + # use new values from mutate
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = "Participant sex",
       y = "Percent in sex group") +
  scale_fill_manual(values = c("#7463AC",
                               "gray80"),
                    name = "Firearm use")
group.gun.use.sex

#plot all three options together (Figure 3.39)
gridExtra::grid.arrange(mosaic.gun.use.sex,
                        stack.gun.use.sex,
                        group.gun.use.sex,
                        nrow = 2)

#3.6.3
#3.6.3.1.
# bar chart with means for bar height (Figure 3.40)
bar.gun.use.age <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use, y = RIDAGEYR)) +
  geom_bar(aes(fill = gun.use), stat = "summary", fun.y = mean) +
  theme_minimal() +
  labs(x = "Firearm use", y = "Mean age of participants") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    guide = FALSE)
bar.gun.use.age

# density plots of age by firearm use category (Figure 3.41)
dens.gun.use.age <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = RIDAGEYR)) +
  geom_density(aes(fill = gun.use), alpha = .8) +
  theme_minimal() +
  labs(x = "Age of participants", y = "Probability density") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    name = "Used firearm")
dens.gun.use.age

# bar chart with median for bar height (Figure 3.43)
bar.gun.use.age.md <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  ggplot(aes(x = gun.use, y = RIDAGEYR)) +
  geom_bar(aes(fill = gun.use), stat = "summary", fun.y = median) +
  theme_minimal() +
  labs(x = "Firearm use", y = "Median age of participants") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    guide = FALSE)
bar.gun.use.age.md

# bar chart with median for bar height and error bars (Figure 3.44)
gun.use.age.md.err <- nhanes.2012.clean %>%
  drop_na(gun.use) %>%
  group_by(gun.use) %>% # specify grouping variable
  summarize(central = median(RIDAGEYR), # median, iqr by group
            iqr.low = quantile(x = RIDAGEYR, probs = .25),
            iqr.high = quantile(x = RIDAGEYR, probs = .75) ) %>%
  ggplot(aes(x = gun.use, y = central)) + # use central tend for y-axis
  geom_col(aes(fill = gun.use)) +
  geom_errorbar(aes(ymin = iqr.low, # lower bound of error bar
                    ymax = iqr.high, # upper bound of error bar
                    linetype = "IQR"),
                width = .2) + # width of error bar
  theme_minimal() +
  labs(x = "Firearm use", y = "Median age of participants") +
  scale_fill_manual(values = c("#7463AC", "gray80"),
                    guide = FALSE) +
  scale_linetype_manual(values = 1, name = "Error bars")
gun.use.age.md.err

#import FBI data
fbi.deaths <- read.csv(file = "[data folder location]/data/fbi_deaths_2016_ch3.csv")
# review the data
summary(object = fbi.deaths)

# make a long data frame
# make a long data frame
fbi.deaths.cleaned <- fbi.deaths %>%
  slice(3:7) %>% # selects rows 3 to 7
  gather(key = year, value = number, X2012,
         X2013, X2014, X2015, X2016) %>% # turn columns into rows
  mutate(year, year = substr(x = year, start = 2, stop = 5)) %>% # remove X from front of year entries
rename(weapons = Weapons)

#3.6.3.2
# plot number of homicides by gun type (Figure 3.45)
bar.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = weapons, y = number)) +
  geom_bar(stat = "summary", fun.y = mean) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides committed")
bar.homicide.gun

# flip the coordinates for better reading (Figure 3.46)
# add color and remove unnecessary legend
bar.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = weapons, y = number)) +
  geom_bar(aes(fill = weapons), stat = "summary", fun.y = mean) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides committed") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", guide = FALSE)
bar.homicide.gun


# highlight handguns using color (Figure 3.47)
bar.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number), y = number)) +
  geom_bar(aes(fill = weapons), stat = "summary", fun.y = mean) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_fill_manual(values = c("Handguns" = "#7463AC",
                               "Firearms, type not stated" = "gray",
                               "Rifles" = "gray",
                               "Shotguns" = "gray",
                               "Other guns" = "gray"), guide=FALSE)
bar.homicide.gun

# gun deaths by gun type (Figure 3.48)
# highlight handguns using color
point.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number), y = number)) +
  geom_point(aes(fill = weapons), stat = "summary", fun.y = mean) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_fill_manual(values = c("Handguns" = "#7463AC",
                               "Firearms, type not stated" = "gray",
                               "Rifles" = "gray",
                               "Shotguns" = "gray",
                               "Other guns" = "gray"), guide=FALSE)
point.homicide.gun

# change fill to color add size to geom_point (Figure 3.49)
point.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number), y = number)) +
  geom_point( aes(color = weapons, size = "Mean"),
              stat = "summary", fun.y = mean) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_color_manual(values = c("Handguns" = "#7463AC",
                                "Firearms, type not stated" = "gray",
                                "Rifles" = "gray",
                                "Shotguns" = "gray",
                                "Other guns" = "gray"), guide = FALSE) +
  scale_size_manual(values = 4, name = "", guide = FALSE)
point.homicide.gun


# add error bars (Figure 3.50)
point.homicide.gun <- fbi.deaths.cleaned %>%
  group_by(weapons) %>%
  summarize(central = mean(x = number),
            spread = sd(x = number)) %>%
  ggplot(aes(x = reorder(x = weapons, X = -central),
             y = central)) +
  geom_errorbar(aes(ymin = central - spread,
                    ymax = central + spread,
                    linetype = "Mean\n+/- sd"),
                width = .2) +
  geom_point(aes(color = weapons, size = "Mean"), stat = "identity") +
  theme_minimal() +
  labs(x = "Firearm type",
       y = "Number of homicides") +
  coord_flip() +
  scale_color_manual(values = c("Handguns" = "#7463AC",
                                "Firearms, type not stated" = "gray",
                                "Rifles" = "gray",
                                "Shotguns" = "gray",
                                "Other guns" = "gray"), guide=FALSE) +
  scale_linetype_manual(values = 1, name = "") +
  scale_size_manual(values = 4, name = "") +
  theme(legend.position = "top")
point.homicide.gun

# change to boxplot (Figure 3.51)
box.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number),
             y = number)) +
  geom_boxplot(aes(color = weapons)) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_color_manual(values = c("Handguns" = "#7463AC",
                                "Firearms, type not stated" = "gray",
                                "Rifles" = "gray",
                                "Shotguns" = "gray",
                                "Other guns" = "gray"), guide=FALSE)
box.homicide.gun

# fix color for boxplots (Figure 3.52)
box.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number),
             y = number)) +
  geom_boxplot(aes(fill = weapons)) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_fill_manual(values = c("Handguns" = "#7463AC",
                               "Firearms, type not stated" = "gray",
                               "Rifles" = "gray",
                               "Shotguns" = "gray",
                               "Other guns" = "gray"), guide=FALSE)
box.homicide.gun

# Add points to boxplots (Figure 3.53)
box.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number),
             y = number)) +
  geom_boxplot(aes(fill = weapons), alpha = .8) +
  geom_jitter() +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_fill_manual(values = c("Handguns" = "#7463AC",
                               "Firearms, type not stated" = "gray",
                               "Rifles" = "gray",
                               "Shotguns" = "gray",
                               "Other guns" = "gray"), guide=FALSE)
box.homicide.gun


# violin plot (Figure 3.54)
violin.homicide.gun <- fbi.deaths.cleaned %>%
  ggplot(aes(x = reorder(x = weapons, X = -number),
             y = number)) +
  geom_violin(aes(fill = weapons)) +
  theme_minimal() +
  labs(x = "Firearm type", y = "Number of homicides") +
  coord_flip() +
  scale_fill_manual(values = c('gray', "#7463AC", 'gray',
                               'gray', 'gray'), guide=FALSE)
violin.homicide.gun

# violin plot of age by sex for NHANES (Figure 3.55)
nhanes.2012.clean %>%
  ggplot(aes(x = sex, y = RIDAGEYR)) +
  geom_violin(aes(fill = sex)) +
  scale_fill_manual(values = c("gray", "#7463AC"), guide = FALSE) +
  labs(y = "Age in years", x = "Sex") +
  theme_minimal()

# plot all three options together (Figure 3.56)
gridExtra::grid.arrange(bar.homicide.gun,
                        point.homicide.gun,
                        box.homicide.gun,
                        ncol = 2)

# bring in the data
guns.manu <- read.csv(file = "[data_folder_location]/data/total_firearms_manufactured_US_1990to2015.csv")
summary(object = guns.manu)

# make wide data long
guns.manu.cleaned <- guns.manu %>%
  gather(key = gun.type, value = num.guns, Pistols,
         Revolvers, Rifles, Shotguns, Total.firearms) %>%
  mutate(gun.type = as.factor(gun.type))

# check the data
summary(object = guns.manu.cleaned)

# plot it (Figure 3.57)
line.gun.manu <- guns.manu.cleaned %>%
  ggplot(aes(x = Year, y = num.guns)) +
  geom_line(aes(linetype = gun.type))
line.gun.manu

# update the y-axis, theme, line color, labels (Figure 3.58)
line.gun.manu <- guns.manu.cleaned %>%
  ggplot(aes(x = Year, y = num.guns/100000)) +
  geom_line(aes(color = gun.type)) +
  theme_minimal() +
  labs(y = "Number of firearms (in 100,000s)") +
  scale_color_brewer(palette = "Set2", name = "Firearm type")
line.gun.manu

#make a handguns category that is pistols + revolvers
# remove pistols and revolvers from graph
guns.manu.cleaned <- guns.manu %>%
  mutate(Handguns = Pistols + Revolvers) %>%
  gather(key = gun.type, value = num.guns, Pistols, Revolvers,
         Rifles, Shotguns, Total.firearms, Handguns) %>%
  mutate(gun.type, gun.type = as.factor(gun.type)) %>%
  filter(gun.type != "Pistols" & gun.type != "Revolvers")

# update the line graph with new data and thicker lines (Figure 3.59)
line.gun.manu <- guns.manu.cleaned %>%
  ggplot(aes(x = Year, y = num.guns/100000)) +
  geom_line(aes(color = gun.type), size = 1) +
  theme_minimal() +
  labs(y = "Number of firearms (in 100,000s)") +
  scale_color_brewer(palette = "Set2", name = "Firearm type")
line.gun.manu

#3.6.4.2 Scatterplots
#use scatterplot instead of line (Figure 3.60)
scatter.gun.manu <- guns.manu.cleaned %>%
  ggplot(aes(x = Year, y = num.guns/100000)) +
  geom_point(aes(color = gun.type)) +
  theme_minimal() +
  labs(y = "Number of firearms (in 100,000s)") +
  scale_color_brewer(palette = "Set2", name = "Firearm type")
scatter.gun.manu

# scatterplot of gun research by funding (Figure 3.61)
scatter.gun.funding <- research.funding %>%
  ggplot(aes(x = Mortality.Rate.per.100.000.Population, y = Funding))+
  geom_point()
scatter.gun.funding

# Line graph of gun research by funding (Figure 3.62)
line.gun.funding <- research.funding %>%
  ggplot(aes(x = Mortality.Rate.per.100.000.Population, y = Funding))+
  geom_line()
line.gun.funding

#scatterplot of gun research by funding (Figure 3.64)
# with axes showing a natural log scale
scatter.gun.funding <- research.funding %>%
  ggplot(aes(x = Mortality.Rate.per.100.000.Population, y =
               Funding/1000000000))+
  geom_point() +
  stat_smooth() +
  scale_x_log10() +
  scale_y_log10() +
  labs(y = "Funding, $US billion", x = "Mortality rate, per 100 000
population")
scatter.gun.funding

#scatterplot of gun research by funding (Figure 3.65)
#with axes showing a natural log scale
scatter.gun.funding <- research.funding %>%
  ggplot(aes(x = Mortality.Rate.per.100.000.Population, y =
               Funding/1000000000))+
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  labs(y = "Funding, $US billion", x = "Mortality rate, per 100 000 population") +
  theme_minimal()
scatter.gun.funding

# fancy graph (Figure 3.66)
scatter.gun.funding.lab <- research.funding %>%
  ggplot(aes(x = Mortality.Rate.per.100.000.Population, y =
               Funding/1000000000))+
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  labs(y = "Funding, $US billion", x = "Mortality rate, per 100 000
population") +
  theme_minimal() +
  geom_text(aes(label = Cause.of.Death))
scatter.gun.funding.lab

library(scales)
# fancy graph with better labels (Figure 3.67)
scatter.gun.funding.lab <- research.funding %>%
  ggplot(aes(x = Mortality.Rate.per.100.000.Population,
             y = Funding/1000000000)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10(breaks = c(1,10,100), labels = comma) +
  scale_y_log10(breaks = c(1,10,100), labels = comma) +
  labs(y = "Funding, $US billion", x = "Mortality rate, per 100 000 population") +
  theme_minimal() +
  ggrepel::geom_text_repel(aes(label = Cause.of.Death), size = 3.5)
scatter.gun.funding.lab


# show graph types (Figure 3.68)
gridExtra::grid.arrange(line.gun.manu,
                        scatter.gun.funding,
                        nrow = 2)

#3.6.5 ACHIEVEMENT 3: CHECK YOUR UNDERSTANDING
#Show relationship between marital status and sex in NHANES
#Explain why you choose the graph type
nhanes.2012 %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR, 
                                  `1` = 'Male', 
                                  `2` = 'Female')) %>%
  mutate(DMDMARTL = recode_factor(.x = DMDMARTL, 
                                  `1` = 'Married', 
                                  `2` = 'Widowed',
                                  `3` = 'Divorced',
                                  `4` = 'Separated',
                                  `5` = 'Never Married',
                                  `6` = 'Living With Partner',
                                  `77` = NA_character_,
                                  `99` = NA_character_)) %>%
  select(RIAGENDR, DMDMARTL)%>%
  drop_na() %>%
  ggplot()+
  geom_bar(aes(x = DMDMARTL, group = RIAGENDR, fill = RIAGENDR),
           position = "dodge")+
  theme_minimal()+
  coord_flip()+
  labs(x = "Marital Status", y = "Count")+
  scale_fill_manual(name = "Sex", values = c("green", "lightblue"))

#Alternatively, to show percent:
nhanes.2012 %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR, 
                                  `1` = 'Male', 
                                  `2` = 'Female')) %>%
  mutate(DMDMARTL = recode_factor(.x = DMDMARTL, 
                                  `1` = 'Married', 
                                  `2` = 'Widowed',
                                  `3` = 'Divorced',
                                  `4` = 'Separated',
                                  `5` = 'Never Married',
                                  `6` = 'Living With Partner',
                                  `77` = NA_character_,
                                  `99` = NA_character_)) %>%
  select(RIAGENDR, DMDMARTL)%>%
  drop_na() %>%
  group_by(DMDMARTL, RIAGENDR) %>% # make groups of marital status by sex
  count() %>% # count how many are in each group
  group_by(DMDMARTL) %>% # pick the variable that will add to 100%
  mutate(percent = 100*(n/sum(n))) %>% # compute percents within chosen variable
  ggplot()+
  geom_bar(aes(x = DMDMARTL, y = percent, group = RIAGENDR, fill = RIAGENDR),
           position = "dodge", stat = "identity")+
  theme_minimal()+
  coord_flip()+
  labs(x = "Marital Status", y = "Percent of Marital Status Group")+
  scale_fill_manual(name = "Sex", values = c("green", "lightblue")) 

#a grouped bar chart is chosen because these are two categorical variables



