# Pipelines and Pathways Community Metrics
# Explore Cville Region PUMS data
# Updated: 2022-07-08
# Authors: Michele Claibourn & Lee LeBoeuf
# ..............................................

# Remaining questions:
# Do we also need the % of people within a demographic group? For example, the percent of Black 
# residents that fall into a specific income band? 

# Resources ----

# A nice intro to using PUMS in R
# https://walker-data.com/tidycensus/articles/pums-data.html
# https://walker-data.com/census-r/analyzing-census-microdata.html

# Data dictionary for 2016-2020 ACS (also in folder)
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2016-2020.pdf

# Proposal outlining goals
# https://docs.google.com/document/d/1FX5uApvSgkbkXeRObXgI1wuEbl5kqeTq36kEFTc4Hxo/edit?usp=sharing

# Calculating proportions by groups with srvyr 
# https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html

# ..............................................
# 0. Set up ----
library(tidyverse)
library(tidycensus)
library(survey)
library(srvyr)


# ..............................................
# 1. Read data ----
pva_cvl <- readRDS("pumsdata/cville_pums_personal.RDS")
# hva_cvl <- readRDS("pumsdata/cville_pums_household.RDS")

# 2. Adjustments and creating necessary categorical variables 
# Income adjustment --converting everything to 2020 dollars based on instructions from the PUMS data dictionary
# (see page 17: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/ACS2016_2020_PUMS_README.pdf)
pva_cvl <- pva_cvl %>% 
  mutate(adj_pernp = PERNP*(ADJINC/1000000),
         adj_wagp = WAGP*(ADJINC/1000000))

# Income bins of interest: 
# Assuming full time (40 hours) for 50 weeks/year 
# $14.99/hour or less = 29,980 (round to 29,999 for simplicity)
# $15.00 - 17.49/hour = 30,000 - 34,980 (round to 34,999)
# $17.50 - 19.99 = 35,000 - 39,980 (round to 39,999)
# $20.00 - 22.60 = 40,000 - 45,200 
# >45,200
pva_cvl$incomebin <- ifelse(pva_cvl$adj_pernp < 29999, "under30k",
                            ifelse(pva_cvl$adj_pernp > 29999 & pva_cvl$adj_pernp <= 34999, "30to35k",
                                   ifelse(pva_cvl$adj_pernp > 34999 & pva_cvl$adj_pernp <= 39999, "35to40k",
                                          ifelse(pva_cvl$adj_pernp > 39999 & pva_cvl$adj_pernp <= 45200, "40to45.2k", 
                                                 ifelse(pva_cvl$adj_pernp >= 45200, "over45.2k", pva_cvl$adj_pernp)))))

pva_cvl <- pva_cvl %>% 
  mutate(incomebin = factor(incomebin, 
                            levels = c("under30k", "30to35k",
                                       "35to40k", "40to45.2k",
                                       "over45.2k")))
unique(pva_cvl$incomebin)
# full-time workers
pva_cvl$fulltime <- ifelse(pva_cvl$WKHP >= 35, 1, 0)

# No HS diploma
# SCHL Character 2
# Educational attainment
# Everything 15 and under mean no HS diploma 
nohs <- as.character(1:15)
pva_cvl <- pva_cvl %>%
  mutate(hs = ifelse(SCHL %in% nohs, "nohs", "yeshs"))

pva_cvl_svy <- to_survey(pva_cvl)

# 3. Calculating estimates 
# As of now, percents are calculated out of the entire full-time worker population estimates 
# Total population estimate for all of the below data frames: 107011 (full-time workers)

# Adults with no HS degree/equivalent 

hs <- pva_cvl_svy %>% 
  filter(fulltime == 1) %>%
  rename(demographic = hs) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_prop() * 100, 2),
            totalWorkers = survey_total()) 

# All racial groups with and without high school diploma 
raceHS <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  group_by(RAC1P, hs, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total())

raceHS$RAC1P <- recode_factor(raceHS$RAC1P, '1' = "White", '2' = "Black", '3' = "AmericanIndian", '5' = "AmericanIndianAKNative", 
                       '6' = "Asian", '7' = "NHandPI", '8' = "SomeOtherRace", '9' = "TwoOrMoreRaces")

raceHS$demographic <- paste0(raceHS$RAC1P, raceHS$hs)

raceHS <- raceHS[,3:8] # Getting rid of unnecessary columns 

# Black adults 
# RAC1P = 2 designates Black or African American alone
black <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P == 2, "black", "nonblack")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Asian adults
# RAC1P = 6 designates Asian alone 
asian <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P == 6, "asian", "nonasian")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Latino adults 
# All entries in the variable HISP that are greater than 1 are Latino 
latino <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(HISP > 1, "latino", "nonlatino")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Multiracial adults
# RAC1P = 9 designates two or more races 
multi <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P == 9, "multi", "nonmulti")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# American Indian or Alaskan Native adults
# RAC1P = 3 and 5 designates American Indian and Alaskan Native 
aian <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P %in% c(3,5), "amind-aknat", "nonaian")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Native Hawaiian,Pacific Islander adults
# RAC1P = 7 designates Native Hawaiian or Pacific Islander 
nhpi <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P == 7, "haw-pi", "nonhaw-pi")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Some Other Race adults
# RAC1P = 8 designates some other race 
otherrace <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P == 8, "other", "nonother")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# White adults
# RAC1P = 1 designates White alone 
white <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(RAC1P == 1, "white", "nonwhite")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 


# Adults with disabilities 
# DIS Character 1
# Disability recode
# 1 .With a disability
# 2 .Without a disability

disability <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(DIS == 1, "disability", "nodisability")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Sex
# SEX Character 1
# 1 .Male
# 2 .Female

sex <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(SEX == 1, "male", "female")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Language 
# LANX Character 1
# Language other than English spoken at home
# b .N/A (less than 5 years old)
# 1 .Yes, speaks another language
# 2 .No, speaks only English

language <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = ifelse(LANX == 1, "nonEnglish", "English")) %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

# Everyone

everyone <- pva_cvl_svy %>%
  filter(fulltime == 1) %>%
  mutate(demographic = "Everyone") %>%
  group_by(demographic, incomebin) %>%
  summarize(percentOfDemo = round(survey_mean() * 100, 2),
            totalWorkers = survey_total()) 

sum(everyone$totalWorkers)
# 107011 full time workers 

# 4. Combining data frames and calculating percentages out of all workers 

fulldata <- rbind(white %>% filter(demographic == "white"), 
                  black %>% filter(demographic == "black"), 
                  asian %>% filter(demographic == "asian"), 
                  latino %>% filter(demographic == "latino"), 
                  multi %>% filter(demographic == "multi"), 
                  aian %>% filter(demographic == "amind-aknat"), 
                  nhpi %>% filter(demographic == "haw-pi"), 
                  otherrace %>% filter(demographic == "other"), 
                  disability, sex, language, hs, everyone, raceHS)

fulldata <- fulldata %>%
  mutate(percentofWorkers = round(totalWorkers / sum(everyone$totalWorkers) * 100, 2))

write.csv(fulldata, "pumsdata/07-13-pumsdata.csv")




