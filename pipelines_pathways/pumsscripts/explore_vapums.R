# Pipelines and Pathways Community Metrics
# Explore Cville Region PUMS data
# Updated: 2022-06-23
# Authors: Michele Claibourn
# ..............................................

# Resources ----

# A nice intro to using PUMS in R
# https://walker-data.com/tidycensus/articles/pums-data.html
# https://walker-data.com/census-r/analyzing-census-microdata.html

# Data dictionary for 2016-2020 ACS (also in folder)
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2016-2020.pdf

# Proposal outlining goals
# https://docs.google.com/document/d/1FX5uApvSgkbkXeRObXgI1wuEbl5kqeTq36kEFTc4Hxo/edit?usp=sharing


# ..............................................
# 0. Set up ----
library(tidyverse)
library(survey)
library(srvyr)


# ..............................................
# 1. Read data ----
pva_cvl <- readRDS("pumsdata/cville_pums_personal.RDS")
hva_cvl <- readRDS("pumsdatacville_pums_household.RDS")


# ..............................................
# 2. Find variables ----
# need to narrow down via appropriate variables -- possibilities

# a. Earnings
# PERNP: total person's earnings 
summary(pva_cvl$PERNP) # includes losses (negative values)
# WAGP: wages or salary income past 12 months
summary(pva_cvl$WAGP) # so this might be most relevant?
# PINCP: total person's income (includes non-salary income)
# SEMP: self-employment income past 12 months (not precisely relevant for UVA pay comparison)

# need to look into using ADJINC - income adjustment factor
#   to make 2016-2020 values comparable to one another
#   if I'm understanding the documentation correctly...
pva_cvl <- pva_cvl %>% 
  mutate(adj_pernp = PERNP*(ADJINC/1000000),
         adj_wagp = WAGP*(ADJINC/1000000))
summary(pva_cvl$adj_pernp)
summary(pva_cvl$adj_wagp)

# b. among full-time workers 
# ESR: Employment status recode
count(pva_cvl, ESR) # 1 and 2 are civilian employment
# WKHP: Usual hours worked per week past 12 months
summary(pva_cvl$WKHP) # for full-time status (35+)

# not sure if we need to further filter for full-year... e.g., full-time, full-year earnings
# WKW: Weeks worked during past 12 months (2016-2018)
count(pva_cvl, WKW) # values are categories 1-6; 1 and 2 approx all year
# WKWN: Weeks worked during past 12 months (2019 or later)
summary(pva_cvl$WKWN) # values are number of weeks

# AGEP: age (if we want to filter to 18 and above)
summary(pva_cvl$AGEP)
# SCH: school enrollment (if we need to further filter out students; though full-time work would probably suffice)
count(pva_cvl, SCH) # 1 is no/not enrolled

# Earnings by other characteristics
#   c. by sex (SEX)
count(pva_cvl, SEX) # 1=male, 2=female

#   c. by race
# RAC1P: detailed race codes
count(pva_cvl, RAC1P)
# RACAIAN, RACASN, RACBLK, RACNH, RACPI, RACSOR, RACWHT: binary (not mutually exclusive)

#   c. by ethnicity
# HISP: Recoded detailed Hispanic origin
count(pva_cvl, HISP) # 1 is not Hispanic

#   d. by educational attainment
# SCHL: educational attainment
count(pva_cvl, SCHL) # 15 and under are no HS degree; 

#   e. by disability status 
# DIS: Disability recode
count(pva_cvl, DIS) # 1=with disability
# Specific disabilities: DDRS, DEAR, DEYE, DOUT, DPHY, DREM

#   f. by english spoken in home 
# ENG: ability to speak english
count(pva_cvl, ENG) # 1=very well, 2=well, 3=not well, 4=not at all
# but only asked of not-native english speakers?
# LANX: language other than english spoken at home
count(pva_cvl, LANX) # 1=yes
# NATIVITY: foreign born
count(pva_cvl, NATIVITY) # 2=yes


# ..............................................
# 3. Check estimation ----
# next steps: need to review
# survey: https://cran.r-project.org/web/packages/survey/index.html 
# srvyr: https://github.com/gergness/srvyr