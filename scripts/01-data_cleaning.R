#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander [CHANGE THIS TO YOUR NAME!!!!]
# Data: 3 January 2021
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read.csv(file = 'raw.csv') 
# Just keep some variables that may be of interest (change 
# this depending on your interests)
clean_data <- raw_data %>%
  dplyr::select(clswrkr, ind, tenure, prov, sex, age, educ, tothrswk, d09b,h03, whyloss) %>%
  filter(clswrkr != 9, clswrkr != 8, clswrkr !=7,
         ind != 99,
         tenure != 0, tenure != 9,
         tothrswk != 99,
         whyloss !=9, whyloss !=0,
         d09b != 0, d09b != 22,
         h03 != 97, h03 != 8,h03 !=9
  ) %>%
  mutate(clswrkr = case_when(
    clswrkr == 1 ~ 'Paid worker, private', 
    clswrkr == 2 ~ 'Paid worker, govt business',
    clswrkr == 3 ~ 'Paid worker, govt non-business',
    clswrkr == 4 ~ 'Employer',
    clswrkr == 5 ~ 'Own account',
    clswrkr == 6 ~ 'Unpaid family worker')) %>%
  mutate(ind = case_when(
    ind == 1 ~ 'Agriculture', 
    ind == 2 ~ 'Other primary',
    ind == 3 ~ 'Manufacturing, non-durables',
    ind == 4 ~ 'Manufacturing, durables',
    ind == 5 ~ ' Construction',
    ind == 6 ~ 'Transportation, etc.',
    ind == 7 ~ 'Wholesale trade',
    ind == 8 ~ 'Retail trade',
    ind == 9 ~ 'Finance, etc.',
    ind == 10 ~ 'Community services',
    ind == 11 ~ 'Personal services',
    ind == 12 ~ 'Business/misc. Services',
    ind == 13 ~ 'Public administration',
    ind == 14 ~ 'Never worked',
    ind == 15 ~ 'Last worked 5+ years ago'
  )) %>%
  mutate(tenure = case_when(
    tenure == 1 ~ '1-6 months', 
    tenure == 2 ~ '7-12 months',
    tenure == 3 ~ '1-5 years',
    tenure == 4 ~ '6-10 years',
    tenure == 5 ~ '11-20 years',
    tenure == 6 ~ 'Over 20 years'
  )) %>%
  mutate(prov = case_when(
    prov == 0 ~ 'NL',
    prov == 1 ~ 'PE', 
    prov == 2 ~ 'NS',
    prov == 3 ~ 'NB',
    prov == 4 ~ 'QC',
    prov == 5 ~ 'ON',
    prov == 6 ~ 'MB',
    prov == 7 ~ 'SK',
    prov == 8 ~ 'AB',
    prov == 9 ~ 'BC'
  )) %>%
  mutate(sex = case_when(
    sex == 1 ~'Male',
    sex == 2~ 'Female'
  )) %>%
  mutate(age = case_when(
    age == 1 ~ '15-19 years', 
    age == 2 ~ '20-24 years',
    age == 3 ~ '25-34 years',
    age == 4 ~ '35-44 years',
    age == 5 ~ '45-54 years',
    age == 6 ~ '55-64 years',
    age == 7 ~ '65-69 years',
    age == 8 ~ ' 70-74 years',
    age == 9 ~ '75-79 years',
    age == 10 ~ '80-84 years',
    age == 11 ~ '85+ years')) %>%
  mutate(educ = case_when(
    educ == 1 ~ 'None/Elementary',
    educ == 2 ~ 'Some/Complete High School',
    educ == 3 ~ 'Some post- secondary',
    educ == 4 ~ 'Post-secondary cer/diploma',
    educ == 5 ~ 'University Degree'
  )) %>%
  mutate(whyloss = case_when(
    whyloss == 1 ~ 'Illness/disability/personal', 
    whyloss == 2 ~ 'Bad weather',
    whyloss == 3 ~ 'Labour dispute',
    whyloss == 4 ~ 'Layoff',
    whyloss == 5 ~ 'Lost job/new job',
    whyloss == 6 ~ 'Vacation',
    whyloss == 7 ~ 'Working short-time',
    whyloss == 8 ~ 'Other'
  ))%>%
  mutate(d09b = case_when(
    d09b == 1 ~ 'Mental disorders', 
    d09b == 2 ~ 'Sight disorders',
    d09b == 3 ~ 'Hearing disorders',
    d09b == 4 ~ 'Other central nervous sys dis',
    d09b == 5 ~ 'Ischaemic heart disease',
    d09b == 6 ~ 'Other heart',
    d09b == 7 ~ 'Other circulatory',
    d09b == 8 ~ 'Emphysema/asthma',
    d09b == 9 ~ 'Other respiratory',
    d09b == 10 ~ 'Digestive system diseases',
    d09b == 11 ~ 'Lower limbs arthritis/rheum',
    d09b == 12 ~ 'Upper limbs arthritis/rheum',
    d09b == 13 ~ 'Back arthritis/rheumatism',
    d09b == 14 ~ 'Other/ns arthritis/rheumatism',
    d09b == 15 ~ 'Lower limbs musculoskeletal',
    d09b == 16 ~ ' Upper limbs musculoskeletal',
    d09b == 17 ~ 'Back musculoskeletal problems',
    d09b == 18 ~ 'Other/ns musculoskeletal probs',
    d09b == 19 ~ 'Neoplasms',
    d09b == 20 ~ 'Endocrine/nutritional etc.',
    d09b == 21 ~ 'Other',
    d09b == 22 ~ 'Unknown'
  )) %>%
  mutate(h03 = case_when(
    h03 == 0 ~ 'None', 
    h03 == 1 ~ '$1-4999',
    h03 == 2 ~ '$5000-9999',
    h03 == 3 ~ '$10000-14999',
    h03 == 4 ~ '$15000-19999',
    h03 == 5 ~ '$20000-24999',
    h03 == 6 ~ '$25000-29999',
    h03 == 7 ~ '$30000+',
    h03 == 9 ~ 'Do not know '
  )) %>%
  rename(class_of_work = clswrkr,
         work_hrs_wk = tothrswk,
         why_work_lim = d09b,
         income = h03) 