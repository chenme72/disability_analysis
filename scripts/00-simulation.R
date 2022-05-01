n <-  654
set.seed(03)
x_work_hours <- sample(1:8,size = n,replace = TRUE, prob = c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8))

n <- 654
set.seed(03)
x_ind <- sample(1:13 ,size = n,replace = TRUE, prob = c(1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13,1/13))

n <-  654
set.seed(03)
x_class_of_work <- sample(1:6,size = n,replace = TRUE, prob = c(1/6,1/6,1/6,1/6,1/6,1/6))

n <-  654
set.seed(03)
x_tenure <- sample(1:6,size = n,replace = TRUE, prob = c(1/6,1/6,1/6,1/6,1/6,1/6))


n <-  654
set.seed(03)
x_prov <- sample(1:10,size = n,replace = TRUE, prob = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1))

n <-  654
set.seed(03)
x_sex <- sample(1:2,size = n,replace = TRUE, prob = c(1/2,1/2))

n <-  654
set.seed(03)
x_age <-sample(1:11,size = n,replace = TRUE, prob = c(1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11,1/11))

n <-  654
set.seed(03)
x_educ <-sample(1:5,size = n,replace = TRUE, prob = c(1/5,1/5,1/5,1/5,1/5))

n <-  654
set.seed(03)
x_why_lim <-sample(1:20,size = n,replace = TRUE, prob = c(1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20,1/20))

n <-  654
set.seed(03)
x_income<-sample(1:8,size = n,replace = TRUE, prob = c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8))

n <-  654
set.seed(03)
x_whyloss<-sample(1:8,size = n,replace = TRUE, prob = c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8))


#generate the dataset for the simulation data 
df_simulation <- data.frame(Class_of_Work = x_class_of_work, Ind = x_ind, Tenure = x_tenure, Prov = x_prov, Sex = x_sex, Educ = x_educ, Age = x_age, Limitation = x_why_lim,Income = x_income, Whyloss = x_whyloss)
#clean the simulation dataset
#clean the education level variable
df_simulation_clean <- df_simulation %>%
  mutate(x_class_of_work= case_when(
    x_class_of_work == 1 ~ 'Paid worker, private', 
    x_class_of_work == 2 ~ 'Paid worker, govt business',
    x_class_of_work == 3 ~ 'Paid worker, govt non-business',
    x_class_of_work == 4 ~ 'Employer',
    x_class_of_work == 5 ~ 'Own account',
    x_class_of_work == 6 ~ 'Unpaid family worker')) %>%
  mutate(x_ind = case_when(
    x_ind == 1 ~ 'Agriculture', 
    x_ind == 2 ~ 'Other primary',
    x_ind == 3 ~ 'Manufacturing, non-durables',
    x_ind == 4 ~ 'Manufacturing, durables',
    x_ind == 5 ~ ' Construction',
    x_ind == 6 ~ 'Transportation, etc.',
    x_ind == 7 ~ 'Wholesale trade',
    x_ind == 8 ~ 'Retail trade',
    x_ind == 9 ~ 'Finance, etc.',
    x_ind == 10 ~ 'Community services',
    x_ind == 11 ~ 'Personal services',
    x_ind == 12 ~ 'Business/misc. Services',
    x_ind == 13 ~ 'Public administration'
  )) %>%
  mutate(x_tenure = case_when(
    x_tenure == 1 ~ '1-6 months', 
    x_tenure == 2 ~ '7-12 months',
    x_tenure == 3 ~ '1-5 years',
    x_tenure == 4 ~ '6-10 years',
    x_tenure == 5 ~ '11-20 years',
    x_tenure == 6 ~ 'Over 20 years'
  )) %>%
  mutate(x_prov = case_when(
    x_prov == 10 ~ 'NL',
    x_prov == 1 ~ 'PE', 
    x_prov == 2 ~ 'NS',
    x_prov == 3 ~ 'NB',
    x_prov == 4 ~ 'QC',
    x_prov == 5 ~ 'ON',
    x_prov == 6 ~ 'MB',
    x_prov == 7 ~ 'SK',
    x_prov == 8 ~ 'AB',
    x_prov == 9 ~ 'BC'
  )) %>%
  mutate(x_sex = case_when(
    x_sex == 1 ~'Male',
    x_sex == 2~ 'Female'
  )) %>%
  mutate(x_age = case_when(
    x_age == 1 ~ '15-19 years', 
    x_age == 2 ~ '20-24 years',
    x_age == 3 ~ '25-34 years',
    x_age == 4 ~ '35-44 years',
    x_age == 5 ~ '45-54 years',
    x_age == 6 ~ '55-64 years',
    x_age == 7 ~ '65-69 years',
    x_age == 8 ~ ' 70-74 years',
    x_age == 9 ~ '75-79 years',
    x_age == 10 ~ '80-84 years',
    x_age == 11 ~ '85+ years')) %>%
  mutate(x_educ = case_when(
    x_educ == 1 ~ 'None/Elementary',
    x_educ == 2 ~ 'Some/Complete High School',
    x_educ == 3 ~ 'Some post- secondary',
    x_educ == 4 ~ 'Post-secondary cer/diploma',
    x_educ == 5 ~ 'University Degree'
  )) %>%
  mutate(x_whyloss = case_when(
    x_whyloss == 1 ~ 'Illness/disability/personal', 
    x_whyloss == 2 ~ 'Bad weather',
    x_whyloss == 3 ~ 'Labour dispute',
    x_whyloss == 4 ~ 'Layoff',
    x_whyloss == 5 ~ 'Lost job/new job',
    x_whyloss == 6 ~ 'Vacation',
    x_whyloss == 7 ~ 'Working short-time',
    x_whyloss == 8 ~ 'Other'
  ))%>%
  mutate(x_why_lim = case_when(
    x_why_lim == 1 ~ 'Mental disorders', 
    x_why_lim == 2 ~ 'Sight disorders',
    x_why_lim == 3 ~ 'Hearing disorders',
    x_why_lim == 4 ~ 'Other central nervous sys dis',
    x_why_lim == 5 ~ 'Ischaemic heart disease',
    x_why_lim == 6 ~ 'Other heart',
    x_why_lim == 7 ~ 'Other circulatory',
    x_why_lim == 8 ~ 'Emphysema/asthma',
    x_why_lim == 9 ~ 'Other respiratory',
    x_why_lim == 10 ~ 'Digestive system diseases',
    x_why_lim == 11 ~ 'Lower limbs arthritis/rheum',
    x_why_lim == 12 ~ 'Upper limbs arthritis/rheum',
    x_why_lim == 13 ~ 'Back arthritis/rheumatism',
    x_why_lim == 14 ~ 'Other/ns arthritis/rheumatism',
    x_why_lim == 15 ~ 'Lower limbs musculoskeletal',
    x_why_lim == 16 ~ ' Upper limbs musculoskeletal',
    x_why_lim == 17 ~ 'Back musculoskeletal problems',
    x_why_lim == 18 ~ 'Other/ns musculoskeletal probs',
    x_why_lim == 19 ~ 'Neoplasms',
    x_why_lim == 20 ~ 'Endocrine/nutritional etc.'
  )) %>%
  mutate(x_income = case_when(
    x_income == 1 ~ 'None', 
    x_income == 2 ~ '$1-4999',
    x_income == 3 ~ '$5000-9999',
    x_income == 4 ~ '$10000-14999',
    x_income == 5 ~ '$15000-19999',
    x_income == 6 ~ '$20000-24999',
    x_income == 7 ~ '$25000-29999',
    x_income == 8 ~ '$30000+'
  ))

