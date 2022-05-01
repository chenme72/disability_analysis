# The Labour Force of the Canadian Disabled Population 
## Canadian Health and Disability Survey 1984, Adults File analysis 

- Author: Meixuan Chen
- Data: April 30th, 2022
- email:meixuan.chen@mail.utoronto.ca

# Overview: 
This repository discusses the labour force of the Canadian disabled population in 1984 and analyzes the relationship between their weekly working hours and income with other influencing factors, such as class of work, education level, job tenure, etc. 

# Repo structure

- 'scripts' includes all the code necessary to simulate data, gather the data and cleaning the data.
- 'inputs' contains the datasheet pdf which gives some information on how the dataset was created, the rmd file used to produce the pdf and the bib file for the references used.
- 'outputs' contains the data folder which includes both the raw and clean dataset obtained.
- 'outputs' also contains the files necessary to reproduce the paper. We have the pdf copy of the paper on 'Labour Force of Canadian Disabled Population' with the rmd file in order to reproduce it and a reference bib file containing the references used for the paper.

# Obataining Data
The data can be accessed from CHASS Data Centre-> SDA @ CHASS ->  Canadian Health and Disability Survey 1983-1984 adult file -> Data and Document 


# Preprocessing and Cleaning

After obtaining the 'raw data', some cleaning was done. The code used to obtain the clean dataset is located at 'scripts/02-clean_and_prepare_data.R'
There is no mutated new variables in the dataset that was used in the analysis, however, all of the represented numbers from the survey was translated into meaningful texts by using the case_when function. 
