##########################################################
## Following code written by Sari Inkila for the edX 
## Choose Your Own (CYO) Project
##########################################################


# Note: The process of loading the libraries could take a couple of minutes
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(xfun)) install.packages("xfun", repos = "http://cran.us.r-project.org")
if(!require(latexpdf)) install.packages("latexpdf", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(rjstat)) install.packages("rjstat", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(timetk)) install.packages("timetk", repos = "http://cran.us.r-project.org")
if(!require(fpp2)) install.packages("fpp2", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
if(!require(vars)) install.packages("vars", repos = "http://cran.us.r-project.org")
if(!require(rsample)) install.packages("rsample", repos = "http://cran.us.r-project.org")
if(!require(tsibble)) install.packages("tsibble", repos = "http://cran.us.r-project.org")
if(!require(ISOweek)) install.packages("ISOweek", repos = "http://cran.us.r-project.org")
if(!require(crop)) install.packages("crop", repos = "http://cran.us.r-project.org")

#Loading libraries
library(ggthemes)
library(httr)
library(rjstat)
library(dplyr)
library(dslabs)
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(tinytex)
library(xfun)
library(latexpdf)
library(rmarkdown)
library(ggplot2)
library(formatR)
library(jsonlite)
library(tidyr)
library(gridExtra)
library(ISOweek)
library(timetk)
library(fpp2)
library(forecast)
library(TTR)
library(vars)
library(rsample)
library(tsibble)
library(GGally)
library(crop)


options(digits = 7)
options(pillar.sigfig = 7)


##########################################################
## This project works with confirmed Finnish Covid-19 case data
## sourced from the Finnish Insitute of Health and 
## Welfare (THL) website
#########################################################

# 1. Downloading data

## Data source used: 
## https://thl.fi/en/web/thlfi-en/statistics/statistical-databases/open-data/confirmed-corona-cases-in-finland-covid-19-

## Below download follows the THL json interface parameters as per 
## instructions given on the website 
## (Listed 5 steps done for each of the data sets)

# Step 1: address base
url_base <- "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.json"

## Repeat steps 2.-5. for data required / available

##### Population across all health care district
## Population per week combined across all health care district 
## as well as per health care district
## Step 2: Create the request add-on 
## sid : 445344
## All
request_population <- "?row=hcdmunicipality2020-445222.&column=dateweek20200101-509030&filter=measure-445344"

## Step 3: Combine the parts- All
url <- paste0(url_base, request_population)

## Step 4: Get cube (returns a list, where df as first item)
cube_population <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


## Step 5: Save cube and check 20 first rows
res_population <- cube_population[[1]]
head(res_population, 20)

## Modifying the data for easier analysis

#Patterns for extracting extra words
pattern1 <- "Year"
pattern2 <- "Week"


# creating a new field for trimmed year & week combination for identifier 
# across data frames and a date format field for time series processing.
population <- res_population %>%
  filter(dateweek20200101 >= "Year 2020 Week 08" 
         & dateweek20200101 <= "Year 2021 Week 12" 
         & dateweek20200101 != "All times") %>%
  mutate(year_week= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "-")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week)) %>%
  mutate(year_week1= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week1= str_squish(year_week1)) %>%
  mutate(year_week1= str_replace(year_week1, " ", "-")) %>%
  mutate(year_week1= str_replace(year_week1, pattern2, "W")) %>%
  mutate(year_week1= str_replace(year_week1, " ", "")) %>%
  mutate(year_week1= str_trim(year_week1)) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week1 = paste0(year_week1, "-1")) %>%
  mutate(wdate = ISOweek2date(year_week1)) 
  
# Leaving just required columns
population <- population  %>%
  mutate(population = value) %>%
  arrange(dateweek20200101) %>%
  dplyr::select(dateweek20200101, year_week, wdate, population)



#########################################################
## All tested per week combined across all health care district
## as well as per health care district
## Request all tested SID: 445356
## Step 2: Create the request add-on 
request_tested <- "?row=hcdmunicipality2020-445222.&column=dateweek20200101-509030&filter=measure-445356"



## Step 3: Combine the parts
url <- paste0(url_base, request_tested)


## Step 4: Get cube (returns a list, where df as first item)
cube_tested <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


# Save cube and check 20 first rows
res_tested <- cube_tested[[1]]
head(res_tested, 20)


## Modifying the data for easier analysis

#Patterns for extracting extra words
pattern1 <- "Year"
pattern2 <- "Week"

#creating a new field for trimmed year & week combination
tested <- res_tested %>%
  mutate(year_week= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "-")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week))


tested <- tested %>%
  filter(dateweek20200101 >= "Year 2020 Week 08" 
         & dateweek20200101 <= "Year 2021 Week 12" 
         & dateweek20200101 != "All times") %>%
  mutate(tested = value) %>%
  arrange(dateweek20200101) %>%
  dplyr::select(year_week, tested)


#########################################################
## All confirmed cases per week combined across all health care districts
## as well as per health care district
## Step 2: Create the request add-on 
## SID = 444833
request_cases <- "?row=hcdmunicipality2020-445222.&column=dateweek20200101-509030&filter=measure-444833"


## Step 3: Combine the parts
url <- paste0(url_base, request_cases)


## Step 4: Get cube (returns a list, where df as first item)
cube_cases <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


## Step 5: Save cube and check 20 first rows
res_cases <- cube_cases[[1]]
head(res_cases, 20)



## Modifying the data for easier analysis
#Patterns for extracting extra words
pattern1 <- "Year"
pattern2 <- "Week"

#creating a new field for trimmed year & week combination
confirmed_cases <- res_cases %>%
  mutate(year_week= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "-")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week))

confirmed_cases <- confirmed_cases%>%
  filter(dateweek20200101 >= "Year 2020 Week 08" & 
           dateweek20200101 <= "Year 2021 Week 12" & 
           dateweek20200101 != "All times") %>%
   mutate(nr_confirmed = value) %>%
  dplyr::select(year_week, nr_confirmed) %>%
  arrange(year_week) 


#########################################################
## All confirmed deaths per week
## Step 2: Create the request add-on 
## sid = 492118)
request_deaths <- "?row=hcdmunicipality2020-445222.&column=dateweek20200101-509030&filter=measure-492118"

## Per health care district - There is no health care district summaries 
## of the deaths available. 

## Step 3: Combine the parts
url <- paste0(url_base, request_deaths)

## Step 4: Get cube (returns a list, where df as first item)
cube_deaths <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


## Step 5: Save cube and check 20 first rows
res_deaths <- cube_deaths[[1]]
head(res_deaths, 20)


## Modifying the data for easier analysis

#Patterns for extracting extra words
pattern1 <- "Year"
pattern2 <- "Week"

#creating a new field for trimmed year & week combination
confirmed_deaths <- res_deaths %>%
  mutate(year_week= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "-")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week))

##Only including cases back from the week I started working on this case
confirmed_deaths <- confirmed_deaths %>%
  filter(dateweek20200101 >= "Year 2020 Week 08" 
         & dateweek20200101 <= "Year 2021 Week 12" 
         & dateweek20200101 != "All times") %>%
  mutate(nr_deaths = value) %>%
  dplyr::select(year_week, nr_deaths) %>%
  arrange(year_week)

##########################################################
### Next retrieving supporting data for the weekly data
##########################################################

## All confirmed cases per age group - Note! Limited availability
## sid = 444833
## Step 2: Create the request add-on 
request_cases_age <- "?row=ttr10yage-444309&column=dateweek20200101-509030.&filter=measure-444833"

## Step 3: Combine the parts
url <- paste0(url_base, request_cases_age)

## Step 4: Get cube (returns a list, where df as first item)
cube_cases_age <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_cases_age <- cube_cases_age[[1]]
head(res_cases_age, 20)

## Modifying the data for easier analysis
cases_age <- res_cases_age %>%
  filter(ttr10yage != "All ages") %>%
  mutate(nr_confirmed = value) %>%
  dplyr::select(ttr10yage, nr_confirmed)

## All confirmed deaths per age group
## Step 2: Create the request add-on 
## sid = 492118
request_death_age <- "?row=ttr10yage-444309&column=dateweek20200101-509030.&filter=measure-492118"

## Step 3: Combine the parts
url <- paste0(url_base, request_death_age)

## Step 4: Get cube (returns a list, where df as first item)
cube_death_age <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_death_age <- cube_death_age[[1]]
head(res_death_age, 20)

## Modifying the data for easier analysis
death_age <- res_death_age %>%
  filter(ttr10yage <= "All ages") %>%
  mutate(nr_deaths = value, 
         nr_deaths = ifelse(nr_deaths =="..", 0, nr_deaths)) %>%
  dplyr::select(ttr10yage, nr_deaths)

## Joining the tables related to age into one
cases_deaths_age <- left_join(cases_age, death_age, by = "ttr10yage")

## Changing the data types to numeric
cases_deaths_age  <- cases_deaths_age %>%
  mutate(nr_confirmed = as.numeric(nr_confirmed), 
         nr_deaths = as.numeric(nr_deaths)) %>%
  dplyr::select(ttr10yage, nr_confirmed, nr_deaths)


##########################################################
## Sex related data - Note! Limited availability
## All confirmed cases per sex
## Step 2: Create the request add-on 
## sid = 444833
request_cases_sex<- "?row=sex-444328&column=dateweek20200101-509030.&filter=measure-444833"

## Step 3: Combine the parts
url <- paste0(url_base, request_cases_sex)

## Step 4: Get cube (returns a list, where df as first item)
cube_cases_sex <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_cases_sex <- cube_cases_sex[[1]]
head(res_cases_sex, 20)

## Modifying the data for easier analysis
cases_sex <- res_cases_sex %>%
  filter(sex != "All sexes") %>%
  mutate(nr_cases = value) %>%
  dplyr::select(sex, nr_cases)


## All confirmed deaths per sex
## Step 2: Create the request add-on 
## sid = 492118
request_death_sex<- "?row=sex-444328&column=dateweek20200101-509030.&filter=measure-492118"

## Step 3: Combine the parts
url <- paste0(url_base, request_death_sex)

## Step 4: Get cube (returns a list, where df as first item)
cube_death_sex <- fromJSONstat(url, naming = "label", 
                               use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_death_sex <- cube_death_sex[[1]]
head(res_death_sex, 20)

## Modifying the data for easier analysis
death_sex <- res_death_sex %>%
  filter(sex != "All sexes") %>%
  mutate(nr_deaths = value, ) %>%
  dplyr::select(sex, nr_deaths)


## Joining the tables related to sex into one
cases_deaths_sex <- left_join(cases_sex, death_sex, by = "sex")

## Changing the data types to numeric
cases_deaths_sex <- cases_deaths_sex %>%
  mutate(nr_confirmed = as.numeric(nr_cases), 
         nr_deaths = as.numeric(nr_deaths)) %>%
  dplyr::select(sex, nr_confirmed, nr_deaths)


### Case fatality rate per age group and sex
## Case fatality rate = The number of COVID-19 deaths in a population / 
## Divided by the total number of COVID-19 cases * 100 

cases_deaths_age <-  cases_deaths_age %>%
  mutate(case_fatality_rate = nr_deaths/nr_confirmed*100)

cases_deaths_sex <-  cases_deaths_sex %>%
  mutate(case_fatality_rate = nr_deaths/nr_confirmed*100)



##########################################################
## Downloading self-reported Covid-19 symptoms data per
## instructions given on the site
## Self-reported symptoms across and per health care district
#########################################################

## https://sampo.thl.fi/pivot/prod/en/epirapo/omaolosymp/fact_epirapo_omaolosymp.json

# 1. Downloading data

# Step 1: address base
url_base_s <- "https://sampo.thl.fi/pivot/prod/en/epirapo/omaolosymp/fact_epirapo_omaolosymp.json"


##### Self-reported symptoms across health care districts
## Nr of symptoms reported total sid:533175
## Step 2: Create the request add-on 
request_symp <- "?row=hcdmunicipality2020-445222.&column=dateweek20200101-509030&filter=measure-460489"


## Step 3: Combine the parts- All
url <- paste0(url_base_s, request_symp)


## Step 4: Get cube (returns a list, where df as first item)
cube_symp <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


## Step 5: Save cube and check 20 first rows
res_symp <- cube_symp[[1]]

head(res_symp, 20)


## Nr symptoms across health care districts
nr_symptoms <- res_symp %>%
  mutate(year_week= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "-")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week)) %>%
  filter(dateweek20200101 >= "Year 2020 Week 08" & 
           dateweek20200101 <= "Year 2021 Week 12" & 
           dateweek20200101!= "All times") %>%
  mutate(nr_sympt_reported = value) %>%
  dplyr::select(dateweek20200101, year_week, nr_sympt_reported)



##### Number of reports with urgent referral to treatment per week 
## across health care districts
## Nr of reports with urgent referral to treatment sid:452216
## Step 2: Create the request add-on 
request_reftotreat <- "?row=hcdmunicipality2020-445222.&column=dateweek20200101-509030&filter=measure-452216"


## Step 3: Combine the parts- All
url <- paste0(url_base_s, request_reftotreat)


## Step 4: Get cube (returns a list, where df as first item)
cube_reftotreat <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


## Step 5: Save cube and check 20 first rows
res_reftotreat <- cube_reftotreat[[1]]

head(res_reftotreat, 20)


## Nr symptoms across health care districts
nr_reftotreat <- res_reftotreat %>%
  mutate(year_week= str_replace(dateweek20200101, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "-")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week)) %>%
  filter(dateweek20200101 >= "Year 2020 Week 08" & 
           dateweek20200101 <= "Year 2021 Week 12" & 
           dateweek20200101!= "All times") %>%
  mutate(nr_reftotreat = value) %>%
  dplyr::select(dateweek20200101, year_week, nr_reftotreat)

### Nr of reported symptoms per age group
## Nr of symptoms reported total sid:533175
## Step 2: Create the request add-on 
request_symp_age <- "?row=ttr10yage-444309&column=dateweek20200101-509030.&filter=measure-533175"


## Step 3: Combine the parts
url <- paste0(url_base_s, request_symp_age)

## Step 4: Get cube (returns a list, where df as first item)
cube_symp_age <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_symp_age <- cube_symp_age[[1]]
head(res_symp_age, 20)

## Modifying the data for easier analysis
nr_symp_age_age <- res_symp_age %>%
  filter(ttr10yage != "All ages") %>%
  mutate(nr_symptoms = value) %>%
  dplyr::select(ttr10yage, nr_symptoms)


### Nr of reports with urgent referral to treatment per age group
## Nr of reports with urgent referral to treatment sid:452216
## Step 2: Create the request add-on 
request_reftotreat_age <- "?row=ttr10yage-444309&column=dateweek20200101-509030.&filter=measure-452216"


## Step 3: Combine the parts
url <- paste0(url_base_s, request_reftotreat_age)

## Step 4: Get cube (returns a list, where df as first item)
cube_reftotreat_age <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_reftotreat_age <- cube_reftotreat_age[[1]]
head(res_reftotreat_age, 20)

## Modifying the data for easier analysis
reftotreat_age <- res_reftotreat_age %>%
  filter(ttr10yage != "All ages") %>%
  mutate(nr_reftotreat = value) %>%
  dplyr::select(ttr10yage, nr_reftotreat)


##### Compiling age related data to one data frame
## Joining the tables related to age into one
Per_age_group <- left_join(cases_deaths_age, nr_symp_age_age, by = "ttr10yage")
Per_age_group <- left_join(Per_age_group, reftotreat_age, by = "ttr10yage")

## Changing the data types to numeric
Per_age_group <- Per_age_group %>%
  mutate(nr_confirmed = as.numeric(nr_confirmed), 
         nr_deaths = as.numeric(nr_deaths), 
         nr_symptoms = as.numeric(nr_symptoms), 
         nr_reftotreat = as.numeric(nr_reftotreat)) %>%
  dplyr::select(ttr10yage, nr_confirmed, nr_deaths, nr_symptoms, nr_reftotreat, case_fatality_rate)

## Replacing NAs with 0
Per_age_group [is.na(Per_age_group)] <- 0



#########################################################
## Downloading Vaccination progress data 
##  per instructions given on the site
#########################################################

# 1. Downloading data

## Data source used: 
## https://sampo.thl.fi/pivot/prod/en/vaccreg/cov19cov/fact_cov19cov.json

## Below download follows the THL json interface parameters as per 
## instructions given on the website 
## (Listed 5 steps done for each of the data sets)

# Step 1: address base
url_base_v <- "https://sampo.thl.fi/pivot/prod/en/vaccreg/cov19cov/fact_cov19cov.json"

## Repeat steps 2.-5. for data required / available

#########################################################
## Across all health care districts
## All areas sid: 518362
## Nr of first doses given (cov_vac_dose2) sid: 533170
## NOte! Using first dose, as decision was made to only give second dose after 12 weeks
## Step 2: Create the request add-on 
request_vacc <- "?row=area-518362.&column=dateweek20201226-525425&filter=measure-533170"


## Step 3: Combine the parts- All
url <- paste0(url_base_v, request_vacc)


## Step 4: Get cube (returns a list, where df as first item)
cube_vacc <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)


## Step 5: Save cube and check 20 first rows
res_vacc <- cube_vacc[[1]]
head(res_vacc, 20)


## Per health care district people vaccinated
vacc_peop <- res_vacc %>%
  mutate(year_week= str_replace(dateweek20201226, pattern1, "")) %>%
  mutate(year_week= str_squish(year_week)) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_replace(year_week, pattern2, "-")) %>%
  mutate(year_week= str_replace(year_week, " ", "")) %>%
  mutate(year_week= str_trim(year_week))


vacc_peop <- vacc_peop  %>%
  filter(dateweek20201226 >= "Year 2020 Week 08" & 
           dateweek20201226 <= "Year 2021 Week 12" & 
           dateweek20201226 != "All times") %>%
  mutate(vacc_peop = value, id = paste0(area, dateweek20201226)) %>%
  arrange(dateweek20201226) %>%
  dplyr::select(id, area, year_week, vacc_peop)


#########################################################
## Vaccination per health care district - population
## Weekly population for 2021 / 
## All areas sid: 518362
## Population (measure/POP) sid: 433796
## Step 2: Create the request add-on 
request_pop_hd <- "?row=area-518362&column=dateweek20201226-525425&filter=measure-433796"

## Step 3: Combine the parts- All
url <- paste0(url_base_v, request_pop_hd)

## Step 4: Get cube (returns a list, where df as first item)
cube_pop_hd <- fromJSONstat(url, naming = "label", use_factors = F, silent = T)

## Step 5: Save cube and check 20 first rows
res_pop_hd <- cube_pop_hd[[1]]
head(res_pop_hd, 20)

# Using population per health care district in the vaccination data for 
# missing 2021 population data
pop_hd <- res_pop_hd %>%
  filter(area == "All areas" & dateweek20201226 == "All times") %>%
  summarise(population_all = sum(as.numeric(value)))


##########################################################
## Combining downloaded data
## Across all health care districts
##########################################################

## Combining population, tested, confirmed cases and confirmed deaths 
## and self-reported data and vaccination data into 
## a single data frame
## Also replacing NAs in the population of the year 2021 weeks, 
## with the last population data available for year 2020

# Replacing the NAs
population[is.na(population)] <- pop_hd[1,1]
#population[is.na(wdate)]

combined_df <- left_join(population, tested, by = "year_week")
combined_df <- left_join(combined_df, confirmed_cases, by = "year_week")
combined_df <- left_join(combined_df, confirmed_deaths, by = "year_week")
combined_df <- left_join(combined_df, nr_symptoms, by = "year_week")
combined_df <- left_join(combined_df, nr_reftotreat, by = "year_week")
combined_df <- left_join(combined_df, vacc_peop, by = "year_week")


## Replacing NAs with 0
combined_df [is.na(combined_df)] <- 0

## Changing the data types to numeric
combined_df <- combined_df %>%
  mutate(population = as.numeric(population), 
         tested = as.numeric(tested), 
         nr_confirmed = as.numeric(nr_confirmed), 
         nr_deaths = as.numeric(nr_deaths),
         nr_sympt_reported = as.numeric(nr_sympt_reported),
         nr_reftotreat = as.numeric(nr_reftotreat),
         vacc_peop = as.numeric(vacc_peop)) %>%
  dplyr::select(wdate, 
                year_week, 
                population, 
                tested, 
                nr_confirmed, 
                nr_deaths, 
                nr_sympt_reported, 
                nr_reftotreat, 
                vacc_peop)





##########################################################
## 2. Visualizing downloaded data
##########################################################

## Scaling to x tested, confirmed cases and deaths per 100 000 inhabitants
combined_df <- combined_df %>%
  mutate(tested_per_100k = tested/(population/100000), 
         confirmed_per_100k = nr_confirmed/(population/100000),
         deaths_per_100k = nr_deaths/(population/100000),
         vacc_peop_per_100k = vacc_peop/(population/100000))

## Calculating Case fatality rate = The number of COVID-19 deaths in a population / 
## Divided by the total number of COVID-19 cases * 100 

combined_df <- combined_df %>%
  mutate(case_fatality_rate = nr_deaths/nr_confirmed*100)

## Mortality rate i.e. covid-19 deaths per 1000 inhabitants in a year
first_year_summary <- combined_df %>%
  filter(year_week <="2021-06") %>%
  summarise(average_population = mean(population), 
            total_tested = sum(tested), 
            total_confirmed = sum(nr_confirmed), 
            total_deaths = sum(nr_deaths), ) %>%
  mutate(total_confirmed_per_100k = total_confirmed/(average_population/100000), 
         annual_case_fatality_rate = total_deaths/total_confirmed*100, 
         mortality_rate = total_deaths/(average_population/1000))

# Visualization of the data

## Per 100 000
# Population
population_fig_2 <- combined_df %>%
  filter(year_week<= "2021-06") %>%
  ggplot(aes(x = wdate, y = population)) +
  geom_path() +
  ggtitle("Population", subtitle = "Per week") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Tested
tested_fig_2 <- combined_df %>%
  filter(year_week<= "2021-06") %>%
  ggplot(aes(x = wdate, y = tested_per_100k)) +
  geom_path() +
  ggtitle("Tested 2020", subtitle = "Per week per 100 k inhabitants") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Confirmed cases
confirmed_fig_2 <- combined_df %>%
  filter(year_week<= "2021-06") %>%
  ggplot(aes(x =  wdate, y = confirmed_per_100k)) +
  geom_path() +
  ggtitle("Confirmed cases", subtitle = "Per week per 100 k inhabitants") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Deaths
deaths_fig_2 <- combined_df %>%
  filter(year_week<= "2021-06") %>%
  ggplot(aes(x = wdate, y = deaths_per_100k)) +
  geom_path() +
  ggtitle("Confirmed deaths", subtitle = "Per week per 100 k inhabitants") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Displaying in a one column grid 
grid.arrange(tested_fig_2, confirmed_fig_2, deaths_fig_2, ncol = 1)


## Age distributions

# Cases per age group
age_distribution_cc <- cases_deaths_age %>%
  ggplot(aes(x= ttr10yage, y= nr_confirmed)) +
  geom_bar(stat = "identity") +
  ggtitle("Confirmed Covid-19 cases", subtitle = "Per age group") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Deaths per age group
age_distribution_d <- cases_deaths_age %>%
  ggplot(aes(x= ttr10yage, y= nr_deaths)) +
  geom_bar(stat = "identity") +
  ggtitle("Confirmed Covid-19 deaths", subtitle = "Per age group") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Case fatality rate per age group
age_distribution_f <- cases_deaths_age %>%
  ggplot(aes(x= ttr10yage, y= case_fatality_rate)) +
  geom_bar(stat = "identity") +
  ggtitle("Covid-19 case fatality rate", subtitle = "Per age group") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Displaying in a 3 column grid 
grid.arrange(age_distribution_cc, age_distribution_d, age_distribution_f, ncol = 3)

## Sex distributions
# Confirmed cases
sex_distribution_cc <- cases_deaths_sex %>%
  ggplot(aes(x= sex, y= nr_confirmed)) +
  geom_bar(stat = "identity") +
  ggtitle("Confirmed Covid-19 cases", subtitle = "Per sex") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Deaths
sex_distribution_d <- cases_deaths_sex %>%
  ggplot(aes(x= sex, y= nr_deaths)) +
  geom_bar(stat = "identity") +
  ggtitle("Confirmed Covid-19 deaths", subtitle = "Per sex") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Case fatality rate
sex_distribution_f <- cases_deaths_sex %>%
  ggplot(aes(x= sex, y= case_fatality_rate)) +
  geom_bar(stat = "identity") +
  ggtitle("Covid-19 case fatality rate", subtitle = "Per sex") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(sex_distribution_cc, sex_distribution_d, sex_distribution_f, ncol = 3)

##########################################################
# Create train, test, validation data sets, 
## validation set = final hold-out test set)
##########################################################

## As the Covid-19 data is time series data and there is a limited amount of 
## it available, train and test set will use the data up to week 6 (90%) 
# the last 10% of the available data (weeks 7-12/2021) will be used as 
# validation set


train_set <- combined_df %>%
  filter(year_week <= "2021-06")


# Transforming the train & test set to time series format for analysis and 
# model development
train_n_test_set_data <- train_set %>%
  dplyr::select(wdate, 
                year_week, 
                nr_sympt_reported, 
                nr_reftotreat, 
                tested, 
                nr_confirmed, 
                nr_deaths, 
                vacc_peop,
                tested_per_100k,
                confirmed_per_100k,
                deaths_per_100k) %>%
  mutate(avg_daily_case_rate = confirmed_per_100k/7)



# In reality 2020 was a leap year, but with only total of 57 weeks of data, 
# it doesn't make much difference
season_duration <- 52 

## Convert the data from data frame to multivariate time series format
train_n_test_set_tsdata <- tsibble::as_tsibble(
  train_n_test_set_data, 
  key = year_week,
  index = wdate, 
  regular = T) 

# Creating the time series for train
train_n_test_set_tsdata <- ts(train_n_test_set_tsdata, start = c(2020, 8), 
                              frequency = season_duration) 

# For the forecast development we need separate train and test sets
# Train set is the first 41 weeks (80% of the development data set)
train_set_tsdata <- head(train_n_test_set_tsdata, 41)

# Test set is the last 11 weeks (20% of the development data set)
test_set_tsdata <- tail(train_n_test_set_tsdata, 11)


##########################################################
## Model performance metrics
##########################################################

## Determining the model performance using:
## RMSE (a residual mean squared error)
## Mean Absolute Error (MAE)
## Mean absolute percentage error (MAPE)

## "A forecast method that minimizes the MAE will lead to forecasts of the 
## median, while minimizing the RMSE will lead to forecasts of the mean.
## Note that the the forecast errors MAE and RMSE are on the same scale 
## as the data.
## Percentage errors (MAPE) have the advantage of being unit-free, and so are 
## frequently used to compare forecast performances between data sets.However, 
## measures based on percentage errors have the disadvantage of being 
## infinite or undefined if training data observation in the period of interest 
## are zero,  have extreme values or are close to zero. Another problem with 
## percentage errors that is often overlooked is that they assume the unit of 
## measurement has a meaningful zero.
## A good way to choose the best forecasting model is to find the model with 
## the smallest RMSE computed using time series cross-validation."
## Source: https://otexts.com/fpp2/accuracy.html


##########################################################
## Preliminary analysis
##########################################################

## Plotting the train set time series data using autoplot

# Seasonal plot: confirmed cases of COVID-19 time series
ggseasonplot(train_n_test_set_tsdata[,"nr_confirmed"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Nr of confirmed cases") +
  ggtitle("Seasonal plot: Confirmed cases of COVID-19") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Conclusion: The plot indicates that there is seasonality in the data, however, 
# with only 1 year of data it can't easily be used in the forecasts. Also, need 
# to keep in mind that the first half of 2020 there was not enough testing 
# capacity to confirm all of the COVI-19 cases.

# Seasonal plot: Deaths of COVID-19  time series
ggseasonplot(train_n_test_set_tsdata[,"nr_deaths"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Nr of deaths") +
  ggtitle("Seasonal plot: Nr of deaths")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Looking at the cases vs. deaths  peaks aligned with the same weeks
autoplot(train_n_test_set_tsdata[,6:7], facets=TRUE) +
  ylab("COVID-19 data")+
  geom_vline(xintercept = 2020.25, colour="red") +
  geom_vline(xintercept = 2020.29, colour="blue") +
  geom_vline(xintercept = 2020.345, colour="blue") +
  geom_vline(xintercept = 2020.77, colour="red") +
  geom_vline(xintercept = 2020.903, colour="red") +
  geom_vline(xintercept = 2020.924, colour="blue") +
  geom_vline(xintercept = 2020.942, colour="red") +
  geom_vline(xintercept = 2021.02, colour="red") +
  geom_vline(xintercept = 2021.057, colour="blue") +
  geom_vline(xintercept = 2021.077, colour="red") +
  ggtitle("Faceted data points aligned by week") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Looking at the different COVID-19 related data sets aligned with the same weeks
autoplot(train_n_test_set_tsdata[,3:8], facets=TRUE) +
  ylab("COVID-19 data")+
  geom_vline(xintercept = 2020.25) +
  ggtitle("Faceted data points aligned by week") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Looking at all the data points together
autoplot(train_n_test_set_tsdata[,3:8]) +
  ylab("Weekly number") + xlab("Week")+
  ggtitle("Combined data points")

# To see the relationships between the different COVID-19 data points in the 
# time series, I plotted each of them against the others.
# These plots are arranged in a scatter plot matrix
GGally::ggpairs(as.data.frame(train_n_test_set_tsdata[,3:8]))

# Conclusion: As the first three data points are used to determine who should be 
# tested it is little surprise that they have fairly high correlation. Number of
# people vaccinated is so low and has been on going for such a short time, that
# it is not going to have much effect on anything yet.

# Looking at the confirmed cases and deaths per 100 000 inhabitants
autoplot(train_n_test_set_tsdata[,10:11]) +
  ylab("Weekly number") + xlab("Week")+
  ggtitle("The confirmed cases and deaths per 100k")

# Adjusted case rate seven-day average for cases per 100 000 per 100 000 inhabitants
autoplot(train_n_test_set_tsdata[,12]) +
  geom_hline(yintercept = 1, colour="yellow") +
  geom_hline(yintercept = 4, colour="orange") +
  geom_hline(yintercept = 7, colour="red") +
  ylab("Adjusted daily rate") + xlab("Week")+
  ggtitle("The daily confirmed Covid-19 case rate per 100k")



# Lagged plots: the horizontal axis shows lagged values of the time series
confirmed_covid <- window(train_n_test_set_tsdata[,"nr_confirmed"], start=c(2020,8))
gglagplot(confirmed_covid)

# Conclusion: Highest correlation with lag 1


# Plotting autocorrelation. 
# Autocorrelation (ACF) measures the linear relationship between lagged values of 
# a time series
# Autocorrelation: confirmed cases of COVID-19 time series
ggAcf(train_n_test_set_tsdata[,"nr_confirmed"])

# Conclusion: For the first 11 lags the correlations are significantly 
# different from zero


## Theory states: When data has a trend, the autocorrelations for small lags 
## tend to be large and positive because observations nearby in time are also 
## nearby in size. So the ACF of trended time series tend to have positive 
## values that slowly decrease as the lags increase 
## => Conclusion: Confirmed cases data appears to have a trend
## (source: https://otexts.com/fpp2/autocorrelation.html)

## These graphs show the challenges that the forecasting models will phase: 
# - Data appears to have seasonality, but there is only one year's data, 
# so it will be difficult to take into account
# - Data is incomplete as there was not enough testing capacity at the 
# beginning of the pandemic and only the severe cases got tested.

# To a novice in a time series forecasting this will be true challenge to build 
# a model that is able to forecast with any accuracy the change in the trend.

##########################################################
## Model development
##########################################################


#############
## 1. Model: Naïve

## To get started I'll create the baseline forecast on naive(), which forecasts
## the last observed value. 
## Definition of naive method: For naïve forecasts, we simply set all forecasts 
## to be the value of the last observation

### On the number of confirmed COVID-109 cases, h= 11 i.e. 11 weeks
forecast_naive_case <- forecast(naive(train_set_tsdata[,"nr_confirmed"], h = 11))

# Fit the model function to be used with time series cross-validation to 
# determine model with lowest cross-validation RMSE
forecast_naive <- function(x, h){forecast(naive(x, h = h))}

# Using tsCV() function for time series cross-validation
errors_naive <- tsCV(train_n_test_set_tsdata[,"nr_confirmed"], 
                     forecast_naive, h=11)

# Calculate the RMSE of the tsCV results
RMSE_naive <- sqrt(mean(errors_naive^2, na.rm=TRUE))


## Residuals are useful in checking whether a model has adequately captured the 
## information in the data. A good forecasting method will yield residuals with 
## the following properties:
## - The residuals are uncorrelated. If there are correlations between 
## residuals, then there is information left in the residuals which should be 
## used in computing forecasts.
## - The residuals have zero mean. If the residuals have a mean other than zero, 
## then the forecasts are biased.
## In addition to these essential properties, it is useful (but not necessary) 
## for the residuals to also have the following two properties.
## - The residuals have constant variance.
## - The residuals are normally distributed.
## Source: https://otexts.com/fpp2/residuals.html

# Calculating residuals
res_naive <- residuals(forecast_naive_case)

# Mean of residuals
mean(res_naive, na.rm=TRUE)
# Conclusion: Mean of the residuals is not zero.

# Checking residuals with checkresiduals() which produces:
# - a time plot, 
# - ACF plot and histogram of the residuals (with an overlaid normal 
# distribution for comparison)
# - a Ljung-Box test with the correct degrees of freedom

checkresiduals(forecast_naive_case)

# Conclusion:
# - The variations in the residuals do not stay the same over time
# - ACF of residuals suggests that naive model hasn't captured all 
# the elements affecting the forecast 
# - Residuals do not appear to be normally distributed.
# - from Ljung-Box test is that as the p-value is small (under 0,05), I can 
# conclude that the residuals are distinguishable from a white noise series.

# plot results forecast results
plot_naive <- autoplot(forecast_naive_case,  ylab="Test set") + 
  autolayer(test_set_tsdata[,"nr_confirmed"])
plot_naive

# Conclusion: There are elements left to use in forecasting that the naive 
# model does not capture. 
# The whole forecasting window is not within the 95% prediction 
# interval. However, the last values of the test
# set are within the 95% prediction interval and closing on the forecast, which
# is the last observed value in the training set

# Next I'll be checking the forecast errors, i.e. the difference between 
# an observed value and its forecast. The forecast errors will be calculated on 
# the test set.

# Accuracy of the forecast
accuracy_naive_case <- accuracy(forecast_naive_case, 
                                test_set_tsdata[,"nr_confirmed"])


# Creating a results table (following the convention used in the course 8) with 
# RMSE, MAE and MAPE:
accuracy_results <- tibble(method = "Naive", 
                       forecasting = "Test set",
                       MAE = accuracy_naive_case[2,"MAE"],
                       MAPE = accuracy_naive_case[2,"MAPE"], 
                       RMSE = accuracy_naive_case[2,"RMSE"],
                       RMSE_CV = RMSE_naive)

## Conclusion: As you would expect the Naive method does not capture all the 
## elements affecting the forecast. Accuracy of the method using MAE 
## are off by about 820 confirmed cases.

# Display accuracy results: 
accuracy_results %>% 
  arrange(RMSE)

#############
## 2. Model:  Simple Exponential Smoothing

# Simple Exponential Smoothing
# Exponential Smoothing methods are an extension of the naive method, wherein 
# the forecasts are produced using weighted averages of past observations, with 
# the weights decaying exponentially as the observations get older. In simple 
# words, higher weights are given to the more recent observations and vice 
# versa. The value of the smoothing parameter for the level is decided by the 
# parameter 'alpha'.
# Source: https://www.pluralsight.com/guides/time-series-forecasting-using-r

#Forecast for 11 weeks (h - periods)
forecast_ses_conf_case <- forecast(ses(train_set_tsdata[,"nr_confirmed"], h = 11))

# Fit the model function to be used with time series cross-validation to 
# determine model with lowest cross-validation RMSE
forecast_ses <- function(x, h){forecast(ses(x, h = h))}

# Using tsCV() function for time series cross-validation
errors_ses <- tsCV(train_n_test_set_tsdata[,"nr_confirmed"], 
                   forecast_ses, h=11)

# Calculate the RMSE of the tsCV results
RMSE_ses <- sqrt(mean(errors_ses^2, na.rm=TRUE))
 
# Calculating residuals
res_ses <- residuals(forecast_ses_conf_case)

# Mean of residuals
mean(res_ses, na.rm=TRUE)
# Conclusion: Mean of the residuals is not zero.

# Checking residuals with checkresiduals()
checkresiduals(forecast_ses_conf_case)

# Conclusion:
# - The variations in the residuals do not stay the same over time
# - ACF of residuals suggests that naive model has not captured all 
# the elements affecting the forecast 
# - Residuals do not appear to be normally distributed.


# plot results and the test data
plot_ses <-  autoplot(forecast_ses_conf_case,  ylab="Test set") + 
  autolayer(test_set_tsdata[,"nr_confirmed"])
plot_ses

# accuracy of the forecast
accuracy_ses_cons_case <- accuracy(forecast_ses_conf_case, 
                                   test_set_tsdata[,"nr_confirmed"])


#Adding the results as a new row to the RMSE tibble
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "Simple exponential smoothing", 
                                     forecasting = "Test set",  
                                     MAE = accuracy_ses_cons_case[2,"MAE"],
                                     MAPE = accuracy_ses_cons_case[2,"MAPE"],
                                     RMSE = accuracy_ses_cons_case[2,"RMSE"], 
                                     RMSE_CV = RMSE_ses))


# Display accuracy results: 
accuracy_results %>% 
  arrange(RMSE)


#############
## 3. Model: Auto ARIMA

## Researching online for models used in times series, and in COVID-19 
## time series in particular, model used was ARIMA. As I have little experience with time series 
## analysis, I'll see how those models work on my data. 
## Reference: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0244173

## Definition of ARIMA: When differencing is combined with autoregression and 
## a moving average model, we obtain a non-seasonal ARIMA model. 
## ARIMA = AutoRegressive Integrated Moving Average (in this context, 
## integration is the reverse of differencing). 
## Source: https://otexts.com/fpp2/non-seasonal-arima.html

# Confirmed cases per auto ARIMA forecast without cross-validation
fit_arima_conf_case <- auto.arima(train_set_tsdata[,"nr_confirmed"])

# forecast with auto ARIMA for the next 11 weeks (h - periods)
forecast_arima_conf_case <- forecast(fit_arima_conf_case, h=11)


# Fit the model function to be used with time series cross-validation to 
# determine model with lowest cross-validation RMSE
forecast_arima <- function(x, h){forecast(auto.arima(x), h = h)}

# Using tsCV() function for time series cross-validation
errors_arima <- tsCV(train_n_test_set_tsdata[,"nr_confirmed"], 
                     forecast_arima, h=11)

# Calculate the RMSE of the tsCV results
RMSE_arima <- sqrt(mean(errors_arima^2, na.rm=TRUE))

# Calculating residuals
res_arima <- residuals(forecast_arima_conf_case)

# Mean of residuals
mean(res_arima, na.rm=TRUE)
# Conclusion: Mean of the residuals is not zero.


# Checking residuals with checkresiduals()
checkresiduals(forecast_arima_conf_case)
# Conclusion:
# - The variations in the residuals do not stay the same over time
# - ACF of residuals suggests that Arima model has captured all 
# the elements affecting the forecast 
# - Residuals do not appear to be normally distributed.
# - from Ljung-Box test is that as the p-value is over 0.05, I can conclude 
# that the residuals are not distinguishable from a white noise series.
# - The prediction intervals for ARIMA models assume that the residuals are 
# uncorrelated and normally distributed. As neither of these assumptions holds
# the the prediction intervals may be incorrect.

# plot results and the test data
plot_arima <- autoplot(forecast_arima_conf_case,  ylab="Test set") + autolayer(test_set_tsdata[,"nr_confirmed"])
plot_arima

# Conclusion: the plot confirms that the test data does not fit in the 
# 95% prediction interval

# accuracy of the forecast
accuracy_arima_conf_case <- accuracy(forecast_arima_conf_case, 
                                     test_set_tsdata[,"nr_confirmed"])


#Adding the results as a new row to the RMSE tibble
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "Auto ARIMA", 
                                     forecasting = "Test set",  
                                     RMSE = accuracy_arima_conf_case[2,"RMSE"], 
                                     MAE = accuracy_arima_conf_case[2,"MAE"],
                                     MAPE = accuracy_arima_conf_case[2,"MAPE"],
                                     RMSE_CV = RMSE_arima))


# Display accuracy results: 
accuracy_results %>% 
  arrange(RMSE)

#############
## 4. Multivariate time series using VAR

# The VAR method name comes from vector autoregression. So far all the other models I have used have # been univariate and forecasting the nr of confirmed COVID-19 cases based on the based observations # of the its own time series data. Next I used the VAR method to create a multivariate time series 
# model that uses all but people vaccinated: 
# - nr of tested,
# - nr of deaths,
# - nr of symptoms reported
# - nr of urgently referred to treatment.

# Using VARselect to determine the information criteria and FPE for different VAR(p)
select <- VARselect(train_set_tsdata[, 3:7], lag.max = 11, type="both")
select[["selection"]]

# -> Both SC and AIC recommend using the lag length 6

# Multivariate analysis using VAR()
fit_mv6 <- vars::VAR(train_set_tsdata[, 3:7], p=6, type="both")

# Testing the model for autocorrelation in the errors using a portmanteau test 
# for a model with no autocorrelation.
serial.test(fit_mv6, lags.pt=10, type="PT.asymptotic")

# Conclusion - Doesn't work

# Multivariate analysis using VAR()
fit_mv5 <- vars::VAR(train_set_tsdata[, 3:7], p=5, type="both")

# Testing the model for autocorrelation in the errors using a portmanteau test 
# for a model with no autocorrelation.
serial.test(fit_mv5, lags.pt=10, type="PT.asymptotic")

# p-value still under the significance level alpha of 0.05. 

# Multivariate analysis using VAR()
fit_mv4 <- vars::VAR(train_set_tsdata[, 3:7], p=4, type="both")

# Testing the model for autocorrelation in the errors using a portmanteau test 
# for a model with no autocorrelation.
serial.test(fit_mv4, lags.pt=10, type="PT.asymptotic")

# p-value still under the significance level alpha of 0.05. 

# Multivariate analysis using VAR()
fit_mv3 <- vars::VAR(train_set_tsdata[, 3:7], p=3, type="both")

# Testing the model for autocorrelation in the errors using a portmanteau test 
# for a model with no autocorrelation.
test <- serial.test(fit_mv3, lags.pt=10, type="PT.asymptotic")
test[["serial"]][["p.value"]]

# Forecasts for 11 weeks
forecast_mv <- forecast(fit_mv3, h=11)


# Conclusion: The null hypothesis of no autocorrelation is accepted, since the 
# p-value is higher than the significance level alpha of 0.05. 


# plot results and the test data
mv_forecast_plot <- autoplot(forecast_mv[["forecast"]][["nr_confirmed"]],  
                             ylab="Test set")+ 
  autolayer(test_set_tsdata[,"nr_confirmed"])
mv_forecast_plot

# accuracy of the forecast
accuracy_forecast_mv <- accuracy(forecast_mv, test_set_tsdata[, 3:7], d=0 , D=1)


#Adding the results as a new row to the RMSE tibble
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "VAR", 
                                     forecasting = "Test set",  
                                     RMSE = accuracy_forecast_mv[8,"RMSE"], 
                                     MAE = accuracy_forecast_mv[8,"MAE"],
                                     MAPE = accuracy_forecast_mv[8,"MAPE"]))

# Display accuracy results: 
accuracy_results %>% 
  arrange(RMSE)


######
## Forecast with combination
######

# Similarly to how in predictions, the combinations of models produced best 
# results, I'm trying the same for the forecast as the theory says it works here 
# too. (source: https://otexts.com/fpp2/combinations.html) 

# Calculating the mean of all of the models
mean_naive <- forecast_naive_case[["mean"]]
mean_ses <- forecast_ses_conf_case[["mean"]]
mean_arima <- forecast_arima_conf_case[["mean"]]
mean_var <- forecast_mv[["forecast"]][["nr_confirmed"]][["mean"]]

# Creating the combined model from the means
Combination <- (mean_naive + mean_ses + mean_arima + mean_var)/4


# plot results and the test data
plot_combined <- autoplot(Combination, series="Combination mean point forecast", ylab="Test set") + 
  autolayer(train_set_tsdata[,"nr_confirmed"]) + 
  autolayer(test_set_tsdata[,"nr_confirmed"])  
plot_combined



# accuracy of the forecast
accuracy_combined_conf_case <- accuracy(Combination, 
                                     test_set_tsdata[,"nr_confirmed"])


#Adding the results as a new row to the RMSE tibble
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "Combination", 
                                     forecasting = "Test set",  
                                     RMSE = accuracy_combined_conf_case[1,"RMSE"], 
                                     MAE = accuracy_combined_conf_case[1,"MAE"],
                                     MAPE = accuracy_combined_conf_case[1,"MAPE"]))


# Display accuracy results: 
accuracy_results %>% 
  arrange(RMSE)


#########################################################################
## Results
#########################################################################

# Visualizing the individual models and the combined model
autoplot(train_set_tsdata[,"nr_confirmed"]) +
  autolayer(forecast_naive_case, series="Naïve", PI=FALSE) +
  autolayer(forecast_ses_conf_case, series="SES", PI=FALSE) +
  autolayer(forecast_arima_conf_case, series="ARIMA", PI=FALSE) +
  autolayer(forecast_mv[["forecast"]][["nr_confirmed"]], series="VAR", PI=FALSE)+
  autolayer(Combination, series="Combination") +
  autolayer(test_set_tsdata[,"nr_confirmed"], series="Test data") +
  xlab("Time") + ylab("Nr of cases confirmed") +
  ggtitle("Comparison of the forecasts across models")

  
## Comparing all models with error metrics
accuracy_results %>% 
  arrange(RMSE)

## Conclusion: The multivariate VAR performed worst on all metrics and Simple 
## Exponential Smoothing performed best on all other than the RMSE of time 
## series cross-validation, but the difference to the Naïve model is tiny. 
##  
## None of the forecasting models did particularly well with the best two 
## models having Mean absolute percentage error (MAPE) at best just under 42%
## and the Mean Absolute Error (MAE) being off by about 820 cases confirmed.

#########################################################################
## Validating the model results on the validation data set
#########################################################################


## As the Covid-19 data is time series data and there is a limited amount of 
## it available, test and validation sets were created by taking specified weeks 
## out of the training data set and used as test and validation data.

# Train and test set will use the data up to week 6 (90%) 
# the last 10% of the available data (weeks 7-12/2021) will be used as 
# validation set

# The full data set up to week 12/2021
covid_set <- combined_df %>%
  filter(year_week <= "2021-12") 


# Transforming the data set to time series format
covid_set_data <-covid_set %>%
  dplyr::select(wdate, 
                year_week, 
                nr_sympt_reported, 
                nr_reftotreat, 
                tested, 
                nr_confirmed, 
                nr_deaths, 
                vacc_peop,
                tested_per_100k,
                confirmed_per_100k,
                deaths_per_100k) %>%
  mutate(avg_daily_case_rate = confirmed_per_100k/7)



# In reality 2020 was a leap year, but with only total of 57 weeks of data, 
# it doesn't make much difference
season_duration <- 52 

## Convert the data from data frame to time series format
covid_set_tsdata <- tsibble::as_tsibble(
  covid_set_data, 
  key = year_week,
  index = wdate, 
  regular = T) 

# Creating the time series for train
covid_set_tsdata <- ts(covid_set_tsdata, start = c(2020, 8), 
                              frequency = season_duration) 

# For the simple forecasts we need separate train and test sets
# Development data set is the first 52 weeks (90% of the data set)
dev_tsdata <- head(covid_set_tsdata, 52)

# Validation data set is the last 6 weeks (10% of the data set)
validation_tsdata <- tail(covid_set_tsdata, 6)


### For the validation of the forecasting model of the confirmed COVID-19 cases 
### I'll be using the the Naïve as it performed 
### best on the test data

### Naive forecast for 6 weeks (h - periods)
forecast_naive_val <- forecast(naive(dev_tsdata[,"nr_confirmed"], h = 6))


# plot results and the validation data
autoplot(forecast_naive_val) + autolayer(validation_tsdata[,"nr_confirmed"])

# Fit the model function to be used with time series cross-validation to 
# determine model with lowest cross-validation RMSE
forecast_naive_v <- function(x, h){forecast(naive(x, h = h))}

# Using tsCV() function for time series cross-validation
errors_naive_v <- tsCV(dev_tsdata[,"nr_confirmed"], 
                     forecast_naive_v, h=6)

# Calculate the RMSE of the tsCV results
RMSE_naive_v <- sqrt(mean(errors_naive_v^2, na.rm=TRUE))

# accuracy of the forecast
accuracy_forecast_cc_validation <- accuracy(forecast_naive_val, 
                                            validation_tsdata[,"nr_confirmed"])


#Adding the results as a new row to the RMSE tibble
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "Naive", 
                                     forecasting = "Validation set",  
                                     RMSE = accuracy_forecast_cc_validation[2,"RMSE"], 
                                     MAE = accuracy_forecast_cc_validation[2,"MAE"],
                                     MAPE = accuracy_forecast_cc_validation[2,"MAPE"],
                                     RMSE_CV = RMSE_naive_v))


# Display accuracy results: 
accuracy_results %>% 
  arrange(RMSE)


# Visualizing the individual models from the development phase with the actual 
# data
autoplot(train_set_tsdata[,"nr_confirmed"]) +
  autolayer(forecast_naive_case, series="Naïve", PI=FALSE) +
  autolayer(forecast_ses_conf_case, series="SES", PI=FALSE) +
  autolayer(forecast_arima_conf_case, series="ARIMA", PI=FALSE) +
  autolayer(forecast_mv[["forecast"]][["nr_confirmed"]], series="VAR", PI=FALSE)+
  autolayer(Combination, series="Combination") +
  autolayer(covid_set_tsdata[,"nr_confirmed"],  series="Actual data") +
  xlab("Time") + ylab("Nr of cases confirmed") +
  ggtitle("Comparison of the model forecasts with training set data")

# Just to see if any of the models would have performed better, I'm running a 
# visual comparison of the developed models using the full development data set 
# to see, if any of them were doing better than the Naïve model. 

# Recalculating the all of the  models on the whole development data with same 
# parameters
forecast_ses_val <- forecast(ses(dev_tsdata[,"nr_confirmed"], h = 6))
forecast_arima_val <- forecast(auto.arima(dev_tsdata[,"nr_confirmed"]), h=6)
forecast_mv_val <- forecast(VAR(dev_tsdata[, 3:7], p=3, type="both"), h=6)

# Calculating the mean of all of the models
mean_naive_val <- forecast_naive_val[["mean"]]
mean_ses_val <- forecast_ses_val[["mean"]]
mean_arima_val <- forecast_arima_val[["mean"]]
mean_var_val <- forecast_mv_val[["forecast"]][["nr_confirmed"]][["mean"]]


# Creating the combined model from the means
Combination_val <- (mean_naive_val + mean_ses_val + mean_arima_val + 
                      mean_var_val)/4

# Visualizing the individual models and the combined model
autoplot(dev_tsdata[,"nr_confirmed"]) +
  autolayer(forecast_naive_val, series="Naïve", PI=FALSE) +
  autolayer(forecast_ses_val, series="SES", PI=FALSE) +
  autolayer(forecast_arima_val, series="ARIMA", PI=FALSE) +
  autolayer(forecast_mv_val[["forecast"]][["nr_confirmed"]], series="VAR", PI=FALSE)+
  autolayer(Combination_val, series="Combination val") +
  autolayer(covid_set_tsdata[,"nr_confirmed"],  series="Actual data") +
  xlab("Time") + ylab("Validation set") +
  ggtitle("Comparison of the forecasts with actual data across models")

# Conclusion: None of the models improved much on the mean point forecast with 
# the addition of the test data 

# Seasonal plot: confirmed cases of COVID-19 time series all data upto week 12/2021
ggseasonplot(covid_set_tsdata[,"nr_confirmed"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Nr of confirmed cases") +
  ggtitle("Seasonal plot: Confirmed cases of COVID-19")

# Adjusted case rate seven-day average for cases per 100 000 per 100 000 inhabitants
autoplot(covid_set_tsdata[,12]) +
  geom_hline(yintercept = 1, colour="yellow") +
  geom_hline(yintercept = 4, colour="orange") +
  geom_hline(yintercept = 7, colour="red") +
  ylab("Adjusted daily rate") + xlab("Week")+
  ggtitle("The confirmed Covid-19 case rate per 100k", 
          subtitle ="Adjusted case rate seven-day average")



