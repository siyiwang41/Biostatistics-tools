
library(ggplot2)
library(tidyverse)
library(anytime)
library(lubridate)
library(openxlsx)
library("readxl")


covid <- read.csv("IPHIS_REPORT.csv")
mobilityCT <- read.csv("ctuid_level_weekly_20210207_20210213.csv", fileEncoding="UTF-8-BOM")


# GTA, CT population
CTpopulation <- read.xlsx("OntarioCTpopulation.xlsx", cols = c(1:4))
colnames(CTpopulation) <- c("HUID","PHU","CTID","CT_pop")


# Load link file changing UID to PHU
linkphu <- read.csv("D:/UT/Master/COVID-19/mobility/update/ON_CENSUS_wPHU.csv")

# Load link file between CSD and CT
csdct <- read_xlsx("D:/UT/Master/COVID-19/COVID spatial/data/CTs16_CSDs_v12a_04June2020_FINAL_wIRI.xlsx", sheet = 3)
length(unique(csdct$CT2016uid)) # 2377


# Check missing data problem in CT level
sum(covid$phunum_all=="Missing") # 10181
sum(is.na(covid$CENSUS_TRACT_NAME)) # 10181


# Format all dates and clean dates
covid$ACCURATE_EPISODE_DATE <- as.Date(substr(covid$ACCURATE_EPISODE_DATE, start = 1, stop = 9), "%d%b%Y")
covid$SPECIMENDATE <- as.Date(substr(covid$SPECIMENDATE, start = 1, stop = 9), "%d%b%Y")
covid$CASE_CREATED_DATE <- as.Date(substr(covid$CASE_CREATED_DATE, start = 1, stop = 9), "%d%b%Y")
covid$CASE_REPORTED_DATE <- as.Date(substr(covid$CASE_REPORTED_DATE, start = 1, stop = 9), "%d%b%Y")

min(covid$ACCURATE_EPISODE_DATE) # 1900-01-01
max(covid$ACCURATE_EPISODE_DATE) # 2021-02-25
min(covid$CASE_CREATED_DATE) # 2020-01-24
max(covid$CASE_CREATED_DATE) # 2021-02-25



# Some error accurate_episode_dates e.g. 2019-12-31, replace them with specimen_date (or report date)
# Check ACCURATE_EPISODE_DATE < "2020-01-20"
errordate <- subset(covid, ACCURATE_EPISODE_DATE < "2020-01-20")[, c("CASE_REPORTED_DATE",
                                                                     "SPECIMENDATE",
                                                                     "CASE_CREATED_DATE",
                                                                     "ACCURATE_EPISODE_DATE")]
covid$date <- if_else(covid$ACCURATE_EPISODE_DATE < "2020-01-20", 
                      if_else(covid$SPECIMENDATE < "2020-01-20", covid$CASE_REPORTED_DATE , covid$SPECIMENDATE),
                      covid$ACCURATE_EPISODE_DATE)

t1 <- min(covid$date); t2 <- max(covid$date)


# Aggregate daily covid first in CT level
dailycovid <- covid %>% 
  filter(!is.na(CENSUS_TRACT_NAME)) %>%
  arrange(CENSUS_TRACT_NAME, date) %>%
  group_by(CENSUS_TRACT_NAME, date) %>%
  summarise(I = n(),
            I_LTCH = sum(LTCH_RESIDENT=="YES")) %>%
  complete(date = seq.Date(as.Date("2019-12-29"),
                           max(covid$date), by=1),
           fill = list(I = 0,
                       I_LTCH = 0)) %>%
  mutate(N = cumsum(I),
         N_LTCH = cumsum(I_LTCH))



# Aggregate weekly covid in CT level
weeklycovid <- dailycovid %>%
  mutate(w_o_y = epiweek(ymd(date)),
         covid_wk_day_1 = cut(as.Date(date), "week", start.on.monday = FALSE),
         year = substr(covid_wk_day_1, start = 1, stop = 4)) %>%
  group_by(CENSUS_TRACT_NAME, covid_wk_day_1) %>% 
  summarise(Iwk = sum(I), 
            Iwk_LTCH = sum(I_LTCH)) %>%
  mutate(Nwk = cumsum(Iwk),
         Nwk_LTCH = cumsum(Iwk_LTCH))


# check number of CTID
length(unique(weeklycovid$CENSUS_TRACT_NAME)) #2344