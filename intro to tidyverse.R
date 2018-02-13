library(tidyverse)
install.packages('lubridate')
library(lubridate)

getwd()
setwd('/Users/popester')

# donor <- read.csv('data/small_donations_data.csv') %>% 
# mutate(receipt_year = receipt_date %>% mdy() %>% year())

donor <- read.csv('https://raw.githubusercontent.com/gbearden/r_course_evans_school/master/data/small_donations_data.csv')

police <- read.csv('https://raw.githubusercontent.com/gbearden/r_course_evans_school/master/data/small_police_data.csv')

donor %>% head(1)

police %>% head(1)

donor %>% 
  select(id, amount, type, party, receipt_date) %>%
  select(id, receipt_date, everything())

x <- c(1,2,3)
y <- c(4,5,1)

ifelse(x>y, 'yes','no')
# this is saying if x is greater than y, display yes, or else no

donor %>%
  mutate(
    amount_per_day = (amount / 365) %>% round(2)
    , amount_perc_max = (amount / max(amount, na.rm = TRUE)) %>% round(2)
    , amount_candidate = ifelse(type %in% 'Candidate', amount, NA)
    , amount_committee = ifelse(type %in% 'Political Committee', amount, NA)
  )

police %>%
  mutate(
    parking_violation = ifelse(event_clearance_subgroup %in% 'PARKING VIOLATIONS', 1, 0)
    , incident_location = ifelse(parking_violation %in% 1, incident_location, NA)
    ) %>%
  select(event_clearance_subgroup, parking_violation, incident_location) %>%
  head()

donor %>%
  mutate(
    amount_per_day = (amount / 365)
    , amount_candidate = ifelse(type %in% 'Candidate', amount, NA)
    , amount_committee = ifelse(type %in% 'Political COmmittee', amount, NA)
  ) %>%
  select(amount_per_day, amount_candidate, amount_committee)

donor %>%
  rename(
    donor_id = id
    , donor_f_name = first_name
    , donor_l_name = last_name
    ) %>%
  as.data.frame() %>%
  head(1)

police %>%
  transmute(
    general_offense_number
    , event_location = incident_location
    , initial_type_group = ifelse(initial_type_group %in% NA, 'Unknown', as.character(initial_type_group))
  )

donor_100 <- donor %>% filter(amount > 100)

donor %>%
  filter(amount > 100 & type %in% 'Candidate') %>%
  select(id, amount, type) %>%
  head(2)

donor %>% nrow()

donor %>%
  transmute(
    id
    , candidate_flag = ifelse(type %in% 'Candidate', 1, 0)
    , party
    , receipt_date = receipt_date %>% mdy()
  ) %>%
  filter(
    receipt_date >= '2017-01-01' &
      ! party %in% c('REPUBLICAN', 'DEMOCRAT')
  ) %>%
  sample_n(4)

donor %>% 
  gather(column_name, year, c(receipt_year, election_year)) %>%
  select(contributor_name, column_name, year) %>% sample_n(100)

donor %>% 
  spread(receipt_year, amount) 

donor %>% 
  spread(receipt_year, amount) %>% 
  filter(contributor_name %in% c('BEZOS JACKIE', 'COSTCO')) %>% 
  select(26, 37:50) %>% as.data.frame()

police %>%
  gather(group, value, c(event_clearance_group, event_clearance_subgroup, initial_type_group, initial_type_subgroup)) %>%
  select(general_offense_number, group, value) %>%
  sample_n(8)
