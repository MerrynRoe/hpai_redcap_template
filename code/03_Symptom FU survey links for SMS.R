# Merging the unique links and contact details from RedCap to input to SMS messaging system
# For symptom follow up

# Load packages
# Loading packagaes
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2
)

# RedCap data from exports

# REDCap contact data
# Import REDCap case list
source(here::here("code", "api_tokens.R"))

# Using API
#!/usr/bin/env Rscript
token <- api_token
url <- "https://redcap.gvhealth.org.au/redcap/api/"
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='true',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
redcap <- httr::content(response)

contact_dat <- redcap %>%
  select(
    record_id,
    sms_consent,
    phone,
    first_name, 
    date_last_exposure_a, 
    date_last_exposure_b
    # re-exposed self-report? create new date variable?
  ) %>%
  filter(sms_consent == 1)

# REDCap unique survey link
link_dat <- read.csv(here::here("raw_data", "HPAISurvey_Participants_2025-11-19_1242.csv")) # Update with most recent version

link_dat <- link_dat %>%
  clean_names() %>%
  rename(record_id = record) %>%
  filter(survey_link != "")

# Merge data
merged_data <- left_join(contact_dat, link_dat, by = "record_id") %>%
  mutate(date_last_exposure_a = as.Date(date_last_exposure_a),
         date_last_exposure_b = as.Date(date_last_exposure_b),
         date_last_exposre_all =  pmax(date_last_exposure_a, date_last_exposure_b, na.rm = TRUE))

# Filter to last high risk exposure in the last 10 days
date_10_days_ago <- Sys.Date() - 10 # Calculate the date 10 days ago from today

essendex <- merged_data %>%
  filter(date_last_exposre_all >= date_10_days_ago) %>%
  select(first_name, phone, survey_link) %>%
  mutate(phone = paste0("+", phone)) # Format phone number for essendex

write.csv(essendex, file = here::here("outputs", paste0("sms_list_sympt_fu_", format(Sys.time(), "%Y%m%d"), ".csv")), row.names = FALSE)
