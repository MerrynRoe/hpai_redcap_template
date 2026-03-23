# Merging the unique links and contact details from RedCap to input to SMS messaging system
# For traige survey
# Ensure facility case list has been processed and uploaded to REDCap as per 01_...

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
    exposure_ip1_yn, 
    exposure_ip1_yn
    # re-exposed self-report? create new date variable?
  ) %>%
  filter(sms_consent != 0 | is.na(sms_consent)) %>%
  filter(!is.na(first_name))

# REDCap unique survey link
# Download from survey distribution tools, ensure it is the correct survey and you add '_triage_link' to doc name to avoid confusion
link_dat <- read.csv(here::here("raw_data", "HPAISurvey_Participants_2025-11-26_1055_triage_link.csv")) # Update with most recent version

link_dat <- link_dat %>%
  clean_names() %>%
  rename(record_id = record) %>%
  filter(survey_link != "")

# Merge data
merged_data <- left_join(contact_dat, link_dat, by = "record_id")


# Filter to send triage out if any triage exposure Qs are missing, this should capture when we create new site/exposures
# Final essendex list

essendex <- merged_data %>%
  #filter(is.na(exposure_ip1_yn) | is.na(exposure_ip2_yn)) %>%
  filter(is.na(exposure_ip1_yn)) %>%
  select(first_name, phone, survey_link) %>%
  mutate(phone = paste0("+", phone)) # Format phone number for essendex

write.csv(essendex, file = here::here("outputs", paste0("sms_list_triage_", format(Sys.time(), "%Y%m%d"), ".csv")), row.names = FALSE)

