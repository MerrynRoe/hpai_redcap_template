# Merging the unique links and contact details from RedCap to input to SMS messaging system
# For triaging survey

# Load packages
# Loading packagaes
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2
)

# Contact list from facilities
facility_a_dat_1 <- read.csv(here::here("raw_data", "20251114_facilty_a_contact_list.csv")) %>% # Update with most recent version
  clean_names() %>%
  # Ensure you have a phone, first_name and first_name_check variable for join
  mutate(first_name_check = str_to_lower(first_name)) %>%
  mutate(phone_clean = phone %>% 
           # remove all non-digits
           str_replace_all("[^0-9]", "") %>%
           
           # remove leading 00 (common from overseas formatting)
           str_replace("^00", "") %>%
           
           # convert +61 variants like 6104..., 061..., etc.
           str_replace("^0?61", "61") %>%
           
           # convert local mobiles 04xxxxxxxx → 614xxxxxxxx
           str_replace("^04", "614") %>%
           
           # convert mobiles written without leading 0 (e.g., 405...) → 61405...
           str_replace("^4", "614") %>%
           
           # final formatting: 61405 000 000
           {str_replace(., "^(61\\d{3})(\\d{3})(\\d{3})$", "\\1 \\2 \\3")}
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
  mutate(first_name_check = str_to_lower(first_name),
         phone = as.numeric(phone)) %>%
  mutate(phone_clean = phone %>% 
           # remove all non-digits
           str_replace_all("[^0-9]", "") %>%
           
           # remove leading 00 (common from overseas formatting)
           str_replace("^00", "") %>%
           
           # convert +61 variants like 6104..., 061..., etc.
           str_replace("^0?61", "61") %>%
           
           # convert local mobiles 04xxxxxxxx → 614xxxxxxxx
           str_replace("^04", "614") %>%
           
           # convert mobiles written without leading 0 (e.g., 405...) → 61405...
           str_replace("^4", "614") %>%
           
           # final formatting: 61405 000 000
           {str_replace(., "^(61\\d{3})(\\d{3})(\\d{3})$", "\\1 \\2 \\3")}
  )

# Join facility list to REDCap list to see if they have already been contacted for triage

contacts_joined_dat <- facility_a_dat_1 %>%
  left_join(contact_dat, by = c("phone_clean", "first_name_check"))

# Filter to send triage out if any triage exposure Qs are missing, this should capture when we create new site/exposures

