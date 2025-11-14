# Importing facility contact lists and matching to exhisting REDCap IDs

# Load packages
# Loading packagaes
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2
)

# Import caselist - ensure most upto date version
dat_contacts_facility_a <- read.csv(here::here("raw_data", "20251114_facilty_a_contact_list.csv"))

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

max_id <- max(redcap$record_id, na.rm = TRUE) # We will need this to add record_id numbers

# Match on key feilds, ie first name and mobile number

dat_contacts_facility_a <- dat_contacts_facility_a %>%
  mutate(contact_list_flag_a = 1) %>% # Create contact list flag, ensure variable is updated per facility
  mutate(
    phone_clean = phone %>%
      str_remove_all("[^0-9]") %>%          # remove spaces, +, brackets, etc.
      str_replace("^0", "") %>%             # drop leading 0 (e.g., 04...)
      str_replace("^61", "61") %>%          # ensure consistent 61 prefix
      { ifelse(str_starts(., "4"), paste0("61", .), .) } # add "61" if missing
  ) %>%
  mutate(first_name_clean = str_to_lower(first_name))

redcap <- redcap %>%
  mutate(
    phone_clean = phone %>%
      str_remove_all("[^0-9]") %>%          # remove spaces, +, brackets, etc.
      str_replace("^0", "") %>%             # drop leading 0 (e.g., 04...)
      str_replace("^61", "61") %>%          # ensure consistent 61 prefix
      { ifelse(str_starts(., "4"), paste0("61", .), .) } # add "61" if missing
  ) %>%
  mutate(first_name_clean = str_to_lower(first_name)) 

redcap_id_key <- redcap %>%
  select(record_id, first_name_clean, phone_clean)

dat_contacts_facility_a <- dat_contacts_facility_a %>%
  left_join(redcap_id_key,
            by = c("first_name_clean", "phone_clean")) 

redcap_filled <- dat_contacts_facility_a %>%
  arrange(-is.na(record_id), record_id) %>%
  mutate(
    record_id = if_else(
      is.na(record_id),
      max_id + row_number(),   # sequential values
      record_id
    )
  ) %>%
  select(-phone, -first_name) %>%
  rename(phone = phone_clean,
         first_name = first_name_clean) %>%
  mutate(first_name = str_to_title(first_name)) %>%   # Capitalise first letter
  select(record_id, everything())

# Export .csv to import into REDCap
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

write.csv(
  redcap_filled,
  here::here("outputs", paste0(timestamp, "_import_to_redcap.csv")),
  row.names = FALSE
)