# Importing facility contact lists and matching to existing REDCap IDs

# Load packages
# Loading packagaes
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2,
  keyring
)

# Import caselist - ensure most upto date version
dat_contacts_facility_ip1 <- read.csv(here::here("raw_data", "test_AI_contact_upload_template_20260326.csv"))

# Import REDCap case list

# Using Keyring to store APIs - run this for the first time
#keyring::key_set("hpai_redcap_token")

# Using API
#!/usr/bin/env Rscript

url <- "https://redcap.gvhealth.org.au/redcap/api/"
formData <- list("token"=keyring::key_get("hpai_redcap_token"),
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

redcap <- redcap %>%
  select(record_id, first_name, phone, exposure_ip1_yn, exposure_ip1_yn)

# Collect the maximum Record ID to assign new Record IDs for new contacts
max_id <- if (length(redcap$record_id) == 0 || all(is.na(redcap$record_id))) {
  0
} else {
  max(redcap$record_id, na.rm = TRUE) #if no values (ie first import) = 0
}

# Match on key fields, ie first name and mobile number

dat_contacts_facility_ip1 <- dat_contacts_facility_ip1 %>%
  mutate(case_list_ip1_yn = 1) %>% ### Create contact list flag, ensure variable is updated per facility ###
  mutate(
    contact_number = phone %>%
      str_remove_all("[^0-9]") %>%          # remove spaces, +, brackets, etc.
      str_replace("^0", "") %>%             # drop leading 0 (e.g., 04...)
      str_replace("^61", "61") %>%          # ensure consistent 61 prefix
      { ifelse(str_starts(., "4"), paste0("61", .), .) } # add "61" if missing
  ) %>%
  mutate(contact_name = str_to_lower(first_name)) %>%
  select(contact_name, contact_number, email)

redcap <- redcap %>%
  mutate(
    ## Todo clean entries with spaces ie 0401 088 851
    contact_number = phone %>%
      str_remove_all("[^0-9]") %>%          # remove spaces, +, brackets, etc.
      str_replace("^0", "") %>%             # drop leading 0 (e.g., 04...)
      str_replace("^61", "61") %>%          # ensure consistent 61 prefix
      { ifelse(str_starts(., "4"), paste0("61", .), .) } # add "61" if missing
  ) %>%
  mutate(contact_name = str_to_lower(first_name)) %>%
  select(contact_name, contact_number, record_id, exposure_ip1_yn)

dat_contacts_facility_ip1 <- dat_contacts_facility_ip1 %>%
  left_join(redcap,
            by = c("contact_name", "contact_number")) # Matches by phone and contact number

# Only upload new cases
dat_contacts_facility_ip1_new <- dat_contacts_facility_ip1 %>%
  filter(is.na(record_id))

dat_contacts_facility_ip1_new <- dat_contacts_facility_ip1_new %>%
   mutate(
    record_id = if_else(       # Create new REDCap ID adding to the last value of pulled data
      is.na(record_id),
      max_id + row_number(),   # sequential values
      record_id
    )
  ) %>%
  mutate(contact_name = str_to_title(contact_name)) %>%   # Capitalise first letter
  mutate(email = case_when(
    is.na(email) ~ "PHUepi.analytics@gvhealth.org.au", # email can't be missing to get a unique link, using EPI email as default
    email == "" ~ "PHUepi.analytics@gvhealth.org.au",
    !is.na(email) ~ email
  )) %>%
  select(record_id, everything(), -exposure_ip1_yn)

# Export .csv to import into REDCap
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

write.csv(
  dat_contacts_facility_ip1_new,
  here::here("outputs", paste0(timestamp, "_import_to_redcap_for_triage.csv")),
  row.names = FALSE
)
