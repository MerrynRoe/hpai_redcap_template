# Merging the unique links and contact details from RedCap to input to SMS messaging system
# For symptom follow up

# Load packages
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2
)

# Import updated REDCap data from API or adapt to be from a recent export

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

# Clean all date variables
date_vars <- redcap %>%
  select(contains("date"), - email_updated) %>%
  names()

redcap <- redcap %>%
  mutate(across(all_of(date_vars), as.Date))

# Calculate date_last_exposure for each ip
redcap <- redcap %>%
  rowwise() %>%
  mutate(
    last_exposure_date_ip1 = {
      vals <- c_across(contains("date_ip1"))
      if (all(is.na(vals))) NA else max(vals, na.rm = TRUE)
    },
    last_exposure_date_ip2 = {
      vals <- c_across(contains("date_ip2"))
      if (all(is.na(vals))) NA else max(vals, na.rm = TRUE)
    }
  ) %>%
  ungroup()

# Don't forget to check your data!
redcap %>%
  select(record_id, sms_consent, contains("date_ip1")) %>%
  view()

# Cleaning further
contact_dat <- redcap %>%
  select(
    record_id,
    sms_consent,
    phone,
    contact_number,
    first_name, ## Note using first_name (PHO verified) instead of contact_name (provided by facility)
    last_exposure_date_ip1, # This is last date of any exposure, helpful for SitReps
    last_exposure_date_ip2,
    high_risk_exposure_date_ip1, #This is last date of PHO assigned high risk exposure and is used for SMS filtering
    high_risk_exposure_date_ip2
  ) 

# There are still multiple rows per individual
contact_dat <- contact_dat %>%
  arrange(record_id, desc(high_risk_exposure_date_ip1)) %>% # Sort with most recent exposure date/s first (might need checking per IP site)
  # One row per individual and back fill from repeat instances
  group_by(record_id) %>%
  summarise(
    across(everything(), ~ {
      x <- .
      if (all(is.na(x))) NA else x[which(!is.na(x))[1]]
    }),
    .groups = "drop"
  ) %>%
  filter(sms_consent == 1) # ensuring we only contact those who have consented for further SMS followup

# Use new PHO verified phone number, if missing revert back to original contact number
contact_dat <- contact_dat %>%
  mutate(phone = as.character(phone),
         contact_number = as.character(contact_number)) %>%
  mutate(contact_number = case_when(
    !is.na(phone) ~ phone,
    is.na(phone) ~ contact_number,
    TRUE ~ NA
  )) %>%
  select(-phone)

# REDCap unique survey link
## As per Work instructions Section 12.3 (a) update line below to reflect updated .csv ##
## remember to name the export '_fu_link' to avoid confusion ##
link_dat <- read.csv(here::here("raw_data", "HPAIExposureManagement_Participants_2026-08-24_0947_fu_link.csv")) # Update with most recent version

link_dat <- link_dat %>%
  clean_names() %>%
  rename(record_id = record) %>%
  filter(survey_link != "")

# Merge data
merged_data <- left_join(contact_dat, link_dat, by = "record_id") %>%
  mutate(high_risk_exposure_date_ip1 = as.Date(high_risk_exposure_date_ip1),
         high_risk_exposure_date_ip2 = as.Date(high_risk_exposure_date_ip2),
         last_exposure_date_all =  pmax(high_risk_exposure_date_ip1, high_risk_exposure_date_ip2, na.rm = TRUE))

# Filter to last high risk exposure in the last 10 days
date_10_days_ago <- Sys.Date() - 10 # Calculate the date 10 days ago from today

genesis <- merged_data %>%
  filter(last_exposure_date_all >= date_10_days_ago) %>% # Filter based on the 10 day cutoff defined above
  select(first_name, contact_number, survey_link) %>%
  mutate(contact_number = paste0("+", contact_number))%>% # Format phone number for genesis
  mutate(first_name = str_to_title(first_name))

## Export sheet ready for genesis
### Note if you open this excel sheet the contact number formatting breaks - check in R not in excel ### 

write.csv(genesis, file = here::here("outputs", paste0("sms_list_sympt_fu_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")), row.names = FALSE)

