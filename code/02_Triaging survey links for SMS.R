# Merging the unique links and contact details from RedCap to input to SMS messaging system
# For triage survey
# Ensure facility case list has been processed and uploaded to REDCap as per Set up work instructions section 11

# Load packages
# Loading packagaes
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

contact_dat <- redcap %>%
  select(
    record_id,
    sms_consent,
    contact_number,
    contact_name, 
    exposure_ip1_yn, 
    exposure_ip2_yn)

# Import REDCap unique survey link
## As per Work instructions Section 11.8 (a) update line below to reflect the name of your new import .csv ##
link_dat <- read.csv(here::here("raw_data", "HPAISurvey_Participants_2026-06-29_1419_triage_link.csv")) # Update with most recent version


link_dat <- link_dat %>%
  clean_names() %>%
  rename(record_id = record) %>%
  filter(survey_link != "")

# Merge data
merged_data <- left_join(contact_dat, link_dat, by = "record_id")


# Filter to send triage out if any triage exposure Qs are missing
## As per Work instructions Section 11.8 (b) update chunk below to reflect format required for your SMS system ##

genesis <- merged_data %>%
  #filter(is.na(exposure_ip1_yn) | is.na(exposure_ip2_yn)) %>% # Use this when second IP included and expand as additional IPs added
  filter(is.na(exposure_ip1_yn)) %>%
  select(contact_name, contact_number, survey_link) %>%
  mutate(contact_number = paste0("+", contact_number)) %>% # Format phone number for genesis
  filter(contact_number != "+NA") %>%
  mutate(contact_name = str_to_title(contact_name))

## Export sheet ready for genesis
### Note if you open this excel sheet the contact number formatting breaks - check in R not in excel ### 

write.csv(genesis, file = here::here("outputs", paste0("sms_list_triage_", format(Sys.time(), "%Y%m%d"), ".csv")), row.names = FALSE)
