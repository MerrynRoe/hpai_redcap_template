# Version: 2.0
# Date of last update: 19/10/2026

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
    phone_contact_list,
    first_name_contact_list, 
    triage_yn_ip1) # Add other triage ip sites as needed

# Import REDCap unique survey link
## As per Work instructions Section 11.8 (a) update line below to reflect the name of your new import .csv ##
link_dat <- read.csv(here::here("raw_data", "DRAFTHPAIRevisedScope151026_Participants_2026-10-19_1708.csv")) # Update with most recent version


link_dat <- link_dat %>%
  clean_names() %>%
  rename(record_id = record) %>%
  filter(survey_link != "")

# Merge data
merged_data <- left_join(contact_dat, link_dat, by = "record_id")


# Filter to send triage out if any triage exposure Qs are missing
## As per Work instructions Section 11.8 (b) update chunk below to reflect format required for your SMS system ##

genesis <- merged_data %>%
  #filter(is.na(triage_yn_ip1) | is.na(triage_yn_ip2)) %>% # Use this when second IP included and expand as additional IPs added
  filter(is.na(triage_yn_ip1)) %>%
  select(first_name_contact_list, phone_contact_list, survey_link) %>%
  mutate(contact_number = paste0("+", phone_contact_list)) %>% # Format phone number for genesis
  filter(contact_number != "+NA") %>%
  mutate(contact_name = str_to_title(first_name_contact_list))

## Export sheet ready for genesis
### Note if you open this excel sheet the contact number formatting breaks - check in R not in excel ### 

write.csv(genesis, file = here::here("outputs", paste0("sms_list_triage_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")), row.names = FALSE)
