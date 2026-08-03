# Pulling all vars required for DH bulk upload template + formatting

source(here::here("code", "04_cleaning for reporting and PHESS.R"))

# Find all previous bulk uploads
## IMPORTANT! Ensure any "phess_bulk_upload_".csv's in this folder that have NOT 
## been bulk uploaded to PHESS are moved to another folder structure or deleted

previous_files <- list.files(
  here::here("outputs"),
  pattern = "^phess_bulk_upload_\\d{8}.*\\.csv$",
  full.names = TRUE
)

# Extract previously uploaded record_ids
previous_record_ids <-
  if (length(previous_files) == 0) {
    character(0)
  } else {
    previous_files %>%
      map_dfr(
        ~ read_csv(.x, col_types = cols(.default = col_character()))
      ) %>%
      mutate(
        record_id = str_remove(
          OTHER_REFERENCES,
          "^REDCap Record ID: "
        )
      ) %>%
      pull(record_id) %>%
      unique()
  }

# Select key variables

bulk_upload <- dat_clean %>%
  select(record_id, first_name, middle_name, last_name, birth_date, sex,
         address_street, address_suburb_town, address_state, postcode, contact_number, exposure_risk_calculated_all, exposure_ip1_yn, exposure_ip2_yn) %>%
  # Clean state = "VIC/QLD", gender/sex = "MALE/FEMALE"
  mutate(
    sex = case_when(
      sex == 1 ~ "MALE",
      sex == 2 ~ "FEMALE",
      sex == 3 ~ "OTHER",
      #sex == 4 ~ "Not Stated", # Will be listed as missing check ok with DQ team
      TRUE ~ NA_character_
    ),
    address_state = case_when(
      address_state == 1 ~ "VIC",
      address_state == 2 ~ "NSW",
      address_state == 3 ~ "TAS",
      address_state == 4 ~ "QLD",
      address_state == 5 ~ "WA",
      address_state == 6 ~ "SA",
      address_state == 7 ~ "ACT",
      address_state == 8 ~ "NT",
      TRUE ~ NA_character_
    ),
    exposure_risk_calculated_all = case_when(
      exposure_risk_calculated_all == "High Risk" ~ "HIGH",
      exposure_risk_calculated_all == "Low Risk" ~ "LOW",
      # OTHER
      TRUE ~ "NOT_ASSIGNED"
    )
  ) %>%
  # Create rows required for bulk uploads
  mutate(A = "A", # A = alive may need updating
         UNKNOWN = "UNKNOWN",
         X = "X",
         `1057` = "1057", # The organism (?) code
         AT_RISK = "AT_RISK",
         CONTACT = "CONTACT",
         O = "O", # Outbreak
         EXPOSED = "EXPOSED",
         CONTACT_RISK_ASSESSMENT = exposure_risk_calculated_all, # 'HIGH' / not 'high risk'
         YES = "YES", # LINKED_TO_AN_OUTBREAK
         `Is the case linked to an outbreak of Avian Influenza in humans` = "12345678910", ### Update to PHESS outbreak ID ###
         AUSTRALIA = "Australia",
         HOME_CONTACT = NA,
         #OTHER_REFERENCES = paste0("REDCap Record ID: ",record_id), ## Add back in when Bulk upload allows, will make linking easier
         DATE_RECEIVED = as.Date('2026-08-01') ### Think about the date to put here ? date of contact upload
  ) %>%
  # Filter out cases already uploaded
  filter(!record_id %in% previous_record_ids) %>%
  # Filter out cases triaged out
  filter(exposure_ip1_yn == 1 | exposure_ip2_yn == 1) %>%
  # Filter out incomplete cases
  filter(!is.na(exposure_risk_calculated_all) # Keep only those with complete minimum data
         & !is.na(first_name)
         & !is.na(last_name)
         & !is.na(birth_date)
         & !is.na(sex)
         & !is.na(address_street)
         & !is.na(postcode)
         & !is.na(contact_number)
         ) %>%
  select(-exposure_risk_calculated_all, - record_id, -exposure_ip1_yn, -exposure_ip2_yn) %>%
  # Format var names to match DH template
  select(first_name, middle_name, last_name, birth_date, sex,
         address_street, address_suburb_town, address_state, postcode, AUSTRALIA, HOME_CONTACT, contact_number, everything()) %>%
  rename_with(toupper)


# Export for DH 

  message(
    nrow(bulk_upload),
    " new records will be exported (",
    length(previous_record_ids),
    " previously uploaded records skipped)."
  )
  
  if (nrow(bulk_upload) == 0) {
    stop("No new records to upload.")
  }

write.csv(bulk_upload, file = here::here("outputs", paste0("phess_bulk_upload_", format(Sys.time(), "%Y%m%d"), ".csv")), 
          row.names = FALSE,
          na = "")
