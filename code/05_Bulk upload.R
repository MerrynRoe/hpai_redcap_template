# Version: 1.1
# Date of last update: 20/8/2026

# Pulling all vars required for DH bulk upload template + formatting

source(here::here("code", "03_cleaning for reporting and PHESS.R"))

# Find all previous bulk uploads
## IMPORTANT! Ensure any "phess_bulk_upload_".csv's in this folder that have NOT ##
## been bulk uploaded to PHESS are moved to another folder structure or deleted ##

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
          `Other references`,
          "^REDCap Record ID: "
        )
      ) %>%
      pull(record_id) %>%
      unique()
  }


# Initialise variables from IP1
dat_clean <- dat_clean %>%
  mutate(
    birth_date = birth_date_ip1,
    sex = sex_ip1,
    postcode = postcode_ip1
  )

# Reconcile across IP sites
for (ip in ip_sites) {
  
  dat_clean <- dat_clean %>%
    mutate(
        first_name = coalesce(
          na_if(.data[[paste0("first_name_", ip)]], ""),
          first_name_contact_list
        ),
        last_name = coalesce(
          na_if(.data[[paste0("last_name_", ip)]], ""),
          last_name_contact_list
        ),
      birth_date = coalesce(
        birth_date,
        .data[[paste0("birth_date_", ip)]]
      ),
      sex = coalesce(
        sex,
        .data[[paste0("sex_", ip)]]
      ),
      postcode = coalesce(
        postcode,
        .data[[paste0("postcode_", ip)]]
      )
    )
}


bulk_upload <- dat_clean %>%
  select(record_id, first_name, first_name_contact_list, last_name, last_name_contact_list, birth_date, sex,
        postcode, phone_contact_list, contact_upload_date, contains("triage_yn_ip"), contains("exposure_risk_calculated_")) %>%
  # Clean state = "VIC/QLD", gender/sex = "MALE/FEMALE"
  mutate(
    sex = case_when(
      sex == 1 ~ "MALE",
      sex == 0 ~ "FEMALE",
      sex == 99 ~ "OTHER",
      #sex == 4 ~ "Not Stated", # Will be listed as missing
      TRUE ~ NA_character_
    ),
    exposure_risk_calculated_all = case_when(
      exposure_risk_calculated_all == "High Risk" ~ "HIGH",
      exposure_risk_calculated_all == "Low Risk" ~ "LOW",
      # exposure_risk_calculated_all == OTHER is a bulk upload option if needed
      TRUE ~ "NOT_ASSIGNED"
    ),
    postcode_num = as.integer(stringr::str_pad(
      as.character(postcode),
      width = 4,
      side = "left",
      pad = "0"
    )),
    
    address_state = case_when(
      # ACT
      between(postcode_num, 200, 299) |
        between(postcode_num, 2600, 2618) |
        between(postcode_num, 2900, 2920) ~ "ACT",
      
      # NSW
      between(postcode_num, 1000, 2599) |
        between(postcode_num, 2619, 2899) |
        between(postcode_num, 2921, 2999) ~ "NSW",
      
      # VIC
      between(postcode_num, 3000, 3999) |
        between(postcode_num, 8000, 8999) ~ "VIC",
      
      # QLD
      between(postcode_num, 4000, 4999) |
        between(postcode_num, 9000, 9999) ~ "QLD",
      
      # SA
      between(postcode_num, 5000, 5999) ~ "SA",
      
      # WA
      between(postcode_num, 6000, 6999) ~ "WA",
      
      # TAS
      between(postcode_num, 7000, 7999) ~ "TAS",
      
      # NT
      between(postcode_num, 800, 999) ~ "NT",
      
      TRUE ~ NA_character_
    )) %>%
  select(-postcode_num) %>%
  # Create rows required for bulk uploads
  mutate(A = "A", # A = alive may need updating
         UNKNOWN = "UNKNOWN",
         X = "X",
         `1057` = "1057", # The organism (?) code
         AT_RISK = "AT_RISK",
         CONTACT = "CONTACT",
         O = "O", # Outbreak
         EXPOSED = "EXPOSED",
         CONTACT_RISK_ASSESSMENT = exposure_risk_calculated_all, 
         YES = "YES", # LINKED_TO_AN_OUTBREAK
         `Is the case linked to an outbreak of Avian Influenza in humans` = "12345678910", ### Update to PHESS outbreak ID ###
         AUSTRALIA = "Australia",
         HOME_CONTACT = NA,
         `Other references` = paste0("REDCap Record ID: ",record_id), ## Add back in when Bulk upload allows, will make linking easier PHESS IDs back into REDCap in script 06
         DATE_RECEIVED = contact_upload_date ## Date contact first uploaded to REDCap
  ) %>%
  # Filter out cases already uploaded
  filter(!record_id %in% previous_record_ids) %>%
  # Filter - only keep people with any triage = Yes
  filter(if_any(starts_with("triage_yn_ip"), ~ .x == 1)) %>%
  # Filter out incomplete cases
  ## 10 days since last exposure + completed PHA + completed interview data ##
  ## Min 30 cases for bulk upload ##
  filter(!is.na(exposure_risk_calculated_all) # Keep only those with complete minimum data, remove this whole filter step for final upload at end of incident
         & !is.na(first_name)
         & !is.na(last_name)
         & !is.na(birth_date)
         & !is.na(sex)
         & !is.na(postcode)
         & !is.na(phone_contact_list)
         ) %>%
  select(-exposure_risk_calculated_all, - record_id, -contains("triage_yn_ip"), -contact_upload_date) %>%
  # Format var names to match DH template
  select(first_name, last_name, birth_date, sex,
         address_state, postcode, AUSTRALIA, HOME_CONTACT, phone_contact_list, everything(), DATE_RECEIVED) 

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

write.csv(bulk_upload, file = here::here("outputs", paste0("phess_bulk_upload_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")), 
          row.names = FALSE,
          na = "")
