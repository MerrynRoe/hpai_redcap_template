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
  # Create rows required for bulk uploads
  mutate(AVIAN_INFLUENZA_IN_HUMANS = "Avian Influenza in humans",
         INFLUENZA_A = "Influenza A",
         A = "A",
         UNKNOWN = "UNKNOWN",
         X = "X",
         `1246` = "1246",
         AT_RISK = "at risk",
         CONTACT = "CONTACT",
         OUTBREAK = "OUTBREAK",
         E = "E",
         EXPOSED = "EXPOSED",
         CONTACT_RISK_ASSESSMENT = exposure_risk_calculated_all,
         LINKED_TO_AN_OUTBREAK = "YES",
         LINKED_TO_AN_OUTBREAK_SPECIFY = "12345678910", ### Update to PHESS outbreak ID ###
         OTHER_REFERENCES = paste0("REDCap Record ID: ",record_id)
  ) %>%
  # Filter out cases triaged out
  filter(exposure_ip1_yn == 1 | exposure_ip2_yn == 1) %>%
  select(-exposure_risk_calculated_all) %>%
  # Filter out incomplete cases
  filter(!is.na(exposure_risk_calculated_all) # Keep only those with complete minimum data
         & !is.na(first_name)
         & !is.na(last_name)
         & !is.na(birth_date)
         & !is.na(sex)
         & !is.na(address_street)
         & !is.na(postcode)
         & !is.na(contact_number)
         ) 
  # Format var names to match DH template
  rename_with(toupper)


# Export for DH 

write.csv(bulk_upload, file = here::here("outputs", paste0("phess_bulk_upload_", format(Sys.time(), "%Y%m%d"), ".csv")), 
          row.names = FALSE,
          na = "")
