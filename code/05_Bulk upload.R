# Pulling all vars required for DH bulk upload template + formatting

source(here::here("code", "04_cleaning for reporting and PHESS.R"))

# Select key variables

bulk_upload <- dat_clean %>%
  select(first_name, middle_name, last_name, birth_date, sex,
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
         LINKED_TO_AN_OUTBREAK_SPECIFY = "12345678910" ### Update to PHESS outbreak ID ###
  ) %>%
  # Filter out cases triaged out
  filter(exposure_ip1_yn != 1 | exposure_ip2_yn != 1) %>%
  select(-exposure_risk_calculated_all, -exposure_ip1_yn, -exposure_ip2_yn) %>%
  # Format var names to match DH template
  rename_with(toupper)

## TODO
# Filter out already uploaded cases???? Check with DH - on Q list



# Export for DH 

write.csv(bulk_upload, file = here::here("outputs", paste0("phess_bulk_upload_", format(Sys.time(), "%Y%m%d"), ".csv")), row.names = FALSE)