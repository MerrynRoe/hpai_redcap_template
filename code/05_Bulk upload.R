# Pulling all vars required for DH bulk upload template + formatting

source(here::here("code", "04_cleaning for reporting and PHESS.R"))

# Select key variables

bulk_upload <- redcap %>%
  select(first_name, middle_name, last_name, birth_date, sex,
         address_street, address_suburb_town, address_state, postcode, contact_number) %>%
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
  )

# Export for DH 