# Version: 2.0
# Date of last update: 17/11/2026

# Cleaning ahead of data analyses, bulk uploads and reporting

# Load packages
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2
)

# Define IP sites, this will be used from here on in loops
#ip_sites <- c("ip1", "ip2")
ip_sites <- c("ip1")

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
  select(contains("date")) %>%
  names()

redcap <- redcap %>%
  mutate(across(all_of(date_vars), as.Date))

# Calculates risk levels and creates following variables:
# exposure_risk_calculated_ip1, exposure_risk_calculated_ipX 
# exposure_risk_calculated_all
## NOTE - assumes completed risk surveys have no true 'missing' values. PHOs can enter 'unsure' if info is not available.

dat_clean <- redcap # Create a clean dataset before starting loop for risk assessment

for (ip in ip_sites) {
  
  dat_clean[[paste0("exposure_risk_calculated_", ip)]] <-
    case_when(
      
      # 🔴 HIGH RISK = Direct contact OR enclosed space >15min
      (
        dat_clean[[paste0("exposure_bird_contact_", ip)]] %in% c("1", "2") |
          dat_clean[[paste0("exposure_product_contact_", ip)]] %in% c("1", "2") |
          dat_clean[[paste0("exposure_cleaning_contact_", ip)]] %in% c("1", "2") |
          (
            dat_clean[[paste0("exposure_indoors_no_contact_", ip)]] %in% c("1", "2") &
              dat_clean[[paste0("exposure_indoors_duration_", ip)]] %in% c("1", "2")
          )
      ) ~ "High Risk",
      
      # 🟠 LOW RISK = Indoor exposure <=15min
      (
        dat_clean[[paste0("exposure_indoors_no_contact_", ip)]] %in% c("1", "2") &
          dat_clean[[paste0("exposure_indoors_duration_", ip)]] == "0"
      ) ~ "Low Risk",
      
      # 🟢 NEGLIGIBLE RISK = No reported exposure
      (
        dat_clean[[paste0("exposure_bird_contact_", ip)]] == "0" &
          dat_clean[[paste0("exposure_product_contact_", ip)]] == "0" &
          dat_clean[[paste0("exposure_cleaning_contact_", ip)]] == "0" &
          dat_clean[[paste0("exposure_indoors_no_contact_", ip)]] == "0"
      ) ~ "Negligible Risk",
      
      TRUE ~ NA_character_
    )
}

# Check
dat_clean %>%
  select(record_id, exposure_risk_calculated_ip1) %>%
  view()

# Add high level risk assessment
dat_clean <- dat_clean %>%
  mutate(
    exposure_risk_calculated_all = case_when(
      if_any(starts_with("exposure_risk_calculated_ip"), ~ .x == "High Risk") ~ "High Risk",
      if_any(starts_with("exposure_risk_calculated_ip"), ~ .x == "Low Risk") ~ "Low Risk",
      if_any(starts_with("exposure_risk_calculated_ip"), ~ .x == "Negligible Risk") ~ "Negligible Risk",
      TRUE ~ NA_character_
    ))

# Check
dat_clean %>%
  filter(!is.na(exposure_risk_calculated_ip1)) %>%
  select(record_id, first_name_contact_list, contains("exposure_risk_calculated_ip"), exposure_risk_calculated_all) %>%
  view()


# Create a QA list of REDCap ID where exposure_risk_assessment_ip1 =! exposure_risk_calculated_ip1 for each IP 
# Use this to check the REDCap calculation matches the R calculation
# Inbuilt redundancy
for (ip in ip_sites) {
  
  dat_clean[[paste0("exposure_risk_qa_", ip)]] <- case_when(
    
    dat_clean[[paste0("exposure_risk_label_", ip)]] == "High risk" &
      dat_clean[[paste0("exposure_risk_calculated_", ip)]] != "High Risk" ~
      "Risk assessment mismatch",
    
    dat_clean[[paste0("exposure_risk_label_", ip)]] == "Low risk" &
      dat_clean[[paste0("exposure_risk_calculated_", ip)]] != "Low Risk" ~
      "Risk assessment mismatch",
    
    dat_clean[[paste0("exposure_risk_label_", ip)]] == "Negligible risk" &
      dat_clean[[paste0("exposure_risk_calculated_", ip)]] != "Negligible Risk" ~
      "Risk assessment mismatch",
    
    TRUE ~ NA_character_
  )
}

# Check for any mis-matches
exposure_risk_qa <- bind_rows(
  lapply(ip_sites, function(ip) {
    
    dat_clean %>%
      filter(
        !is.na(.data[[paste0("exposure_risk_qa_", ip)]])
      ) %>%
      transmute(
        record_id,
        ip_site = ip,
        risk_label = .data[[paste0("exposure_risk_label_", ip)]],
        risk_calculated = .data[[paste0("exposure_risk_calculated_", ip)]],
        qa_result = .data[[paste0("exposure_risk_qa_", ip)]]
      )
    
  })
)

exposure_risk_qa

# Check
dat_clean %>%
  filter(!is.na(exposure_risk_assessment_ip1)) %>%
  select(record_id, contact_name ,exposure_risk_assessment_ip1, exposure_risk_calculated_ip1, exposure_risk_qa_ip1, exposure_risk_assessment_ip2, exposure_risk_calculated_ip2, exposure_risk_qa_ip2) %>%
  view()


# Last exposure date
dat_clean <- dat_clean %>%
  mutate(
    last_exposure_date_all = purrr::reduce(
      dplyr::select(., starts_with("exposure_date_ip")) %>% # Selects any 'last_exposure_date_ipX' variable
        as.list(), # Creates a list of all per individual
      ~ pmax(.x, .y, na.rm = TRUE) # Selects the most recent
    ),
    last_exposure_date_all = if_else(
      is.infinite(last_exposure_date_all), # If all were missing (pmax produces 'Inf'), change to date formatted 'missing'
      contact_upload_date, # If last exposure date unknown, use date of contact
      last_exposure_date_all
    )
  )

# One row per individual and back fill from repeat instances
dat_clean <- dat_clean %>%
  group_by(record_id) %>%
  summarise(
    across(everything(), ~ {
      x <- .
      if (all(is.na(x))) NA else x[which(!is.na(x))[1]]
    }),
    .groups = "drop"
  )

