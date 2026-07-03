# Cleaning ahead of data analyses, bulk uploads and reporting

# Load packages
# Loading packagaes
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,       # to clean column names
  ggplot2
)

# RedCap data from exports

# REDCap contact data
# Import REDCap case list

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

# Clean all date variables
date_vars <- redcap %>%
  select(contains("date"), -email_updated) %>%
  names()

redcap <- redcap %>%
  mutate(across(all_of(date_vars), as.Date))

# Calculate risk levels
# exposure_risk_calculated_ip1, 
# exposure_risk_calculated_all
## Note - requires form to be completed (no 'missing') values. PHOs can enter 'unsure' if info is not available.

dat_clean <- redcap

for (ip in c("ip1", "ip2")) { # Add exposure site suffixes as required, ensure REDCap form is duplicated for new site before running
  
  dat_clean[[paste0("exposure_risk_calculated_test_", ip)]] <-
    with(dat_clean, case_when(
      
      # 🔴 HIGH RISK
      (
        dat_clean[[paste0("high_risk_activities_", ip)]] %in% c("1","3") |
          dat_clean[[paste0("contact_animals_", ip)]] %in% c("1","3") |
          dat_clean[[paste0("contact_objects_", ip)]] %in% c("1","3") |
          dat_clean[[paste0("contact_other_", ip)]] %in% c("1","3")
      ) &
        (
          dat_clean[[paste0("high_risk_ppe_", ip)]] %in% c("0","3") |
            dat_clean[[paste0("high_risk_ppe_breach_", ip)]] %in% c("1","3") |
            dat_clean[[paste0("high_risk_ppe_removal_", ip)]] %in% c("0","3") |
            dat_clean[[paste0("contact_ppe_", ip)]] %in% c("0","3") |
            dat_clean[[paste0("contact_ppe_breach_", ip)]] %in% c("1","3") |
            dat_clean[[paste0("contact_ppe_removal_", ip)]] %in% c("0","3")
        ) ~ "High Risk",
      
      # 🟠 LOW RISK
      (
        dat_clean[[paste0("high_risk_activities_", ip)]] %in% c("1","3") |
          dat_clean[[paste0("contact_animals_", ip)]] %in% c("1","3") |
          dat_clean[[paste0("contact_objects_", ip)]] %in% c("1","3") |
          dat_clean[[paste0("contact_other_", ip)]] %in% c("1","3")
      ) &
        (
          dat_clean[[paste0("high_risk_ppe_", ip)]] == "1" |
            dat_clean[[paste0("high_risk_ppe_breach_", ip)]] == "0" |
            dat_clean[[paste0("high_risk_ppe_removal_", ip)]] == "1" |
            dat_clean[[paste0("contact_ppe_", ip)]] == "1" |
            dat_clean[[paste0("contact_ppe_breach_", ip)]] == "0" |
            dat_clean[[paste0("contact_ppe_removal_", ip)]] == "1"
        ) ~ "Low Risk",
      
      # 🟡 LOW RISK (Vicinity)
      dat_clean[[paste0("vicinity_animals_", ip)]] %in% c("1","3") &
        (
          dat_clean[[paste0("vicinity_ppe_", ip)]] %in% c("0","3") |
            dat_clean[[paste0("vicinity_ppe_breach_", ip)]] %in% c("1","3") |
            dat_clean[[paste0("vicinity_ppe_removal_", ip)]] %in% c("0","3") |
            dat_clean[[paste0("vicinity_exposure_time_", ip)]] == "1"
        ) ~ "Low Risk",
      
      # 🟢 NEGLIGIBLE
      (
        dat_clean[[paste0("high_risk_activities_", ip)]] == "0" &
          dat_clean[[paste0("contact_animals_", ip)]] == "0" &
          dat_clean[[paste0("contact_objects_", ip)]] == "0" &
          dat_clean[[paste0("contact_other_", ip)]] == "0"
      ) &
        (
          (
            dat_clean[[paste0("vicinity_animals_", ip)]] == "0" &
              dat_clean[[paste0("vicinity_objects_", ip)]] == "0" &
              dat_clean[[paste0("vicinity_other_", ip)]] == "0"
          ) |
            (
              (
                dat_clean[[paste0("vicinity_animals_", ip)]] %in% c("1","3") |
                  dat_clean[[paste0("vicinity_objects_", ip)]] %in% c("1","3") |
                  dat_clean[[paste0("vicinity_other_", ip)]] %in% c("1","3")
              ) &
                dat_clean[[paste0("vicinity_ppe_", ip)]] == "1" &
                dat_clean[[paste0("vicinity_ppe_breach_", ip)]] == "0" &
                dat_clean[[paste0("vicinity_ppe_removal_", ip)]] == "1"
            )
        ) &
        dat_clean[[paste0("vicinity_exposure_time_", ip)]] == "0" ~ "Negligible Risk",
      
      TRUE ~ NA_character_
      
    ))
}

# Check
dat_clean %>%
  select(record_id,  exposure_risk_assessment_ip1, exposure_risk_calculated_ip1, exposure_risk_assessment_ip2) %>%
  view()

# Export QA list of REDCap ID where exposure_risk_assessment_ip1 =! exposure_risk_calculated_ip1 for each IP to send to ops manager
dat_clean <- dat_clean %>%
  mutate(
    exposure_risk_qa_ip1 = case_when(
      exposure_risk_assessment_ip1 == 0 & exposure_risk_calculated_ip1 != "High Risk" ~ "Risk assessment mismatch",
      exposure_risk_assessment_ip1 == 1 & exposure_risk_calculated_ip1 != "Low Risk" ~ "Risk assessment mismatch",
      exposure_risk_assessment_ip1 == 2 & exposure_risk_calculated_ip1 != "Negligible Risk" ~ "Risk assessment mismatch",
      TRUE ~ NA_character_
  ),
    exposure_risk_qa_ip2 = case_when(
      exposure_risk_assessment_ip2 == 0 & exposure_risk_calculated_ip2 != "High Risk" ~ "Risk assessment mismatch",
      exposure_risk_assessment_ip2 == 1 & exposure_risk_calculated_ip2 != "Low Risk" ~ "Risk assessment mismatch",
      exposure_risk_assessment_ip2 == 2 & exposure_risk_calculated_ip2 != "Negligible Risk" ~ "Risk assessment mismatch",
      TRUE ~ NA_character_
  )) %>%
  # Adding labels to REDCap 'PHO assigned' risk
  mutate(
    across(
      c(exposure_risk_assessment_ip1, exposure_risk_assessment_ip2),
      ~ factor(.,
               levels = c(0, 1, 2),
               labels = c("High Level Exposure",
                          "Low Level Exposure",
                          "Negligible Level Exposure"))
    )
  )

# Check
dat_clean %>%
  filter(!is.na(exposure_risk_assessment_ip1)) %>%
  select(record_id, contact_name ,exposure_risk_assessment_ip1, exposure_risk_calculated_ip1, exposure_risk_qa_ip1, exposure_risk_assessment_ip2, exposure_risk_calculated_ip2, exposure_risk_qa_ip2) %>%
  view()

# Add high level risk assessment
dat_clean <- dat_clean %>%
  mutate(exposure_risk_calculated_all = case_when(
    exposure_risk_calculated_ip1 == "High Risk" | exposure_risk_calculated_ip2 == "High Risk" ~ "High Risk",
    exposure_risk_calculated_ip1 == "Low Risk" | exposure_risk_calculated_ip2 == "Low Risk" ~ "Low Risk",
    exposure_risk_calculated_ip1 == "Negligible Risk" | exposure_risk_calculated_ip2 == "Negligible Risk" ~ "Negligible Risk",
    TRUE ~ NA_character_
  ))

# Last exposure date
dat_clean <- dat_clean %>%
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
  ungroup() %>%
  mutate(last_exposure_date_ip1 = as.Date(last_exposure_date_ip1),
         last_exposure_date_ip2 = as.Date(last_exposure_date_ip2),
         last_exposure_date_all =  pmax(last_exposure_date_ip1, last_exposure_date_ip2, na.rm = TRUE))

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

## TODO
# Confirmed cases variable?
  # exposure_ip1_yn exposure_ip2_yn == 1
  # test_results == 2 or == 3 

# QA export
dat_qa_risk <- dat_clean %>%
  filter(exposure_risk_qa_ip1 == "Risk assessment mismatch" | exposure_risk_qa_ip2 == "Risk assessment mismatch") %>%
  select(record_id, first_name, phone, pho_name, 
         exposure_risk_assessment_ip1, exposure_risk_calculated_ip1, exposure_risk_qa_ip1, 
         exposure_risk_assessment_ip2, exposure_risk_calculated_ip2, exposure_risk_qa_ip2)

write.csv(dat_qa_risk, file = here::here("outputs", paste0("dat_qa_risk_", format(Sys.time(), "%Y%m%d"), ".csv")), row.names = FALSE)
