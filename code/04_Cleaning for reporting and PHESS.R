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
source(here::here("code", "api_tokens.R"))

# Using API
#!/usr/bin/env Rscript
token <- api_token
url <- "https://redcap.gvhealth.org.au/redcap/api/"
formData <- list("token"=token,
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

# Calculate risk levels
# exposure_risk_calculated_ip1, 
# exposure_risk_calculated_all
# unprotected high-risk activities

dat_clean <- redcap %>%
  mutate(exposure_risk_calculated_ip1 = case_when(
    # ----------------------
    # 🔴 HIGH RISK
    # ----------------------
    high_risk_ppe_ip1 %in% c("2", "3") |
      high_risk_ppe_breach_ip1 %in% c("1", "3") |
      high_risk_ppe_removal_ip1 %in% c("2", "3") |
      
      contact_animals_ppe_ip1 %in% c("2", "3") |
      contact_animals_ppe_breach_ip1 %in% c("1", "3") |
      contact_animals_ppe_removal_ip1 %in% c("2", "3") |
      
      contact_objects_ppe_ip1 %in% c("2", "3") |
      contact_objects_ppe_breach_ip1 %in% c("1", "3") |
      contact_objects_ppe_removal_ip1 %in% c("2", "3") |
      
      contact_other_ppe_ip1 %in% c("2", "3") |
      contact_other_ppe_breach_ip1 %in% c("1", "3") |
      contact_other_ppe_removal_ip1 %in% c("2", "3")
    ~ "High Risk",
    
    
    # ----------------------
    # 🟠 LOW RISK (protected direct/high-risk contact)
    # ----------------------
    (
      high_risk_activities_ip1 %in% c("1", "3") &
        (high_risk_ppe_ip1 == "1" |
           high_risk_ppe_breach_ip1 == "2" |
           high_risk_ppe_removal_ip1 == "1")
    ) |
      
      (
        contact_animals_ip1 %in% c("1", "3") &
          (contact_animals_ppe_ip1 == "1" |
             contact_animals_ppe_breach_ip1 == "2" |
             contact_animals_ppe_removal_ip1 == "1")
      ) |
      
      (
        contact_objects_ip1 %in% c("1", "3") &
          (contact_objects_ppe_ip1 == "1" |
             contact_objects_ppe_breach_ip1 == "2" |
             contact_objects_ppe_removal_ip1 == "1")
      ) |
      
      (
        contact_other_ip1 %in% c("1", "3") &
          (contact_other_ppe_ip1 == "1" |
             contact_other_ppe_breach_ip1 == "2" |
             contact_other_ppe_removal_ip1 == "1")
      )
    ~ "Low Risk",
    
    
    # ----------------------
    # 🟡 LOW RISK (vicinity exposure)
    # ----------------------
    vicinity_animals_ppe_ip1 %in% c("2", "3") |
      vicinity_animals_ppe_breach_ip1 %in% c("1", "3") |
      vicinity_animals_ppe_removal_ip1 %in% c("2", "3") |
      
      vicinity_objects_ppe_ip1 %in% c("2", "3") |
      vicinity_objects_ppe_breach_ip1 %in% c("1", "3") |
      vicinity_objects_ppe_removal_ip1 %in% c("2", "3") |
      
      vicinity_other_ppe_ip1 %in% c("2", "3") |
      vicinity_other_ppe_breach_ip1 %in% c("1", "3") |
      vicinity_other_ppe_removal_ip1 %in% c("2", "3") |
      
      vicinity_exposure_time_ip1 == "1"
    ~ "Low Risk",
    
    
    # ----------------------
    # 🟢 NEGLIGIBLE RISK
    # ----------------------
    (
      high_risk_activities_ip1 == "2" &
        contact_animals_ip1 == "2" &
        contact_objects_ip1 == "2" &
        contact_other_ip1 == "2"
    ) &
      
      (
        (
          vicinity_animals_ip1 == "2" &
            vicinity_objects_ip1 == "2" &
            vicinity_other_ip1 == "2"
        ) |
          
          (
            (
              vicinity_animals_ip1 %in% c("1", "3") &
                vicinity_animals_ppe_ip1 == "1" &
                vicinity_animals_ppe_breach_ip1 == "2" &
                vicinity_animals_ppe_removal_ip1 == "1"
            ) |
              
              (
                vicinity_objects_ip1 %in% c("1", "3") &
                  vicinity_objects_ppe_ip1 == "1" &
                  vicinity_objects_ppe_breach_ip1 == "2" &
                  vicinity_objects_ppe_removal_ip1 == "1"
              ) |
              
              (
                vicinity_other_ip1 %in% c("1", "3") &
                  vicinity_other_ppe_ip1 == "1" &
                  vicinity_other_ppe_breach_ip1 == "2" &
                  vicinity_other_ppe_removal_ip1 == "1"
              )
          ) &
          vicinity_exposure_time_ip1 == "0"
      )
    ~ "Negligible Risk",
    
    TRUE ~ NA_character_
  ),
  # IP 2
  exposure_risk_calculated_ip2 = case_when(
    # ----------------------
    # 🔴 HIGH RISK
    # ----------------------
    high_risk_ppe_ip2 %in% c("2", "3") |
      high_risk_ppe_breach_ip2 %in% c("1", "3") |
      high_risk_ppe_removal_ip2 %in% c("2", "3") |
      
      contact_animals_ppe_ip2 %in% c("2", "3") |
      contact_animals_ppe_breach_ip2 %in% c("1", "3") |
      contact_animals_ppe_removal_ip2 %in% c("2", "3") |
      
      contact_objects_ppe_ip2 %in% c("2", "3") |
      contact_objects_ppe_breach_ip2 %in% c("1", "3") |
      contact_objects_ppe_removal_ip2 %in% c("2", "3") |
      
      contact_other_ppe_ip2 %in% c("2", "3") |
      contact_other_ppe_breach_ip2 %in% c("1", "3") |
      contact_other_ppe_removal_ip2 %in% c("2", "3")
    ~ "High Risk",
    
    
    # ----------------------
    # 🟠 LOW RISK (protected direct/high-risk contact)
    # ----------------------
    (
      high_risk_activities_ip2 %in% c("1", "3") &
        (high_risk_ppe_ip2 == "1" |
           high_risk_ppe_breach_ip2 == "2" |
           high_risk_ppe_removal_ip2 == "1")
    ) |
      
      (
        contact_animals_ip2 %in% c("1", "3") &
          (contact_animals_ppe_ip2 == "1" |
             contact_animals_ppe_breach_ip2 == "2" |
             contact_animals_ppe_removal_ip2 == "1")
      ) |
      
      (
        contact_objects_ip2 %in% c("1", "3") &
          (contact_objects_ppe_ip2 == "1" |
             contact_objects_ppe_breach_ip2 == "2" |
             contact_objects_ppe_removal_ip2 == "1")
      ) |
      
      (
        contact_other_ip2 %in% c("1", "3") &
          (contact_other_ppe_ip2 == "1" |
             contact_other_ppe_breach_ip2 == "2" |
             contact_other_ppe_removal_ip2 == "1")
      )
    ~ "Low Risk",
    
    
    # ----------------------
    # 🟡 LOW RISK (vicinity exposure)
    # ----------------------
    vicinity_animals_ppe_ip2 %in% c("2", "3") |
      vicinity_animals_ppe_breach_ip2 %in% c("1", "3") |
      vicinity_animals_ppe_removal_ip2 %in% c("2", "3") |
      
      vicinity_objects_ppe_ip2 %in% c("2", "3") |
      vicinity_objects_ppe_breach_ip2 %in% c("1", "3") |
      vicinity_objects_ppe_removal_ip2 %in% c("2", "3") |
      
      vicinity_other_ppe_ip2 %in% c("2", "3") |
      vicinity_other_ppe_breach_ip2 %in% c("1", "3") |
      vicinity_other_ppe_removal_ip2 %in% c("2", "3") |
      
      vicinity_exposure_time_ip2 == "1"
    ~ "Low Risk",
    
    
    # ----------------------
    # 🟢 NEGLIGIBLE RISK
    # ----------------------
    (
      high_risk_activities_ip2 == "2" &
        contact_animals_ip2 == "2" &
        contact_objects_ip2 == "2" &
        contact_other_ip2 == "2"
    ) &
      
      (
        (
          vicinity_animals_ip2 == "2" &
            vicinity_objects_ip2 == "2" &
            vicinity_other_ip2 == "2"
        ) |
          
          (
            (
              vicinity_animals_ip2 %in% c("1", "3") &
                vicinity_animals_ppe_ip2 == "1" &
                vicinity_animals_ppe_breach_ip2 == "2" &
                vicinity_animals_ppe_removal_ip2 == "1"
            ) |
              
              (
                vicinity_objects_ip2 %in% c("1", "3") &
                  vicinity_objects_ppe_ip2 == "1" &
                  vicinity_objects_ppe_breach_ip2 == "2" &
                  vicinity_objects_ppe_removal_ip2 == "1"
              ) |
              
              (
                vicinity_other_ip2 %in% c("1", "3") &
                  vicinity_other_ppe_ip2 == "1" &
                  vicinity_other_ppe_breach_ip2 == "2" &
                  vicinity_other_ppe_removal_ip2 == "1"
              )
          ) &
          vicinity_exposure_time_ip2 == "0"
      )
    ~ "Negligible Risk",
    
    TRUE ~ NA_character_
  )
  )

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
  ))

# Check
dat_clean %>%
  select(record_id,  exposure_risk_assessment_ip1, exposure_risk_calculated_ip1, exposure_risk_qa_ip1, exposure_risk_assessment_ip2, exposure_risk_calculated_ip2, exposure_risk_qa_ip2) %>%
  view()

## To do
# Confirmed cases
# exposure_ip1_yn exposure_ip2_yn == 1
# test_results == 2 or == 3 