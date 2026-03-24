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
    ## High Risk
    # Statement 1 - unprotected high-risk activities
    ((high_risk_activities_ip1 == 1 | high_risk_activities_ip1 == 3) &
    (high_risk_ppe_ip1 == 2 | high_risk_ppe_ip1 == 3 | high_risk_ppe_breach_ip1 == 1 | high_risk_ppe_breach_ip1 == 3 |
      high_risk_ppe_removal_ip1 == 2 | high_risk_ppe_removal_ip1 == 3)) |
    # Statement 2 - unprotected direct contacts
      ((contact_animals_ip1 == 1 | contact_animals_ip1 == 3) &
      (contact_animals_ppe_ip1 == 2 | contact_animals_ppe_ip1 == 3 | contact_animals_ppe_breach_ip1 == 1 | contact_animals_ppe_breach_ip1 == 3 |
      contact_animals_ppe_removal_ip1 == 2 | contact_animals_ppe_removal_ip1 == 3)) |
    # Statement 3 - contact objects
      ((contact_objects_ip1 == 1 | contact_objects_ip1 == 3) &
        (contact_objects_ppe_ip1 == 2 | contact_objects_ppe_ip1 == 3 | contact_objects_ppe_breach_ip1 == 1 | contact_objects_ppe_breach_ip1 == 3 |
        contact_objects_ppe_removal_ip1 == 2 | contact_objects_ppe_removal_ip1 == 3)) |
    # Statement 4 - contact other
      ((contact_other_ip1 == 1 | contact_other_ip1 == 3) &
        (contact_other_ppe_ip1 == 2 | contact_other_ppe_ip1 == 3 | contact_other_ppe_breach_ip1 == 1 | contact_other_ppe_breach_ip1 == 3 |
          contact_other_ppe_removal_ip1 == 2 | contact_other_ppe_removal_ip1 == 3)) 
    ~ "High Risk",
    
    ## Low Risk
    # Statement 1 - protected high-risk activities
    ((high_risk_activities_ip1 == 1 | high_risk_activities_ip1 == 3) &
       high_risk_ppe_ip1 == 1 & high_risk_ppe_breach_ip1 == 2 & high_risk_ppe_removal_ip1 == 1) |
    # Statement 2 - protected direct contacts
    ((contact_animals_ip1 == 1 | contact_animals_ip1 == 3) &
       contact_animals_ppe_ip1 == 1 & contact_animals_ppe_breach_ip1 == 2 & contact_animals_ppe_removal_ip1 == 1)  |
    # Statement 3 - protected contact objects
    ((contact_objects_ip1 == 1 | contact_objects_ip1 == 3) &
      contact_objects_ppe_ip1 == 1 & contact_objects_ppe_breach_ip1 == 2 & contact_objects_ppe_removal_ip1 == 1) |
    # Statement 4 - protected contacts other
    ((contact_other_ip1 == 1 | contact_other_ip1 == 3) &
      contact_other_ppe_ip1 == 1 & contact_other_ppe_breach_ip1 == 2 & contact_other_ppe_removal_ip1 == 1)  |
    # Statement 5 - unprotected vicinity exposures animal
    ((vicinity_animals_ip1 == 1 | vicinity_animals_ip1 ==3) &
       (vicinity_animals_ppe_ip1 == 2 | vicinity_animals_ppe_ip1 == 3 | vicinity_animals_ppe_breach_ip1 == 1 | vicinity_animals_ppe_breach_ip1 == 3 | 
          vicinity_animals_ppe_removal_ip1 == 2 | vicinity_animals_ppe_removal_ip1 == 3)) |
    # Statement 6 - unprotected vicinity objects
      ((vicinity_objects_ip1 == 1 | vicinity_objects_ip1 ==3) &
         (vicinity_objects_ppe_ip1 == 2 | vicinity_objects_ppe_ip1 == 3 | vicinity_objects_ppe_breach_ip1 == 1 | vicinity_objects_ppe_breach_ip1 == 3 | 
            vicinity_objects_ppe_removal_ip1 == 2 | vicinity_objects_ppe_removal_ip1 == 3)) |
    # Statement 7 - unprotected vicinity other
      ((vicinity_other_ip1 == 1 | vicinity_other_ip1 ==3) &
         (vicinity_other_ppe_ip1 == 2 | vicinity_other_ppe_ip1 == 3 | vicinity_other_ppe_breach_ip1 == 1 | vicinity_other_ppe_breach_ip1 == 3 | 
            vicinity_other_ppe_removal_ip1 == 2 | vicinity_other_ppe_removal_ip1 == 3)) |
    # Statement 8 - prolonged vicinity exposures
      (vicinity_exposure_time_ip1 == 1)
    ~ "Low Risk",
    

    ## Negligible
    # no high-risk activities or direct contact
    (high_risk_activities_ip1 == 2 & contact_animals_ip1 == 2 & contact_objects_ip1 == 2 & contact_other_ip1 == 2 &
    # no vicinity exposures, 
      ((vicinity_animals_ip1 == 2 & vicinity_objects_ip1 == 2 & vicinity_other_ip1 == 2) | 
         # or protected, not prolonged vicinity exposures
      ((vicinity_animals_ip1 ==1 | vicinity_animals_ip1 == 3) &
      (vicinity_animals_ppe_ip1 == 1 & vicinity_animals_ppe_breach_ip1 == 2 & vicinity_animals_ppe_removal_ip1 == 1)) |
      ((vicinity_objects_ip1 == 1 | vicinity_objects_ip1 == 3)  &
         vicinity_objects_ppe_ip1 == 1 & vicinity_objects_ppe_breach_ip1 == 2 & vicinity_objects_ppe_removal_ip1 == 1) |
      ((vicinity_other_ip1 == 1 | vicinity_other_ip1 == 3) &
         (vicinity_other_ppe_ip1 == 1 & vicinity_other_ppe_breach_ip1 == 2 & vicinity_other_ppe_removal_ip1 == 1)) &
        (vicinity_exposure_time_ip1 == 0)))
    
    ~ "Negligible",
    TRUE ~ NA_character_))

## TODO: test on Wednesday when data is entered

dat_clean <- dat_clean %>%
  mutate(exposure_risk_calculated_ip1_test = case_when(
    ## High Risk
    # Statement 1 - unprotected high-risk activities
    ((high_risk_activities_ip1 == 1 | high_risk_activities_ip1 == 3) &
       (high_risk_ppe_ip1 == 2 | high_risk_ppe_ip1 == 3 | high_risk_ppe_breach_ip1 == 1 | high_risk_ppe_breach_ip1 == 3 |
          high_risk_ppe_removal_ip1 == 2 | high_risk_ppe_removal_ip1 == 3)) |
      # Statement 2 - unprotected direct contacts
      ((contact_animals_ip1 == 1 | contact_animals_ip1 == 3) &
         (contact_animals_ppe_ip1 == 2 | contact_animals_ppe_ip1 == 3 | contact_animals_ppe_breach_ip1 == 1 | contact_animals_ppe_breach_ip1 == 3 |
            contact_animals_ppe_removal_ip1 == 2 | contact_animals_ppe_removal_ip1 == 3)) |
      # Statement 3 - contact objects
      ((contact_objects_ip1 == 1 | contact_objects_ip1 == 3) &
         (contact_objects_ppe_ip1 == 2 | contact_objects_ppe_ip1 == 3 | contact_objects_ppe_breach_ip1 == 1 | contact_objects_ppe_breach_ip1 == 3 |
            contact_objects_ppe_removal_ip1 == 2 | contact_objects_ppe_removal_ip1 == 3)) |
      # Statement 4 - contact other
      ((contact_other_ip1 == 1 | contact_other_ip1 == 3) &
         (contact_other_ppe_ip1 == 2 | contact_other_ppe_ip1 == 3 | contact_other_ppe_breach_ip1 == 1 | contact_other_ppe_breach_ip1 == 3 |
            contact_other_ppe_removal_ip1 == 2 | contact_other_ppe_removal_ip1 == 3)) 
    ~ "High Risk",
    
    ## Low Risk
    # Statement 1 - protected high-risk activities
    ((high_risk_activities_ip1 == 1 | high_risk_activities_ip1 == 3) &
       high_risk_ppe_ip1 == 1 & high_risk_ppe_breach_ip1 == 2 & high_risk_ppe_removal_ip1 == 1) |
      # Statement 2 - protected direct contacts
      ((contact_animals_ip1 == 1 | contact_animals_ip1 == 3) &
         contact_animals_ppe_ip1 == 1 & contact_animals_ppe_breach_ip1 == 2 & contact_animals_ppe_removal_ip1 == 1)  |
      # Statement 3 - protected contact objects
      ((contact_objects_ip1 == 1 | contact_objects_ip1 == 3) &
         contact_objects_ppe_ip1 == 1 & contact_objects_ppe_breach_ip1 == 2 & contact_objects_ppe_removal_ip1 == 1) |
      # Statement 4 - protected contacts other
      ((contact_other_ip1 == 1 | contact_other_ip1 == 3) &
         contact_other_ppe_ip1 == 1 & contact_other_ppe_breach_ip1 == 2 & contact_other_ppe_removal_ip1 == 1)  |
      # Statement 5 - unprotected vicinity exposures animal
      ((vicinity_animals_ip1 == 1 | vicinity_animals_ip1 ==3) &
         (vicinity_animals_ppe_ip1 == 2 | vicinity_animals_ppe_ip1 == 3 | vicinity_animals_ppe_breach_ip1 == 1 | vicinity_animals_ppe_breach_ip1 == 3 | 
            vicinity_animals_ppe_removal_ip1 == 2 | vicinity_animals_ppe_removal_ip1 == 3)) |
      # Statement 6 - unprotected vicinity objects
      ((vicinity_objects_ip1 == 1 | vicinity_objects_ip1 ==3) &
         (vicinity_objects_ppe_ip1 == 2 | vicinity_objects_ppe_ip1 == 3 | vicinity_objects_ppe_breach_ip1 == 1 | vicinity_objects_ppe_breach_ip1 == 3 | 
            vicinity_objects_ppe_removal_ip1 == 2 | vicinity_objects_ppe_removal_ip1 == 3)) |
      # Statement 7 - unprotected vicinity other
      ((vicinity_other_ip1 == 1 | vicinity_other_ip1 ==3) &
         (vicinity_other_ppe_ip1 == 2 | vicinity_other_ppe_ip1 == 3 | vicinity_other_ppe_breach_ip1 == 1 | vicinity_other_ppe_breach_ip1 == 3 | 
            vicinity_other_ppe_removal_ip1 == 2 | vicinity_other_ppe_removal_ip1 == 3)) |
      # Statement 8 - prolonged vicinity exposures
      (vicinity_exposure_time_ip1 == 1)
    ~ "Low Risk",
    
    TRUE ~ "Negligible"))


# Compare to reported risk levels
# exposure_risk_assessment_ip1, exposure_risk_assessment_ip2
dat_clean <- dat_clean %>%
  mutate(risk_ass_qa_flag = case_when(
    # inconsistency between calculated and PHO assigned
    (exposure_risk_assessment_ip1 == 0 & exposure_risk_calculated_ip1 != "High Risk") &
    (exposure_risk_assessment_ip1 == 1 & exposure_risk_calculated_ip1 != "Low Risk") &
    (exposure_risk_assessment_ip1 == 2 & exposure_risk_calculated_ip1 != "Negligible")
    ~ 1,
    
    # match between calculated and PHO assigned
    (exposure_risk_assessment_ip1 == 0 & exposure_risk_calculated_ip1 == "High Risk") &
      (exposure_risk_assessment_ip1 == 1 & exposure_risk_calculated_ip1 == "Low Risk") &
      (exposure_risk_assessment_ip1 == 2 & exposure_risk_calculated_ip1 == "Negligible")
    ~ 0,
    TRUE ~ NA
  ))

## TODO:
# Create a exposure_risk_calculated_all variable to cover both ips

# Export QA list of REDCap ID where exposure_risk_assessment_ip1 =! exposure_risk_calculated_ip1 for each IP to send to ops manager

