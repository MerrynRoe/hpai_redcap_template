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
# unprotected high-risk activities

dat_clean <- redcap %>%
  mutate(

  # ======================
  # IP1
  # ======================
  exposure_risk_calculated_ip1 = case_when(

    # 🔴 HIGH RISK (direct/high risk activities w breach)
    (
      high_risk_activities_ip1 %in% c("1", "3") | 
        contact_animals_ip1 %in% c("1", "3")
      ) &
      
    (  
      high_risk_ppe_ip1 %in% c("2", "3") |
      high_risk_ppe_breach_ip1 %in% c("1", "3") |
      high_risk_ppe_removal_ip1 %in% c("2", "3") |

      contact_ppe_ip1 %in% c("2", "3") |
      contact_ppe_breach_ip1 %in% c("1", "3") |
      contact_ppe_removal_ip1 %in% c("2", "3")
    ) ~ "High Risk",


    # 🟠 LOW RISK (protected direct/high-risk contact)
    (
      high_risk_activities_ip1 %in% c("1", "3") | 
        contact_animals_ip1 %in% c("1", "3")
    ) &
      
      (
        high_risk_ppe_ip1 == "1" |
        high_risk_ppe_breach_ip1 == "2" |
        high_risk_ppe_removal_ip1 == "1" |

        contact_ppe_ip1 == "1" |
        contact_ppe_breach_ip1 == "2" |
        contact_ppe_removal_ip1 == "1"
      
    ) ~ "Low Risk",


    # 🟡 LOW RISK (vicinity exposure w compromised PPE OR exceed 15 time)
    vicinity_animals_ip1 %in% c("1", "3") &
  (
      vicinity_ppe_ip1 %in% c("2", "3") |
      vicinity_ppe_breach_ip1 %in% c("1", "3") |
      vicinity_ppe_removal_ip1 %in% c("2", "3") |
      vicinity_exposure_time_ip1 == "1"
    ) ~ "Low Risk",


    # 🟢 NEGLIGIBLE RISK
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
          vicinity_animals_ip1 %in% c("1", "3") |
          vicinity_objects_ip1 %in% c("1", "3") |
          vicinity_other_ip1 %in% c("1", "3")
        ) &
        vicinity_ppe_ip1 == "1" &
        vicinity_ppe_breach_ip1 == "2" &
        vicinity_ppe_removal_ip1 == "1"
      )
    ) &
    vicinity_exposure_time_ip1 == "0" ~ "Negligible Risk",

    TRUE ~ NA_character_
  ),


  # ======================
  # IP2
  # ======================
  exposure_risk_calculated_ip2 = case_when(
    
    # 🔴 HIGH RISK (direct/high risk activities w breach)
    (
      high_risk_activities_ip2 %in% c("1", "3") | 
        contact_animals_ip2 %in% c("1", "3")
    ) &
      
      (  
        high_risk_ppe_ip2 %in% c("2", "3") |
          high_risk_ppe_breach_ip2 %in% c("1", "3") |
          high_risk_ppe_removal_ip2 %in% c("2", "3") |
          
          contact_ppe_ip2 %in% c("2", "3") |
          contact_ppe_breach_ip2 %in% c("1", "3") |
          contact_ppe_removal_ip2 %in% c("2", "3")
      ) ~ "High Risk",
    
    
    # 🟠 LOW RISK (protected direct/high-risk contact)
    (
      high_risk_activities_ip2 %in% c("1", "3") | 
        contact_animals_ip2 %in% c("1", "3")
    ) &
      
      (
        high_risk_ppe_ip2 == "1" |
          high_risk_ppe_breach_ip2 == "2" |
          high_risk_ppe_removal_ip2 == "1" |
          
          contact_ppe_ip2 == "1" |
          contact_ppe_breach_ip2 == "2" |
          contact_ppe_removal_ip2 == "1"
        
      ) ~ "Low Risk",
    
    
    # 🟡 LOW RISK (vicinity exposure w compromised PPE OR exceed 15 time)
    vicinity_animals_ip2 %in% c("1", "3") &
      (
        vicinity_ppe_ip2 %in% c("2", "3") |
          vicinity_ppe_breach_ip2 %in% c("1", "3") |
          vicinity_ppe_removal_ip2 %in% c("2", "3") |
          vicinity_exposure_time_ip2 == "1"
      ) ~ "Low Risk",
    
    
    # 🟢 NEGLIGIBLE RISK
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
              vicinity_animals_ip2 %in% c("1", "3") |
                vicinity_objects_ip2 %in% c("1", "3") |
                vicinity_other_ip2 %in% c("1", "3")
            ) &
              vicinity_ppe_ip2 == "1" &
              vicinity_ppe_breach_ip2 == "2" &
              vicinity_ppe_removal_ip2 == "1"
          )
      ) &
      vicinity_exposure_time_ip2 == "0" ~ "Negligible Risk",
    
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
