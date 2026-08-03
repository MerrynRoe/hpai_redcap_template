# Formatting PHESS IDs generated from bulk upload to PHESS (report .txt file) ready to bulk upload into REDCap

# Code written by Alex Fidao (alexander.fidao@health.vic.gov.au)

################################################################################### 
##### Roster import output convert code for uploading PHESSIDs to REDCap     ##### 
################################################################################### 
### Code to read in the .txt roster import 'results' file and outputs           ###
### a csv with a list of the created PHESS IDs, with their first and last name. ###
################################################################################### 
#                                                                                 #
# The .txt roster import 'results' file contains                                  #
# a list of the PHESS IDs created by the roster import, with first name           #
# and last name. The .txt results file is dirty though, with a lot of other       #
# text that needs to be ignored.                                                  #
#                                                                                 #
# As discussed this code can be replaces with a PHAR code that just looks up      #
# cases linked to the relevent outbreak (if the .txt file is first used to link   #
# the newly created cases to their Outbreak) or to the OTHER_REFERENCES           #
# field, if we can get it added to the roster import template, which can contain  #
# the REDCap ID.                                                                  #
#                                                                                 #
################################################################################### 

### 0. SETUP

# Load packages
pacman::p_load(
  rio,           # to import data
  here,          # to locate files
  tidyverse,     # to clean, handle, and plot the data (includes ggplot2 package)
  janitor        # to clean column names
)

### 1. INPUT DATA
txt.filename <- "1784176123268_AvianInfluenzaOutbreakContacts1TEST_result.txt"   # adjust filename of input
txt.line.vector  <- readLines(here("raw_data",txt.filename), warn = FALSE)

### 2. EXTRACT FIELDS

## regex with three capture groups: 
# 1. digits after "created new event"
# 2. the first whitespace-free token after "created new person", and
# 3. everything to end of line:
string.pattern <- "created new event (\\d+) and created new person (\\S+)\\s+(.+)$"

## str_match returns a character matrix match.matrix, with one row per input line:
# col 1 (V1) returns the part of the line that matches the format in string.pattern IF there was a match
# cols 2:4 = the three capture groups (V2 = PHESS ID, V3 / V4 = first / last name
# unmatched lines have NA entries for all cols
match.matrix <- str_match(txt.line.vector, string.pattern)

# convert match.matrix to a DF:
ROSTER_IMPORT_CASE_LIST <- data.frame(
  EVENT_ID   = match.matrix[, 2],
  FIRST_NAME = match.matrix[, 3],
  LAST_NAME  = str_trim(match.matrix[, 4]),
  stringsAsFactors = FALSE
)

ROSTER_IMPORT_CASE_LIST <- ROSTER_IMPORT_CASE_LIST[!is.na(ROSTER_IMPORT_CASE_LIST$EVENT_ID), ]   # drop non-matching lines

### 3. Left join with existing minimum dataset
## TODO - add mobile / contact number in for better matching specificity? ##

## Run REDCap cleaning for most upto date REDCap data
source(here::here("code", "04_cleaning for reporting and PHESS.R"))

## Check for any First / Last name duplicates in both datasets
# Ensure duplicates are removed from data sources (REDCap / PHESS) before progressing
ROSTER_IMPORT_CASE_LIST %>%
  count(FIRST_NAME, LAST_NAME) %>%
  filter(n > 1)

dat_clean %>%
  count(first_name, last_name) %>%
  filter(n > 1)

## Clean name text for better matching

dat_clean_joined <- dat_clean %>%
  mutate(
    first_name_join = str_to_upper(str_trim(first_name)),
    last_name_join  = str_to_upper(str_trim(last_name))
  ) %>%
  left_join(
    ROSTER_IMPORT_CASE_LIST %>%
      mutate(
        FIRST_NAME_JOIN = str_to_upper(str_trim(FIRST_NAME)),
        LAST_NAME_JOIN  = str_to_upper(str_trim(LAST_NAME))
      ),
    by = c(
      "first_name_join" = "FIRST_NAME_JOIN",
      "last_name_join"  = "LAST_NAME_JOIN"
    )
  ) %>%
  select(-first_name_join, -last_name_join)

## TODO - check if alert message appropriate for any non-join events?? ##
unmatched_phess <- ROSTER_IMPORT_CASE_LIST %>%
  mutate(
    FIRST_NAME_JOIN = str_to_upper(str_trim(FIRST_NAME)),
    LAST_NAME_JOIN  = str_to_upper(str_trim(LAST_NAME))
  ) %>%
  left_join(
    dat_clean %>%
      mutate(
        first_name_join = str_to_upper(str_trim(first_name)),
        last_name_join  = str_to_upper(str_trim(last_name))
      ),
    by = c(
       "FIRST_NAME_JOIN" = "first_name_join",
       "LAST_NAME_JOIN" = "last_name_join"
    )
  ) %>%
  select(-FIRST_NAME_JOIN, -LAST_NAME_JOIN)

if (nrow(unmatched_phess) > 0) {
  
  warning(
    paste0(
      nrow(unmatched_phess),
      " record(s) from PHESS could not be matched to REDCap. ",
      "Please review the following names:\n",
      paste(
        paste(unmatched_phess$FIRST_NAME, unmatched_phess$LAST_NAME),
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
  
} else {
  
  message("✓ All PHESS records were successfully matched to REDCap.")
  
}

## Export ready for REDCap import
output.file <- here("outputs", paste0("PHESS import event name links ", format(Sys.Date(), "%d%m%Y"), ".csv"))

write.csv(dat_phess_id_to_redcap, output.file, row.names = FALSE)