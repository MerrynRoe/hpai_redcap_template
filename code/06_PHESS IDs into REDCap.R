# Formatting PHESS IDs generated from bulk upload to PHESS (report .txt file) ready to bulk upload into REDCap

# Code written by Alex Fidao (alexander.fidao@health.vic.gov.au)

################################################################################### 
#                                                                                 #
# The .txt roster import 'results' file contains                                  #
# a list of the PHESS IDs created by the 'roster import', with first name         #
# and last name. The .txt results file is dirty, with a lot of other              #
# text that needs to be ignored.                                                  #
#                                                                                 #
# As discussed this code can be replaced with a PHAR code that just looks up      #
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
# Ensure .txt report is saved in the 'raw_data' folder with appropriate versiona controlled naming convention
txt.filename <- "1784176123268_AvianInfluenzaOutbreakContacts1TEST_result.txt"   # adjust filename of input as required
txt.line.vector  <- readLines(here("Inputs",txt.filename), warn = FALSE)

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
  EVENT_ID   = m[, 2],
  FIRST_NAME = m[, 3],
  LAST_NAME  = str_trim(m[, 4]),
  stringsAsFactors = FALSE
)
ROSTER_IMPORT_CASE_LIST <- ROSTER_IMPORT_CASE_LIST[!is.na(df$EVENT_ID), ]   # drop non-matching lines

# --- export ---
output.file <- here("outputs", paste0("PHESS import event name links ", format(Sys.Date(), "%d%m%Y"), ".csv"))

write.csv(ROSTER_IMPORT_CASE_LIST, output.file, row.names = FALSE)