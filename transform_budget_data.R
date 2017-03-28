#### This R script cleans up budget survey data for DSA teams use ####
#### Variables have also been renamed to make easier for analysis ####
#### Datasets are exported with numeric and text answers as needed ####

# load in required libraries
library(tidyverse) #need readr, tidyr, dplyr
library(lubridate)
library(stringr)

# make sure to set the working directory
setwd()
# read in data from Qualtrics and limit to only the relevant data needed
responses = read_csv("raw_data/Budget Survey Test - Text.csv", col_names = TRUE)
responses_df = responses[c(3:nrow(responses)),]
names(responses_df) = tolower(names(responses_df))

# clean up responses for valid entries
responses_df$valid_date_range = interval(mdy_hms("03/24/2017 14:00:00"),mdy_hms("03/30/2017 6:00:00"))
responses_df$recordeddate = ymd_hms(responses_df$recordeddate)
valid_response_df = responses_df %>% filter(status != "Survey Preview" & recordeddate %within% valid_date_range)

# select only needed variables and rename for others
valid_response_df = valid_response_df %>%
  select(startdate,enddate,status,ipaddress,progress,`duration (in seconds)`,finished,recordeddate,responseid,recipientlastname,recipientfirstname,
    recipientemail,externalreference,locationlatitude,locationlongitude,distributionchannel,q18,q1,q3_3,q3_1,q3_11,q3_5,q3_2,q3_4,q3_7,q3_8,q3_6,
    q3_9,q3_10,q3_20,q4_1,q4_17,q4_9,q4_2,q4_3,q4_4,q4_5,q4_6,q4_7,q4_12,q4_13,q4_31,q4_10,q4_30,q4_11,q4_8,q25,q6,q2,q12,q13,q14_1,q14_2,q14_3,q14_4,
    q14_5,q14_10,q14_11,q14_12,q14_13,q14_14,q14_15,q14_16,q15_1,q15_14,q15_15,q15_3,q15_4,q15_5,q15_6,q15_7,q15_8,q15_10,q15_11,q15_12,q15_13,q15_16,
    q15_17,q15_18,q26,q20,q21) %>%
  rename(duration=`duration (in seconds)`,language=q18,community_role=q1,acceptable_central_office=q3_3,acceptable_athletic_cuts=q3_1,
    acceptable_consolidate_athletic_teams=q3_11,acceptable_eliminate_athletics=q3_5,acceptable_police_cuts=q3_2,acceptable_increase_class_size=q3_4,
    acceptable_close_schools=q3_7,acceptable_furlough=q3_8,acceptable_cut_counseling=q3_6,acceptable_cut_bus=q3_9,acceptable_cut_custodian=q3_10,
    acceptable_four_days=q3_20,rank_ms_athletic=q4_1,rank_consolidate_athletic=q4_17,rank_elim_ms_hs=q4_9,rank_elim_bus=q4_2,rank_elim_bus_extra=q4_3,
    rank_police=q4_4,rank_central_office=q4_5,rank_inc_1=q4_6,rank_inc_2=q4_7,rank_counseling=q4_12, rank_custodian=q4_13, rank_library=q4_31,
    rank_close_hs=q4_10, rank_consolidate_elem=q4_30, rank_furlough=q4_11, rank_four_days=q4_8, budget=q25, additional_input=q6, call_legistor=q2,
    community_role_sp=q12, concerned_sp=q13, acceptable_central_office_sp=q14_1, acceptable_athletic_cuts_sp=q14_2, acceptable_consolidate_athletic_teams_sp=q14_3,
    acceptable_eliminate_athletics_sp=q14_4, acceptable_police_cuts_sp=q14_5, acceptable_increase_class_size_sp=q14_10, acceptable_close_schools_sp=q14_11,
    acceptable_furlough_sp=q14_12, acceptable_cut_counseling_sp=q14_13, acceptable_cut_bus_sp=q14_14, acceptable_cut_custodian_sp=q14_15, acceptable_four_days_sp=q14_16,
    rank_ms_athletic_sp=q15_1, rank_consolidate_athletic_sp=q15_14, rank_elim_ms_hs_sp=q15_15, rank_elim_bus_sp=q15_3, rank_elim_bus_extra_sp=q15_4, rank_police_sp=q15_5,
    rank_central_office_sp=q15_6, rank_inc_1_sp=q15_7, rank_inc_2_sp=q15_8, rank_counseling_sp=q15_10, rank_custodian_sp=q15_11, rank_library_sp=q15_12, rank_close_hs_sp=q15_13,
    rank_consolidate_elem_sp=q15_16, rank_furlough_sp=q15_17, rank_four_days_sp=q15_18, budget_sp=q26, additional_input_sp=q20, call_legistor_sp=q21)

write_csv(valid_response_df, path = "clean_data/budget_survey_text.csv")

##### create a survey response file with numeric answers for those who need this one too
responses_numeric = read_csv("raw_data/Budget Survey Test - Numeric.csv", col_names = TRUE)
responses_numeric_df = responses_numeric[c(3:nrow(responses_numeric)),]
names(responses_numeric_df) = tolower(names(responses_numeric_df))

# clean up responses for valid entries
responses_numeric_df$valid_date_range = interval(mdy_hms("03/24/2017 14:00:00"),mdy_hms("03/30/2017 6:00:00"))
responses_numeric_df$recordeddate = ymd_hms(responses_numeric_df$recordeddate)
valid_response_num_df = responses_numeric_df %>% filter(status != "Survey Preview" & recordeddate %within% valid_date_range)

# select only needed variables and rename for others
valid_response_num_df = valid_response_num_df %>%
  select(startdate,enddate,status,ipaddress,progress,`duration (in seconds)`,finished,recordeddate,responseid,recipientlastname,recipientfirstname,
    recipientemail,externalreference,locationlatitude,locationlongitude,distributionchannel,q18,q1,q3_3,q3_1,q3_11,q3_5,q3_2,q3_4,q3_7,q3_8,q3_6,
    q3_9,q3_10,q3_20,q4_1,q4_17,q4_9,q4_2,q4_3,q4_4,q4_5,q4_6,q4_7,q4_12,q4_13,q4_31,q4_10,q4_30,q4_11,q4_8,q25,q6,q2,q12,q13,q14_1,q14_2,q14_3,q14_4,
    q14_5,q14_10,q14_11,q14_12,q14_13,q14_14,q14_15,q14_16,q15_1,q15_14,q15_15,q15_3,q15_4,q15_5,q15_6,q15_7,q15_8,q15_10,q15_11,q15_12,q15_13,q15_16,
    q15_17,q15_18,q26,q20,q21) %>%
  rename(duration=`duration (in seconds)`,language=q18,community_role=q1,acceptable_central_office=q3_3,acceptable_athletic_cuts=q3_1,
    acceptable_consolidate_athletic_teams=q3_11,acceptable_eliminate_athletics=q3_5,acceptable_police_cuts=q3_2,acceptable_increase_class_size=q3_4,
    acceptable_close_schools=q3_7,acceptable_furlough=q3_8,acceptable_cut_counseling=q3_6,acceptable_cut_bus=q3_9,acceptable_cut_custodian=q3_10,
    acceptable_four_days=q3_20,rank_ms_athletic=q4_1,rank_consolidate_athletic=q4_17,rank_elim_ms_hs=q4_9,rank_elim_bus=q4_2,rank_elim_bus_extra=q4_3,
    rank_police=q4_4,rank_central_office=q4_5,rank_inc_1=q4_6,rank_inc_2=q4_7,rank_counseling=q4_12, rank_custodian=q4_13, rank_library=q4_31,
    rank_close_hs=q4_10, rank_consolidate_elem=q4_30, rank_furlough=q4_11, rank_four_days=q4_8, budget=q25, additional_input=q6, call_legistor=q2,
    community_role_sp=q12, concerned_sp=q13, acceptable_central_office_sp=q14_1, acceptable_athletic_cuts_sp=q14_2, acceptable_consolidate_athletic_teams_sp=q14_3,
    acceptable_eliminate_athletics_sp=q14_4, acceptable_police_cuts_sp=q14_5, acceptable_increase_class_size_sp=q14_10, acceptable_close_schools_sp=q14_11,
    acceptable_furlough_sp=q14_12, acceptable_cut_counseling_sp=q14_13, acceptable_cut_bus_sp=q14_14, acceptable_cut_custodian_sp=q14_15, acceptable_four_days_sp=q14_16,
    rank_ms_athletic_sp=q15_1, rank_consolidate_athletic_sp=q15_14, rank_elim_ms_hs_sp=q15_15, rank_elim_bus_sp=q15_3, rank_elim_bus_extra_sp=q15_4, rank_police_sp=q15_5,
    rank_central_office_sp=q15_6, rank_inc_1_sp=q15_7, rank_inc_2_sp=q15_8, rank_counseling_sp=q15_10, rank_custodian_sp=q15_11, rank_library_sp=q15_12, rank_close_hs_sp=q15_13,
    rank_consolidate_elem_sp=q15_16, rank_furlough_sp=q15_17, rank_four_days_sp=q15_18, budget_sp=q26, additional_input_sp=q20, call_legistor_sp=q21)

write_csv(valid_response_num_df, path = "clean_data/budget_survey_numeric.csv")
