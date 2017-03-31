#### This R creates graphs for budget survey analyses ####

# load in required libraries
library(tidyverse) #need ggplot2, readr, tidyr, dplyr
library(lubridate)
library(stringr)
library(extrafont)
library(htmltools)
library(webshot)
library(formattable)

loadfonts(device = "win")
# read in data from Qualtrics and limit to only the relevant data needed
valid_responses_df = read_csv("clean_data/budget_survey_text.csv", col_names = TRUE)

##### Create a rank table for all community members #####
comm_rankings = valid_responses_df %>% select(community_role, rank_ms_athletic, rank_consolidate_athletic, rank_elim_ms_hs, rank_elim_bus_extra, rank_police, rank_central_office,
  rank_inc_1, rank_inc_2, rank_counseling, rank_custodian, rank_library, rank_close_hs, rank_consolidate_elem, rank_furlough, rank_four_days, rank_elim_bus)

comm_rankings_long = comm_rankings %>%
  separate(community_role, into = c("member1","member2","member3","member4","member5","member6","member7","member8","member9","member10","member11","member12"), sep = ",")

#export the table for team member to community member list
library(reshape2)
comm_rankings_long2 = melt(comm_rankings_long, id = c("member1","member2","member3","member4","member5","member6","member7","member8","member9","member10","member11","member12"))
write_excel_csv(comm_rankings_long2, path = "clean_data/role_rank_incomplete.csv")
