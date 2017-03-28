
# load in required libraries
library(tidyverse) #need ggplot2, readr, tidyr, dplyr
library(lubridate)
library(stringr)
library(extrafont)

loadfonts(device = "win")
# make sure to set the working directory
setwd()
# read in data from Qualtrics and limit to only the relevant data needed
valid_responses_df = read_csv("budget_survey_text.csv", col_names = TRUE)

##### who responded to the survey #####
members = valid_responses_df %>% select(community_role, community_role_sp)
members$community_role[members$community_role_sp == "Maestro de TPS"] = "TPS teacher"
members$community_role[members$community_role_sp == "Padre de un alumno actual o futuro de TPS"] = "Parent of current or future TPS student"
members$community_role_sp = NULL

members_counted = members %>%
  separate(community_role, into = c("member1","member2","member3","member4","member5","member6","member7","member8","member9","member10","member11","member12"), sep = ",") %>%
  gather() %>% na.omit() %>% group_by(value) %>% summarize(count = n()) %>% mutate(total = sum(count), percent = count / total) %>% arrange(percent)

members_counted$ymax = cumsum(members_counted$percent)
members_counted$ymin = c(0, head(members_counted$ymax, n = -1))

# creates a donut chart all self-identified community coles
members_plot = ggplot(members_counted, aes(fill = value, ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5)) +
  geom_rect() + coord_polar(theta = "y") +
  xlim(c(0, 4)) +
  theme(text=element_text(family="Arial Narrow", size = 14, face = "bold")) +
  theme(panel.grid=element_blank()) +
  theme(panel.background=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "4433\n Community Members\n Responded", size = 7.5, fontface = 2, family="Arial Narrow") +
  labs(x = "", y = "",title="") + scale_fill_brewer(palette = "Paired")

##### what things did folks pick for the 1000 question #####
eng_budget = valid_responses_df %>% select(budget) %>% na.omit()
span_budget = valid_responses_df %>% select(budget_sp) %>% rename(budget = budget_sp) %>% na.omit()
budget = bind_rows(eng_budget, span_budget)
budget_sep = budget %>% separate(budget, into = c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15","b16","b17","b18","b19","b20","b21","b22","b23","b24","b25"), sep = ",")
budget_sep$id = rownames(budget_sep)

# create a long data set to get the dollar values
budget = budget_sep %>% gather(budget_sep, selection, -id) %>% na.omit()
budget$selection = str_replace_all(budget$selection, " ", "")
budget$value = str_extract(budget$selection, "\\$[0-9].+")
budget = budget %>% na.omit() %>% arrange(id, budget_sep)
budget$value = as.numeric(str_sub(budget$value, start = 2))

budget_clean = budget

# clean up categories for selection based on value
budget_clean$item[budget_clean$value == 11.52] = "MS Athletics"
budget_clean$item[budget_clean$value == 151.32] = "HS Athletics"
budget_clean$item[budget_clean$value == 54.17] = "Campus Police"
budget_clean$item[budget_clean$value == 25] = "Central Office"
budget_clean$item[budget_clean$value == 36.5] = "Custodial Services"
budget_clean$item[budget_clean$value == 566.58] = "Shortened Year"
budget_clean$item[budget_clean$value == 59.00] = "4 Day Week"
budget_clean$item[budget_clean$value == 26.08] = "Extra Transportation"
budget_clean$item[budget_clean$value == 367.25] = "Transportation"

#the following items have to be assigned based on title
budget_clean$item[grepl("Library",budget_clean$selection)] = "Library Services"
budget_clean$item[grepl("Studentcounsel",budget_clean$selection)] = "Counseling Services"
budget_clean$item[grepl("Close1high",budget_clean$selection)] = "Close HS"
budget_clean$item[grepl("Closeanelem",budget_clean$selection)] = "Close 1st ES"
budget_clean$item[grepl("Closeanadditionalelementaryschoo",budget_clean$selection)] = "Close 2nd ES"
budget_clean$item[budget_clean$selection == "additionalelementaryschool-$27.69"] = "Close 3rd ES"
budget_clean$item[grepl("byonestudent",budget_clean$selection)] = "Increase by 1"
budget_clean$item[grepl("byanadditionalonestudent",budget_clean$selection)] = "Increase by 2"

budget_clean$item = factor(budget_clean$item, levels = c("MS Athletics","HS Athletics","Campus Police",
                                     "Central Office","Custodial Services",
                                     "Library Services","Counseling Services","Close HS",
                                     "Close 1st ES","Close 2nd ES","Close 3rd ES",
                                     "Increase by 1","Increase by 2","Shortened Year",
                                     "4 Day Week","Extra Transportation","Transportation"))

budget_clean = budget_clean %>% group_by(id) %>% arrange(id, item) %>% mutate(rising_total = cumsum(value)) %>% na.omit()
# create a step graph
budget_decisions = ggplot(budget_clean, aes(x = item, y = rising_total, group = id)) +
  geom_step(alpha = 0.1, color = "steelblue", size = 0.2) +
  theme_minimal()
