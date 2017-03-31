#### This R creates graphs for budget survey analyses ####

# load in required libraries
library(tidyverse) #need ggplot2, readr, tidyr, dplyr
library(lubridate)
library(stringr)
library(reshape2)

loadfonts(device = "win")
# read in data analyzed by JP
acceptability = read_csv("clean_data/acceptability_responses.csv", col_names = TRUE)
accept_long = melt(acceptability, id = c("id"))

accept_long$agreement[accept_long$value == 1] = "Very"
accept_long$agreement[accept_long$value == 2] = "Somewhat"
accept_long$agreement[accept_long$value == 3] = "Not"

accept_long$agreement = factor(accept_long$agreement, levels = c("Not","Very","Somewhat"))

accept_level = accept_long %>% group_by(variable, agreement) %>% summarize(count = n()) %>% na.omit()
accept_level = accept_level %>% group_by(variable) %>% mutate(total = sum(count), percent = round((count / total) * 100, 1))
accept_level$percent2 = ifelse(accept_level$agreement == "Not", -accept_level$percent, accept_level$percent)

accept_level = accept_level %>% group_by(variable) %>% mutate(positive = ifelse((agreement == "Very" | agreement == "Somewhat"), sum(percent2), percent2))

accept_level = accept_level %>% arrange(-positive)

accept_level$variable = as.character(accept_level$variable)
accept_level$variable[accept_level$variable == "accept_central_office"] = "Central Office"
accept_level$variable[accept_level$variable == "accept_athletic_cuts"] = "Cuts to Athletics"
accept_level$variable[accept_level$variable == "accept_cons_athletic_teams"] = "Consolidate Athletic Teams"
accept_level$variable[accept_level$variable == "accept_cut_custodian"] = "Custodial Services"
accept_level$variable[accept_level$variable == "accept_four_days"] = "Four Day School Weeks"
accept_level$variable[accept_level$variable == "accept_close_schools"] = "Close/Consolidate Schools"
accept_level$variable[accept_level$variable == "accept_cut_bus"] = "Transportation"
accept_level$variable[accept_level$variable == "accept_furlough"] = "Furlough"
accept_level$variable[accept_level$variable == "accept_increase_class_size"] = "Increase Class Sizes"
accept_level$variable[accept_level$variable == "accept_police_cuts"] = "Campus Security"
accept_level$variable[accept_level$variable == "accept_cut_counseling"] = "Student Counseling Services"
accept_level$variable[accept_level$variable == "accept_eliminate_athletics"] = "Eliminate Athletics"

accept_graph = ggplot(accept_level, aes(x = reorder(variable, percent2), y = percent2, fill = agreement)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = paste0(percent,"%"), family = "Arial Narrow"), fontface = "bold", position = position_stack(0.5)) +
  scale_fill_manual(values = c("tomato1","aquamarine3","azure3")) +
  theme(text=element_text(family="Arial Narrow", size = 14, face = "bold"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank())

ggsave("acceptability.jpeg", plot = accept_graph, path = "visualizations/", width = 10, height = 5)
