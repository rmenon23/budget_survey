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

accept_long$level[accept_long$value == 1] = "Very"
accept_long$level[accept_long$value == 2] = "Somewhat"
accept_long$level[accept_long$value == 3] = "Not"

accept_long$level = factor(accept_long$level, levels = c("Not","Somewhat", "Very"))

accept_level = accept_long %>% group_by(variable, level) %>% summarize(count = n()) %>% na.omit()
accept_level = accept_level %>% group_by(variable) %>% mutate(total = sum(count), percent = round((count / total) * 100, 1)) 
accept_level$percent2 = ifelse(accept_level$level == "Not", -accept_level$percent, accept_level$percent)

accept_level = accept_level %>% group_by(variable) %>% mutate(positive = ifelse((level == "Very" | level == "Somewhat"), sum(percent2), percent2))

accept_level = accept_level %>% arrange(-positive)

accept_graph = ggplot(accept_level, aes(x = variable, y = percent2, fill = level)) +
  geom_bar(stat = "identity") + 
  coord_flip()
  geom_text(label = percent) + 
  facet_grid(. ~ variable)
