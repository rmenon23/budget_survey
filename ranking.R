#### This R script creates graph/table for the ranking questions ####

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
valid_responses_df = read_csv("clean_data/budget_survey_numeric.csv", col_names = TRUE)

valid_responses_df$rank_ms_athletic[is.na(valid_responses_df$rank_ms_athletic)] = valid_responses_df$rank_ms_athletic_sp
valid_responses_df$rank_consolidate_athletic[is.na(valid_responses_df$rank_consolidate_athletic)] = valid_responses_df$rank_consolidate_athletic_sp
valid_responses_df$rank_elim_ms_hs[is.na(valid_responses_df$rank_elim_ms_hs)] = valid_responses_df$rank_elim_ms_hs_sp
valid_responses_df$rank_elim_bus_extra[is.na(valid_responses_df$rank_elim_bus_extra)] = valid_responses_df$rank_elim_bus_extra_sp
valid_responses_df$rank_police[is.na(valid_responses_df$rank_police)] = valid_responses_df$rank_police_sp
valid_responses_df$rank_central_office[is.na(valid_responses_df$rank_central_office)] = valid_responses_df$rank_central_office_sp
valid_responses_df$rank_inc_1[is.na(valid_responses_df$rank_inc_1)] = valid_responses_df$rank_inc_1_sp
valid_responses_df$rank_inc_2[is.na(valid_responses_df$rank_inc_2)] = valid_responses_df$rank_inc_2_sp
valid_responses_df$rank_counseling[is.na(valid_responses_df$rank_counseling)] = valid_responses_df$rank_counseling_sp
valid_responses_df$rank_custodian[is.na(valid_responses_df$rank_custodian)] = valid_responses_df$rank_custodian_sp
valid_responses_df$rank_library[is.na(valid_responses_df$rank_library)] = valid_responses_df$rank_library_sp
valid_responses_df$rank_close_hs[is.na(valid_responses_df$rank_close_hs)] = valid_responses_df$rank_close_hs_sp
valid_responses_df$rank_consolidate_elem[is.na(valid_responses_df$rank_consolidate_elem)] = valid_responses_df$rank_consolidate_elem_sp
valid_responses_df$rank_furlough[is.na(valid_responses_df$rank_furlough)] = valid_responses_df$rank_furlough_sp
valid_responses_df$rank_four_days[is.na(valid_responses_df$rank_four_days)] = valid_responses_df$rank_four_days_sp
valid_responses_df$rank_elim_bus[is.na(valid_responses_df$rank_elim_bus)] = valid_responses_df$rank_elim_bus_sp

rankings = valid_responses_df %>% select(rank_ms_athletic, rank_consolidate_athletic, rank_elim_ms_hs, rank_elim_bus_extra, rank_police, rank_central_office,
  rank_inc_1, rank_inc_2, rank_counseling, rank_custodian, rank_library, rank_close_hs, rank_consolidate_elem, rank_furlough, rank_four_days, rank_elim_bus) %>%
  gather() %>% na.omit()

ranked_first = rankings %>% filter(value == 1) %>% group_by(key) %>% summarize(count = n()) %>% mutate(total = sum(count), percent = (count / total) * 100) %>% arrange(-percent) %>% top_n(3)
ranked_last = rankings %>% filter( value == 16) %>% group_by(key) %>% summarize(count = n()) %>% mutate(total = sum(count), percent = (count / total) * 100) %>% arrange(-percent) %>% top_n(3)
ranked_last$percent = -ranked_last$percent
rank_list = bind_rows(ranked_first, ranked_last)
rank_list$percent = round(as.numeric(rank_list$percent),0)
rank_list$percent_label = abs(rank_list$percent)

rank_list$sign[rank_list$percent > 0] = "pos"
rank_list$sign[rank_list$percent < 0] = "neg"

rank_list$key[rank_list$key == "rank_central_office"] = "Central Office"
rank_list$key[rank_list$key == "rank_four_days" & rank_list$sign == "pos"] = "Four Day School Weeks"
rank_list$key[rank_list$key == "rank_four_days" & rank_list$sign == "neg"] = " Four Day School Weeks"
rank_list$key[rank_list$key == "rank_ms_athletic"] = "Eliminate Middle School Athletics"
rank_list$key[rank_list$key == "rank_furlough"] = "Furlough"
rank_list$key[rank_list$key == "rank_close_hs"] = "Close a High School"

rank_list = rank_list %>% arrange(-percent)

first_last_graph = ggplot(rank_list, aes(x = reorder(key, percent), y = percent, fill = sign)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = paste0(percent_label,"%"), family = "Arial Narrow"), size = 5, fontface = "bold", position = position_stack(0.5)) +
  scale_fill_manual(values = c("#E84F25","#6CCD40")) +
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

ggsave("First and Last Rank.jpeg", plot = first_last_graph, path = "visualizations/", width = 8, height = 5)

rankings$value = as.numeric(rankings$value)
overall_rank = rankings %>% group_by(key) %>% summarize(rank = sum(value)) %>% mutate(ranking = rank(rank)) %>% arrange(ranking) %>% select(key, ranking)

overall_rank$key[overall_rank$key == "rank_central_office"] = "Further reduce central office services to schools"
overall_rank$key[overall_rank$key == "rank_consolidate_athletic"] = "Consolidate athletic teams across schools (ex:  combine one school's team with another)"
overall_rank$key[overall_rank$key == "rank_ms_athletic"] = "Eliminate middle school athletic programming"
overall_rank$key[overall_rank$key == "rank_elim_bus_extra"] = "Eliminate all bus transportation for extracurriculars (games, competitions, field trips, etc.)"
overall_rank$key[overall_rank$key == "rank_inc_1"] = "Further increase class size by 1 student"
overall_rank$key[overall_rank$key == "rank_custodian"] = "Reduce custodial services to schools"
overall_rank$key[overall_rank$key == "rank_police"] = "Further reduce campus security and police expenditures"
overall_rank$key[overall_rank$key == "rank_library"] = "Reduce library staffing and distribute library staff across schools"
overall_rank$key[overall_rank$key == "rank_four_days"] = "4-day school and staff work weeks (same number of hours for student and staff spread over 4 days)"
overall_rank$key[overall_rank$key == "rank_elim_ms_hs"] = "Eliminate middle and high school athletic programming"
overall_rank$key[overall_rank$key == "rank_inc_2"] = "Further increase class size by 2 students"
overall_rank$key[overall_rank$key == "rank_counseling"] = "Reduce counseling positions and distribute counseling staff across schools."
overall_rank$key[overall_rank$key == "rank_consolidate_elem"] = "Consolidate schools (ex:  combine 3 elementary schools)"
overall_rank$key[overall_rank$key == "rank_furlough"] = "Shorten school year and reduce the paid work year of all TPS employees by 7 days"
overall_rank$key[overall_rank$key == "rank_close_hs"] = "Close schools (ex:  close a high school)"
overall_rank$key[overall_rank$key == "rank_elim_bus"] = "Eliminate bus services except as required by law for special education and homeless students"

overall_rank = overall_rank %>% rename(`Budget Item` = key, Rank = ranking) %>% select(Rank, `Budget Item`)
write_excel_csv(overall_rank, path = "visualizations/overall_rank.csv")

overall_rank$Rank = as.character(overall_rank$Rank)
overall_rank_table = formattable(overall_rank, list(Rank = formatter("span",
  style = x ~ style(display = "block", "border-radius" = "4px", "padding-right" = "4px", color = "white", "background-color" = "#E84F25", "font-weight" = "bold"))))

export_formattable <- function(f, file, width = "80%", height = NULL, background = "white", delay = 0.2){
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

export_formattable(overall_rank_table, "visualizations/overall_rank.png")
