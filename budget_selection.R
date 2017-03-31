#### This R creates graphs for budget survey analyses ####

# load in required libraries
library(tidyverse) #need ggplot2, readr, tidyr, dplyr
library(lubridate)
library(stringr)
# library(extrafont)
# library(htmltools)
# library(webshot)
# library(formattable)

loadfonts(device = "win")
# read in data from Qualtrics and limit to only the relevant data needed
valid_responses_df = read_csv("clean_data/budget_survey_text.csv", col_names = TRUE)

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
budget_clean$item[budget_clean$value == 36.50] = "Custodial Services"
budget_clean$item[budget_clean$value == 566.58] = "Shortened Year"
budget_clean$item[budget_clean$value == 59.00] = "4 Day Week"
budget_clean$item[budget_clean$value == 26.08] = "Extra Transportation"
budget_clean$item[budget_clean$value == 367.25] = "Transportation"

#the following items have to be assigned based on title
#### CAUTION - NEED TO INCLUDE SPANISH RESPONSES HERE ####
budget_clean$item[grepl("Library",budget_clean$selection) | grepl("Biblioteca",budget_clean$selection)] = "Library Services"
budget_clean$item[grepl("Studentcounsel",budget_clean$selection) | grepl("ServiciosdeConsejería",budget_clean$selection)] = "Counseling Services"
budget_clean$item[grepl("Close1high",budget_clean$selection) | grepl("Cerrarunaescuelapreparatoria",budget_clean$selection)] = "Close HS"
budget_clean$item[grepl("Closeanelem",budget_clean$selection) | grepl("Cerrarunaescuelaprimaria",budget_clean$selection)] = "Close 1st ES"
budget_clean$item[grepl("Closeanadditionalelementaryschoo",budget_clean$selection) | grepl("Cerrarunasegundaescuelaprimaria",budget_clean$selection)] = "Close 2nd ES"
budget_clean$item[budget_clean$selection == "additionalelementaryschool-$27.69" | grepl("Cerrarunaterceraescuelaprimaria",budget_clean$selection)] = "Close 3rd ES"
budget_clean$item[grepl("byonestudent",budget_clean$selection) | budget_clean$selection == "Númerodealumnosporsalón:Aumentarelpromediodealumnosporsalóndesdekínderadoceavogradoporunalumno-$127.75"] = "Increase by 1"
budget_clean$item[grepl("byanadditionalonestudent",budget_clean$selection) | budget_clean$selection == "Númerodealumnosporsalón:Aumentarelpromediodealumnosporsalóndesdekínderadoceavogradoporunalumnoadicional-$127.75"] = "Increase by 2"

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

##### Need to create the top 3 budgeted bundles #####
budget_spread = budget_clean %>% select(id, item, value) %>% spread(item, value)

# create a blank character vector to fill the selections that are non-empty
budget_spread$selection1 = ""
budget_spread$selection2 = ""
budget_spread$selection3 = ""
budget_spread$selection4 = ""
budget_spread$selection5 = ""
budget_spread$selection6 = ""
budget_spread$selection7 = ""
budget_spread$selection8 = ""
budget_spread$selection9 = ""
budget_spread$selection10 = ""
budget_spread$selection11 = ""
budget_spread$selection12 = ""
budget_spread$selection13 = ""
budget_spread$selection14 = ""
budget_spread$selection15 = ""
budget_spread$selection16 = ""
budget_spread$selection17 = ""

# fill selection based on the corresponding column header it belongs to
budget_spread$selection1[!is.na(budget_spread$`MS Athletics`)] = colnames(budget_spread)[2]
budget_spread$selection2[!is.na(budget_spread$`HS Athletics`)] = colnames(budget_spread)[3]
budget_spread$selection3[!is.na(budget_spread$`Campus Police`)] = colnames(budget_spread)[4]
budget_spread$selection4[!is.na(budget_spread$`Central Office`)] = colnames(budget_spread)[5]
budget_spread$selection5[!is.na(budget_spread$`Custodial Services`)] = colnames(budget_spread)[6]
budget_spread$selection6[!is.na(budget_spread$`Library Services`)] = colnames(budget_spread)[7]
budget_spread$selection7[!is.na(budget_spread$`Counseling Services`)] = colnames(budget_spread)[8]
budget_spread$selection8[!is.na(budget_spread$`Close HS`)] = colnames(budget_spread)[9]
budget_spread$selection9[!is.na(budget_spread$`Close 1st ES`)] = colnames(budget_spread)[10]
budget_spread$selection10[!is.na(budget_spread$`Close 2nd ES`)] = colnames(budget_spread)[11]
budget_spread$selection11[!is.na(budget_spread$`Close 3rd ES`)] = colnames(budget_spread)[12]
budget_spread$selection12[!is.na(budget_spread$`Increase by 1`)] = colnames(budget_spread)[13]
budget_spread$selection13[!is.na(budget_spread$`Increase by 2`)] = colnames(budget_spread)[14]
budget_spread$selection14[!is.na(budget_spread$`Shortened Year`)] = colnames(budget_spread)[15]
budget_spread$selection15[!is.na(budget_spread$`4 Day Week`)] = colnames(budget_spread)[16]
budget_spread$selection16[!is.na(budget_spread$`Extra Transportation`)] = colnames(budget_spread)[17]
budget_spread$selection17[!is.na(budget_spread$`Transportation`)] = colnames(budget_spread)[18]

# concatenate all of the selections together to get the bundle for each respondant
args = c(list(budget_spread$selection1,budget_spread$selection2,budget_spread$selection3,
  budget_spread$selection4,budget_spread$selection5,budget_spread$selection6,budget_spread$selection7,budget_spread$selection8,
  budget_spread$selection9,budget_spread$selection10,budget_spread$selection11,budget_spread$selection12,budget_spread$selection13,
  budget_spread$selection14,budget_spread$selection15,budget_spread$selection16,budget_spread$selection17), sep = ",")

budget_spread$selection = do.call(paste, args)

# collapse by to get the frequency of each bundle
budget_freq = budget_spread %>% group_by(selection) %>% summarize(freq = n()) %>% arrange(-freq)
budget_freq2 = budget_freq %>% top_n(4) %>% separate(selection, into = c("item1","item2","item3","item4","item5","item6","item7","item8","item9","item10",
  "item11","item12","item13","item14","item15","item16","item17"),sep = ",") %>% mutate(ranking = rank(desc(freq)))

top_budget_bundle = budget_freq2 %>% gather(budget_freq, item, -ranking) %>% arrange(ranking) %>% filter(item != "")

item_to_value = budget_clean %>% group_by(item, value) %>% summarize() %>% select(item = item, value = value)
write_excel_csv(item_to_value, path = "clean_data/item_to_value.csv")


top_budget_bundle = top_budget_bundle %>% left_join(item_to_value, by = "item") %>% filter(budget_freq != "freq") %>% select(ranking, item, value)
budget_1 = top_budget_bundle %>% filter(ranking == 1) %>% rename(rank_1_values = value) %>% select(item, rank_1_values)
total_bud_1 = data.frame(item = c("Total"), rank_1_values = (sum(budget_1$rank_1_values)))
budget_1 = bind_rows(budget_1, total_bud_1)

budget_2 = top_budget_bundle %>% filter(ranking == 2) %>% rename(rank_2_values = value) %>% select(item, rank_2_values)
total_bud_2 = data.frame(item = c("Total"), rank_2_values = (sum(budget_2$rank_2_values)))
budget_2 = bind_rows(budget_2, total_bud_2)

budget_3 = top_budget_bundle %>% filter(ranking == 3) %>% rename(rank_3_values = value) %>% select(item, rank_3_values)
total_bud_3 = data.frame(item = c("Total"), rank_3_values = (sum(budget_3$rank_3_values)))
budget_3 = bind_rows(budget_3, total_bud_3)

budget_4 = top_budget_bundle %>% filter(ranking == 4) %>% rename(rank_4_values = value) %>% select(item, rank_4_values)
total_bud_4 = data.frame(item = c("Total"), rank_4_values = (sum(budget_4$rank_4_values)))
budget_4 = bind_rows(budget_4, total_bud_4)

top_budgets = budget_1 %>% full_join(budget_2, by = "item") %>% full_join(budget_3, by = "item")  %>% full_join(budget_4, by = "item")

# top_budgets_table = formattable(top_budgets, list(rank_1_values = color_tile("green")))

# export the created table and format the table in Excel
write_excel_csv(top_budgets, path = "visualizations/top_budget_combos.csv")
