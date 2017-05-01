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

##### who responded to the survey #####
members = valid_responses_df %>% select(community_role, community_role_sp)
members$community_role[is.na(members$community_role)] = members$community_role_sp
members$community_role_sp = NULL

members$id = rownames(members)
members_counted = members %>%
  separate(community_role, into = c("member1","member2","member3","member4","member5","member6","member7","member8","member9","member10","member11","member12"), sep = ",") %>%
  gather(members, member, -id) %>% na.omit()

members_counted$member[members_counted$member == "Alumno de TPS"] = "TPS student"
members_counted$member[members_counted$member == "Maestro de TPS"] = "TPS teacher"
members_counted$member[members_counted$member == "Miembro del personal de una escuela de TPS"] = "TPS school building staff member"
members_counted$member[members_counted$member == "Padre de un alumno actual o futuro de TPS"] = "Parent of current or future TPS student"
members_counted$member[members_counted$member == "Residente en Tulsa pero sin hijos en las Escuelas de TPS"] = "Tulsa resident (non-parent)"
members_counted$member[members_counted$member == "Voluntario en las Escuelas Públicas de Tulsa"] = "Volunteer for Tulsa Public Schools"
members_counted$member[members_counted$member == "Otro puesto en las escuelas de TPS"] = "Other TPS employee"
members_counted$member[members_counted$member == "Otro educador"] = "Other educator"
members_counted$member[members_counted$member == "Miembro del personal de la oficina central de TPS"] = "TPS central office staff member"
members_counted$member[members_counted$member == "Empleado retirado de TPS"] = "Retired TPS employee"
members_counted$member[members_counted$member == "Director de TPS"] = "TPS principal"
members_counted$member[members_counted$member == "Otro"] = "Other"

members_counted = members_counted %>% group_by(member) %>% summarize(count = n())
members_counted$adjust_count = members_counted$count + 20
write_excel_csv(members_counted, path = "visualizations/members_counted.csv")

members_plot = ggplot(members_counted, aes(x = reorder(member, count), y = adjust_count)) +
      theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.background = element_blank(),
            text=element_text(family="Arial Narrow", size = 14, face = "bold"),
            legend.position = "none")+
      geom_bar(stat = "identity", fill = "#6E2B70") +
      coord_flip() +
      geom_text(aes(label=count, family="Arial Narrow"), size = 3.9, color = "white", hjust = 1.05, fontface = "bold")

ggsave("community members.jpeg", plot = members_plot, path = "visualizations/", width = 10, height = 5)

# creates a donut chart all self-identified community coles
members_plot2 = ggplot(members_counted, aes(fill = value, ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5)) +
 geom_rect() + coord_polar(theta = "y") +
 geom_text_repel(aes(label = paste(value,"\n",count), x = 4, y = (ymin + ymax)), size = 4) +
 xlim(c(0, 4)) +
 theme(text=element_text(family="Arial Narrow", size = 14, face = "bold")) +
 theme(panel.grid=element_blank()) +
 theme(panel.background=element_blank()) +
 theme(axis.text=element_blank()) +
 theme(axis.ticks=element_blank()) +
 annotate("text", x = 0, y = 0, label = "4433\n Community Members\n Responded", size = 7.5, fontface = 2, family="Arial Narrow") +
 labs(x = "", y = "",title="") + scale_fill_brewer(palette = "Paired")
