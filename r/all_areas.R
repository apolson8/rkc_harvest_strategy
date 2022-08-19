library(tidyverse)
library(lubridate)
library(readxl)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(tidyr)
library(padr)
library(anytime)
library(RColorBrewer)
library(cowplot)
library(ggridges)
library(janitor)
library(treemap)
library(treemapify)
library(wesanderson)

# global ---------
cur_yr = 2022 # most recent year of data
fig_path <- paste0('figures/', cur_yr) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', cur_yr) # output and results
dir.create(output_path) 

##THEMES FOR GRAPHS ---------
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=16,base_family='seriff') + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(face = "bold")))

#Load Data------
read.csv("data/survey/rkc survey specimen data 1978-1985.csv") %>%
  clean_names() ->survey_78_85

read.csv("data/survey/rkc survey specimen data 1986-1995.csv") %>%
  clean_names() ->survey_86_95

read.csv("data/survey/rkc survey specimen data 1996-2006.csv") %>%
  clean_names() ->survey_96_06

read.csv("data/survey/rkc survey specimen data 2007-2017.csv") %>%
  clean_names() ->survey_07_17

read.csv("data/survey/rkc survey specimen data 2018-2022.csv") %>%
  clean_names() %>% 
  mutate(tag_no = ifelse(tag_no == "NO TAG", "NA", tag_no)) %>%
  mutate_at('tag_no', as.numeric) -> survey_18_22


#Join sets of survey data
bind_rows(survey_78_85, survey_86_95, survey_96_06, survey_07_17, survey_18_22) -> survey_data

#Combine JNU and Barlow Cove to represent "Juneau Area"

survey_data %>%
  mutate(location = ifelse(location == "Barlow Cove", "Juneau", location)) -> survey_data

#Create density plots for male crab

#Repeat rows that have a subsample rate > 1
data.frame(lapply(survey_data, rep, survey_data$number_of_specimens)) -> survey_rep

survey_rep %>%
  filter(sex == "Male",
         year > 2004,
         location == c("Barlow Cove", 
         "Deadman Reach", 
         "Excursion Inlet",
         "Gambier Bay",
         "Juneau",
         "Lynn Sisters",
         "Pybus Bay",
         "Seymour Canal")) %>%
  ggplot(aes(x = length_millimeters,
             y = year,
             group = year,
             fill = year)) +
  geom_density_ridges(aes(point_fill = year, 
                          point_color = year),
                      jittered_points = FALSE, 
                      scale = 5.0, 
                      alpha = 0.3, 
                      point_alpha = 1, 
                      lwd = 0.6) +
  scale_x_continuous(breaks = seq(0, 250, by = 20),
                                  limits = c(20, 240),
                     name = "Carapace Length (mm)") +
  scale_y_continuous(breaks = seq(0, 2022, by = 2),
                     name = "Year") +
  scale_fill_viridis_c() +
  ggtitle("RKC Male Length Frequencies") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  facet_wrap(~ location)-> fig1

ggsave(paste0('./figures/', cur_yr,'/rkc_male_length_comps_all_areas.png'), fig1,  
       dpi = 600, width = 9, height = 10)

survey_rep %>%
  filter(sex == "Female",
         year > 2004,
         location == c("Seymour Canal")) %>%
                       #"Deadman Reach", 
                       #"Excursion Inlet",
                       #"Gambier Bay",
                       #"Juneau",
                       #"Lynn Sisters",
                       #"Pybus Bay",
                       #"Seymour Canal")) %>%
  ggplot(aes(x = length_millimeters,
             y = year,
             group = year,
             fill = year)) +
  geom_density_ridges(aes(point_fill = year, 
                          point_color = year),
                      jittered_points = FALSE, 
                      scale = 5.0, 
                      alpha = 0.3, 
                      point_alpha = 1, 
                      lwd = 0.6) +
  scale_x_continuous(breaks = seq(0, 250, by = 20),
                     limits = c(40, 200) ,
                     name = "Carapace Length (mm)") +
  scale_y_continuous(breaks = seq(0, 2022, by = 2),
                     name = "Year") +
  scale_fill_viridis_c() +
  ggtitle("Seymour RKC Female Length Frequencies") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) -> fig2
  #facet_wrap(~ location)-> fig2

ggsave(paste0('./figures/', cur_yr,'/rkc_female_length_comps_seymour.png'), fig2,  
       dpi = 600, width = 9, height = 10)
