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
read.csv("data/survey/JNU area survey 1979-1993.csv") %>%
  clean_names() -> jnu_survey_part_1

read.csv("data/survey/JNU area survey 1994-2022.csv") %>%
  clean_names() -> jnu_survey_part_2

#Join both stes of survey data
bind_rows(jnu_survey_part_1, jnu_survey_part_2) -> jnu_survey


#Create density plots for male crab

#Repeat rows that have a subsample rate > 1
data.frame(lapply(jnu_survey, rep, jnu_survey$number_of_specimens)) -> jnu_survey_rep

jnu_survey_rep %>%
  filter(sex == "Male") %>%
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
                     name = "Carapace Length (mm)") +
  scale_y_continuous(breaks = seq(0, 2020, by = 5),
                     name = "Year") +
  scale_fill_viridis_c() +
  ggtitle("JNU Area RKC Male Length Frequencies") +
  theme(legend.title = element_blank(),
        strip.background = element_blank()) -> fig1

ggsave(paste0('./figures/', cur_yr,'/rkc_male_length_comps.png'), fig1,  
       dpi = 600, width = 9, height = 10)

#Female density plots
jnu_survey_rep %>%
  filter(sex == "Female") %>%
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
                     name = "Carapace Length (mm)") +
  scale_y_continuous(breaks = seq(0, 2020, by = 5),
                     name = "Year") +
  scale_fill_viridis_c() +
  #scale_fill_natparks_c("Banff") +
  ggtitle("JNU Area RKC Female Length Frequencies") +
  theme(legend.title = element_blank(),
        strip.background = element_blank()) -> fig1

ggsave(paste0('./figures/', cur_yr,'/rkc_female_length_comps.png'), fig1,  
       dpi = 600, width = 9, height = 10)

         
