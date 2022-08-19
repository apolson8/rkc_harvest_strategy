#Southeast RKC/BKC Harvest Strategy
#Andrew Olson 9-15-21

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
read.csv("data/survey/biomass_2022.csv") %>%
  clean_names() -> area_biomass

read.csv("data/survey/regional_biomass_2022.csv") %>%
  clean_names() -> regional_biomass

read.csv("data/fishery/rkc_fishticket.csv") %>%
  clean_names() -> rkc_fishticket


#Create Harvest Rate Model Example-----

h_strat <- data.frame(mmb = c(0.5, 0.5, 1.0, 1.5,
                              0.5, 0.5, 1.0, 1.5,
                              0.5, 0.5, 1.0, 1.5,
                              0.5, 0.5, 1.0, 1.5),
                      harv_rate = c(0, 0.05, 0.08, 0.08,
                                    0, 0.05, 0.09, 0.09,
                                    0, 0.05, 0.1, 0.1,
                                    0, 0.025, 0.09, 0.09),
                      scenario = c("Scenario 1.1", "Scenario 1.1", "Scenario 1.1", "Scenario 1.1", 
                                   "Scenario 1.2", "Scenario 1.2", "Scenario 1.2", "Scenario 1.2",
                                   "Scenario 1.3", "Scenario 1.3", "Scenario 1.3", "Scenario 1.3",
                                   "Scenario 2.1", "Scenario 2.1", "Scenario 2.1", "Scenario 2.1")
)

print(h_strat)

#Visualize harvest model
h_strat %>%
  filter(scenario == "Scenario 1.3") %>%
  ggplot(aes(mmb, harv_rate)) +
  geom_line(lwd = 1) +
  #geom_segment(aes(x = 0, y = 0, xend = 0.5, yend= 0), lwd = 1) +
  geom_vline(xintercept = 1, color = "green", lwd = 1, linetype = "dashed") +
  geom_vline(xintercept = 0.5, color = "orange", lwd = 1, linetype = "dashed") +
  #geom_vline(xintercept = 0.2, color = "red", lwd = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1.5, 0.1),
                     limits = c(0, 1.5),
                     expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 0.15, 0.01), 
                     limits = c(0.0, 0.15),
                     expand = c(0,0)) +
  xlab(bquote(LMB/LMB[AVG])) +
  ylab("Harvest rate on LMB") +
  theme(legend.position = "none", #"none" to remove legend
        legend.title = element_blank()) -> fig1

ggsave(paste0('./figures/', cur_yr,'/rkc_hcrs.png'), fig1,  
       dpi = 600, width = 10, height = 6)


glimpse(regional_biomass)

#Regional biomass figures----

##Determine mean MMB 1979-2020
regional_biomass %>%
  filter(year %in% 1979:2020) %>%
  summarise(mean_mmb = mean(adj_mature)) -> avg_mmb

avg_mmb

avg_mmb * 0.5 -> avg_mmb_fifty

avg_mmb_fifty

regional_biomass %>%
  ggplot(aes(year, adj_mature)) +
  geom_line(lwd = 1) +
  geom_point(size = 3,
             aes(shape = status)) +
  geom_hline(yintercept = avg_mmb$mean,
             color = "green",
             lwd = 1,
             alpha = 0.5) +
  geom_hline(yintercept = avg_mmb$mean * 0.5,
             color = "orange",
             lwd = 1, 
             linetype = "dashed",
             alpha = 0.5) +
  ylab("CSA Adjusted MMB (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1978, cur_yr, 2)) +
  scale_y_continuous(breaks =seq(0, 10000000, 500000),
                     limits = c(0, 7000000),
                     expand = c(0,0),
                     labels = scales::comma) +
  ggtitle("Mature Male Biomass (MMB) of Southeast red king crab") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))


#Retrospective TAC recommendations via St. Matthew BKC 5 AAC 34.917
avg_mmb
avg_mmb_fifty


#HCR 1.1
regional_biomass %>%
  mutate(scenario = "HCR 1.1", 
         mmb_non_survey = (adj_mature/0.528)-adj_mature,
         mmb_bkc = adj_mature * 0.0106,
         mmb_regional = adj_mature + mmb_non_survey + mmb_bkc,
         lmb_non_survey = (adj_legal/0.528)-adj_legal,
         lmb_bkc = adj_legal * 0.0106,
         lmb_regional = adj_legal + lmb_non_survey + lmb_bkc,
         harv_rate_mature = ifelse(adj_mature < avg_mmb_fifty$mean, 0,
                                   ifelse(adj_mature >= avg_mmb_fifty$mean & 
                                            adj_mature < avg_mmb$mean, 
                                          0.1*(adj_mature/avg_mmb$mean),
                                          ifelse(adj_mature >= avg_mmb$mean, 0.08, 999))),
         harv_rate_legal = 0.25, 
         max_tac_legal = 0.25 * lmb_regional,
         max_tac_mature = harv_rate_mature * mmb_regional,
         max_tac = ifelse(max_tac_mature < max_tac_legal,
                          max_tac_mature, 
                          max_tac_legal))-> rkc_hcr1.1

rkc_hcr1.1 %>%
  select(scenario, year, mmb_regional,
         harv_rate_mature, max_tac) -> rkc_tac_summary_hcr1.1



#HCR 1.2
regional_biomass %>%
  mutate(scenario = "HCR 1.2", 
         mmb_non_survey = (adj_mature/0.528)-adj_mature,
         mmb_bkc = adj_mature * 0.0106,
         mmb_regional = adj_mature + mmb_non_survey + mmb_bkc,
         lmb_non_survey = (adj_legal/0.528)-adj_legal,
         lmb_bkc = adj_legal * 0.0106,
         lmb_regional = adj_legal + lmb_non_survey + lmb_bkc,
         harv_rate_mature = ifelse(adj_mature < avg_mmb_fifty$mean, 0,
                                   ifelse(adj_mature >= avg_mmb_fifty$mean & 
                                            adj_mature < avg_mmb$mean, 
                                          0.1*(adj_mature/avg_mmb$mean),
                                          ifelse(adj_mature >= avg_mmb$mean, 0.09, 999))),
         harv_rate_legal = 0.25, 
         max_tac_legal = 0.25 * lmb_regional,
         max_tac_mature = harv_rate_mature * mmb_regional,
         max_tac = ifelse(max_tac_mature < max_tac_legal,
                          max_tac_mature, 
                          max_tac_legal))-> rkc_hcr1.2

rkc_hcr1.2 %>%
  select(scenario, year, mmb_regional,
         harv_rate_mature, max_tac) -> rkc_tac_summary_hcr1.2

#HCR 1.3
regional_biomass %>%
  mutate(scenario = "HCR 1.3", 
        mmb_non_survey = (adj_mature/0.528)-adj_mature,
         mmb_bkc = adj_mature * 0.0106,
         mmb_regional = adj_mature + mmb_non_survey + mmb_bkc,
         lmb_non_survey = (adj_legal/0.528)-adj_legal,
         lmb_bkc = adj_legal * 0.0106,
         lmb_regional = adj_legal + lmb_non_survey + lmb_bkc,
         harv_rate_mature = ifelse(adj_mature < avg_mmb_fifty$mean, 0,
                                   ifelse(adj_mature >= avg_mmb_fifty$mean & 
                                            adj_mature < avg_mmb$mean, 
                                          0.1*(adj_mature/avg_mmb$mean),
                                          ifelse(adj_mature >= avg_mmb$mean, 0.1, 999))),
         harv_rate_legal = 0.25, 
         max_tac_legal = 0.25 * lmb_regional,
         max_tac_mature = harv_rate_mature * mmb_regional,
         max_tac = ifelse(max_tac_mature < max_tac_legal,
                          max_tac_mature, 
                          max_tac_legal))-> rkc_hcr1.3


rkc_hcr1.3 %>%
  select(scenario, year, mmb_regional,
         harv_rate_mature, max_tac) -> rkc_tac_summary_hcr1.3


#HCR 2.1
regional_biomass %>%
  mutate(scenario = "HCR 2.1", 
         mmb_non_survey = (adj_mature/0.528)-adj_mature,
         mmb_bkc = adj_mature * 0.0106,
         mmb_regional = adj_mature + mmb_non_survey + mmb_bkc,
         lmb_non_survey = (adj_legal/0.528)-adj_legal,
         lmb_bkc = adj_legal * 0.0106,
         lmb_regional = adj_legal + lmb_non_survey + lmb_bkc,
         harv_rate_mature = ifelse(adj_mature < avg_mmb_fifty$mean, 0,
                                   ifelse(adj_mature >= avg_mmb_fifty$mean & 
                                            adj_mature < avg_mmb$mean, 
                                          0.05*(adj_mature/avg_mmb$mean),
                                          ifelse(adj_mature >= avg_mmb$mean, 0.09, 999))),
         harv_rate_legal = 0.25, 
         max_tac_legal = 0.25 * lmb_regional,
         max_tac_mature = harv_rate_mature * mmb_regional,
         max_tac = ifelse(max_tac_mature < max_tac_legal,
                          max_tac_mature, 
                          max_tac_legal))-> rkc_hcr2.1

rkc_hcr2.1 %>%
  select(scenario, year, mmb_regional,
         harv_rate_mature, max_tac) -> rkc_tac_summary_hcr2.1


bind_rows(rkc_tac_summary_hcr1.1, rkc_tac_summary_hcr1.2,
          rkc_tac_summary_hcr1.3, rkc_tac_summary_hcr2.1) -> hcr_summary


write.csv(hcr_summary, paste0('./output/', cur_yr,'/hcr_summary.csv'))


#Visualize MMB scenarios----
regional_biomass %>%
  ggplot(aes(year)) +
  geom_line(aes(y = adj_mature)) +
  geom_line(aes(y = adj_legal), 
            linetype = "dashed") +
  geom_point(size = 3,
             aes(y = adj_mature,
                 shape = status)) +
  geom_point(size = 3,
             aes(y = adj_legal,
                 shape = status)) +
  geom_hline(yintercept = avg_mmb$mean,
             color = "green",
             lwd = 1) +
  geom_hline(yintercept = avg_mmb$mean * 0.5,
             color = "orange",
             lwd = 1,
             linetype = "dashed") +
  ylab("CSA Adjusted Biomass (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1978, cur_yr, 2)) +
  scale_y_continuous(breaks =seq(0, 10000000, 500000),
                     limits = c(0, 7000000),
                     expand = c(0,0),
                     labels = scales::comma) +
  ggtitle("Mature and Legal Male Biomass of Southeast red king crab") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = c(0.8, 0.8)) -> fig2

ggsave(paste0('./figures/', cur_yr,'/rkc_strategies.png'), fig2,  
       dpi = 600, width = 12, height = 8)


#MMB, LMB and harvest figure

#combine regional biomass and harvest data frames

rkc_fishticket %>%
  filter(i_registration_area_code == "A",
         !is.na(pounds)) %>%
  group_by(year) %>%
  summarise(total_lbs = sum(pounds),
            permits = length(unique(cfec_no))) %>%
  filter(permits >= 3) -> harv_summary

left_join(regional_biomass_summary, 
          harv_summary, by = "year") -> bio_harv

bio_harv %>%
  ggplot(aes(year)) +
  geom_bar(aes(year, total_lbs), 
           stat = "identity") +
  geom_line(aes(y = mmb_regional),
            lwd = 0.75) +
  geom_line(aes(y = lmb_regional),
            lwd = 0.75,
            linetype = "dashed") +
  geom_point(aes(y = mmb_regional),
             size = 3) +
  geom_point(aes(y = lmb_regional),
             size = 3,
             shape = "triangle") +
  ylab("CSA Adjusted Regional Biomass (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1978, cur_yr, 2)) +
  scale_y_continuous(breaks =seq(0, 14000000, 500000),
                     limits = c(0, 14000000),
                     expand = c(0,0),
                     labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = c(0.8, 0.8),
        legend.title = element_blank()) -> fig3

ggsave(paste0('./figures/', cur_yr,'/rkc_biomass_harvest.png'), fig3,  
       dpi = 600, width = 12, height = 8)

#Old code-----

##Determine mean LMB 1979-2020
regional_biomass %>%
  filter(year %in% 1979:2020) %>%
  summarise(mean_lmb = mean(adj_legal)) -> avg_lmb

avg_lmb

avg_lmb * 0.5 -> avg_lmb_fifty

avg_lmb_fifty

regional_biomass %>%
  ggplot(aes(year)) +
  geom_line(aes(y = adj_mature)) +
  geom_line(aes(y = adj_legal), 
            linetype = "dashed") +
  geom_point(size = 3,
             aes(y = adj_mature,
                 shape = status)) +
  geom_point(size = 3,
             aes(y = adj_legal,
                 shape = status)) +
  geom_hline(yintercept = avg_lmb$mean,
             color = "green",
             lwd = 1,
             alpha = 0.5) +
  geom_hline(yintercept = avg_lmb$mean * 0.5,
             color = "orange",
             lwd = 1, 
             linetype = "dashed",
             alpha = 0.5) +
  ylab("CSA Adjusted Biomass (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1978, cur_yr, 2)) +
  scale_y_continuous(breaks =seq(0, 10000000, 500000),
                     limits = c(0, 7000000),
                     expand = c(0,0),
                     labels = scales::comma) +
  ggtitle("Mature and Legal Male Biomass of Southeast red king crab") +
  labs(fill = "Commercial Fishery Status") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = c(0.8, 0.8)) -> fig3

ggsave(paste0('./figures/', cur_yr,'/rkc_strategies_lmb.png'), fig3,  
       dpi = 600, width = 12, height = 8)


#Determine % diff of MMB vs LMB
regional_biomass %>%
  mutate(pct_change = (abs(legal - mature)/((legal + mature)/2))) %>%
  summarise(avg_change = mean(pct_change))

#Retrospective TAC using LMB instead of MMB
avg_lmb
avg_lmb_fifty


lmb_non_survey = (avg_lmb_fifty/0.528)-avg_lmb_fifty
lmb_bkc = avg_lmb_fifty * 0.0106
avg_lmb_regional = avg_lmb_fifty + lmb_non_survey + lmb_bkc
max_tac = 0.05 * avg_lmb_regional


#Taking survey biomass and applying harvest rate to survey and non-survey areas
regional_biomass %>%
  mutate(lmb_non_survey = (adj_legal/0.528)-adj_legal,
         lmb_bkc = adj_legal *0.0106,
         lmb_regional = adj_legal + lmb_non_survey + lmb_bkc,
         harv_rate_lmb = ifelse(adj_legal < avg_lmb_fifty$mean, 0,
                                ifelse(adj_legal >= avg_lmb_fifty$mean & 
                                         adj_legal < avg_lmb$mean, 
                                       0.1*(adj_legal/avg_lmb$mean),
                                       ifelse(adj_legal >= avg_lmb$mean, 0.1, 999))),
         max_tac_lmb = harv_rate_lmb * lmb_regional) -> rkc_tac_lmb

#join mature and legal TACs and plot data
left_join(rkc_tac_mature, rkc_tac_lmb, by = c("year", "legal", "mature",
                                              "adj_legal", "adj_mature",
                                              "lmb_non_survey",
                                              "lmb_bkc",
                                              "lmb_regional",
                                              "status")) -> rkc_tac_summary

rkc_tac_lmb %>%
  filter(year > 2003) %>%
  select("year", "adj_legal", "lmb_regional", 
         "harv_rate_lmb", "max_tac_lmb") ->rkc_tac_lmb_summary

write.csv(rkc_tac_lmb_summary,paste0('./output/', cur_yr,'/rkc_tac_lmb_summary.csv'))

write.csv(rkc_tac_summary, paste0('./output/', cur_yr,'/rkc_tac_summary.csv'))


#Simplified version for harvest rate is applied to regional biomass (survy and non_survey areas)-----
##Determine mean regional LMB 1979-2020
regional_biomass %>%
  mutate(lmb_non_survey = (adj_legal/0.528)-adj_legal,
         lmb_bkc = adj_legal *0.0106,
         lmb_regional = adj_legal + lmb_non_survey + lmb_bkc,
         mmb_non_survey = (adj_mature/0.528)-adj_mature,
         mmb_bkc = adj_mature *0.0106,
         mmb_regional = adj_mature + mmb_non_survey + mmb_bkc) -> regional_biomass_summary
 

regional_biomass_summary %>%
  filter(year %in% 1979:2021) %>%
  summarise(mean_reg_lmb = mean(lmb_regional)) -> avg_lmb_regional

avg_lmb_regional

avg_lmb_regional * 0.5 -> avg_lmb_regional_fifty

avg_lmb_regional_fifty

#minimum threshold value @ 5% harvest rate
avg_lmb_regional_fifty * .05

regional_biomass_summary %>%
  ggplot(aes(year)) +
  geom_line(aes(y = mmb_regional)) +
  geom_line(aes(y = lmb_regional), 
            linetype = "dashed") +
  geom_point(size = 3,
             aes(y = mmb_regional,
                 shape = status)) +
  geom_point(size = 3,
             aes(y = lmb_regional,
                 shape = status)) +
  geom_hline(yintercept = avg_lmb_regional$mean,
             color = "green",
             lwd = 1,
             alpha = 0.5) +
  geom_hline(yintercept = avg_lmb_regional$mean * 0.5,
             color = "orange",
             lwd = 1, 
             linetype = "dashed",
             alpha = 0.5) +
  ylab("CSA Adjusted Regional Biomass (lbs)") +
  xlab("Year") +
  scale_x_continuous(breaks = seq(1978, cur_yr, 2)) +
  scale_y_continuous(breaks =seq(0, 100000000, 500000),
                     limits = c(0, 14000000),
                     expand = c(0,0),
                     labels = scales::comma) +
  ggtitle("Mature and Legal Male Biomass of Southeast RKC for Surveyed and Nonsurveyed Areas") +
  labs(fill = "Commercial Fishery Status") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = c(0.8, 0.8)) -> fig5

ggsave(paste0('./figures/', cur_yr,'/rkc_strategies_lmb_regional.png'), fig5,  
       dpi = 600, width = 12, height = 8)


#Apply harvest rate strategy 5%-10% to determine maxTAC based on regional LMB
regional_biomass_summary %>%
  mutate(harv_rate_lmb = ifelse(lmb_regional < avg_lmb_regional_fifty$mean, 0,
                       ifelse(lmb_regional >= avg_lmb_regional_fifty$mean & 
                                lmb_regional < avg_lmb_regional$mean, 
                              0.1*(lmb_regional/avg_lmb_regional$mean),
                              ifelse(lmb_regional >= avg_lmb_regional$mean, 0.1, 999))),
max_tac_lmb = harv_rate_lmb * lmb_regional) %>%
  select(year, status, lmb_regional, harv_rate_lmb, max_tac_lmb)-> rkc_tac_lmb_regional

#Current year area contribution to regional biomass treemap----
##note need to add in non-survey areas and color-code based on stock health

stck_hlth <- c("Poor", "Below Average", "Moderate", "Above Average", "Unknown")

regional_biomass_summary %>% 
  select(year, lmb_non_survey, lmb_bkc) %>%
  filter(year == cur_yr) -> nonsurvey_areas

#manually add in the values for nonsurvey and BKC areas 
#manually add in stock health status from assessment results
area_biomass %>% 
  filter(year == cur_yr) %>%
  add_row(year = cur_yr, location = "Non Survey", adj_legal = 623635.6) %>%
  add_row(year = cur_yr, location = "BKC", adj_legal = 7394.838) %>%
  mutate(stck_hlth_status = ifelse(location == "Pybus", "Below Average",
                            ifelse(location == "Gambier", "Moderate",
                            ifelse(location == "Seymour", "Below Average",
                            ifelse(location == "Peril", "Below Average",
                            ifelse(location == "Juneau", "Healthy",
                            ifelse(location == "LynnSisters", "Moderate",
                            ifelse(location == "Excursion", "Poor",
                            ifelse(location == "Non Survey", "Unknown", "Unknown")))))))))-> stck_hlth_summary

#Reorder factor levels
stck_hlth_summary$stck_hlth_status <- factor(stck_hlth_summary$stck_hlth_status,
                                             levels = c('Unknown', 'Healthy', 
                                                        'Above Average',
                                                        'Moderate', 'Below Average',
                                                        'Poor'))

stck_hlth_summary %>%
  ggplot(aes(area = adj_legal, 
             fill = stck_hlth_status, 
             label = location)) +
  geom_treemap() +
  geom_treemap_text(color = "black",
                    place = "centre") +
 scale_fill_manual(values = c("darkorchid1", "dodgerblue1",
                               "green1", "orange1", "firebrick1")) +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 5)) +
  ggtitle("2022 Legal Male Biomass by Area")
