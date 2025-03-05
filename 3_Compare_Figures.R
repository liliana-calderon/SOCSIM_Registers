#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Registers - Compare figures from SOCSIM microsimulation and Swedish Registers
# U:/SOCSIM/SOCSIM_Registers/3_Compare_Figures.R

# Replication and comparison of all plots from the Swedish Kinship Universe using SOCSIM outputs 
# formatted in 2_Count_Simulated_Kin.R as reference tables and cohort size 
# to match the structure of kin counts used in the Swedish Kinship Universe

# This code is a modified and extended version of the 3_socsim_plots_improved.Rmd 
# Attempting to replicate Martin's plot with SOCSIM data
# created by Diego Alburez-Gutierrez (2021-05-24)

# Created on 30-11-2023
# Last modified on 05-03-2025
#------------------------------------------------------------------------------------------------------
## General settings and functions -----

# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(tidyverse)
library(cowplot)
library(data.table)
library(extrafont)
library(fs)
library(ggthemes)
library(ggtext)
library(magick)
library(RColorBrewer)
library(readxl)
library(viridis)

## Load reference table, N_Cohort and N_Cohort_sex
load("Output_/reference_table_SweBorn.RData")
load("Output_/N_Cohort.RData")
load("Output_/N_Cohort_sex.RData")

## Load theme for the graphs
source("Functions/Functions_Graphs.R")
#------------------------------------------------------------------------------------------------------
# Import the data for the figures included in "The Swedish Kinship Universe" (online appendix 2) ----

# Set the path of the file
file_SKU <- "kolk_esm2.xlsx"

## Set the names of the sheets to keep from each file
sheets_to_keep <- set_names(c("Fig1a", "Fig1b", "Fig2a", "Fig2b", "Fig3", "Fig4a", "Fig4b",
                              "Fig5a", "Fig5b", "Fig6a", "Fig6b", "Fig7", "Fig8b"))

SKU <- map_dfr(sheets_to_keep, ~ read_excel(path=file_SKU, sheet = .x), .id = "Fig")

SKU <- SKU %>% rename(Kon = gender)  # male=1 and female=2

#------------------------------------------------------------------------------------------------------
## Fig. 1b: Average number of living and dead grandchildren in 2017 by sex and birth cohort ----

# Create table with mean number of grandchildren 
grandchild_table <- reference_table_SweBorn %>% 
  filter(refGroup == "grandchild" & !is.na(refID) & !is.na(refIDbirthYear) & IDbirthYear >= 1915) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>% 
  setDT() %>% 
  left_join(., N_Cohort_sex, by = c("IDbirthYear", "Kon"))

grandchild_table <- distinct(grandchild_table[, .(mean_grandchildren = .N / N_17),keyby = .(IDbirthYear, Kon, isAlive)])

# Plot from SOCSIM output
Fig1b_SOCSIM <- ggplot() +
  geom_area(data = grandchild_table, mapping = aes(x = IDbirthYear, y = mean_grandchildren, fill = isAlive), color = "#4D4D4D") +
  geom_area(data=data.frame(x = c(2017-73,2017), y = c(4,4)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-73), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Microsimulation", "2" = "Women in Microsimulation")), ncol = 1, scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = c("#225EA8", "#7FCDBB"), 
                    limits = c("FALSE", "TRUE"), 
                    labels = c("Deceased grandchildren", "Living grandchildren")) +
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Average Number of Grandchildren in Microsimulation", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() + 
  guides(fill = guide_legend(reverse = TRUE))

# Plot from Swedish Kinship Universe
Fig1b_SKU <- ggplot() +
  geom_area(data = SKU %>% 
              filter(Fig == "Fig1b") %>% 
              mutate(isAlive = if_else(type == "registered deceased", FALSE, TRUE)), 
            mapping = aes(x = IDbirthYear, y = mean_grandchildren, fill = isAlive), color = "#4D4D4D") +
  geom_area(data=data.frame(x = c(2017-73,2017), y = c(4,4)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-73), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Registers", "2" = "Women in Registers")), ncol = 1, scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = c("#225EA8", "#7FCDBB"), 
                    limits = c("FALSE", "TRUE"), 
                    labels = c("Deceased grandchildren", "Living grandchildren")) +
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Average Number of Grandchildren in Registers", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) 

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig1b_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig1b") %>% 
                          mutate(isAlive = if_else(type == "registered deceased", FALSE, TRUE)) %>% 
                          select(IDbirthYear, Kon, isAlive, mean_grandchildren_SKU = mean_grandchildren), 
                        grandchild_table %>% 
                          rename(mean_grandchildren_SOCSIM = mean_grandchildren)) %>% 
  mutate(Difference = mean_grandchildren_SOCSIM - mean_grandchildren_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = isAlive), linewidth =1) +
  geom_vline(xintercept = c(2017-73), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men", "2" = "Women")), ncol = 1, scales = "free_x", strip.position = "top") +
  scale_color_manual(values = c("#225EA8", "#7FCDBB"), 
                     limits = c("FALSE", "TRUE"), 
                     labels = c("Deceased grandchildren", "Living grandchildren")) +
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Difference between the Average Number of Grandchildren", color = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  guides(color = guide_legend(reverse = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig1b_nl <- plot_grid(Fig1b_SOCSIM + theme(legend.position = "none"), 
                      Fig1b_SKU + theme(legend.position = "none"), align = "hv")
legend_1b <- get_plot_component(Fig1b_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig1b_nl, legend_1b, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig1b.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM vs Swedish Registers
Fig1b_Diff
ggsave(file="Graphs/Fig1b_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 1a: Proportional distribution of the number of living grandchildren in 2017 by sex and birth cohort ----

# Create table with distribution of grandchildren 
grandchildren_dist_Table <-  reference_table_SweBorn %>%
  filter(refGroup == "grandchild" & IDbirthYear >= 1915) %>%
  left_join(., N_Cohort_sex, by = c("IDbirthYear", "Kon")) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>% 
  setDT()

grandchildren_dist_Table_living <- rbind(grandchildren_dist_Table[is.na(refID),.(n_grandchildren = 0),keyby = .(ID, IDbirthYear, Kon, N_17)][,.(freq = .N),keyby = .(IDbirthYear, Kon, N_17, n_grandchildren)],
                                         grandchildren_dist_Table[isAlive == TRUE & !is.na(refID),.(n_grandchildren = .N),keyby = .(ID, IDbirthYear, Kon, N_17)][,.(freq = .N),keyby = .(IDbirthYear, Kon, N_17, n_grandchildren)]) %>%
  mutate(n_grandchildren = ifelse(n_grandchildren > 5 & n_grandchildren < 11, "6-10",
                                  ifelse(n_grandchildren > 10,">10",as.character(n_grandchildren))),
         n_grandchildren = factor(n_grandchildren, levels = c(">10","6-10","5","4","3","2","1","0")),
         n_grandchildren = fct_recode(n_grandchildren,
                                      "No grandchildren" = "0",
                                      "1 grandchild" = "1",
                                      "2 grandchildren" = "2",
                                      "3 grandchildren" = "3",
                                      "4 grandchildren" = "4",
                                      "5 grandchildren" = "5",
                                      "6-10 grandchildren" = "6-10",
                                      "11 or more grandchildren" = ">10")) %>%
  .[,.(freq =sum(freq)) ,keyby = .(IDbirthYear, Kon,  N_17, n_grandchildren)]%>%
  .[,.(n_grandchildren = n_grandchildren, freq = freq, N_17 = sum(freq), proportion = freq /sum(freq)),keyby = .(IDbirthYear, Kon)]

grandchildren_dist_Table <- rbind(grandchildren_dist_Table[is.na(refID),.(n_grandchildren = 0),keyby = .(ID, IDbirthYear, Kon, N_17)][,.(freq = .N),keyby = .(IDbirthYear, Kon, N_17, n_grandchildren)],
                                  grandchildren_dist_Table[!is.na(refID),.(n_grandchildren = .N),keyby = .(ID, IDbirthYear, Kon, N_17)][,.(freq = .N),keyby = .(IDbirthYear, Kon, N_17, n_grandchildren)]) %>%
  mutate(n_grandchildren = ifelse(n_grandchildren > 5 & n_grandchildren < 11, "6-10",
                                  ifelse(n_grandchildren > 10,">10",as.character(n_grandchildren))),
         n_grandchildren = factor(n_grandchildren, levels = c(">10","6-10","5","4","3","2","1","0")),
         n_grandchildren = fct_recode(n_grandchildren,
                                      "No grandchildren" = "0",
                                      "1 grandchild" = "1",
                                      "2 grandchildren" = "2",
                                      "3 grandchildren" = "3",
                                      "4 grandchildren" = "4",
                                      "5 grandchildren" = "5",
                                      "6-10 grandchildren" = "6-10",
                                      "11 or more grandchildren" = ">10")) %>%
  .[,.(freq =sum(freq)) ,keyby = .(IDbirthYear, Kon,  N_17, n_grandchildren)]%>%
  .[,.(n_grandchildren = n_grandchildren, freq = freq, N_17 = sum(freq), proportion = freq /sum(freq)),keyby = .(IDbirthYear, Kon)]

# Plot from SOCSIM output
Fig1a_SOCSIM <- ggplot(data = grandchildren_dist_Table_living) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_grandchildren), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-73,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-73), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Microsimulation", "2" = "Women in Microsimulation")), ncol = 1, scales = "free_x", strip.position = "top") +
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Proportion in Microsimulation", fill = "")+
  scale_fill_manual(values = c("#081D58", "#225EA8","#1D91C0", "#41B6C4",
                               "#7FCDBB","#C7E9B4","#EDF8B1", "#F6EAD2"),
                    limits = c("11 or more grandchildren",
                               "6-10 grandchildren",
                               "5 grandchildren",
                               "4 grandchildren",
                               "3 grandchildren",
                               "2 grandchildren",
                               "1 grandchild",
                               "No grandchildren"),
                    labels = c("11 or more grandchildren",
                               "6-10 grandchildren",
                               "5 grandchildren",
                               "4 grandchildren",
                               "3 grandchildren",
                               "2 grandchildren",
                               "1 grandchild",
                               "No grandchildren")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() + 
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig1a_SKU <- ggplot(data = SKU %>% 
                      filter(Fig == "Fig1a") %>% 
                      mutate(n_grandchildren = factor(n_grandchildren, 
                                                      levels = c("11 or more grandchildren",
                                                                 "6-10 grandchildren",
                                                                 "5 grandchildren",
                                                                 "4 grandchildren",
                                                                 "3 grandchildren",
                                                                 "2 grandchildren",
                                                                 "1 grandchild",
                                                                 "No grandchildren")))) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_grandchildren), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-73,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) +
  geom_vline(xintercept = c(2017-73), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Registers", "2" = "Women in Registers")), ncol = 1, scales = "free_x", strip.position = "top") +
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Proportion in Registers", fill = "")+
  scale_fill_manual(values = c("#081D58", "#225EA8","#1D91C0", "#41B6C4",
                               "#7FCDBB","#C7E9B4","#EDF8B1", "#F6EAD2"),
                    limits = c("11 or more grandchildren",
                               "6-10 grandchildren",
                               "5 grandchildren",
                               "4 grandchildren",
                               "3 grandchildren",
                               "2 grandchildren",
                               "1 grandchild",
                               "No grandchildren"),
                    labels = c("11 or more grandchildren",
                               "6-10 grandchildren",
                               "5 grandchildren",
                               "4 grandchildren",
                               "3 grandchildren",
                               "2 grandchildren",
                               "1 grandchild",
                               "No grandchildren")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        panel.background = element_rect(fill = NA),
        legend.key = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig1a_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig1a") %>% 
                          select(IDbirthYear, Kon, n_grandchildren, proportion_SKU = proportion), 
                        grandchildren_dist_Table_living %>% 
                          rename(proportion_SOCSIM = proportion)) %>% 
  mutate(Difference = proportion_SOCSIM - proportion_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = n_grandchildren), linewidth =1)+
  geom_vline(xintercept = c(2017-73), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men", "2" = "Women")), ncol = 1, scales = "free_x", strip.position = "top") +
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Difference between Proportions", color = "")+
  scale_color_manual(values = c("#081D58", "#225EA8","#1D91C0", "#41B6C4",
                                "#7FCDBB","#C7E9B4","#EDF8B1", "#F6EAD2"),
                     limits = c("11 or more grandchildren",
                                "6-10 grandchildren",
                                "5 grandchildren",
                                "4 grandchildren",
                                "3 grandchildren",
                                "2 grandchildren",
                                "1 grandchild",
                                "No grandchildren"),
                     labels = c("11 or more grandchildren",
                                "6-10 grandchildren",
                                "5 grandchildren",
                                "4 grandchildren",
                                "3 grandchildren",
                                "2 grandchildren",
                                "1 grandchild",
                                "No grandchildren")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank()) +
  guides(color = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig1a_nl <- plot_grid(Fig1a_SOCSIM + theme(legend.position = "none"), 
                      Fig1a_SKU + theme(legend.position = "none"), align = "hv")
legend_1a <- get_plot_component(Fig1a_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig1a_nl, legend_1a, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig1a.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig1a_Diff
ggsave(file="Graphs/Fig1a_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 2b: Average number of living and dead children in 2017 by sex and birth cohort ----

# Create table with mean number of children
child_table <- reference_table_SweBorn %>% 
  filter(refGroup == "child" & !is.na(refID) & IDbirthYear >= 1915) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>%
  group_by(ID) %>%
  mutate(n_partners = max(refTypeIII),
         n_partners = ifelse(n_partners >2, ">2", as.character(n_partners)),
         n_partners = factor(n_partners, levels = c("1","2",">2"))) %>%
  ungroup() %>%
  setDT() %>% 
  left_join(., N_Cohort_sex, by = c("IDbirthYear", "Kon"))

child_table <- rbind(child_table[, .(mean_children = .N / N_17, Type = "not living"),keyby = .(IDbirthYear, Kon)],
                     child_table[isAlive == TRUE, .(mean_children = .N / N_17),keyby = .(IDbirthYear, Type = n_partners, Kon)]) %>%
  distinct() %>% 
  mutate(Type = factor(Type, levels = c("not living", ">2", "2", "1")))

# Plot from SOCSIM output with two sexes facetted
Fig2b_SOCSIM <- ggplot() +
  geom_area(data = child_table %>% filter(Type == "not living"),
            mapping = aes(x = IDbirthYear, y = mean_children, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = child_table %>% filter(Type != "not living"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Microsimulation", "2" = "Women in Microsimulation")), ncol = 1,scales = "free_x") +
  scale_fill_manual(values = c("#7FCDBB","#41B6C4", "#225EA8","#FFE6CC"), 
                    limits = c("1", "2", ">2","not living"), 
                    labels = c("One childbearing partner","Two childbearing partners","Three or more childbearing partners","Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Children in Microsimulation", fill = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() + 
  theme(legend.key = element_blank(),
        strip.placement = "outside") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe with two sexes facetted
Fig2b_SKU <- ggplot() +
  geom_area(data = SKU %>% 
              mutate(type = factor(type, levels = c("registered deceased", ">2", "2", "1"))) %>% 
              filter(Fig == "Fig2b" & type == "registered deceased"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = type), color = "#4D4D4D", lwd = 0.5)+
  geom_area(data = SKU %>% 
              mutate(type = factor(type, levels = c("registered deceased", ">2", "2", "1"))) %>% 
              filter(Fig == "Fig2b" & type != "registered deceased"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Registers", "2" = "Women in Registers")), ncol = 1,scales = "free_x") +
  scale_fill_manual(values = c("#7FCDBB","#41B6C4", "#225EA8", "#FFE6CC"), 
                    limits = c("1", "2", ">2", "registered deceased"), 
                    labels = c("One childbearing partner","Two childbearing partners", "Three or more childbearing partners", "Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Children in Registers", fill = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,2.5, by = 0.5)) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank(),
        strip.placement = "outside")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) 

# The data for children (both in the .xlsx and SOCSIM) include both the alive and the deceased kin 
# as registered deceased or not living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU) with two sexes facetted
Fig2b_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig2b") %>% 
                          select(IDbirthYear, Kon, mean_children, type) %>% 
                          pivot_wider(names_from = type, values_from = mean_children) %>% 
                          mutate_at(c(3:6), ~ replace_na(.,0)) %>% 
                          mutate(`not living` = `registered deceased` - `1` - `2` - `>2`) %>%
                          select(-`registered deceased`) %>% 
                          pivot_longer(cols = c(3:6), 
                                       names_to = "Type", values_to = "mean_children_SKU"),
                        child_table %>% 
                          pivot_wider(names_from = Type, values_from = mean_children) %>% 
                          rename(all = `not living`) %>% 
                          mutate_at(c(3:6), ~ replace_na(.,0)) %>% 
                          mutate(`not living` = all - `1` - `2` - `>2`) %>%
                          select(-all) %>% 
                          pivot_longer(cols = c(3:6), 
                                       names_to = "Type", values_to = "mean_children_SOCSIM")) %>% 
  mutate(Difference = mean_children_SOCSIM - mean_children_SKU, 
         Type = factor(Type, levels = c("not living", ">2", "2", "1"))) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men", "2" = "Women")), ncol = 1,scales = "free_x") +
  scale_color_manual(values = c("#7FCDBB","#41B6C4", "#225EA8","#FFE6CC"), 
                     limits = c("1", "2", ">2","not living"), 
                     labels = c("One childbearing partner","Two childbearing partners","Three or more childbearing partners","Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Children", color = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank(),
        strip.placement = "outside")+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig2b_nl <- plot_grid(Fig2b_SOCSIM + theme(legend.position = "none"), 
                      Fig2b_SKU + theme(legend.position = "none"), align = "hv")
legend_2b <- get_plot_component(Fig2b_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig2b_nl, legend_2b, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig2b.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig2b_Diff
ggsave(file="Graphs/Fig2b_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")

#------------------------------------------------------------------------------------------------------
# Plot from SOCSIM output for women only 
Fig2b_SOCSIM_W <- ggplot() +
  geom_area(data = child_table %>% filter(Type == "not living" & Kon == "2"),
            mapping = aes(x = IDbirthYear, y = mean_children, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = child_table %>% filter(Type != "not living" & Kon == "2"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#7FCDBB","#41B6C4", "#225EA8","#FFE6CC"), 
                    limits = c("1", "2", ">2","not living"), 
                    labels = c("One childbearing partner","Two childbearing partners","Three or more childbearing partners","Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Number of Children per Woman in Microsimulation", fill = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() + 
  theme(legend.key = element_blank(),
        strip.placement = "outside") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe  for women only
Fig2b_SKU_W <- ggplot() +
  geom_area(data = SKU %>% 
              mutate(type = factor(type, levels = c("registered deceased", ">2", "2", "1"))) %>% 
              filter(Fig == "Fig2b" & type == "registered deceased" & Kon == "2"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = type), color = "#4D4D4D", lwd = 0.5)+
  geom_area(data = SKU %>% 
              mutate(type = factor(type, levels = c("registered deceased", ">2", "2", "1"))) %>% 
              filter(Fig == "Fig2b" & type != "registered deceased" & Kon == "2"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#7FCDBB","#41B6C4", "#225EA8", "#FFE6CC"), 
                    limits = c("1", "2", ">2", "registered deceased"), 
                    labels = c("One childbearing partner","Two childbearing partners", "Three or more childbearing partners", "Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Number of Children per Woman in Registers", fill = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,2.5, by = 0.5)) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank(),
        strip.placement = "outside")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) 

# The data for children (both in the .xlsx and SOCSIM) include both the alive and the deceased kin 
# as registered deceased or not living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)  for women only
Fig2b_Diff_W <- left_join(SKU %>% 
                          filter(Fig == "Fig2b") %>% 
                          select(IDbirthYear, Kon, mean_children, type) %>% 
                          pivot_wider(names_from = type, values_from = mean_children) %>% 
                          mutate_at(c(3:6), ~ replace_na(.,0)) %>% 
                          mutate(`not living` = `registered deceased` - `1` - `2` - `>2`) %>%
                          select(-`registered deceased`) %>% 
                          pivot_longer(cols = c(3:6), 
                                       names_to = "Type", values_to = "mean_children_SKU"),
                        child_table %>% 
                          pivot_wider(names_from = Type, values_from = mean_children) %>% 
                          rename(all = `not living`) %>% 
                          mutate_at(c(3:6), ~ replace_na(.,0)) %>% 
                          mutate(`not living` = all - `1` - `2` - `>2`) %>%
                          select(-all) %>% 
                          pivot_longer(cols = c(3:6), 
                                       names_to = "Type", values_to = "mean_children_SOCSIM")) %>% 
  mutate(Difference = mean_children_SOCSIM - mean_children_SKU, 
         Type = factor(Type, levels = c("not living", ">2", "2", "1"))) %>%
  filter(!is.na(Difference) & Kon == "2") %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  scale_color_manual(values = c("#7FCDBB","#41B6C4", "#225EA8","#FFE6CC"), 
                     limits = c("1", "2", ">2","not living"), 
                     labels = c("One childbearing partner","Two childbearing partners","Three or more childbearing partners","Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Number of Children per Woman", color = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(-0.4,0.4, by = 0.2), limits = c(-0.4, 0.4)) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank(),
        strip.placement = "outside")+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig2b_nl_W <- plot_grid(Fig2b_SOCSIM_W + theme(legend.position = "none"), 
                      Fig2b_SKU_W + theme(legend.position = "none"), align = "hv")
legend_2b_W <- get_plot_component(Fig2b_SOCSIM_W, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig2b_nl_W, legend_2b_W, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig2b_W.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig2b_Diff_W
ggsave(file="Graphs/Fig2b_Diff_W.pdf", width=17, height=9, dpi=300, device = "pdf")

#------------------------------------------------------------------------------------------------------
# Plot from SOCSIM output for men only

Fig2b_SOCSIM_M <- ggplot() +
  geom_area(data = child_table %>% filter(Type == "not living" & Kon == "1"),
            mapping = aes(x = IDbirthYear, y = mean_children, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = child_table %>% filter(Type != "not living" & Kon == "1"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#7FCDBB","#41B6C4", "#225EA8","#FFE6CC"), 
                    limits = c("1", "2", ">2","not living"), 
                    labels = c("One childbearing partner","Two childbearing partners","Three or more childbearing partners","Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Number of Children per Man in Microsimulation", fill = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() + 
  theme(legend.key = element_blank(),
        strip.placement = "outside") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe for men only
Fig2b_SKU_M <- ggplot() +
  geom_area(data = SKU %>% 
              mutate(type = factor(type, levels = c("registered deceased", ">2", "2", "1"))) %>% 
              filter(Fig == "Fig2b" & type == "registered deceased" & Kon == "1"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = type), color = "#4D4D4D", lwd = 0.5)+
  geom_area(data = SKU %>% 
              mutate(type = factor(type, levels = c("registered deceased", ">2", "2", "1"))) %>% 
              filter(Fig == "Fig2b" & type != "registered deceased" & Kon == "1"), 
            mapping = aes(x = IDbirthYear, y = mean_children, fill = type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#7FCDBB","#41B6C4", "#225EA8", "#FFE6CC"), 
                    limits = c("1", "2", ">2", "registered deceased"), 
                    labels = c("One childbearing partner","Two childbearing partners", "Three or more childbearing partners", "Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Number of Children per Man in Registers", fill = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,2.5, by = 0.5)) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank(),
        strip.placement = "outside")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) 

# The data for children (both in the .xlsx and SOCSIM) include both the alive and the deceased kin 
# as registered deceased or not living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)  for men only
Fig2b_Diff_M <- left_join(SKU %>% 
                            filter(Fig == "Fig2b") %>% 
                            select(IDbirthYear, Kon, mean_children, type) %>% 
                            pivot_wider(names_from = type, values_from = mean_children) %>% 
                            mutate_at(c(3:6), ~ replace_na(.,0)) %>% 
                            mutate(`not living` = `registered deceased` - `1` - `2` - `>2`) %>%
                            select(-`registered deceased`) %>% 
                            pivot_longer(cols = c(3:6), 
                                         names_to = "Type", values_to = "mean_children_SKU"),
                          child_table %>% 
                            pivot_wider(names_from = Type, values_from = mean_children) %>% 
                            rename(all = `not living`) %>% 
                            mutate_at(c(3:6), ~ replace_na(.,0)) %>% 
                            mutate(`not living` = all - `1` - `2` - `>2`) %>%
                            select(-all) %>% 
                            pivot_longer(cols = c(3:6), 
                                         names_to = "Type", values_to = "mean_children_SOCSIM")) %>% 
  mutate(Difference = mean_children_SOCSIM - mean_children_SKU, 
         Type = factor(Type, levels = c("not living", ">2", "2", "1"))) %>%
  filter(!is.na(Difference) & Kon == "1") %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  scale_color_manual(values = c("#7FCDBB","#41B6C4", "#225EA8","#FFE6CC"), 
                     limits = c("1", "2", ">2","not living"), 
                     labels = c("One childbearing partner","Two childbearing partners","Three or more childbearing partners","Deceased children")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Number of Children per Woman", color = " ") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(-0.75,0.5, by = 0.25), limits = c(-0.75, 0.5)) +
  theme_bw() + theme_graphs2() +
  theme(legend.key = element_blank(),
        strip.placement = "outside")+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers for men only
Fig2b_nl_M <- plot_grid(Fig2b_SOCSIM_M + theme(legend.position = "none"), 
                        Fig2b_SKU_M + theme(legend.position = "none"), align = "hv")
legend_2b_M <- get_plot_component(Fig2b_SOCSIM_M, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig2b_nl_M, legend_2b_M, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig2b_M.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers for men only
Fig2b_Diff_M
ggsave(file="Graphs/Fig2b_Diff_M.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 2a: Proportional distribution of the number of living children in 2017 by sex and birth cohort ----

# Create table with distribution of children
children_dist_Table <-  
  reference_table_SweBorn %>%
  filter(refGroup == "child" & IDbirthYear >= 1915) %>%
  left_join(., N_Cohort_sex, by = c("IDbirthYear", "Kon")) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>% 
  setDT()

children_dist_Table_living <- 
  rbind(children_dist_Table[is.na(refID),.(n_children = 0),keyby = .(ID, IDbirthYear, Kon, N_17)][,.(freq = .N),keyby = .(IDbirthYear, Kon, N_17, n_children)], 
        children_dist_Table[isAlive == TRUE & !is.na(refID),.(n_children = .N),keyby = .(ID, IDbirthYear, Kon, N_17)][,.(freq = .N),keyby = .(IDbirthYear, Kon, N_17, n_children)]
  ) %>%
  mutate(n_children = ifelse(n_children > 3, ">3", as.character(n_children)),
         n_children = factor(n_children, levels = c(">3","3","2","1","0")),
         n_children = fct_recode(n_children,
                                 "4 or more children" = ">3",
                                 "3 children" = "3",
                                 "2 children" = "2",
                                 "1 child" = "1",
                                 "No children" = "0")) %>%
  .[,.(freq =sum(freq)) ,keyby = .(IDbirthYear, Kon,  N_17, n_children)] %>%
  .[,.(n_children = n_children, freq = freq, N_17 = sum(freq), proportion = freq /sum(freq)),keyby = .(IDbirthYear, Kon)]

# Plot from SOCSIM output
Fig2a_SOCSIM <- ggplot(data = children_dist_Table_living) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_children), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Microsimulation", "2" = "Women in Microsimulation")), ncol = 1, scales = "free_x") +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Microsimulation", fill = "")+
  scale_fill_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                    limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                    labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig2a_SKU <- ggplot(data = SKU %>% 
                      filter(Fig == "Fig2a") %>%  
                      mutate(n_children = factor(n_children, 
                                                 levels = c("4 or more children", "3 children", 
                                                            "2 children", "1 child", "No children" )))) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_children), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men in Registers", "2" = "Women in Registers")), ncol = 1, scales = "free_x") +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Registers", fill = "")+
  scale_fill_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                    limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                    labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig2a_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig2a") %>% 
                          select(IDbirthYear, Kon, n_children, proportion_SKU = proportion), 
                        children_dist_Table_living %>% 
                          filter(IDbirthYear <= 2017) %>% 
                          select(IDbirthYear, Kon, n_children, proportion_SOCSIM = proportion)) %>% 
  mutate(Difference = proportion_SOCSIM - proportion_SKU, 
         n_children = factor(n_children, 
                             levels = c("4 or more children", "3 children", 
                                        "2 children", "1 child", "No children" ))) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = n_children), linewidth =1) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  facet_wrap(~ Kon, labeller = labeller(Kon = c("1" = "Men", "2" = "Women")), ncol = 1, scales = "free_x") +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between Proportions", color = "")+
  scale_color_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                     limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                     labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside")+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig2a_nl <- plot_grid(Fig2a_SOCSIM + theme(legend.position = "none"), 
                      Fig2a_SKU + theme(legend.position = "none"), align = "hv")
legend_2a <- get_plot_component(Fig2a_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig2a_nl, legend_2a, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig2a.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig2a_Diff
ggsave(file="Graphs/Fig2a_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
# Plot from SOCSIM output for women only

Fig2a_SOCSIM_W <- ggplot(data = children_dist_Table_living %>% filter(Kon == "2")) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_children), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Microsimulation", fill = "")+
  scale_fill_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                    limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                    labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe for women only
Fig2a_SKU_W <- ggplot(data = SKU %>% 
                      filter(Fig == "Fig2a" & Kon == "2") %>%  
                      mutate(n_children = factor(n_children, 
                                                 levels = c("4 or more children", "3 children", 
                                                            "2 children", "1 child", "No children" )))) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_children), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Registers", fill = "")+
  scale_fill_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                    limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                    labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU) for women only
Fig2a_Diff_W <- left_join(SKU %>% 
                          filter(Fig == "Fig2a" & Kon == "2") %>% 
                          select(IDbirthYear, Kon, n_children, proportion_SKU = proportion), 
                        children_dist_Table_living %>% 
                          filter(IDbirthYear <= 2017 & Kon == "2") %>% 
                          select(IDbirthYear, Kon, n_children, proportion_SOCSIM = proportion)) %>% 
  mutate(Difference = proportion_SOCSIM - proportion_SKU, 
         n_children = factor(n_children, 
                             levels = c("4 or more children", "3 children", 
                                        "2 children", "1 child", "No children" ))) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = n_children), linewidth =1) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between Proportions", color = "")+
  scale_color_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                     limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                     labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside")+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers for women only
Fig2a_nl_W <- plot_grid(Fig2a_SOCSIM_W + theme(legend.position = "none"), 
                      Fig2a_SKU_W + theme(legend.position = "none"), align = "hv")
legend_2a_W <- get_plot_component(Fig2a_SOCSIM_W, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig2a_nl_W, legend_2a_W, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig2a_W.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers for women only
Fig2a_Diff_W
ggsave(file="Graphs/Fig2a_Diff_W.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
# Plot from SOCSIM output for women only

Fig2a_SOCSIM_M <- ggplot(data = children_dist_Table_living %>% filter(Kon == "1")) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_children), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Microsimulation", fill = "")+
  scale_fill_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                    limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                    labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe for men only
Fig2a_SKU_M <- ggplot(data = SKU %>% 
                        filter(Fig == "Fig2a" & Kon == "1") %>%  
                        mutate(n_children = factor(n_children, 
                                                   levels = c("4 or more children", "3 children", 
                                                              "2 children", "1 child", "No children" )))) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_children), color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(2017-40,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Registers", fill = "")+
  scale_fill_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                    limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                    labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside")+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU) for men only
Fig2a_Diff_M <- left_join(SKU %>% 
                            filter(Fig == "Fig2a" & Kon == "1") %>% 
                            select(IDbirthYear, Kon, n_children, proportion_SKU = proportion), 
                          children_dist_Table_living %>% 
                            filter(IDbirthYear <= 2017 & Kon == "1") %>% 
                            select(IDbirthYear, Kon, n_children, proportion_SOCSIM = proportion)) %>% 
  mutate(Difference = proportion_SOCSIM - proportion_SKU, 
         n_children = factor(n_children, 
                             levels = c("4 or more children", "3 children", 
                                        "2 children", "1 child", "No children" ))) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = n_children), linewidth =1) +
  geom_vline(xintercept = c(2017-40), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between Proportions", color = "")+
  scale_color_manual(values = c("#253494", "#2C7FB8", "#41B6C4", "#A1DAB4", "#FFE6CC"),
                     limits = c("4 or more children", "3 children", "2 children", "1 child", "No children"), 
                     labels = c("4 or more children", "3 children", "2 children", "1 child", "No children")) +  
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        axis.line = element_line(),
        legend.key = element_blank(),
        strip.placement = "outside")+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers for men only
Fig2a_nl_M <- plot_grid(Fig2a_SOCSIM_M + theme(legend.position = "none"), 
                        Fig2a_SKU_M + theme(legend.position = "none"), align = "hv")
legend_2a_M <- get_plot_component(Fig2a_SOCSIM_M, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig2a_nl_M, legend_2a_M, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig2a_M.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers for men only
Fig2a_Diff_M
ggsave(file="Graphs/Fig2a_Diff_M.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 3: Average number of nieces and nephews by birth cohort and through full or half-sister/brother ----

# Create table with mean number of nieces and nephews
sibchild_table <- reference_table_SweBorn %>% 
  filter(refGroup == "sibchild" & !is.na(refID) & IDbirthYear >= 1932) %>%
  mutate(isAlive = is.na(refIDdeathYear),
         Type = paste(refTypeIIII, refTypeIII, "side", sep = " "),
         Type = factor(Type, levels = c("half brother side", "half sister side", "full brother side", "full sister side"))) %>%
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))

sibchild_table  <- rbind(sibchild_table[, .(mean_kin = .N / N_17, Type = "not living"),keyby = .(IDbirthYear)],
                         sibchild_table[isAlive == TRUE, .(mean_kin = .N / N_17),keyby = .(IDbirthYear, Type)]) %>%
  distinct()

# Plot from SOCSIM output
Fig3_SOCSIM <- ggplot() +
  geom_area(data = sibchild_table %>% filter(Type == "not living"),
            mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = sibchild_table %>% filter(Type != "not living"),
            mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(1930,1940), y = c(4.5,4.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_area(data=data.frame(x = c(2017-46,2017), y = c(4.5,4.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_vline(xintercept = c(1940, 2017-46), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Nieces/Nephews in Microsimulation", fill = "") +
  scale_fill_manual(values = c("#FFE6CC", "#A1DAB4", "#2C7FB8", "#41B6C4", "#253494"),
                    limits = c(levels=c("not living", 
                                        "half brother side", "half sister side", 
                                        "full brother side", "full sister side")),
                    labels = c("Deceased nieces \n and nephews", 
                               "Half brother's side", "Half sister's side", 
                               "Full brother's side", "Full sister's side")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig3_SKU <- 
  ggplot() +
  geom_area(data = SKU %>%
              mutate(type = factor(type,
                                   levels = c("registered deceased",
                                              "half brother side", "half sister side", 
                                              "full brother side", "full sister side"))) %>% 
              filter(Fig == "Fig3" & type == "registered deceased"), 
            mapping = aes(x = IDbirthYear, y = mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = SKU %>%
              mutate(type = factor(type,
                                   levels = c("registered deceased",
                                              "half brother side", "half sister side", 
                                              "full brother side", "full sister side"))) %>% 
              filter(Fig == "Fig3" & type != "registered deceased"), 
            mapping = aes(x = IDbirthYear, y = mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(1930,1940), y = c(4.5,4.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_area(data=data.frame(x = c(2017-46,2017), y = c(4.5,4.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_vline(xintercept = c(1940, 2017-46), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Nieces/Nephews in Registers", fill = "") +
  scale_fill_manual(values = c( "#FFE6CC","#A1DAB4",  "#2C7FB8","#41B6C4", "#253494"),
                    limits = c("registered deceased",
                               "half brother side", "half sister side", 
                               "full brother side", "full sister side"),
                    labels = c("Deceased nieces \n and nephews", 
                               "Half brother's side", "Half sister's side",
                               "Full brother's side", "Full sister's side")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))

# The data for niblings (both in the .xlsx and SOCSIM) include both the alive and the deceased kin 
# as registered deceased or not living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig3_Diff <- left_join(SKU %>% 
                         filter(Fig == "Fig3") %>% 
                         select(IDbirthYear, mean_kin, type) %>% 
                         pivot_wider(names_from = type, values_from = mean_kin) %>% 
                         mutate_at(c(2:6), ~ replace_na(.,0)) %>% 
                         # The not living from the table include both the living and the deceased
                         # So, we need to estimate the actual number of deceased
                         mutate(alive = `half brother side` + `full brother side` + `half sister side` + `full sister side`,
                                `not living` = `registered deceased` - alive) %>%
                         select(-c(`registered deceased`, alive)) %>% 
                         pivot_longer(cols = c(2:6), 
                                      names_to = "Type", values_to = "mean_kin_SKU"),
                       sibchild_table %>% 
                         pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                         rename(all = `not living`) %>% 
                         mutate_at(c(2:6), ~ replace_na(.,0)) %>% 
                         mutate(alive = `half brother side` + `full brother side` + `half sister side` + `full sister side`,
                                `not living` = all - alive) %>%
                         select(-c(all, alive)) %>% 
                         pivot_longer(cols = c(2:6), 
                                      names_to = "Type", values_to = "mean_kin_SOCSIM")) %>%  
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(1940, 2017-46), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Nieces/Nephews", 
       color = "") +
  scale_color_manual(values = c("#FFE6CC", "#A1DAB4", "#2C7FB8", "#41B6C4", "#253494"),
                     limits = c(levels=c("not living", 
                                        "half brother side", "half sister side", 
                                        "full brother side", "full sister side")),
                     labels = c("Deceased nieces \n and nephews", 
                               "Half brother's side", "Half sister's side", 
                               "Full brother's side", "Full sister's side")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", 
                                    "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = "#000000"), nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig3_nl <- plot_grid(Fig3_SOCSIM + theme(legend.position = "none"), 
                     Fig3_SKU + theme(legend.position = "none"), align = "hv")
legend_3 <- get_plot_component(Fig3_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig3_nl, legend_3, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig3.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig3_Diff
ggsave(file="Graphs/Fig3_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 4a: Average number of siblings by birth cohort and whether full or half-sibling by birth cohort ----

# Create table with mean number of siblings
sibling_table <- reference_table_SweBorn %>% 
  filter(refGroup == "sibling" & !is.na(refID) & IDbirthYear >= 1932) %>%
  mutate(isAlive = is.na(refIDdeathYear),
         refTypeIIII = factor(refTypeIIII, levels = c("half.mother", "half.father", "full", "not living"))) %>%
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))

sibling_table <- rbind(sibling_table[, .(mean_siblings = .N / N_17, refTypeIIII = "not living"),keyby = .(IDbirthYear)],
                       sibling_table[isAlive == TRUE, .(mean_siblings = .N / N_17),keyby = .(IDbirthYear, refTypeIIII)]) %>%
  distinct()

# Plot from SOCSIM output
Fig4a_SOCSIM <- ggplot() +
  geom_area(data = sibling_table[refTypeIIII == "not living"], mapping = aes(x = IDbirthYear, y = mean_siblings, fill = refTypeIIII), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = sibling_table[refTypeIIII != "not living"], mapping = aes(x = IDbirthYear, y = mean_siblings, fill = refTypeIIII), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(1930,1940), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_area(data=data.frame(x = c(2017-13,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_vline(xintercept = c(1940, 2017-13), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#225EA8", "#A1DAB4", "#41B6C4", "#FFE6CC"), 
                    limits = c("full", "half.mother", "half.father", "not living"), 
                    labels = c("Full siblings", "Half siblings on mother's side", "Half siblings on father's side", 
                               "Deceased siblings")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Siblings in Microsimulation", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9)) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig4a_SKU <- ggplot() +
  geom_area(data = SKU %>%
              filter(Fig == "Fig4a") %>%
              mutate(type = factor(type,
                                   levels = c("not living",
                                              "half.father", 
                                              "half.mother", 
                                              "full"))) %>% 
              filter(Fig == "Fig4a" & type == "not living"), 
            mapping = aes(x = IDbirthYear, y = mean_siblings, fill = type), 
            color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = SKU %>%
              filter(Fig == "Fig4a") %>%
              mutate(type = factor(type,
                                   levels = c("not living",
                                              "half.father", 
                                              "half.mother", 
                                              "full"))) %>% 
              filter(Fig == "Fig4a" & type != "not living"), 
            mapping = aes(x = IDbirthYear, y = mean_siblings, fill = type), 
            color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(1930,1940), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_area(data=data.frame(x = c(2017-13,2017), y = c(2.5,2.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_vline(xintercept = c(1940, 2017-13), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#225EA8", "#A1DAB4", "#41B6C4", "#FFE6CC"), 
                    limits = c("full", "half.mother", "half.father", "not living"), 
                    labels = c("Full siblings", "Half siblings on mother's side", "Half siblings on father's side", 
                                "Deceased siblings")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Siblings in Registers", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9)) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# The data for siblings (both in the .xlsx and SOCSIM) include both the alive and the deceased cousins as non.living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig4a_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig4a") %>% 
                          select(IDbirthYear, mean_siblings, type) %>% 
                          pivot_wider(names_from = type, values_from = mean_siblings) %>% 
                          rename(all = `not living`) %>% 
                          mutate_at(c(2:5), ~ replace_na(.,0)) %>% 
                          mutate(alive = full +  half.father + half.mother,
                                 `not living` = all - alive) %>%
                          select(-c(all, alive)) %>% 
                          pivot_longer(cols = c(2:5), 
                                       names_to = "Type", values_to = "mean_siblings_SKU"), 
                        sibling_table %>% 
                          pivot_wider(names_from = refTypeIIII, values_from = mean_siblings) %>% 
                          rename(all = `not living`) %>% 
                          mutate_at(c(2:5), ~ replace_na(.,0)) %>% 
                          mutate(alive = full +  half.father + half.mother,
                                 `not living` = all - alive) %>%
                          select(-c(all, alive)) %>% 
                          pivot_longer(cols = c(2:5), 
                                       names_to = "Type", values_to = "mean_siblings_SOCSIM")) %>%  
  mutate(Difference = mean_siblings_SOCSIM - mean_siblings_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  scale_color_manual(values = c("#225EA8", "#A1DAB4", "#41B6C4", "#FFE6CC"), 
                     limits = c("full", "half.mother", "half.father", "not living"), 
                     labels = c("Full siblings", "Half siblings on mother's side", "Half siblings on father's side", 
                                "Deceased siblings")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Siblings", color = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9)) +
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig4a_nl <- plot_grid(Fig4a_SOCSIM + theme(legend.position = "none"), 
                      Fig4a_SKU + theme(legend.position = "none"), align = "hv")
legend_4a <- get_plot_component(Fig4a_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig4a_nl, legend_4a, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig4a.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig4a_Diff
ggsave(file="Graphs/Fig4a_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 4b: Proportional distribution of the number of siblings (half- or full) by birth cohort ----

# Create table with distribution of siblings
sibling_dist_Table <-  reference_table_SweBorn %>%
  filter(refGroup == "sibling" & IDbirthYear >= 1932) %>%
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear")) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>% 
  setDT()

sibling_dist_Table_living <- rbind(sibling_dist_Table[is.na(refID),.(n_siblings = 0),keyby = .(ID, IDbirthYear, N_17)][,.(freq = .N),keyby = .(IDbirthYear, N_17, n_siblings)],
                                   sibling_dist_Table[isAlive == TRUE & !is.na(refID),.(n_siblings = .N),keyby = .(ID, IDbirthYear, N_17)][,.(freq = .N),keyby = .(IDbirthYear, N_17, n_siblings)]) %>%
  mutate(n_siblings = ifelse(n_siblings > 5, ">5", as.character(n_siblings)),
         n_siblings = factor(n_siblings, levels = c(">5","5","4","3","2","1","0")),
         n_siblings = fct_recode(n_siblings,
                                 "No siblings" = "0",
                                 "1 sibling" = "1",
                                 "2 siblings" = "2",
                                 "3 siblings" = "3",
                                 "4 siblings" = "4",
                                 "5 siblings" = "5",
                                 "6 or more siblings" = ">5")) %>%
  .[,.(freq =sum(freq)) ,keyby = .(IDbirthYear,  N_17, n_siblings)]%>%
  .[,.(n_siblings = n_siblings, freq = freq, N_17 = sum(freq), proportion = freq /sum(freq)),keyby = .(IDbirthYear)]

# Plot from SOCSIM output
Fig4b_SOCSIM <- ggplot(data = sibling_dist_Table_living) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_siblings), color = "#4D4D4D", lwd = 0.7) +    
  geom_area(data=data.frame(x = c(1930,1940), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_area(data=data.frame(x = c(2017-13,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_vline(xintercept = c(1940, 2017-13), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Microsimulation", fill = "")+
  scale_fill_manual(values = c("#081D58", "#225EA8","#1D91C0", 
                               "#41B6C4","#7FCDBB","#C7E9B4", "#F6EAD2"),
                    limits = c("6 or more siblings", "5 siblings", "4 siblings", 
                               "3 siblings", "2 siblings", "1 sibling", "No siblings"),
                    labels = c("6 or more siblings", "5 siblings", "4 siblings", 
                               "3 siblings", "2 siblings", "1 sibling", "No siblings")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9))+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig4b_SKU <- ggplot(data = SKU %>% 
                      filter(Fig == "Fig4b") %>%  
                      mutate(n_siblings = factor(n_siblings, 
                                                 levels = c("6 or more siblings", "5 siblings", "4 siblings", "3 siblings", "2 siblings", "1 sibling", "No siblings")))) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_siblings), 
            color = "#4D4D4D", lwd = 0.7) +  
  geom_area(data=data.frame(x = c(1930,1940), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_area(data=data.frame(x = c(2017-13,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.25) + 
  geom_vline(xintercept = c(1940, 2017-13), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Registers", fill = "")+
  scale_fill_manual(values = c("#081D58", "#225EA8","#1D91C0", 
                               "#41B6C4","#7FCDBB","#C7E9B4", "#F6EAD2"),
                    limits = c("6 or more siblings", "5 siblings", "4 siblings", 
                               "3 siblings", "2 siblings", "1 sibling", "No siblings"),
                    labels = c("6 or more siblings", "5 siblings", "4 siblings", 
                               "3 siblings", "2 siblings", "1 sibling", "No siblings")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9))+
  guides(fill = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig4b_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig4b") %>% 
                          select(IDbirthYear, n_siblings, proportion_SKU = proportion), 
                        sibling_dist_Table_living %>% 
                          select(IDbirthYear, n_siblings, proportion_SOCSIM = proportion)) %>% 
  mutate(Difference = proportion_SOCSIM - proportion_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = n_siblings), linewidth =1)+
  geom_vline(xintercept = c(1940, 2017-13), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between Proportions", color = "")+
  scale_color_manual(values = c("#081D58", "#225EA8","#1D91C0", 
                                "#41B6C4","#7FCDBB","#C7E9B4", "#F6EAD2"),
                     limits = c("6 or more siblings", "5 siblings", "4 siblings", 
                                "3 siblings", "2 siblings", "1 sibling", "No siblings"),
                     labels = c("6 or more siblings", "5 siblings", "4 siblings", 
                                "3 siblings", "2 siblings", "1 sibling", "No siblings")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9))+
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig4b_nl <- plot_grid(Fig4b_SOCSIM + theme(legend.position = "none"), 
                      Fig4b_SKU + theme(legend.position = "none"), align = "hv")
legend_4b <- get_plot_component(Fig4b_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig4b_nl, legend_4b, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig4b.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig4b_Diff
ggsave(file="Graphs/Fig4b_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 5b: Average number of cousins by birth cohort and by type of aunt or uncle ----

# Create table with mean number of cousins
cousin_table <- reference_table_SweBorn %>% 
  filter(refGroup == "cousin" & !is.na(refID) & IDbirthYear >= 1950) %>%
  mutate(isAlive = is.na(refIDdeathYear),
         Type    = paste(refTypeIII, refTypeIIII, sep = "_"),
         Type = factor(Type),
         Type = fct_recode(Type,
                           "Father's brother's side, one shared grandparent"  = "father.brother_1" ,
                           "Father's brother's side, two shared grandparents" = "father.brother_2" ,
                           "Father's sister's side, one shared grandparent"   = "father.sister_1"  ,
                           "Father's sister's side, two shared grandparents"  = "father.sister_2"  ,
                           "Mother's brother's side, one shared grandparent"  = "mother.brother_1" ,
                           "Mother's brother's side, two shared grandparents" = "mother.brother_2" ,
                           "Mother's sister's side, one shared grandparent"   = "mother.sister_1"  ,
                           "Mother's sister's side, two shared grandparents"  = "mother.sister_2"),
         Type2 = fct_recode(Type,
                            "Father's side, one shared grandparent"  = "Father's brother's side, one shared grandparent" ,
                            "Father's side, two shared grandparents" = "Father's brother's side, two shared grandparents" ,
                            "Father's side, one shared grandparent"  = "Father's sister's side, one shared grandparent",
                            "Father's side, two shared grandparents" = "Father's sister's side, two shared grandparents"  ,
                            "Mothers's side, one shared grandparent" = "Mother's brother's side, one shared grandparent" ,
                            "Mothers's side, two shared grandparents"= "Mother's brother's side, two shared grandparents" ,
                            "Mothers's side, one shared grandparent" = "Mother's sister's side, one shared grandparent",
                            "Mothers's side, two shared grandparents"= "Mother's sister's side, two shared grandparents"),
         Type3 = refTypeIIII,
         Type4 = refTypeIII) %>%
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))

cousin_tableAlt3 <- rbind(cousin_table[, .(mean_cousins = .N / N_17, Type4 = "not living"),keyby = .(IDbirthYear)],
                          cousin_table[isAlive == TRUE, .(mean_cousins = .N / N_17),keyby = .(IDbirthYear, Type4)]) %>%
  distinct()

# Plot from SOCSIM output
Fig5b_SOCSIM <- ggplot() +
  geom_area(data = cousin_tableAlt3[Type4 == "not living"], 
            mapping = aes(x = IDbirthYear, y = mean_cousins, fill = Type4), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = cousin_tableAlt3[Type4 != "not living"], 
            mapping = aes(x = IDbirthYear, y = mean_cousins, fill = Type4), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(1950,1977), y = c(8.5, 8.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_area(data=data.frame(x = c(2017-19,2017), y = c(7,7)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(1977, 2017-19), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#FFE6CC", "#253494", "#2C7FB8", "#41B6C4", "#A1DAB4"),                     limits = c("not living", "father.brother", "father.sister",
                                                                                                                      "mother.brother", "mother.sister"),
                    labels = c("Deceased cousins", "Paternal uncle's side","Paternal aunt's side", 
                               "Maternal uncle's side","Maternal aunt's side")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Cousins in Microsimulation", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig5b_SKU <- ggplot() +
  geom_area(data = SKU %>%
              filter(Fig == "Fig5b") %>%
              mutate(type = factor(type,
                                   levels = c("registered deceased",
                                              "father.brother", "father.sister",
                                              "mother.brother", "mother.sister"))) %>% 
              filter(Fig == "Fig5b" & type == "registered deceased"), 
            mapping = aes(x = IDbirthYear, y =  mean_cousins, fill = type), 
            color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = SKU %>%
              filter(Fig == "Fig5b") %>%
              mutate(type = factor(type,
                                   levels = c("registered deceased",
                                              "father.brother", "father.sister",
                                              "mother.brother", "mother.sister"))) %>% 
              filter(Fig == "Fig5b" & type != "registered deceased"), 
            mapping = aes(x = IDbirthYear, y =  mean_cousins, fill = type), 
            color = "#4D4D4D", lwd = 0.5)+
  geom_area(data=data.frame(x = c(1950,1977), y = c(8.5,8.5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_area(data=data.frame(x = c(2017-19,2017), y = c(7,7)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(1977, 2017-19), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#FFE6CC", "#253494", "#2C7FB8", "#41B6C4", "#A1DAB4"),                     limits = c("registered deceased", "father.brother", "father.sister",
                                                                                                                      "mother.brother", "mother.sister"),
                    labels = c("Deceased cousins", "Paternal uncle's side","Paternal aunt's side", 
                               "Maternal uncle's side","Maternal aunt's side")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Cousins in Registers", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


# The data for cousins (both in the .xlsx and SOCSIM) include both the alive and the deceased cousins 
# as `registered deceased` or non.living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig5b_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig5b") %>% 
                          select(IDbirthYear, mean_cousins, type) %>% 
                          pivot_wider(names_from = type, values_from = mean_cousins) %>%
                          mutate(alive = father.brother + father.sister + mother.brother + mother.sister,
                                 `not living` = `registered deceased`,
                                 `registered deceased` = `not living` - alive) %>% 
                          select(-c(`not living`, alive)) %>% 
                          pivot_longer(cols = c(2:6), 
                                       names_to = "Type", values_to = "mean_cousins_SKU"), 
                        cousin_tableAlt3 %>%
                          pivot_wider(names_from = Type4, values_from = mean_cousins) %>%
                          mutate(alive = father.brother + father.sister + mother.brother + mother.sister,
                                 `registered deceased` = `not living` - alive) %>% 
                          select(-c(`not living`, alive)) %>% 
                          pivot_longer(cols = c(2:6), 
                                       names_to = "Type", values_to = "mean_cousins_SOCSIM")) %>%
  mutate(Difference = mean_cousins_SOCSIM - mean_cousins_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(1977, 2017-19), color = "#000000", lty = 2) +
  scale_color_manual(values = c("#FFE6CC", "#253494", "#2C7FB8", "#41B6C4", "#A1DAB4"), 
                     limits = c("registered deceased", "father.brother", "father.sister",
                                "mother.brother", "mother.sister"),
                     labels = c("Deceased cousins", "Paternal uncle's side","Paternal aunt's side", 
                                "Maternal uncle's side","Maternal aunt's side")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Cousins", color = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig5b_nl <- plot_grid(Fig5b_SOCSIM + theme(legend.position = "none"), 
                      Fig5b_SKU + theme(legend.position = "none"), align = "hv")
legend_5b <- get_plot_component(Fig5b_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig5b_nl, legend_5b, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig5b.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig5b_Diff
ggsave(file="Graphs/Fig5b_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 5a: Proportional distribution of the number of cousins by birth cohort ----

# Create table with distribution of cousins
cousin_dist_Table <-  reference_table_SweBorn %>%
  filter(refGroup == "cousin" & IDbirthYear >= 1950) %>%
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear")) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>% 
  setDT()

cousin_dist_Table_living <- rbind(cousin_dist_Table[is.na(refID),.(n_cousins = 0),keyby = .(ID, IDbirthYear, N_17)][,.(freq = .N),keyby = .(IDbirthYear, N_17, n_cousins)],
                                  cousin_dist_Table[isAlive == TRUE & !is.na(refID),.(n_cousins = .N),keyby = .(ID, IDbirthYear, N_17)][,.(freq = .N),keyby = .(IDbirthYear, N_17, n_cousins)]) %>%
  mutate(n_cousins = ifelse(n_cousins > 5 & n_cousins < 11,"6-10",
                            ifelse(n_cousins > 10,">10",
                                   as.character(n_cousins))),
         n_cousins = factor(n_cousins, levels = c(">10","6-10","5","4","3","2","1","0")),
         n_cousins = fct_recode(n_cousins,
                                "No cousins" = "0",
                                "1 cousin" = "1",
                                "2 cousins" = "2",
                                "3 cousins" = "3",
                                "4 cousins" = "4",
                                "5 cousins" = "5",
                                "6 - 10 cousins" = "6-10",
                                "11 or more cousins" = ">10")) %>%
  .[,.(freq =sum(freq)) ,keyby = .(IDbirthYear,  N_17, n_cousins)]%>%
  .[,.(n_cousins = n_cousins, freq = freq, N_17 = sum(freq), proportion = freq /sum(freq)),keyby = .(IDbirthYear)]

# Plot from SOCSIM output
Fig5a_SOCSIM <- ggplot(data = cousin_dist_Table_living) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_cousins), color = "#4D4D4D", lwd = 0.7) +  
  geom_area(data=data.frame(x = c(1950,1977), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_area(data=data.frame(x = c(2017-19,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(1977, 2017-19), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Microsimulation", fill = "")+
  scale_fill_manual(values = c("#253494","#225EA8", "#1D91C0","#41B6C4",  
                               "#7FCDBB", "#C7E9B4","#EDF8B1", "#FFE6CC"), 
                    limits = c("11 or more cousins", "6 - 10 cousins", "5 cousins",
                               "4 cousins", "3 cousins", "2 cousins", "1 cousin", "No cousins"), 
                    labels = c("11 or more cousins", "6 - 10 cousins", "5 cousins",
                               "4 cousins", "3 cousins", "2 cousins", "1 cousin", "No cousins"))+ 
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9))+
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig5a_SKU <- ggplot(data = SKU %>% 
                      filter(Fig == "Fig5a") %>%
                      mutate(n_cousins = factor(n_cousins,
                                                levels = c("11 or more cousins", "6 - 10 cousins", "5 cousins", "4 cousins", "3 cousins", "2 cousins", "1 cousin", 
                                                           "No cousins")))) +
  geom_area(mapping = aes(x = IDbirthYear, y = proportion, fill = n_cousins), color = "#4D4D4D", lwd = 0.7) +  
  geom_area(data=data.frame(x = c(1950,1977), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_area(data=data.frame(x = c(2017-19,2017), y = c(1,1)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(1977, 2017-19), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Proportion in Registers", fill = "")+
  scale_fill_manual(values = c("#253494","#225EA8", "#1D91C0","#41B6C4",  
                               "#7FCDBB", "#C7E9B4","#EDF8B1", "#FFE6CC"), 
                    limits = c("11 or more cousins", "6 - 10 cousins", "5 cousins",
                               "4 cousins", "3 cousins", "2 cousins", "1 cousin", "No cousins"), 
                    labels = c("11 or more cousins", "6 - 10 cousins", "5 cousins",
                               "4 cousins", "3 cousins", "2 cousins", "1 cousin", "No cousins"))+ 
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(0,1, by = 0.2))+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9))+
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig5a_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig5a") %>% 
                          select(IDbirthYear, n_cousins, proportion_SKU = proportion), 
                        cousin_dist_Table_living %>% 
                          select(IDbirthYear, n_cousins, proportion_SOCSIM = proportion)) %>% 
  mutate(Difference = proportion_SOCSIM - proportion_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = n_cousins), linewidth =1)+
  geom_vline(xintercept = c(1977, 2017-19), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between Proportions", color = "")+
  scale_color_manual(values = c("#253494","#225EA8", "#1D91C0","#41B6C4",  
                                "#7FCDBB", "#C7E9B4","#EDF8B1", "#FFE6CC"), 
                     limits = c("11 or more cousins", "6 - 10 cousins", "5 cousins",
                                "4 cousins", "3 cousins", "2 cousins", "1 cousin", "No cousins"), 
                     labels = c("11 or more cousins", "6 - 10 cousins", "5 cousins",
                                "4 cousins", "3 cousins", "2 cousins", "1 cousin", "No cousins"))+ 
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  scale_y_continuous(breaks = seq(-1,0.4, by = 0.2))+
  theme_bw() + theme_graphs2() +
  guides(color = guide_legend(reverse = TRUE, nrow = 1, byrow = TRUE)) +
  theme(panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig5a_nl <- plot_grid(Fig5a_SOCSIM + theme(legend.position = "none"), 
                      Fig5a_SKU + theme(legend.position = "none"), align = "hv")
legend_5a <- get_plot_component(Fig5a_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig5a_nl, legend_5a, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig5a.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig5a_Diff
ggsave(file="Graphs/Fig5a_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 6a. Average number of living, dead, and unregistered parents, by birth cohort 1932–2017 ----

# Create table with mean number of parents
parent_table <- reference_table_SweBorn %>% 
  filter(refGroup == "parent" & !is.na(refID) & IDbirthYear >= 1933) %>%
  mutate(isAlive = is.na(refIDdeathYear), 
         refTypeII = ifelse(!isAlive, "not living", refTypeII), 
         Type = as.factor(refTypeII)) %>%
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))

parent_table  <- rbind(parent_table[isAlive == TRUE, .(mean_kin = .N / N_17),keyby = .(IDbirthYear, Type)], 
                       parent_table[, .(mean_kin = .N / N_17, Type = "not living"),keyby = .(IDbirthYear)]) %>%
  distinct()

# Plot from SOCSIM output
Fig6a_SOCSIM <- ggplot() +
  geom_area(data = parent_table[Type == "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = parent_table[Type != "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_vline(xintercept = c(1940), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Parents in Microsimulation", fill = "") +
  scale_fill_manual(values = c("#FFFFFF", "#FFE6CC", "#2C7FB8", "#A1DAB4"),
                    limits = c("", "not living", "father", "mother"),
                    labels = c("", "Registered deceased parents", "Fathers", "Mothers")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig6a_SKU <- SKU %>%
  filter(Fig == "Fig6a") %>%
  pivot_wider(names_from = Type, values_from = mean_kin) %>% 
  mutate(unregistered = 2-(father + mother + `registered deceased`)) %>%
  pivot_longer(cols = father:unregistered, 
               names_to = "Type", values_to = "mean_kin") %>% 
  mutate(Type = factor(Type,
                       levels = c("unregistered", "registered deceased", 
                                  "father", "mother"))) %>% 
  ggplot() +
  geom_area(mapping = aes(x = IDbirthYear, y =  mean_kin, fill = Type), 
            color = "#4D4D4D", lwd = 0.5) +
  geom_vline(xintercept = c(1940), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Parents in Registers", fill = "") +
  scale_fill_manual(values = c("#FED98E", "#FFE6CC", "#2C7FB8", "#A1DAB4"),
                    limits = c("unregistered","registered deceased", "father", "mother"),
                    labels = c("Unregistered parents", "Registered deceased parents", "Fathers", "Mothers")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig6a_Diff <- left_join(SKU %>% 
                          # We need estimate the number of unregistered parents which is not included in the table
                          filter(Fig == "Fig6a") %>% 
                          pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                          mutate(unregistered = 2-(father + mother + `registered deceased`)) %>%
                          pivot_longer(cols = father:unregistered, 
                                       names_to = "Type", values_to = "mean_kin_SKU") %>% 
                          select(IDbirthYear, mean_kin_SKU, Type), 
                        # We need to add a value of 0 for unregistered parents and 
                        # estimate the deceased, because for plotting they have all the value of 2
                        parent_table %>%
                          pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                          select(-`not living`) %>% 
                          mutate(`registered deceased` = 2-(father + mother), 
                                 unregistered = 0) %>% 
                          pivot_longer(cols = father:unregistered, 
                                       names_to = "Type", values_to = "mean_kin_SOCSIM") %>% 
                          select(IDbirthYear, mean_kin_SOCSIM, Type)) %>%
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(1940), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Parents", color = "") +
  scale_color_manual(values = c("#FED98E", "#FFE6CC", "#2C7FB8", "#A1DAB4"),
                     limits = c("unregistered","registered deceased", "father", "mother"),
                     labels = c("Unregistered parents", "Registered deceased parents", "Fathers", "Mothers")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig6a_nl <- plot_grid(Fig6a_SOCSIM + theme(legend.position = "none"), 
                      Fig6a_SKU + theme(legend.position = "none"), align = "hv")
legend_6a <- get_plot_component(Fig6a_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig6a_nl, legend_6a, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig6a.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig6a_Diff
ggsave(file="Graphs/Fig6a_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 6b. Average number of parent siblings by birth cohort 1950–2017 ----

# Create table with mean number of aunts and uncles
parsib_table <- reference_table_SweBorn %>% 
  filter(refGroup == "parsib" & !is.na(refID) & IDbirthYear >= 1950) %>%
  mutate(isAlive = is.na(refIDdeathYear),
         Type =  paste(refTypeIII, "'s ", refTypeIIII, " sibling's", " side", sep = ""),
         Type = factor(Type, levels = c("father's half sibling's side", "father's full sibling's side","mother's half sibling's side", "mother's full sibling's side")),
         refTypeII = case_when(refTypeII == "aunt" ~ "sister",
                               refTypeII == "uncle" ~ "brother",
                               TRUE ~ refTypeII),
         Type2  = paste(refTypeIII, "'s ", refTypeIIII," ", refTypeII, "'s", " side", sep = ""),
         Type2 = factor(Type2, levels = c("father's half brother's side",
                                          "father's full brother's side",
                                          "father's half sister's side",
                                          "father's full sister's side",
                                          "mother's half brother's side",
                                          "mother's full brother's side",
                                          "mother's half sister's side",
                                          "mother's full sister's side")),
         Type3 = refTypeIIII,
         Type4 = paste(refTypeIII, refTypeII, sep = " ")) %>%
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))

parsib_tableAlt3  <- rbind(parsib_table[, .(mean_kin = .N / N_17, Type4 = "not living"),keyby = .(IDbirthYear)],
                           parsib_table[isAlive == TRUE, .(mean_kin = .N / N_17),keyby = .(IDbirthYear, Type4)]) %>%
  distinct()

# Plot from SOCSIM output
Fig6b_SOCSIM <- ggplot() +
  geom_area(data = parsib_tableAlt3[Type4 == "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type4), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = parsib_tableAlt3[Type4 != "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type4), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data=data.frame(x = c(1950,1977), y = c(5,5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(1977), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Aunts and Uncles in Microsimulation", fill = "") +
  scale_fill_manual(values = c("#FFE6CC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"), 
                    limits = c("not living", "father brother", "father sister", "mother brother", "mother sister"),
                    labels = c("Deceased parental siblings", "Paternal uncle", "Paternal aunt", "Maternal uncle", "Maternal aunt")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig6b_SKU <- ggplot() +
  geom_area(data = SKU %>% 
              filter(Fig == "Fig6b") %>% 
              mutate(type = factor(type,
                                   levels = c("registered deceased", "father brother", "father sister", "mother brother", "mother sister"))) %>% 
              filter(Fig == "Fig6b" & type == "registered deceased"), 
            mapping = aes(x = IDbirthYear, y =  mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.7) + 
  geom_area(data = SKU %>% 
              filter(Fig == "Fig6b") %>% 
              mutate(type = factor(type,
                                   levels = c("registered deceased", "father brother", "father sister", "mother brother", "mother sister"))) %>% 
              filter(Fig == "Fig6b" & type != "registered deceased"), 
            mapping = aes(x = IDbirthYear, y =  mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.7) +
  geom_area(data=data.frame(x = c(1950,1977), y = c(5,5)), aes(x=x,y=y), fill = "#FFFFFF", alpha = 0.2) + 
  geom_vline(xintercept = c(1977), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Aunts and Uncles in Registers", fill = "") +
  scale_fill_manual(values = c("#FFE6CC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"), 
                    limits = c("registered deceased", "father brother", "father sister", "mother brother", "mother sister"),
                    labels = c("Deceased parental siblings", "Paternal uncle", "Paternal aunt", "Maternal uncle", "Maternal aunt")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 1, byrow = TRUE))


# The data for parents siblings (both in the .xlsx and SOCSIM) include both the alive and the deceased kin
# as `registered deceased` or not living.
# So, we need to estimate the actual registered deceased by subtracting the number of people alive

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig6b_Diff <- left_join(SKU %>% 
                          filter(Fig == "Fig6b") %>% 
                          select(IDbirthYear, mean_kin, type) %>% 
                          pivot_wider(names_from = type, values_from = mean_kin) %>%
                          mutate(alive = `father brother` + `father sister` + `mother brother` + `mother sister`,
                                 `not living` = `registered deceased`,
                                 `registered deceased` = `not living` - alive) %>% 
                          select(-c(`not living`, alive)) %>% 
                          pivot_longer(cols = c(2:6), 
                                       names_to = "Type", values_to = "mean_kin_SKU"), 
                        parsib_tableAlt3 %>% 
                          pivot_wider(names_from = Type4, values_from = mean_kin) %>%
                          mutate(alive = `father brother` + `father sister` + `mother brother` + `mother sister`,
                                 `registered deceased` = `not living` - alive) %>% 
                          select(-c(`not living`, alive)) %>% 
                          pivot_longer(cols = c(2:6), 
                                       names_to = "Type", values_to = "mean_kin_SOCSIM")) %>%
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(1977), color = "#000000", lty = 2) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Aunts and Uncles", color = "") +
  scale_color_manual(values = c("#FFE6CC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"), 
                     limits = c("registered deceased", "father brother", "father sister", "mother brother", "mother sister"),
                     labels = c("Deceased parental siblings", "Paternal uncle", "Paternal aunt", "Maternal uncle", "Maternal aunt")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = "#000000"), nrow = 1, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig6b_nl <- plot_grid(Fig6b_SOCSIM + theme(legend.position = "none"), 
                      Fig6b_SKU + theme(legend.position = "none"), align = "hv")
legend_6b <- get_plot_component(Fig6b_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig6b_nl, legend_6b, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig6b.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig6b_Diff
ggsave(file="Graphs/Fig6b_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")

#------------------------------------------------------------------------------------------------------
## Fig. 7: Average number of living, dead, and unregistered grandparents, by birth cohort ----

# Create table with mean number of grandparents
grandparent_table <- reference_table_SweBorn %>% 
  filter(refGroup == "grandparent" & !is.na(refID) & IDbirthYear >= 1950) %>%
  mutate(isAlive = is.na(refIDdeathYear), 
         Type  =  ifelse(!isAlive, "not living", paste(refTypeII, " (", refTypeIII, "'s side)", sep = ""))) %>% 
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))

grandparent_table  <- 
  rbind(grandparent_table[, .(mean_kin = .N / N_17, Type = "not living"),keyby = .(IDbirthYear)],
        grandparent_table[isAlive == TRUE, .(mean_kin = .N / N_17),keyby = .(IDbirthYear, Type)]) %>%
  distinct() %>% 
  mutate(Type = factor(Type, levels = c("not living", 
                                        "grandfather (father's side)",
                                        "grandmother (father's side)",
                                        "grandfather (mother's side)",
                                        "grandmother (mother's side)")))

# Plot from SOCSIM output
Fig7_SOCSIM <- ggplot() +
  geom_area(data = grandparent_table[Type == "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = grandparent_table[Type != "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_vline(xintercept = c(1977), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#FFFFFF", "#FFE6CC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"),
                    limits = c("", "not living", 
                               "grandfather (father's side)", 
                               "grandmother (father's side)", 
                               "grandfather (mother's side)",
                               "grandmother (mother's side)"), 
                    labels = c("", "Registered deceased grandparents", "Paternal grandfather", "Paternal grandmother", "Maternal grandfather", "Maternal grandmother")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Grandparents in Microsimulation", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 2, byrow = TRUE))

# Plot from Swedish Kinship Universe
SKU_7 <- SKU %>%
  filter(Fig == "Fig7") %>% 
  pivot_wider(names_from = type, values_from = mean_kin) %>% 
  # Add dummy variable to complete the plot with the unregistered
  # The registered deceased from the table include both the living and the deceased
  mutate(unregistered = 4) %>% 
  pivot_longer(cols = c(14:19), 
               names_to = "type", values_to = "mean_kin") %>%
  mutate(type = factor(type,
                       levels = c("unregistered", 
                                  "registered deceased", 
                                  "grandfather (father's side)",
                                  "grandmother (father's side)",
                                  "grandfather (mother's side)",
                                  "grandmother (mother's side)")))

Fig7_SKU  <- ggplot() +
  geom_area(data = SKU_7 %>% 
              filter(type == "unregistered"),
            mapping = aes(x = IDbirthYear, y =  mean_kin, fill = type),
            color = "#4D4D4D", lwd = 0.7) +
  geom_area(data = SKU_7 %>% 
              filter(type == "registered deceased"), 
            mapping = aes(x = IDbirthYear, y =  mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.7) + 
  geom_area(data = SKU_7 %>% 
              filter(type != "registered deceased" & type != "unregistered"), 
            mapping = aes(x = IDbirthYear, y =  mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.7) +
  geom_vline(xintercept = c(1977), color = "#000000", lty = 2) +
  scale_fill_manual(values = c("#FED98E", "#FFE6CC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"),
                    limits = c("unregistered", 
                               "registered deceased", 
                               "grandfather (father's side)", 
                               "grandmother (father's side)", 
                               "grandfather (mother's side)",
                               "grandmother (mother's side)"), 
                    labels = c("Unregistered grandparents", 
                               "Registered deceased grandparents", "Paternal grandfather", "Paternal grandmother", "Maternal grandfather", "Maternal grandmother")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Grandparents in Registers", fill = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 2, byrow = TRUE))  

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig7_Diff <- left_join(SKU %>% 
                         filter(Fig == "Fig7") %>% 
                         select(IDbirthYear, mean_kin, type) %>% 
                         pivot_wider(names_from = type, values_from = mean_kin) %>%
                         mutate_at(c(2:6), ~ replace_na(.,0)) %>% 
                         # The registered deceased from the table include both the living and the deceased
                         # So, we need to estimate the actual number of deceased
                         mutate(alive = `grandfather (father's side)` + `grandmother (father's side)` + 
                                  `grandfather (mother's side)` + `grandmother (mother's side)`,
                                `not living` = `registered deceased`,
                                `registered deceased` = `not living` - alive,
                                unregistered = 4-(alive + `registered deceased`)) %>%
                         select(-c(alive,`not living`)) %>%
                         pivot_longer(cols = c(2:7), 
                                      names_to = "Type", values_to = "mean_kin_SKU"), 
                       grandparent_table %>% 
                         pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                         mutate_at(c(2:6), ~ replace_na(.,0)) %>% 
                         select(-`not living`) %>%
                         # We need to add a value of 0 for unregistered grandparents and 
                         # estimated the deceased, because for plotting not living have all the value of 4
                         mutate(alive = `grandfather (father's side)` + `grandmother (father's side)` + 
                                  `grandfather (mother's side)` + `grandmother (mother's side)`,
                                `registered deceased` = 4 - alive, 
                                unregistered = 0) %>%
                         select(-alive) %>% 
                         pivot_longer(cols = c(2:7), 
                                      names_to = "Type", values_to = "mean_kin_SOCSIM")) %>%
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU) %>%
  filter(!is.na(Difference)) %>% 
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  geom_vline(xintercept = c(1977), color = "#000000", lty = 2) +
  scale_color_manual(values = c("#FED98E", "#FFE6CC", "#A1DAB4", "#41B6C4", "#2C7FB8", "#253494"),
                     limits = c("unregistered", 
                                "registered deceased", 
                                "grandfather (father's side)", 
                                "grandmother (father's side)", 
                                "grandfather (mother's side)",
                                "grandmother (mother's side)"), 
                     labels = c("Unregistered grandparents", 
                                "Registered deceased grandparents", "Paternal grandfather", "Paternal grandmother", "Maternal grandfather", "Maternal grandmother")) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Grandparents", color = "") +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 2, byrow = TRUE))  

# Save figure with comparison SOCSIM vs Swedish Registers
Fig7_nl <- plot_grid(Fig7_SOCSIM + theme(legend.position = "none"), 
                     Fig7_SKU + theme(legend.position = "none"), align = "hv")
legend_7 <- get_plot_component(Fig7_SKU, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig7_nl, legend_7, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig7.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig7_Diff
ggsave(file="Graphs/Fig7_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")

#------------------------------------------------------------------------------------------------------
## Fig. 8a: Distribution of the total number of kin by birth cohort 1915–2017 ----

# Create table with distribution of all living king
violinTable_kin <- reference_table_SweBorn %>%
  filter(!is.na(refID) & IDbirthYear %in% c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010)) %>%
  mutate(isAlive = is.na(refIDdeathYear)) %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = "IDbirthYear") %>%
  setDT()

violinTable_kin_living  <- violinTable_kin[isAlive == TRUE,.(n = .N),keyby = .(IDbirthYear,ID, N_17)][,.(freq = .N),.(IDbirthYear,N_17,n)] %>%
  rbind(.,.[,.(n=0, freq = N_17 - sum(freq)),keyby = .(IDbirthYear, N_17)]) %>% 
  arrange(IDbirthYear, n)

violinTable_kin  <- violinTable_kin[,.(n = .N),keyby = .(IDbirthYear,ID, N_17)][,.(freq = .N),.(IDbirthYear,N_17,n)] %>%
  rbind(.,.[,.(n=0, freq = N_17 - sum(freq)),keyby = .(IDbirthYear, N_17)]) %>% 
  arrange(IDbirthYear, n)

boxplotTable_kin_living <- violinTable_kin_living[,.(n = n, q=cumsum(freq/N_17)),keyby = .(IDbirthYear, N_17)] %>%
  mutate(lim = ifelse(q > 0.025, "qmin"   , FALSE),
         lim = ifelse(q > 0.25 , "qlower" , lim),
         lim = ifelse(q > 0.5  , "qmiddle", lim),
         lim = ifelse(q > 0.75 , "qupper" , lim),
         lim = ifelse(q > 0.975, "qmax"   , lim)) %>%
  distinct(IDbirthYear, lim, .keep_all = TRUE) %>%
  select(IDbirthYear,lim, n)  %>%
  dcast(.,IDbirthYear ~ lim, value.var = "n")

boxplotTable_kin <- violinTable_kin[,.(n = n, q=cumsum(freq/N_17)),keyby = .(IDbirthYear, N_17)] %>%
  mutate(lim = ifelse(q > 0.025, "qmin"   , FALSE),
         lim = ifelse(q > 0.25 , "qlower" , lim),
         lim = ifelse(q > 0.5  , "qmiddle", lim),
         lim = ifelse(q > 0.75 , "qupper" , lim),
         lim = ifelse(q > 0.975, "qmax"   , lim)) %>%
  distinct(IDbirthYear, lim, .keep_all = TRUE) %>%
  select(IDbirthYear,lim, n)  %>%
  dcast(.,IDbirthYear ~ lim, value.var = "n")

annotate_living <-left_join(select(boxplotTable_kin_living, IDbirthYear, qmiddle), violinTable_kin_living, by = c("IDbirthYear"="IDbirthYear", "qmiddle"="n" ))

# Plot from SOCSIM output
Fig8a_SOCSIM <- ggplot() +
  geom_violin(data  = violinTable_kin_living,  mapping = aes(x = IDbirthYear, y = n, weight = freq, fill = as.factor(IDbirthYear)),
              scale = "area", bw = 1) +
  geom_boxplot(data = boxplotTable_kin_living, mapping = aes(x = IDbirthYear,ymin = qmin, lower = qlower, middle = qmiddle, upper = qupper, ymax = qmax, fill = as.factor(IDbirthYear)),stat = "identity", 
               width = 1.5, show.legend = FALSE, color = "maroon")+
  scale_fill_manual(values = c("#FFFFFF", "#FFE6CC", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"))+ 
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Total Number of Living Kin in Microsimulation") +
  scale_y_continuous(breaks = seq(0,80, by = 10), limits = c(0,70)) + 
  scale_x_continuous(breaks   = seq(1920, 2010, by=10), 
                     labels = paste(seq(1920, 2010, by=10), "\n", "(",2018 -seq(1920, 2010, by=10),")"),
                     sec.axis = dup_axis(name = "Percentage With Median Number of Kin by Cohort", labels = round( 100* annotate_living$freq / annotate_living$N_17, 2))) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.major.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") 

## Difference in the Percentage with median number of kin by cohort
Fig8a_Diff <- data.frame(Cohort = seq(1920, 2010, 10), 
                         mean_kin_SOCSIM_per = c(round(100*annotate_living$freq/annotate_living$N_17, 2)), 
                         mean_kin_SKU_per = c(8.27, 8.65, 6.95, 7.05, 5.36, 4.49, 4.5, 5.04, 6, 7.11)) %>% 
  mutate(Difference = mean_kin_SOCSIM_per - mean_kin_SKU_per) %>% 
  ggplot()+
  geom_line(aes(x = Cohort, y = Difference), linewidth =1)+
  labs(x = "Birth Cohort \n (Age in 2017)", y = "Difference in the Percentage With the Median Number of Kin")+
  theme_bw() + theme_graphs2() +
  theme(panel.grid.major.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") 

# Import violin plots from the Swedish Kinship Universe as png
Fig8a_SKU <- ggdraw() + draw_image('Fig8a_SKU.png')

# Save figure with comparison SOCSIM vs Swedish Registers
plot_grid(Fig8a_SOCSIM, Fig8a_SKU,
          rel_heights = c(1.4,0.4), 
          rel_widths = c(0.5,0.5), 
          scale(c(0.7, 0.5)))
ggsave(file="Graphs/Fig8a.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig8a_Diff
ggsave(file="Graphs/Fig8a_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. 8b: Average number of all types of kin by birth cohort 1915–2017 ----

# Create table with mean number of all deceased and living kin
totalkin_table <- reference_table_SweBorn %>% 
  filter(!is.na(refID) & IDbirthYear >= 1915) %>%
  mutate(isAlive = is.na(refIDdeathYear), 
         Type = as.factor(refGroup)) %>%
  setDT() %>% 
  left_join(., select(N_Cohort, IDbirthYear, N_17), by = c("IDbirthYear"))


totalkin_table  <- rbind(totalkin_table[, .(mean_kin = .N / N_17, Type = "not living"),keyby = .(IDbirthYear)],
                         totalkin_table[isAlive == TRUE, .(mean_kin = .N / N_17),keyby = .(IDbirthYear, Type)]) %>%
  distinct()%>%
  mutate(Type = factor(Type, levels = c("not living", "grandchild","child","sibchild","sibling","cousin","parent","parsib","grandparent")))

# Plot from SOCSIM output
Fig8b_SOCSIM <- ggplot() +
  geom_area(data = totalkin_table[Type == "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  geom_area(data = totalkin_table[Type != "not living"], mapping = aes(x = IDbirthYear, y = mean_kin, fill = Type), color = "#4D4D4D", lwd = 0.5) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Kin in Microsimulation", fill = "") +
  scale_fill_manual(values =c("#FFE6CC","#081D58","#253494","#225EA8","#1D91C0","#41B6C4","#7FCDBB","#C7E9B4","#EDF8B1"),
                    limits = c("not living", "grandchild", "child", "sibchild", "sibling", "cousin", "parent", "parsib", "grandparent"),
                    labels = c("Deceasead kin", "Grandchildren", "Children", "Nieces and nephews", "Siblings", "Cousins", "Parents", "Aunts and uncles", "Grandparents")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line"),
        axis.text.x = element_text(family="serif", size = 14, colour = "#000000")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 2, byrow = TRUE))

# Plot from Swedish Kinship Universe
Fig8b_SKU <- SKU %>%
  filter(Fig == "Fig8b") %>% 
  mutate(type = factor(type,
                       levels = c("registered deceased", "grandchild", "child", "sibchild", "sibling", "cousin", "parent", "parsib", "grandparent"))) %>%
  ggplot() +
  geom_area(mapping = aes(x = IDbirthYear, y =  mean_kin, fill = type), 
            color = "#4D4D4D", lwd = 0.5) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Average Number of Kin in Registers", fill = "") +
  scale_fill_manual(values =c("#FFE6CC", "#081D58","#253494","#225EA8","#1D91C0","#41B6C4","#7FCDBB","#C7E9B4","#EDF8B1"),
                    limits = c("registered deceased", "grandchild", "child", "sibchild", "sibling", "cousin", "parent", "parsib", "grandparent"),
                    labels = c("Deceasead kin", "Grandchildren", "Children", "Nieces and nephews", "Siblings", "Cousins", "Parents", "Aunts and uncles", "Grandparents")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line"), 
        axis.text.x = element_text(family="serif", size = 14, colour = "#000000")) +  
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 2, byrow = TRUE)) +
  expand_limits(y = c(0, 43)) 

# Plot difference SOCSIM microsimulation - Swedish registers (SKU)
Fig8b_Diff <- full_join(SKU %>% 
                          filter(Fig == "Fig8b") %>% 
                          select(IDbirthYear, mean_kin_SKU = mean_kin, Type = type),
                        totalkin_table %>% 
                          pivot_wider(names_from = Type, values_from = mean_kin) %>%
                          mutate_at(c(2:10), ~ replace_na(.,0)) %>% 
                          # We need to add a value of 0 for unregistered grandparents and 
                          # estimated the deceased, because for plotting not living have all the value of 4
                          mutate(alive = grandchild + child + sibchild + sibling + cousin + parent + parsib + grandparent, 
                                 `registered deceased` = `not living` - alive) %>% 
                          select(-c(`not living`, alive)) %>%
                          pivot_longer(cols = c(2:10), 
                                       names_to = "Type", values_to = "mean_kin_SOCSIM")) %>% 
  mutate_at(c(2,4), ~ replace_na(.,0)) %>% 
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU) %>%
  filter(!is.na(Difference)) %>%
  ggplot()+
  geom_line(aes(x = IDbirthYear, y = Difference, color = Type), linewidth =1) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Difference between the Average Number of Kin", color = "") +
  scale_color_manual(values =c("#FFE6CC","#081D58","#253494","#225EA8","#1D91C0","#41B6C4","#7FCDBB","#C7E9B4","#EDF8B1"),
                     limits = c("registered deceased", "grandchild", "child", "sibchild", "sibling", "cousin", "parent", "parsib", "grandparent"),
                     labels = c("Deceasead kin", "Grandchildren", "Children", "Nieces and nephews", "Siblings", "Cousins", "Parents", "Aunts and uncles", "Grandparents")) +
  scale_x_continuous(breaks   = c(seq(1920, 2010,by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010,by = 10), 2017), "\n", "(",2017 - c(seq(1920, 2010,by = 10), 2017),")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.minor = element_line(colour = "#999999", linewidth = 0.3, linetype = 9),
        legend.key.height = unit(1, "line")) +
  guides(fill = guide_legend(override.aes = list(color = 0), nrow = 2, byrow = TRUE))

# Save figure with comparison SOCSIM vs Swedish Registers
Fig8b_nl <- plot_grid(Fig8b_SOCSIM + theme(legend.position = "none"), 
                      Fig8b_SKU + theme(legend.position = "none"), align = "hv")
legend_8b <- get_plot_component(Fig8b_SOCSIM, 'guide-box-bottom', return_all = TRUE)
plot_grid(Fig8b_nl, legend_8b, ncol = 1, rel_heights = c(1, .1))
ggsave(file="Graphs/Fig8b.pdf", width=17, height=9, dpi=300, device = "pdf")

# Save figure with difference SOCSIM - Swedish Registers
Fig8b_Diff
ggsave(file="Graphs/Fig8b_Diff.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. I. Difference in the average number of all types of kin by birth cohort 1915–2017 ----

# Prepare the data and add variable to distinguish alive and deceased kin
Diff_All <- full_join(SKU %>% 
                        filter(Fig == "Fig8b") %>% 
                        select(IDbirthYear, mean_kin_SKU = mean_kin, Type = type),
                      totalkin_table %>% 
                        pivot_wider(names_from = Type, values_from = mean_kin) %>%
                        mutate_at(c(2:10), ~ replace_na(.,0)) %>% 
                        mutate(alive = grandchild + child + sibchild + sibling + cousin + parent + parsib + grandparent, 
                               `registered deceased` = `not living` - alive) %>% 
                        select(-c(`not living`, alive)) %>%
                        pivot_longer(cols = c(2:10), 
                                     names_to = "Type", values_to = "mean_kin_SOCSIM")) %>% 
  mutate_at(c(2,4), ~ replace_na(.,0)) %>% 
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU,
         Type = case_when(Type == "registered deceased" ~ "Deceased kin", 
                          Type == "grandparent" ~ "Grandparents" ,
                          Type == "parent" ~ "Parents", 
                          Type == "parsib" ~ "Aunts and uncles", 
                          Type == "sibling" ~ "Siblings", 
                          Type == "cousin" ~ "Cousins", 
                          Type == "child" ~ "Children", 
                          Type == "sibchild" ~ "Nieces and nephews", 
                          Type == "grandchild" ~ "Grandchildren", 
                          TRUE ~ Type),
         Type = factor(Type, levels = c("Deceased kin", "Grandchildren", "Nieces and nephews", 
                                        "Children", "Cousins", "Siblings", 
                                        "Aunts and uncles", "Parents", "Grandparents")),
         Type2 = ifelse(Type == "Deceased kin", "Deceased", "Alive")) %>% 
  filter(!is.na(Difference))

# Plot difference in Kin Alive SOCSIM microsimulation - Swedish registers (SKU) as a heat map
Fig_Dif_Alive <- 
  Diff_All %>%
  filter(Type2 == "Alive") %>%
  ggplot() +
  geom_tile(aes(x = IDbirthYear, y = Type, fill = Difference)) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "", fill = "Difference") +
  scale_fill_viridis_c(option = "D", direction = -1, oob = scales::squish) +
  scale_x_continuous(breaks = c(seq(1920, 2010, by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010, by = 10), 2017), "\n", "(", 2017 - c(seq(1920, 2010, by = 10), 2017), ")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.key.height = unit(1, "line"))

# Plot difference in Kin Deceased SOCSIM microsimulation - Swedish registers (SKU) as a heat map
Fig_Dif_Deceased <- 
  Diff_All %>%
  filter(Type2 == "Deceased") %>%
  ggplot() +
  geom_tile(aes(x = IDbirthYear, y = Type, fill = Difference)) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "", fill = "Difference") +
  scale_fill_viridis_c(option = "F", direction = -1, oob = scales::squish) +
  scale_x_continuous(breaks = c(seq(1920, 2010, by = 10), 2017), 
                     labels = paste(c(seq(1920, 2010, by = 10), 2017), "\n", "(", 2017 - c(seq(1920, 2010, by = 10), 2017), ")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.key.height = unit(1, "line"))

# Save figure with difference SOCSIM - Swedish Registers only for living kin 
Fig_Dif_Alive + theme(legend.position = "right")
ggsave(file="Graphs/Fig_Diff_Alive.pdf", width=17, height=9, dpi=300, device = "pdf")

# Combine the plots for living and deceased kin
Fig_Diff_All <- plot_grid(Fig_Dif_Alive + theme(legend.position = "right"), 
                          Fig_Dif_Deceased + theme(legend.position = "right"),
                          ncol= 1, align = "v", rel_heights = c(8, 3.7))

# Save figure with difference SOCSIM - Swedish Registers with living and deceased kin
Fig_Diff_All
ggsave(file="Graphs/Fig_Diff_All.pdf", width=17, height=9, dpi=300, device = "pdf")
#------------------------------------------------------------------------------------------------------
## Fig. II: Average number of living, dead, and unregistered grandparents and parents by birth cohort ----

# Plot difference SOCSIM microsimulation - Swedish registers (SKU) as a heat map

Fig_Diff_Ancestor <-   bind_rows(left_join(SKU %>% 
                                             # We need estimate the number of unregistered parents which is not included in the table
                                             filter(Fig == "Fig6a") %>% 
                                             pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                                             mutate(unregistered = 2-(father + mother + `registered deceased`)) %>%
                                             pivot_longer(cols = father:unregistered, 
                                                          names_to = "Type", values_to = "mean_kin_SKU") %>% 
                                             select(IDbirthYear, Type, mean_kin_SKU), 
                                           # We need to add a value of 0 for unregistered parents and 
                                           # estimate the deceased, because for plotting they have all the value of 2
                                           parent_table %>%
                                             pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                                             select(-`not living`) %>% 
                                             mutate(`registered deceased` = 2-(father + mother), 
                                                    unregistered = 0) %>% 
                                             pivot_longer(cols = father:unregistered, 
                                                          names_to = "Type", values_to = "mean_kin_SOCSIM") %>% 
                                             select(IDbirthYear, Type, mean_kin_SOCSIM) %>% 
                                             mutate(Type2 = "Parents")),
                                 left_join(SKU %>% 
                                             filter(Fig == "Fig7") %>% 
                                             select(IDbirthYear, mean_kin, type) %>% 
                                             pivot_wider(names_from = type, values_from = mean_kin) %>%
                                             mutate_at(c(2:6), ~ replace_na(.,0)) %>% 
                                             # The registered deceased from the table include both the living and the deceased
                                             # So, we need to estimate the actual number of deceased
                                             mutate(alive = `grandfather (father's side)` + `grandmother (father's side)` + 
                                                      `grandfather (mother's side)` + `grandmother (mother's side)`,
                                                    `not living` = `registered deceased`,
                                                    `registered deceased` = `not living` - alive,
                                                    unregistered = 4-(alive + `registered deceased`)) %>%
                                             select(-c(alive,`not living`)) %>%
                                             pivot_longer(cols = c(2:7), 
                                                          names_to = "Type", values_to = "mean_kin_SKU"), 
                                           grandparent_table %>% 
                                             pivot_wider(names_from = Type, values_from = mean_kin) %>% 
                                           mutate_at(c(2:6), ~ replace_na(.,0)) %>% 
                                           select(-`not living`) %>%
                                           # We need to add a value of 0 for unregistered grandparents and 
                                           # estimated the deceased, because for plotting not living have all the value of 4
                                           mutate(alive = `grandfather (father's side)` + `grandmother (father's side)` + 
                                                    `grandfather (mother's side)` + `grandmother (mother's side)`,
                                                  `registered deceased` = 4 - alive, 
                                                  unregistered = 0) %>%
                                           select(-alive) %>% 
                                           pivot_longer(cols = c(2:7), 
                                                        names_to = "Type", values_to = "mean_kin_SOCSIM")) %>% 
                                   mutate(Type2 = "Grandparents")) %>% 
  mutate(Difference = mean_kin_SOCSIM - mean_kin_SKU,
         Type = case_when(Type == "unregistered" ~ "Unregistered" ,
                          Type == "registered deceased" ~ "Deceased", 
                          Type == "mother" ~ "Mother" ,
                          Type == "father" ~ "Father", 
                          Type == "grandfather (father's side)" ~ "Paternal grandfather", 
                          Type == "grandmother (father's side)" ~ "Paternal grandmother",
                          Type == "grandfather (mother's side)" ~ "Maternal grandfather",
                          Type == "grandmother (mother's side)" ~ "Maternal grandmother",
                          TRUE ~ Type), 
         Type = factor(Type, levels = c("Deceased", "Paternal grandfather", "Maternal grandfather", "Father",
                                        "Paternal grandmother", "Maternal grandmother", "Mother" , "Unregistered")), 
         Type2 = factor(Type2, levels = c("Parents", "Grandparents"))) %>% 
  filter(!is.na(Difference)) %>% 
  ggplot()+
  facet_wrap(. ~ Type2, scales = "free") +
  geom_tile(aes(x = IDbirthYear, y = Type, fill = Difference)) +
  labs(x = "Birth Cohort \n(Age in 2017)", y = "Type of Kin", fill = "Difference") + 
  scale_fill_gradient2(low = "#E9E0B7",
                       mid = "#DEF5E5",
                       high = "#3C3162",
                       midpoint = 0,
                       limits = c(-4,4)) +
  scale_x_continuous(breaks = c(seq(1930, 2010, by = 10), 2017), 
                       labels = paste(c(seq(1930, 2010, by = 10), 2017), "\n", "(", 2017 - c(seq(1930, 2010, by = 10), 2017), ")")) +
  theme_bw() + theme_graphs2() +
  theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.key.height = unit(1, "line")) 

Fig_Diff_Ancestor
ggsave(file="Graphs/Fig_Diff_Ancestors.pdf", width=17, height=9, dpi=300, device = "pdf")