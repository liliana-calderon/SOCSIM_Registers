#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Registers - Compare Input and Output Rates
# U:/SOCSIM/SOCSIM_Registers/1_Compare_Input_Output.R

## Compare input and output age-specific rates from a SOCSIM microsimulation for Sweden (1751-2022)
# as well as summary measures such as TFR, e0

## To run the following code, it is necessary to have already run the simulations and read the .opop file
# c.f. script 0_Set_Up_Simulation.R

# Created on 26-02-2024
# Last modified on 29-07-2024

## Based on code prepared for the biases in genealogies paper
# U:/SOCSIM/SOCSIM_Genealogies/2_Compare_Input_Output.R
#----------------------------------------------------------------------------------------------------
## General settings and functions -----
# Prevent scientific notation (useful for the rate calculation)
options(scipen=999999)

## Load packages 
library(tidyverse)
library(ggh4x)  # To facet scales-
library(HMDHFDplus)
library(patchwork) # To combine ggplots
library(readxl)
library(rsocsim) # Functions to estimate rates
library(svglite) # To save svg files
library(viridis)

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

# Load function to calculate life table from asmr 1x1
# Currently, it only works with asmr calculated with rsocsim::estimate_mortality_rates()
source("Functions/Functions_Life_Table.R")

## Type inside the quotes HFD and HMD credentials (username and password) for the new website
# This information is necessary to run the comparisons below

# HFD credentials
HFD_username <- "Type_here_HFD_username"
HFD_password <- "Type_here_HFD_password"

# HMD credentials
HMD_username <- "Type_here_HMD_username"
HMD_password <- "Type_here_HMD_password"


## Load opop and omar generated in 0_Set_Up_Simulation.R 
# from simulation with no heterogeneous fertility but parity-specific rates
load("opop.RData")
load("omar.RData")

#------------------------------------------------------------------------------------------------------
# Age-Specific Fertility and Mortality rates, 5x5 ----

# Create a sub-folder called "Measures" to save the output measures if it does not exist.
ifelse(!dir.exists("Measures"), dir.create("Measures"), FALSE)

# Retrieve age-specific fertility rates with hetfert 0
asfr <- estimate_fertility_rates(opop = opop,
                                 final_sim_year = 2017, #[Jan-Dec]
                                 year_min = 1750, # Closed [
                                 year_max = 2020, # Open )
                                 year_group = 5, 
                                 age_min_fert = 10, # Closed [
                                 age_max_fert = 55, # Open ) 
                                 age_group = 5) # [,)
save(asfr, file = "Measures/asfr.RData")

# Retrieve age-specific mortality rates with hetfert 0
asmr <- estimate_mortality_rates(opop = opop,
                                 final_sim_year = 2017, #[Jan-Dec]
                                 year_min = 1750, # Closed
                                 year_max = 2020, # Open )
                                 year_group = 5,
                                 age_max_mort = 110, # Open )
                                 age_group = 5) # [,) 
save(asmr, file = "Measures/asmr.RData")


## Plots ASFR and ASMR

# Load ASFR and ASMR from the simulations 
load("Measures/asfr.RData")
load("Measures/asmr.RData")

# Create a sub-folder called "Graphs" to save the plots if it does not exist.
ifelse(!dir.exists("Graphs"), dir.create("Graphs"), FALSE)

## ASFR and ASMR from SOCSIM for women
# Age groups for fertility and mortality must be the same to plot the figure below

# Choose years to plot (in intervals).
yrs_plot <- c("[1800,1805)", "[1900,1905)", "[2000,2005)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(asmr$age)

bind_rows(asfr %>%
            mutate(rate = "ASFR",                   
                   sex = "female"),
          asmr %>% 
            mutate(rate = "ASMR") %>% 
            filter(sex == "female")) %>% 
  mutate(age = factor(as.character(age), levels = age_levels)) %>% 
  # Some ages can have rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(socsim !=0 & !is.infinite(socsim) & !is.nan(socsim)) %>% 
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = socsim, group = year, colour = year)) +
  geom_line(linewidth = 1) +
  facet_wrap(. ~ rate, scales = "free") + 
  facetted_pos_scales(y = list(ASFR = scale_y_continuous(),
                               ASMR =  scale_y_continuous(trans = "log10"))) +
  theme_graphs() +
  scale_color_manual(values = c("#79B727", "#2779B7", "#B72779"))+ 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Age", y = "Estimate")
ggsave(file="Graphs/SOCSIM_ASFR_ASMR.jpeg", width=17, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------
## Comparison with HFC/HFD and HMD data (used as input) ----

# ASFR ----

# Get HFD ASFR for Sweden (by single year of age and 5 calendar year), 1751-1890
HFC <- read_csv(paste0("https://www.fertilitydata.org/File/GetFile/Country/SWE/SWE_ASFRstand_TOT.txt"),
                col_names = T, show_col_types = F) %>% 
  filter(RefCode %in% "SWE_02" & Year2 <= 1890)

# Get HFD ASFR for Sweden (by single year of age and calendar year), 1891-2022
HFD <- readHFDweb(CNTRY = "SWE",
                  item = "asfrRR",
                  username = HFD_username,
                  password = HFD_password)

# Repeat rates of year groups for each calendar year
HFC <- HFC  %>% 
  select(Year1, Age, ASFR) %>% 
  mutate(ASFR = if_else(ASFR == ".", "0", ASFR), 
         ASFR = as.double(ASFR), 
         Year2 = Year1 + 1, 
         Year3 = Year1 + 2, 
         Year4 = Year1 + 3,
         Year5 = Year1 + 4) %>% 
  select(Year1, Year2, Year3, Year4, Year5, Age, ASFR) %>%
  pivot_longer(cols = c(Year1:Year5), names_to = "Delete", values_to = "Year") %>% 
  select(Year, Age, ASFR) %>% 
  arrange(Year, Age)

# Extract year and age breaks used in estimate_fertility_rates() to apply the same values to HFD data

# Year breaks. Extract all the unique numbers from the intervals. 
year_breaks_fert <- unique(as.numeric(str_extract_all(asfr$year, "\\d+", simplify = T)))

# Year range to filter HFD data
year_range_fert <- min(year_breaks_fert):max(year_breaks_fert-1)

# Age breaks of fertility rates. Extract all the unique numbers from the intervals 
age_breaks_fert <- unique(as.numeric(str_extract_all(asfr$age, "\\d+", simplify = T)))


# Wrangle HFC and HFD data
HFCD0 <- bind_rows(HFC, HFD) %>% 
  filter(Year %in% year_range_fert) %>% 
  select(-OpenInterval) %>% 
  mutate(year = cut(Year, breaks = year_breaks_fert, 
                    include.lowest = F, right = F, ordered_results = T),
         age = cut(Age, breaks = age_breaks_fert, 
                   include.lowest = F, right = F, ordered_results = T)) %>% 
  filter(!is.na(age)) %>% 
  group_by(year, age) %>%
  summarise(ASFR = mean(ASFR)) %>%
  ungroup() %>%
  mutate(Source = "HFC/HFD", 
         Rate = "ASFR")

# Wrangle SOCSIM data
SocsimF0 <- asfr %>% 
  rename(ASFR = socsim) %>% 
  mutate(Source = "SOCSIM",
         Rate = "ASFR")

## Plot ASFR from HFD vs SOCSIM   

# Same years to plot than above (in intervals). 
yrs_plot <- c("[1800,1805)", "[1900,1905)", "[2000,2005)") 

bind_rows(HFCD0, SocsimF0) %>%
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = ASFR, group = interaction(year, Source)))+
  geom_line(aes(colour = year, linetype = Source, alpha = Source), linewidth = 1.2)+
  scale_color_manual(values = c("#79B727", "#2779B7", "#B72779"))+ 
  scale_linetype_manual(values = c("HFC/HFD" = "dotted", "SOCSIM" = "solid")) +
  scale_alpha_discrete(guide="none", range = c(1, 0.4))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/HFD_SOCSIM_ASFR.jpeg", width=17, height=9, dpi=300)


# ASMR ----
## We compare the mid-year ASMR with central death rates from HMD life tables 1x1
# Rates will be aggregated as defined in the estimate_mortality_rates()

# Get female life tables from HMD, 1x1
ltf <- readHMDweb(CNTRY = "SWE",
                  item = "fltper_1x1",
                  username = HMD_username,
                  password = HMD_password)

# Get male life tables from HMD, 1x1
ltm <- readHMDweb(CNTRY = "SWE",
                  item = "mltper_1x1",
                  username = HMD_username,
                  password = HMD_password)

# Extract year and age breaks used in the estimate_mortality_rates() to apply the same values to HMD data

# Year breaks. Extract all the unique numbers from the intervals 
year_breaks_mort <- unique(as.numeric(str_extract_all(asmr$year, "\\d+", simplify = T)))

# Year range to filter HMD data
year_range_mort <- min(year_breaks_mort):max(year_breaks_mort-1)

# Age breaks of mortality rates. Extract all the unique numbers from the intervals 
age_breaks_mort <- unique(as.numeric(str_extract_all(asmr$age, "\\d+", simplify = T)))

# Wrangle HMD life tables
HMD <- ltf %>%
  select(Year, Age, mx) %>% 
  mutate(Sex = "Female") %>% 
  bind_rows(ltm %>% 
              select(Year, Age, mx) %>%  
              mutate(Sex = "Male")) %>% 
  filter(Year %in% year_range_mort) %>% 
  mutate(year = cut(Year, breaks = year_breaks_mort, 
                    include.lowest = F, right = F, ordered_results = T),
         age = cut(Age, breaks = age_breaks_mort, 
                   include.lowest = F, right = F, ordered_results = T)) %>% 
  filter(!is.na(age)) %>% 
  group_by(year, Sex, age) %>% 
  summarise(mx = mean(mx)) %>%
  ungroup() %>%
  mutate(Source = "HMD",
         Rate = "ASMR")

# Wrangle SOCSIM data
SocsimM <- asmr %>% 
  rename(mx = socsim) %>% 
  mutate(Sex = ifelse(sex == "male", "Male", "Female"),
         Source = "SOCSIM",
         Rate = "ASMR") %>% 
  select(year, Sex, age,  mx, Source, Rate)


## Plot ASMR from HMD vs SOCSIM   

yrs_plot <- c("[1800,1805)", "[1900,1905)", "[2000,2005)") 

bind_rows(HMD, SocsimM) %>% 
  filter(year %in% yrs_plot) %>% 
  # Some ages can have rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(mx != 0 & !is.infinite(mx) & !is.nan(mx)) %>% 
  ggplot(aes(x = age, y = mx, group = interaction(year, Source))) +
  facet_wrap(~Sex) +
  geom_line(aes(colour = year, linetype = Source), linewidth = 1.3)+
  scale_y_log10() +
  scale_color_manual(values = c("#79B727", "#2779B7", "#B72779"))+ 
  scale_linetype_manual(values = c("HMD" = "dotted", "SOCSIM" = "solid")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/HMD_SOCSIM_ASMR.jpeg", width=17, height=9, dpi=300)

#---------------------------------------------------------------------------------------------------
## Final plot combining ASFR and ASMR ----

# Years to plot
yrs_plot <- c("[1800,1805)", "[1900,1905)", "[2000,2005)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(SocsimM$age)

# Define the same x and y breaks for all plots
y_breaks_asfr <- c(0.0, 0.05, 0.1, 0.15, 0.2)
y_breaks_asmr <- c(0.0, 0.0001, 0.001, 0.01, 0.1, 0.3)

bind_rows(HFCD0 %>% rename(Estimate = ASFR), 
            SocsimF0 %>% rename(Estimate = ASFR)) %>% 
  mutate(Sex = "Female") %>%   
  bind_rows(HMD %>% rename(Estimate = mx),
            SocsimM %>% rename(Estimate = mx)) %>% 
  # There can be rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(Estimate != 0 & !is.infinite(Estimate) & !is.nan(Estimate)) %>% 
  filter(Sex == "Female") %>% 
  mutate(Year = year,
         age = factor(as.character(age), levels = age_levels), 
         Source = ifelse(Source == "SOCSIM", "SOCSIM", "HFC/HFD-HMD"), 
         Rate = ifelse(Rate == "ASFR", "Age-Specific Fertility Rates", "Age-Specific Mortality Rates")) %>%
  filter(Year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = Estimate, group = interaction(Year, Source)))+
  facet_wrap(. ~ Rate, scales = "free") + 
  geom_line(aes(colour = Year, linetype = Source), linewidth = 1.5)+ # SOCSIM solid, HFD HMD transparent
  scale_color_manual(values = c("#79B727", "#2779B7", "#B72779"))+ 
  scale_linetype_manual(values = c("dotted", "solid"))+
  facetted_pos_scales(y = list("Age-Specific Fertility Rates" = scale_y_continuous(breaks = y_breaks_asfr),
                               "Age-Specific Mortality Rates" =  scale_y_continuous(breaks = y_breaks_asmr, trans = "log10")))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Age")+
  theme_graphs()
ggsave(file="Graphs/Socsim_HFD_HMD1.jpeg", width=17, height=9, dpi=300)
#----------------------------------------------------------------------------------------------------
## Summary measures: TFR and e0 ----
# Here, we use the socsim rates by 1 year age group and 1 calendar year
# Total Fertility Rate ----

## Retrieve age-specific fertility rates, by 1 year age group and 1 calendar year
asfr_1 <- estimate_fertility_rates(opop = opop,
                                   final_sim_year = 2017 , #[Jan-Dec]
                                   year_min = 1750, # Closed [
                                   year_max = 2018, # Open )
                                   year_group = 1, 
                                   age_min_fert = 10, # Closed [
                                   age_max_fert = 55, # Open )
                                   age_group = 1) # [,)
save(asfr_1, file = "Measures/asfr_1.RData")

# Load ASFR 1x1 and calculate TFR for plotting ----
load("Measures/asfr_1.RData")

# Year breaks. Extract all the unique numbers from the intervals. 
year_breaks_fert_1 <- unique(as.numeric(str_extract_all(asfr_1$year, "\\d+", simplify = T)))

# Year range to filter HFD data
year_range_fert_1 <- min(year_breaks_fert_1):max(year_breaks_fert_1-1)

# Age breaks of fertility rates. Extract all the unique numbers from the intervals 
age_breaks_fert_1 <- unique(as.numeric(str_extract_all(asfr_1$age, "\\d+", simplify = T)))

# Retrieve age_group size
age_group_fert_1 <- unique(diff(age_breaks_fert_1))

# Calculate TFR from HFC and HFD
TFR_HFCD <- bind_rows(HFC, HFD) %>% 
  filter(Year %in% year_range_fert_1) %>% 
  select(-OpenInterval) %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(ASFR, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Source = "HFC/HFD")

# Calculate TFR from SOCSIM
TFR <-  asfr_1 %>% 
  mutate(Year = as.numeric(str_extract(year, "\\d+"))) %>% 
  group_by(Year) %>% 
  summarise(TFR = sum(socsim)*age_group_fert_1) %>%
  ungroup() %>% 
  mutate(Source = "SOCSIM") 

## Plot TFR from HFD vs SOCSIM 
bind_rows(TFR_HFCD, TFR) %>%
  mutate(transp = ifelse(Source == "SOCSIM", "0", "1")) %>% 
  ggplot(aes(x = Year, y = TFR, group = Source)) +
  geom_line(aes(colour = Source, alpha = transp), linewidth = 1.3)+
  scale_color_manual(values = c("#007A75", "#CA650D"))+
  scale_alpha_discrete(guide = "none", range = c(0.2, 1))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_graphs()
ggsave(file="Graphs/HFD_SOCSIM_TFR.jpeg", width=17, height=9, dpi=300)

# Summary measure of error in TFR ----

# Differences of means
DM_TFR <- bind_rows(TFR_HFCD, TFR) %>%
  filter(Year > 1750) %>% 
  group_by(Year, Source) %>% 
  summarise(TFR = mean(TFR, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = Year, names_from = "Source", values_from = "TFR") %>% 
  mutate(Error = SOCSIM - `HFC/HFD`, 
         Type = "DM") %>% 
  select(Year, Error, Type) 

# Mean of differences
MD_TFR <- bind_rows(TFR_HFCD, TFR) %>% 
  filter(Year > 1750) %>% 
  pivot_wider(id_cols = Year, names_from = "Source", values_from = "TFR") %>% 
  mutate(Error = SOCSIM - `HFC/HFD`) %>% 
  group_by(Year) %>% 
  summarise(Error = mean(Error, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Type = "MD")

bind_rows(DM_TFR, MD_TFR) %>%
  ggplot(aes(x = Year, y = Error, colour = Type)) +
  geom_line(linewidth = 1.3)+
  geom_point(aes(shape = Type), size = 2)+
  theme_graphs()
# ggsave(file="Graphs/HFD_SOCSIM_TFR_Error.jpeg", width=17, height=9, dpi=300)
# The difference between both measure is almost imperceptible. 

# Life Expectancy at birth ----
# Calculate life expectancy at birth 1x1 for the 10 SOCSIM simulations

# Retrieve age-specific mortality rates, by 1 year age group and 1 calendar year
asmr_1 <- estimate_mortality_rates(opop = opop,
                                   final_sim_year = 2017, #[Jan-Dec]
                                   year_min = 1750, # Closed
                                   year_max = 2018, # Open )
                                   year_group = 1,
                                   age_max_mort = 110, # Open )
                                   age_group = 1) # [,)
save(asmr_1, file = "Measures/asmr_1.RData")

# Compute life tables from asmr 1x1 from the SOCSIM simulation
lt <- lt_socsim(asmr_socsim = asmr_1)
save(lt, file = "Measures/lt.RData")


# Load and wrangle life tables for plotting ----

# Load life tables from asmr 1x1 from the SOCSIM simulation
load("Measures/lt.RData")

## Compare with ex at age 0 for Sweden in HMD

# Get female life tables from HMD
ltf <- readHMDweb(CNTRY = "SWE",
                  item = "fltper_1x1",
                  username = HMD_username,
                  password = HMD_password)

# Get male life tables from HMD
ltm <- readHMDweb(CNTRY = "SWE",
                  item = "mltper_1x1",
                  username = HMD_username,
                  password = HMD_password)

# Load asmr 1x1 for the 10 simulations
load("Measures/asmr_1.RData")

# Year breaks. Extract all the unique numbers from the intervals 
year_breaks_mort_1 <- unique(as.numeric(str_extract_all(asmr_1$year, "\\d+", simplify = T)))

# Year range to filter HMD data
year_range_mort_1 <- min(year_breaks_mort_1):max(year_breaks_mort_1-1)

# Wrangle HMD life tables 
lt_HMD <- ltf %>%
  select(Year, Age, ex) %>% 
  mutate(sex = "female") %>% 
  bind_rows(ltm %>% 
              select(Year, Age, ex) %>%  
              mutate(sex = "male")) %>% 
  mutate(sex = factor(sex, levels = c("male", "female")), 
         Source = "HMD") %>% 
  select(Year, ex, Source, sex, Age)

# Wrangle SOCSIM life tables
ltb <- lt %>%
  mutate(Year = as.numeric(str_extract(year, "\\d+")),
         Source = "SOCSIM") %>% 
  select(Year, ex, Source, sex, Age)

bind_rows(lt_HMD, ltb) %>% 
  filter(Age == 0 & Year %in% year_range_mort_1) %>%
  mutate(transp = ifelse(Source == "SOCSIM", "0", "1"),
         Sex = ifelse(sex == "female", "Female", "Male")) %>% 
  ggplot(aes(x = Year, y = ex, group = Source))+
  geom_line(aes(colour = Source, alpha = transp), linewidth = 1.3)+
  scale_color_manual(values = c("#0C0B7F", "#007A75"))+
  scale_alpha_discrete(guide = "none", range = c(0.2, 1))+
  facet_wrap(~Sex) +
  theme_graphs()+
  labs(y = "e0") 
ggsave(file="Graphs/HMD_SOCSIM_e0.jpeg", width=17, height=9, dpi=300)

# Summary measure of error in e0 ----

# Differences of means
DM_e0 <- bind_rows(lt_HMD, ltb) %>%
  filter(Year > 1750 & Age == 0) %>% 
  group_by(Year, sex, Source) %>% 
  summarise(ex = mean(ex, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(Year:sex), names_from = "Source", values_from = "ex") %>% 
  mutate(Error = SOCSIM - HMD, 
         Type = "DM") %>% 
  select(Year, sex, Error, Type) 

# Mean of differences
MD_e0 <- bind_rows(lt_HMD, ltb) %>% 
  filter(Year > 1750 & Age == 0) %>% 
  pivot_wider(id_cols = c(Year, sex), names_from = "Source", values_from = "ex") %>% 
  mutate(Error = SOCSIM - HMD) %>% 
  group_by(Year, sex) %>% 
  summarise(Error = mean(Error, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Type = "MD")

bind_rows(DM_e0, MD_e0) %>%
  ggplot(aes(x = Year, y = Error, colour = Type)) +
  facet_wrap(. ~ sex)+
  geom_line(linewidth = 1.3)+
  geom_point(aes(shape = Type), size = 3)+
  theme_graphs()
# The difference between both measures is almost imperceptible. 
ggsave(file="Graphs/HFD_SOCSIM_e0_Error.jpeg", width=17, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------
## Final plot combining TFR and e0 ----

# Define the same y breaks for all plots
y_breaks_TFR <- c(0:5)
y_breaks_e0 <- c(20, 40, 60, 80)

## Plotting TFR and e0 (for females) from HFD/HMD vs SOCSIM 
bind_rows(TFR_HFCD %>% rename(Estimate = TFR) %>%  mutate(Rate = "TFR"),
          TFR %>% rename(Estimate = TFR) %>% mutate(Rate = "TFR")) %>% 
  mutate(sex = "female") %>%   
  bind_rows(lt_HMD %>% filter(Age == 0) %>% rename(Estimate = ex) %>% mutate(Rate = "e0"),
            ltb %>% filter(Age == 0) %>% rename(Estimate = ex) %>% mutate(Rate = "e0")) %>% 
  filter(sex == "female") %>%
  mutate(transp = ifelse(Source == "SOCSIM", "0", "1"),
         Rate = ifelse(Rate == "TFR", "Total Fertility Rate", "Life Expectancy at Birth"), 
         Rate = factor(Rate, levels = c("Total Fertility Rate", "Life Expectancy at Birth"))) %>% 
  ggplot(aes(x = Year, y = Estimate, group = Source))+
  facet_wrap(. ~ Rate, scales = "free") + 
  geom_line(aes(colour = Source), linewidth = 1.2) +
  scale_color_manual(values = c("#CA650D", "#0C0B7F", "#007A75")) +
  facetted_pos_scales(y = list("Total Fertility Rate" = scale_y_continuous(breaks = y_breaks_TFR, 
                                                                           limits = c(0, NA)),
                               "Life Expectancy at Birth" =  scale_y_continuous(breaks = y_breaks_e0)))+
  scale_x_continuous(breaks = c(1750, 1800, 1850, 1900, 1950, 2000))+
  theme_graphs()
Summary
ggsave(file="Graphs/Socsim_HFD_HMD2.jpeg", width=17, height=9, dpi=300)

#----------------------------------------------------------------------------------------------------
## Compare the number of children by cohort in 2017, between HFD, registers and SOCSIM ----

## I. Number of children by cohort from HFD
# To get the number of children for all cohorts included in the paper, 
# we need to combine the Completed cohort fertility (for female cohorts aged 55 and plus in 2017)
# and the Cumulative cohort fertility rates for female cohorts aged 55 or less in 2017)

# A. Completed cohort fertility
# For cohorts with completed fertility in 2017 (aged 55 and plus)
# The corresponding quantities of TFR represent the completed cohort fertility (CCF).
tfrVH <- readHFDweb(CNTRY = "SWE",
                    item = "tfrVH", 
                    username = HFD_username,
                    password = HFD_password)

ccf <- tfrVH %>% 
  select(-c("CCF40", "OpenInterval")) %>% 
  # Filter cohorts without completed fertility
  filter(!is.na(CCF))


# B. Cumulative cohort fertility rates (horizontal parallelograms)
# Average number of children born to a woman from birth cohort c by age x
# Computed by summing up the set of age-specific fertility rates of the cohort c observed over their reproductive lives up to age x. 
# CCFRs are calculated for all cohorts c who are observed from age xmin that is equal to 15 or younger.

# For cohorts with incomplete fertility in 2017 (aged 55 or less)
# We need to calculate the Year (Cohort + Age) and filter the values for 2017
ccfrVH <- readHFDweb(CNTRY = "SWE",
                     item = "ccfrVH", 
                     username = HFD_username,
                     password = HFD_password)

ccfr <- ccfrVH %>% 
  mutate(Year = Cohort + Age) %>% 
  filter(Year == 2017) %>% 
  select(-c("OpenInterval", "Year")) 

HFD_cc <- full_join(ccf, ccfr) %>% 
  # In 2017, cohorts 1962-1972 were 45 to 55 years old and thus have not completed their fertility schedule
  # Hence, the values of CCFR in 2017 are slightly lower than their final CCF. 
  # So, we need to omit the CCF and keep the CCFR
  mutate(CCF = ifelse(is.na(Age), CCF, NA),
         HFD = ifelse(is.na(CCFR) & !is.na(CCF), CCF, ifelse(is.na(CCF) & !is.na(CCFR), CCFR, NA))) %>% 
  select(Cohort, HFD)


## II. Number of children by cohort from the registers
# Import the data for the figures included in "The Swedish Kinship Universe" (online appendix 2)
# Figure 2b Average number of living and dead children in 2017 by sex and birth cohort. 
# Here the deceased include both the deceased and the living 
SKU <- read_excel(path="kolk_esm2.xlsx", sheet = "Fig2b") 

SKU_cc <- SKU %>% 
  filter(type == "registered deceased" & gender == 2) %>% 
  select(Cohort = IDbirthYear, Registers = mean_children)

left_join(HFD_cc, SKU_cc) %>% 
  # Omit the cohorts not included in the paper
  filter(!is.na(Registers)) %>% 
  pivot_longer(HFD:Registers, names_to = "Source", values_to = "CFR") %>% 
  ggplot(aes(x = Cohort, y = CFR, colour = Source))+
  geom_line(linewidth =1) +
  scale_color_manual(values = c("#CA650D", "#002F5F"))+
  labs(title = "Mean number of children by cohort in 2017, HFD and Registers")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_HFD_Registers.jpg", width=18, height=9, dpi=200)

left_join(HFD_cc, SKU_cc) %>% 
  # Omit the cohorts not included in the paper
  filter(!is.na(Registers)) %>% 
  mutate(Difference = HFD - Registers) %>% 
  ggplot(aes(x = Cohort, y = Difference))+
  geom_line(linewidth =1) +
  labs(title = "Difference in the number of children by cohort in 2017, HFD - Registers")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_HFD_Registers_Diff.jpg", width=18, height=9, dpi=200)

## III. Number of children by female cohort from SOCSIM

# Estimate mean number of children born to woman by birth cohort from total numbers
# This calculation does not account for the age-structure

## Load opop and omar generated in 0_Set_Up_Simulation.R from simulation with no heterogeneous fertility, 
# but parity specific rates
load("opop.RData")

# Function to convert SOCSIM months to calendar years. 
asYr <- function(month, last_month, final_sim_year) {
  return(final_sim_year - trunc((last_month - month)/12))
}


last_month <- max(opop$dob)
final_sim_year <- 2017
cohort_range <- 1915:2017
year_count <- max(cohort_range)


opop <- opop %>% 
  mutate(birth_year = asYr(dob, last_month, final_sim_year), 
         death_year = ifelse(dod == 0, 999999, asYr(dod, last_month, final_sim_year))) 

## Births by Cohort
numerator <- opop %>% 
  left_join(opop %>% select(mom = pid, cohort = birth_year, mother_death = death_year), 
            by = "mom") %>% 
  filter(birth_year <= year_count & cohort %in% cohort_range) %>% 
  filter(is.na(mother_death) | mother_death > year_count) %>% 
  select(Cohort = cohort, birth_year) %>% 
  count(Cohort)

## Women by Cohort alive in 2017
denominator <- opop %>% 
  rename(Cohort = birth_year) %>% 
  filter(fem == 1 & Cohort %in% cohort_range & death_year > year_count) %>% 
  count(Cohort)

# Estimate mean number of children born to woman from birth cohort
SOCSIM_cc <- left_join(numerator %>% rename(nume = n), 
                       denominator %>% rename(deno = n)) %>%
  mutate(SOCSIM = nume/deno)

full_join(SOCSIM_cc %>% select(Cohort, SOCSIM), 
          SKU_cc) %>% 
  mutate(SOCSIM = ifelse(is.na(SOCSIM), 0, SOCSIM)) %>% 
  # Omit the cohorts not included in the paper
  filter(!is.na(Registers)) %>% 
  pivot_longer(SOCSIM:Registers, names_to = "Source", values_to = "CFR") %>% 
  ggplot(aes(x = Cohort, y = CFR, colour = Source))+
  geom_line(linewidth =1.2) +
  scale_color_manual(values = c("#002F5F", "#007A75"))+
  labs(title = "Number of children by cohort in 2017, SOCSIM and Registers")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_SOCSIM_Registers.jpg", width=18, height=9, dpi=200)

full_join(SOCSIM_cc %>% select(Cohort, SOCSIM), 
          SKU_cc) %>% 
  mutate(SOCSIM = ifelse(is.na(SOCSIM), 0, SOCSIM),
         Difference = SOCSIM - Registers) %>% 
  ggplot(aes(x = Cohort, y = Difference))+
  geom_line(linewidth = 1.2) +
  labs(title = "Difference in the number of children by cohort in 2017, SOCSIM - Registers. (From total numbers)")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_SOCSIM_Registers_Diff.jpg", width=18, height=9, dpi=200)



## Approximate SOCSIM Cohort fertility from the diagonals of asfr 1x1
load("Measures/asfr_1.RData")

SOCSIM_cc_diag <- asfr_1 %>% 
  mutate(Year = as.numeric(str_extract(year, "\\d+")), 
         Age = as.numeric(str_extract(age, "\\d+")), 
         Cohort = Year - Age) %>% 
  filter(Year <= 2017 & Cohort %in% cohort_range) %>% 
  group_by(Cohort) %>% 
  summarise(SOCSIM = sum(socsim)) %>% 
  ungroup()

full_join(SOCSIM_cc_diag %>% select(Cohort, SOCSIM), 
          SKU_cc) %>% 
  filter(!is.na(Registers)) %>% 
  pivot_longer(SOCSIM:Registers, names_to = "Source", values_to = "CFR") %>% 
  ggplot(aes(x = Cohort, y = CFR, colour = Source))+
  geom_line(linewidth =1.2) +
  scale_color_manual(values = c("#002F5F", "#007A75"))+
  labs(title = "Number of children by cohort in 2017, SOCSIM and Registers. (From ASFR diagonals)")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_SOCSIM_Registers_D.jpg", width=18, height=9, dpi=200)


full_join(SOCSIM_cc_diag %>% select(Cohort, SOCSIM), 
          SKU_cc) %>% 
  filter(!is.na(Registers)) %>% 
  mutate(Difference = SOCSIM - Registers) %>% 
  ggplot(aes(x = Cohort, y = Difference))+
  geom_line(linewidth =1.2) +
  labs(title = "Difference in the number of children by cohort in 2017, SOCSIM - Registers. (From ASFR diagonals)")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_SOCSIM_Registers_Diff_D.jpg", width=18, height=9, dpi=200)


full_join(SOCSIM_cc_diag %>% select(Cohort, SOCSIM), 
          SKU_cc) %>% 
    left_join(HFD_cc) %>% 
  filter(!is.na(Registers)) %>% 
  pivot_longer(SOCSIM:HFD, names_to = "Source", values_to = "CFR") %>% 
  ggplot(aes(x = Cohort, y = CFR, colour = Source))+
  geom_line(linewidth =1) +
  scale_color_manual(values = c("#CA650D", "#002F5F", "#007A75"))+
  labs(title = "Number of children by cohort in 2017, HFD, Registers and SOCSIM (From ASFR diagonals)")+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_HFD_Registers_SOCSIM_Diff.jpg", width=18, height=9, dpi=200)

full_join(SOCSIM_cc_diag %>% select(Cohort, SOCSIM), 
          SKU_cc) %>% 
  left_join(HFD_cc) %>% 
  filter(!is.na(Registers)) %>% 
  mutate(SOCSIM_Registers = SOCSIM - Registers, 
         HFD_Registers = HFD - Registers) %>% 
  pivot_longer(SOCSIM_Registers:HFD_Registers, names_to = "Source", values_to = "Difference") %>% 
  ggplot(aes(x = Cohort, y = Difference, colour = Source))+
  geom_line(linewidth =1) +
  scale_color_manual(values = c("#CA650D", "#007A75"))+
  theme_bw() + theme_graphs2()
ggsave(file="Graphs/Children_HFD_SOCSIM_Registers_Diff.jpg", width=18, height=9, dpi=200)