#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Registers - Data Format
# U:/SOCSIM/SOCSIM_Registers/1_Data_Format.R

##

# Created on 21-11-2023
# Last modified on 23-11-2023
#------------------------------------------------------------------------------------------------------
## General settings and functions -----
# Prevent scientific notation
options(scipen=999999)

## Load packages 
library(data.table)
library(tidyverse)
library(ggh4x)  # To facet scales-
library(HMDHFDplus)
library(patchwork) # To combine ggplots
library(reshape2)
library(rsocsim) # Functions to estimate rates
library(svglite) # To save svg files
library(viridis)

## Load theme for the graphs and to convert SOCSIM time
source("Functions/Functions_Graphs.R")

# Load the functions from kin_count_sweden (getRefTable and AddBirthsDeaths)
# written by Martin?
source("kin_count_sweden/R/functions.R")

## Type inside the quotes HFD and HMD credentials (username and password) for the new website
# This information is necessary to run the comparisons below

# HFD credentials
# HFD_username <- "Type_here_HFD_username"
# HFD_password <- "Type_here_HFD_password"
# 
# # HMD credentials
# HMD_username <- "Type_here_HMD_username"
# HMD_password <- "Type_here_HMD_password"

#------------------------------------------------------------------------------------------------------
# NB: The code below is an adapted version of the 2_socsim_kin_structures.R created by Diego Alburez-Gutierrez

## 1. Load the data 
# Load saved list with opop from simulations, 
# Temporarily using the one generated for the project U:/SOCSIM/SOCSIM_Genealogies
load("sims_opop.RData")
# Load saved list with omar from simulations
# Temporarily using the one generated for the project U:/SOCSIM/SOCSIM_Genealogies
load("sims_omar.RData")

# Select the first data frame from the opop and omar lists
opop <- sims_opop[[1]]
omar <- sims_omar[[1]]

# Define parameters to convert socsim month to calendar years
last_month <- max(opop$dob)
final_sim_year <- 2022

# Convert SOCSIM months to calendar years
asYr <- function(month, last_month, final_sim_year) {
  return(final_sim_year - trunc((last_month - month)/12))
}

# Add year of birth and year of death to the opop file
opop <- opop %>% 
  mutate(birth_year = asYr(dob, last_month, final_sim_year),
         ## Perhaps it's easier to set 9999 as dod for people alive at the end of the simulation
         death_year = ifelse(dod == 0, 9999, asYr(dod, last_month, final_sim_year)))


## 2.  Format the data

# Q for Martin: in Kon, 1 = M?

opop2 <- opop %>%
  mutate(# age_death = death_year - birth_year,
         Kon = fem + 1, 
         FodelselandGrp = 1) %>% 
  # Reformat to Swedish register format
  select(LopNr = pid, 
         FoddAr = birth_year, 
         Kon,
         FodelselandGrp,
         LopNrFar = pop,
         LopNrMor = mom,
         deathyear = death_year) %>%
  # Add NA for the parents of the founder generation
  mutate(LopNrFar = ifelse(LopNrFar == 0, NA, LopNrFar), 
         LopNrMor = ifelse(LopNrMor == 0, NA, LopNrMor))

year_min <- 1900
year_max <- 2022

# Filter population born after 1800
popdatdeaths <- opop2 %>% 
  # -100 of year_min to be sure that we don't miss parents or grandparents
  filter(between(FoddAr, year_min -100, year_max))

# 3. Create kinship objects

# age_range <- 0:100 # I think this is not necessary

popdat <- popdatdeaths %>% select(-deathyear)

# get Ref Table (from Martin's functions)
refTableList <- getRefTable(df = popdat, ref_TypeI = "all")
# Warning messages while running this code
# In inner_join(x = ., y = ., by = c(refID = "ID")) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.

# Add years of birth and death (from Martin's functions)
# According to Diego, this is not really needed, but still did it
SimRefTableList <- lapply(refTableList, AddBirthsDeaths, df = opop2)

reference_table_SweBorn <- 
  bind_rows(SimRefTableList, .id = "refGroup") %>% 
  mutate(refGroup = gsub("_df", "", refGroup)) %>%
  inner_join(select(filter(popdat, FodelselandGrp == 1), LopNr, Kon, FodelselandGrp, FoddAr), by = c("ID" = "LopNr")) %>% 
  # here he can safely remove all those people who are irrelevant 
  filter(between(IDbirthYear, year_min, year_max)) 

# 4. Get other quantitites --------------

# Get table with number of swedish born parents per person
temp <- reference_table_SweBorn %>% 
  filter(refGroup == "parent" & !is.na(refID)) %>%
  # left_join(., select(distinct(reference_table_SweBorn, ID, .keep_all = TRUE), ID, FodelselandGrp), by = c("refID" = "ID")) %>% 
  filter(FodelselandGrp == 1) %>%
  group_by(ID, IDbirthYear, FoddAr) %>%
  summarise(n_swe_parents = n(), .groups = "drop") %>%
  setDT()

TotalPopulation <- popdatdeaths %>% setDT()

# Get tables with N per cohort
N_Cohort <- list(TotalPopulation[, .(N_ever = .N), keyby = FoddAr], 
                 reference_table_SweBorn[, .(N_17 = uniqueN(ID)), keyby = FoddAr], 
                 temp[n_swe_parents == 2, .N, keyby = FoddAr]) %>%
  reduce(left_join, by = "FoddAr") %>%
  rename(N_sweparent = N) %>% 
  mutate(IDbirthYear = FoddAr)

# Table with N_cohort by kon
temp <- reference_table_SweBorn[!is.na(FoddAr), .(N_17 = uniqueN(ID)), keyby = .(FoddAr, Kon)]

temp <- temp %>% rename(IDbirthYear = FoddAr)

# 5. Export ------

save("reference_table_SweBorn", file = "Output/reference_table_SweBorn.RData")
save(temp, file = "Output/temp.RData")
save(N_Cohort, file = "Output/N_Cohort.RData")