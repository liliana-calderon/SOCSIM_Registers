#------------------------------------------------------------------------------------------------------
# SOCSIM - SOCSIM Registers - Write rates, create initial population and run simulation
# U:/SOCSIM/SOCSIM_Registers/0_Set_Up_Simulation.R

# This code has three main steps: write rates, create the initial populations and run the simulation

## 1. Write the input fertility and mortality rates for a SOCSIM micro-simulation for Sweden,
# using data from HFD (1891-2021) and HMD (1751-2021), with the HMDHFDplus package.
# and the Human Fertility Collection (HFC) for the period not covered in HFD (1751-1890)

## 2. Create an initial population file and empty marriage file for the simulations

## 3. Run a SOCSIM demographic microsimulation using the 'rsocsim' package and data for Sweden (1751-2022)
## and read the output into R. 

# Created on 13-02-2024
# Last modified on 19-03-2024

## Based on code prepared for the biases in genealogies paper
# U:/SOCSIM/SOCSIM_Genealogies/0_Write_Input_Rates.R
# U:/SOCSIM/SOCSIM_Genealogies/1_Run_Simulations.R
#------------------------------------------------------------------------------------------------------
# SOCSIM simulation: rate files and assumptions ----

# The SOCSIM simulation is implemented using the 'rsocsim' package,
# Available at https://github.com/MPIDR/rsocsim 

# It uses as input historical data for Sweden (1751-2022). 
# Input age-specific fertility rates come from the Human Fertility Collection (HFC) for 1751-1890,
# the Human Fertility Database (HFD) for 1891-2022
# and age-specific mortality rates come from the Human Mortality Database for the whole period (1751-2022).
# To run the simulation, original HFC, HFD and HMD rates are converted to monthly rates/probabilities
# and SOCSIM format using the 0_Write_Input_Rates.R script.
# HFC fertility rates are provided by 5 calendar years and hence considered to be constant over each sub-period


# Female fertility rates are identical for all marital status, but are specified for single and married women
# Other marital status (divorced, widowed, cohabiting) follow the SOCSIM rate default rules. 
# Mortality rates correspond to probabilities of death (qx) of HMD period life tables. 
# They are identical for all marital status of each sex, and are only specified for single women and men. 
# Other marital status will follow the rate default rules. 

# The first segment of the simulation runs for 100 years to produce a stable age structure, 
# based on 1751-HFC and 1751-HMD age-specific rates

#------------------------------------------------------------------------------------------------------
# Rate files format (Cf. Socsim oversimplified, p. 26):

# A rate block is a complete set of age speciﬁc rates governing a demographic event 
# for people of a particular sex, group and marital status. 
# In the rate files, the order matters and is always: 
# Event (birth, death, marriage), group (1, ..), sex (F or M), and
# marital status (married, single, divorced, widowed)
# For birth rates, this can be followed by number indicating parity.
# Each subsequent line contains a one month rate or probability
# and the age interval over which it holds (years and months of the upper age bound)
# The interval includes upper age bound in previous line and ends just before upper age bound in current line.
# The first two numbers are years and months of the upper age bound, which are added together.
# e.g. upper age bound of "0 1" refers to the first month of life, thus, [0,1)

#----------------------------------------------------------------------------------------------------
## Global Settings ----

# Clear work space
rm(list=ls(all=TRUE))

# Prevent scientific notation
options(scipen=999999)

# Load necessary packages
library(tidyverse)
library(readr)
# install.packages("HMDHFDplus")
library(HMDHFDplus)

# Load functions to write SOCSIM rate files from HFD/HMD and HFC
source("Functions/Functions_Input_Rates.R")

#----------------------------------------------------------------------------------------------------
## 1.1 Write fertility rate files for SOCSIM using data from HFD ----

## NOTES:
# We keep the whole age range included in HFD [12-55], 
# but limit the open-ended age intervals 12- and 55+ to one year, i.e. [12-13) and [55-56)
# Additionally, SOCSIM requires a final age category with an upper bound
# corresponding to the maximum length of life. Here, we fixed it to [56-110]

# To convert the annual rates to monthly rates
# we assume that the fertility rates are constant over the year and divide them by 12 (months)
# According to SOCSIM oversimplified, these are rates rather than probabilities. 
# So multiplying a rate by the number of months in the age category gives
# the expected number of births that a woman who lives through the age category will experience.

# The rates are identical for married, single, divorced and widowed females. 
# We only specify the rates for single and married, 
# as other marital status will follow the rate default rules. 

# To write the input fertility rate files from HFD, 
# please type inside the quotes the name of the country as used in HFD) 
# and your HFD credentials (username and password) for the new website
# If needed, check the countries' names and availability with getHFDcountries()

write_socsim_rates_HFD(Country = "SWE",
                       HFD_username = "Type_here_HFD_username",
                       HFD_password = "Type_here_HFD_password")


#----------------------------------------------------------------------------------------------------
## 1.2. Write fertility rate files for SOCSIM using data from Human Fertility Collection ----

# For the period before 1751-1890 that is covered by HMD but not by HFD, 
# We use data from the Human Fertility Collection (RefCode = "SWE_02")
# Vital registers in Sweden for this period are available from Statistics Sweden (1969). 
# Historisk statistik för Sverige. Del 1. Bevolkning 1720-1967. Örebro : Statistiska centralbyrån. 
# These data have Age Definition = ACY, Age in Completed Years and age range [15-49] 
# Age Interval is 1 for most ages [15-49], but for Age=14 (AgeInt=-99) and Age=55 (AgeInt=99)
# The age-specific rates hold over a period of 5 calendar years. 
# Hence, we use the same set of rates for each calendar year of the period.

write_socsim_rates_HFC(Country = "SWE",
                       RefCode = "SWE_02", # For Vital statistics 1751 - 1965
                       Year_Max = 1890) # Only before 1891 (start of HFD)

#----------------------------------------------------------------------------------------------------
## 1.3 Write mortality rate files for SOCSIM using data from HMD ----

## NOTES:

# We keep the whole age range included in HMD [0-110+]
# but limit the open-ended age interval 110+ to one year, i.e. [110-111)
# Originally, Socsim had an upper bound of 100 years, but the limit has been extended in rsocsim 

# As SOCSIM inputs, we use the probabilities of death (qx) from period life tables, 
# which are smoothed for ages 80 and above (cf. HMD explanatory notes on old-age mortality).
## To convert the annual probabilities to monthly probabilities,  
# we assume that the probability of dying is constant over the year
# and use the formula 1-(1-nqx)^(1/n) proposed by Kenneth Watcher (2014, p. 53). 
# For the open-ended age interval 110+ monthly probabilities are equal to 1/12

# The probabilities are identical for all marital status 
# (married, single, divorced and widowed) of each sex
# We only specify the rates by sex for single as other marital status will follow the rate default rules. 

# As explained by the HMD explanatory note https://www.mortality.org/Data/ExplanatoryNotes 
# there are some difficulties in the calculation of mortality rates for ages 80+
# For period life tables, the central death rate m(x) is used to compute
# probabilities of death q(x).
# The values of m(x) below age 80 are by definition equal
# to the observed population death rate M(x) shown on each country page.
# At older ages, however, the number of deaths and the exposure-to-risk
# eventually become quite small,
# and thus observed death rates display considerable random variation.
# Therefore, we smooth the M(x) values for ages 80 and older
# and use these smoothed values to compute q(x) above a certain age
# (based on the number of observed deaths).
# For details, see the Methods Protocol (pp. 35-37).
# This procedure helps to avoid certain difficulties
# in period life table calculations at older ages that may be caused by:
# 1) extremely high death rates resulting from exposure being smaller than the number of deaths,
# 2) death rates of zero resulting from no deaths at an age where exposure is non-zero,
# 3) undefined death rates at all ages where exposure is zero.

# To write the input mortality rate files from HMD, 
# please type inside the quotes the name of the country (as used in HMD) 
# and your HMD credentials (username and password) for the new website
# If needed, check the countries' names and availability with getHMDcountries()

write_socsim_rates_HMD(Country = "SWE", 
                       HMD_username = "Type_here_HMD_username",
                       HMD_password = "Type_here_HMD_password")

#------------------------------------------------------------------------------------------------------
## 2. Create initial .opop and empty omar file for the simulation ----

# Set size of initial population
size_opop <-  50000
# For 20000, Time difference of 1.524974 hours

# Create data.frame with 14 columns and nrows = size_opop
presim.opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim.opop$pid <- 1:size_opop

# Add sex randomly
presim.opop$fem <- sample(0:1, nrow(presim.opop), replace = T)

# Add group 1 for all individuals
presim.opop$group <- 1

# Add random dates of birth (max age around 50)
presim.opop$dob <- sample(600:1200, nrow(presim.opop), replace = T)

## Check distribution of sex 
table(presim.opop$fem)

# Write initial population for pre-simulation (without fertility multiplier)
write.table(presim.opop, "presim.opop", row.names = F, col.names = F)


## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# Write empty omar for pre-simulation
write.table(presim.omar, "presim.omar", row.names = F, col.names = F)

#------------------------------------------------------------------------------------------------------
## Run SOCSIM simulations with 'rsocsim' package ----
# The instructions to install the rsocsim package and run the simulation 
# can be found on https://github.com/MPIDR/rsocsim/blob/main/readme.md
# To get the latest version from source, RTools and devtools must be installed.

# library(devtools)
# Install rsocsim from Github with devtools:
# devtools::install_github("MPIDR/rsocsim")

# To get the updates, uninstall and install again the package
# remove.packages("rsocsim")

# Load rsocsim package
library("rsocsim")

# Specify the working directory, where the supfile and ratefiles are.
# If the R session is running through the project file (SWE_example.Rproj), the following command can be used.
folder <- getwd()
# Otherwise, the working directory must be specified after "folder <- "

# Name of the supervisory file stored in the above folder:
# Sup file for rates retrieved from HFC/HFD and HMD (1751-2022), with marry after childbirth 

supfile <- "Sweden_0.sup" # hetfert 0
# supfile <- "Sweden_a0b1.sup" # hetfert 1, alpha 0 (no mother inheritance), beta 1 (exactly initial fmult)
# supfile <- "Sweden_a0b0.sup" # hetfert 1, alpha 0 (no mother inheritance), beta 0 (higher fmult than initial)
# supfile <- "Sweden_a0b05.sup" # hetfert 1, alpha 0 (no mother inheritance), beta 05 (higher fmult than initial, but lower than beta 0)
# supfile <- "Sweden_a0b2.sup" # hetfert 1, alpha 0 (no mother inheritance), beta 2 (lower fmult than initial)

# Random number generator seed:
#seed <- as.character(sample(1:99999, 1, replace = F))

# Save the seed number to use them later to read the data
# save(seed, file = "seed.Rda")
load("seed.Rda")

## Run the simulation for the random seed. 
start <- Sys.time()

### Run a single SOCSIM-simulation with a given folder and supervisory file
rsocsim::socsim(folder, supfile, seed, process_method = "future")
end <- Sys.time()

print(end-start)
# Time difference of 17.25873 minutes for  1 simulation, with initial opop of 5000 and hetfert 0
# Time difference of 1.7874 days for 1 simulation, with initial opop 50000 and hetfert 0
# Time difference of 12.42807 hours for 1 simulation, with initial opop 50000, hetfert 1 alpha 0 beta 1
# Time difference of 6.10245 days for 1 simulation, with initial opop of 5000 and hetfert 1, alpha 0 beta 0.5
#----------------------------------------------------------------------------------------------------
## Read the output .opop and .omar files ----

# Load seeds numbers and convert them into numeric
load("seed.Rda")
seed <- as.numeric(seed)

## No heterogeneous fertility (hetfert_0)
# Read opop
opop_hetfert0 <- rsocsim::read_opop(folder = getwd(),
                                   supfile = "Sweden_0.sup",
                                   seed = seed,
                                   suffix = "",
                                   fn = NULL)
# Save opop to use later
save(opop_hetfert0, file = "opop_hetfert0.RData")

# Read omar
omar_hetfert0 <- rsocsim::read_omar(folder = getwd(),
                                   supfile = "Sweden_0.sup",
                                   seed = seed,
                                   suffix = "",
                                   fn = NULL)
# Save omar to use later
save(omar_hetfert0, file = "omar_hetfert0.RData")


## Heterogeneous fertility, with alpha 0 (no mother inheritance), beta 1 (exactly initial random fmult)
# Read opop
opop_a0b1 <- rsocsim::read_opop(folder = getwd(),
                                supfile = "Sweden_a0b1.sup",
                                seed = seed,
                                suffix = "",
                                fn = NULL)
# Save opop to use later
save(opop_a0b1, file = "opop_a0b1.RData")

# Read omar
omar_a0b1 <- rsocsim::read_omar(folder = getwd(),
                                supfile = "Sweden_a0b1.sup",
                                seed = seed,
                                suffix = "",
                                fn = NULL)
# Save omar to use later
save(omar_a0b1, file = "omar_a0b1.RData")


## Heterogeneous fertility, with alpha 0 (no mother inheritance), beta 2 (higher fmult than initial)

# Read opop
opop_a0b2 <- rsocsim::read_opop(folder = getwd(),
                                supfile = "Sweden_a0b2.sup",
                                seed = seed,
                                suffix = "",
                                fn = NULL)
# Save opop to use later
save(opop_a0b2, file = "opop_a0b2.RData")

# Read omar
omar_a0b2 <- rsocsim::read_omar(folder = getwd(),
                                supfile = "Sweden_a0b2.sup",
                                seed = seed,
                                suffix = "",
                                fn = NULL)
# Save omar to use later
save(omar_a0b2, file = "omar_a0b2.RData")

## Heterogeneous fertility, with alpha 0 (no mother inheritance), beta 0 (higher fmult than initial)
# I had to stop the simulation with alpha 0 and betaT 0 (with initial opop 5000) before it finished
# because it took more than 11 days to reach month 3500 and had already 871700 living individuals. 


## Heterogeneous fertility, with alpha 0 (no mother inheritance), beta 0.5 (higher fmult than initial)
# rsocsim stopped writing the logfile at Segment NO:	255 of 273 
# but on the console it indicated Socsim Main Done Socsim Don after reading rates for 2022
# The population pyramid stopped at month 254 and wrote no opop file

# # Read opop
# opop_a0b05 <- rsocsim::read_opop(folder = getwd(),
#                                  supfile = "Sweden_a0b05.sup",
#                                  seed = seed,
#                                  suffix = "",
#                                  fn = NULL)
# # Save opop to use later
# save(opop_a0b05, file = "opop_a0b05.RData")
# 
# # Read omar
# omar_a0b05 <- rsocsim::read_omar(folder = getwd(),
#                                  supfile = "Sweden_a0b05.sup",
#                                  seed = seed,
#                                  suffix = "",
#                                  fn = NULL)
# # Save omar to use later
# save(omar_a0b05, file = "omar_a0b05.RData")


## To Write intermediate output files, add this line to your sup-file:
# write_output 1