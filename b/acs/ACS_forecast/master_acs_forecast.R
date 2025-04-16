# Master script
# Loads ACS and CPS datasets, runs prep, diagnostics, and forecasting

rm(list = ls())

# Load libraries
library(dplyr)
library(readr)
library(haven)     # for .dta files
library(ggplot2)

# ----------------------
# 1. Load raw data
# ----------------------

# CPS Novosad with granular age
cps_novosadgranage_raw <- read_dta("/Users/Stata/SeminarHealthSociety/cps_aggregated_excel/data/cps_novosad_granage.dta")
cat("CPS Novosad (granular age) loaded: ", nrow(cps_novosadgranage_raw), "rows\n")

# ACS with granular age
acs_granage_raw <- read_dta("/Users/Stata/SeminarHealthSociety/cps_aggregated_excel/data/acs_granage.dta")
cat("ACS (granular age) loaded: ", nrow(acs_granage_raw), "rows\n")

# CPS Novosad with grouped age
cps_novosad_raw <- read_dta("/Users/Stata/SeminarHealthSociety/cps_aggregated_excel/data/cps_novosad.dta")
cat("CPS Novosad (grouped age) loaded: ", nrow(cps_novosad_raw), "rows\n")

# Combine CPS from website CSV
cps_website_raw <- read_csv("/Users/Stata/SeminarHealthSociety/cps_aggregated_excel/data/website_cps_aggregate.csv")
cat("CPS Website (CSV) loaded: ", nrow(cps_website_raw), "rows\n")

# ----------------------
# 2. Run data prep scripts
# ----------------------

source("scripts/load_cps_novosad.R")
cat("CPS Novosad (granular age) prepped\n")

# Other scripts (to be added later):
source("scripts/load_acs_granage.R")

source("scripts/load_cps_novosad_grouped.R")
source("scripts/load_cps_compare.R")

source("scripts/adjusting_tPop.R")
cat("ACS + CPS combined\n")

# ----------------------
# 3. Diagnostics
# ----------------------

source("scripts/diagnostics.R")

# ----------------------
# 4. Forecasting
# ----------------------

# source("scripts/forecast.R")
