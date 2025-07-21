
# Purpose -----------------------------------------------------------------

# Written by Luke Jacobsen 24MAR2023
# 
# This code is meant to remove "bad" values in the parameters used to calculate heat velocity:
#   Tmax, Alpha, Beta
#
# For example, many sensors have Tmax values less than 0, often -99.00 which seems to be an error value of the sensor.
# Negative time does not make physical sense, thus can be removed. 
#
# Alpha and Beta QA to be explored in this script
# 

# Load Libraries ----------------------------------------------------------

library(tidyverse) #provides a powerful syntax and tools to work with data
library(lubridate) #addition to tidyverse for working with date-times
library(plotly) #makes interactive plots
library(zoo) #used for handling NA values in smoothing function

# Load function file
source("src/sap_flow_QA_functions.R")

# Read in data file(s) ----------------------------------------------------

# read sap flow data that has had known issues fixed
SF_tibble <- read_csv("data/processed/sap_flow_fixed.csv") 

# filter and clean SF data
SF_tibble <- SF_tibble %>% 
  select(timestamp, plot, probe, contains(c("Tmax", "Alpha","Beta"))) %>% 
  # Tmax variables from SF data file are explicitly the downstream Tmax values. 
    # Renaming them since the uTmx values are being added. 
  #rename(dTm_outer = Tmax_Outer, dTm_inner = Tmax_Inner, uTm_inner = uTmax_Inner, uTm_outer = uTmax_Outer) %>% 
  
  #rearrange columns 
  #select(timestamp, plot, probe, dTm_outer, dTm_inner, uTm_outer, uTm_inner, everything()) %>% 
  select(timestamp, plot, probe, everything()) %>% 
  
  mutate(across(where(is.numeric), ~na_if(.,-99))) %>% 
  mutate(month = month(timestamp))

# plotting
# plot <- ggplot(SF_tibble) +
#  geom_line(aes(x = timestamp, y = Alpha_Outer, color = probe)) +
#  facet_wrap(~plot)
# ggplotly(plot, dynamicTicks = TRUE)

# where Alpha_Inner <5, retain value. Else NaN
SF_tibble <- SF_tibble %>% 
  mutate(Alpha_Inner = case_when(abs(Alpha_Inner) < 5 ~ Alpha_Inner))

# where Alpha_Outer <5, retain value. Else NaN
SF_tibble <- SF_tibble %>% 
  mutate(Alpha_Outer = case_when(abs(Alpha_Outer) < 5 ~ Alpha_Outer))

start <- slice_min(SF_tibble, timestamp)$timestamp[[1]]
window <- days(2) #hours

SF_tibble <- SF_tibble %>%
  mutate(IQR_window = floor(interval(start, timestamp)/window))

SF_IQR_Inner <- SF_tibble %>%
  group_by(plot, probe, IQR_window) %>%
  summarise(Q1_Inner = quantile(Alpha_Inner, probs = 0.25, na.rm = TRUE),
            Q3_Inner = quantile(Alpha_Inner, probs = 0.75, na.rm = TRUE),
            outlier_threshold_Inner = 1.5*IQR(Alpha_Inner, na.rm = TRUE))

SF_tibble <- left_join(SF_tibble, SF_IQR_Inner, by = c("plot","probe","IQR_window")) %>%
  mutate(Alpha_Inner_QA = case_when(Alpha_Inner <= (Q3_Inner + 6*outlier_threshold_Inner)
                                 & Alpha_Inner >= (Q1_Inner - outlier_threshold_Inner) ~ Alpha_Inner))

SF_IQR_Outer <- SF_tibble %>%
  group_by(plot, probe, IQR_window) %>%
  summarise(Q1_Outer = quantile(Alpha_Outer, probs = 0.25, na.rm = TRUE),
            Q3_Outer = quantile(Alpha_Outer, probs = 0.75, na.rm = TRUE),
            outlier_threshold_Outer = 1.5*IQR(Alpha_Outer, na.rm = TRUE))

SF_tibble <- left_join(SF_tibble, SF_IQR_Outer, by = c("plot","probe","IQR_window")) %>%
  mutate(Alpha_Outer_QA = case_when(Alpha_Outer <= (Q3_Outer + 6*outlier_threshold_Outer)
                                    & Alpha_Outer >= (Q1_Outer - outlier_threshold_Outer) ~ Alpha_Outer))

#SF_tibble$Alpha_Inner_QA <- na.approx(SF_tibble$Alpha_Inner_QA, rule = 2, maxgap = 100)

# plot <- ggplot(SF_tibble) +
#  geom_line(aes(x = timestamp, y = Alpha_Outer_QA, color = probe)) +
#  facet_wrap(~plot)
# ggplotly(plot, dynamicTicks = TRUE)

# commenting out smoothing functions
# # create list of variables to smooth
# variables_to_smooth <- c("Alpha_Outer","Alpha_Inner")
# 
# # run function to smooth variables using default R smooth function
# SF_tibble_QA <- smooth_variable(SF_tibble_QA, variables_to_smooth)

# Read Out Corrected Files ------------------------------------------------

# write QA'd data to file 
write_csv(SF_tibble, "./data/processed/sap_flow_QA.csv")
