# Moments function development

# This is a scratch script to test and build functions for calculating diel 
# temporal moments and associated metrics using a 14-day 15-minute resolution 
# synthetic sap velocity dataset for a single tree

# Setup and Synthetic Dataset Generation ----------------------------------

#load required libraries
library(tidyverse)
library(ggthemes)
library(pracma)
library(dplyr)

#point to master function file
source("src/functions.R")

#create a synthetic sap velocity dataset with noise
start_date <- as.POSIXct("2021-08-01 12:45:00")
index <- seq(1,96*14,1)
datetime <- seq(from = start_date, 
                length.out = length(index), by = "15 mins")

set.seed(317)
noise <- rnorm(n = length(index), mean = 0, sd = 0.2)

sap_vel <- ((sin((index*3.75+75)*(pi/180))) + 0.8 + noise)^2

sap_vel <- sap_vel - min(sap_vel) #shift down so min = 0

sap_vel_tibble <- tibble(index,datetime,sap_vel)
rm(start_date, noise, sap_vel, index, datetime)

# visualize the sap velocity time series
sap_vel_tibble %>%
  ggplot(aes(datetime, sap_vel)) +
  geom_line() +
  scale_y_continuous( expand = expansion(mult = c(0, .05))) +
  theme_bw()

# Calculating Diel Temporal Moments ---------------------------------------

# creating tibble of all moments at each time step
cumulated_sap_tibble_all <- sap_vel_tibble %>%
  format_input() %>%
  
  # FIXME add conditionals to specify highest order
  calculate_cumulative_moment_by_day(. , 0)

# creating plot of all zero moment instances
cumulated_sap_tibble_all %>%
  ggplot(aes(x = h_elapsed, y = zeroth, group = day, colour = day)) +
  geom_line() +
  theme_bw()

# creating daily cumulative totals for all measures for all measures
daily_cumulative_all <- calculate_daily_cumulative(cumulated_sap_tibble_all)

# creating plot of cumulative zeroth moment
daily_cumulative_all %>%
  ungroup() %>%
  ggplot(aes(x = day, y = zeroth)) +
  geom_point(aes(color = completeness)) + 
  theme_bw()

# all statistical measures derived from cumulative totals
all_measures <- calculate_all_measures(daily_cumulative_all)

# Plot Statistical Measures --------------------------------------------

# pivot longer 
long_all_measures <- all_measures %>%
  pivot_longer(!c(day, index, entries, completeness), names_to = "measure", values_to = "value")

# plot faceted long measures
long_all_measures %>%
  ungroup() %>% 
  ggplot(aes(x = day, y = value, group = 1)) + # plotting in groups of 1
  geom_line(aes(color = completeness)) + # basic lineplot
  theme_bw() + 
  labs(title = "All Statistical Measures") + 
  facet_wrap(~ measure, scales = "free") #scale freely

ggsave(path = "docs", "Statistical_Measures.pdf")
