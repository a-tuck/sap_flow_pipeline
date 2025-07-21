# Created on 17MAR2023 by Luke Jacobsen
# Purpose: take preliminary data from master ingest script and correct known issues in the data
# Known data issues are recorded in the Known_Issue_Repository doc in the Sierra_BCZN folder
# 
# Data input: preliminary master ingest file
# Data transformation: correct known issues on a case-by-case basis
# Data output: master file 

# load libraries
library(tidyverse)
library(lubridate)
library(plotly)

# Read in preliminary master ingest file from data folder
prelim_soil_moist <- read_csv("data/processed/soil_moisture_prelim.csv") %>% 
  distinct(timestamp, plot, .keep_all = TRUE) # the "../" throws an error that the file does not exists in the current working directory on my Mac (LJ)
prelim_SF <- read_csv("data/processed/sap_flow_prelim.csv")
prelim_snowdepth <- read_csv("data/processed/snowdepth_prelim.csv")
prelim_rh <- read_csv("data/processed/rh_prelim.csv")
prelim_MET <- read_csv("data/processed/MET_prelim.csv")

# Known Issues # 2 --------------------------------------------------------

# Description: P304_A 15 cm soil moisture probe continually reading negative VWC. Replaced with new probe 08OCT2022 that started reading values consistent with other shallow probes
# Fix: drop 15 cm soil moisture data prior to 08OCT2022

# check issue
soil_moist_A <- filter(prelim_soil_moist, plot == "P304_A")

# p <- ggplot(soil_moist_A) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y = vwc_avg_30cm), color = "green")
# ggplotly(p)

# Notes: 30 cm probe was mis-wired from install (05MAY2022) until 24JUN2022 1030. Issue has been noted in doc
# Question: should 30 cm data (from 24JUNE22 onwards) be corrected to match the jump from probe re-installation in Oct?

# change all values before this date to NA
filter_date_15cm <- ymd_hm("2022-10-08 10:45")

soil_moist_A_correct <- soil_moist_A %>% 
  mutate(vwc_avg_15cm_correct = case_when(timestamp >= filter_date_15cm ~ vwc_avg_15cm))

# add fixed data to soil moisture data

# remove old 15 cm column and move corrected 15 cm col to the correct location
soil_moist_A_correct <- soil_moist_A_correct %>% 
  select(!vwc_avg_15cm) %>% 
  relocate(vwc_avg_15cm_correct, .after = record_vwc) %>% 
  rename(vwc_avg_15cm = vwc_avg_15cm_correct)

# remove P304_A observation from soil moisture data
prelim_soil_moist <- prelim_soil_moist %>% 
  filter(plot != "P304_A") %>% 
  # add corrected P304_A observations  
  bind_rows(soil_moist_A_correct) %>% 
  arrange(timestamp, plot)

# check that issue is fixed
# p <- ggplot(soil_moist_A_correct) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y = vwc_avg_30cm), color = "green")
# ggplotly(p)

# Known Issue #3 ----------------------------------------------------------

# Description: P304_F soil moisture probe wire ports accidentally swapped from 2022-07-26 09:00 until 2022-10-08 11:15
# Fix: swap 15 and 30 cm probe data in the time interval

# look at data
soil_moist_F <- filter(prelim_soil_moist, plot == "P304_F")
# p <- ggplot(soil_moist_F) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y= vwc_avg_30cm), color = "blue")
# ggplotly(p)
  
# extract data within interval
filter_start <- ymd_hm("2022-07-26 09:15") #using inclusive function to filter timestamp, so times must be addjusted +/- 15 min
filter_end <- ymd_hm("2022-10-08 11:15")

soil_moist_F_int <- soil_moist_F %>% 
  filter(between(timestamp, filter_start, filter_end)) %>% 
  # rename cols for swap
  rename(vwc_deep = vwc_avg_15cm, vwc_shallow = vwc_avg_30cm) %>% 
  # swap locations
  select(1:3,vwc_shallow,vwc_deep, plot) %>% 
  # rename back to proper names
  rename(vwc_avg_15cm = vwc_shallow, vwc_avg_30cm = vwc_deep)

# check that extraction was successful
# p <- ggplot(soil_moist_F_int) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y= vwc_avg_30cm), color = "blue")
# ggplotly(p)

# remove P304_F data within interval from soil moist data and replace with corrected data
prelim_soil_moist <- prelim_soil_moist %>% 
  filter(!(plot == "P304_F" & between(timestamp, filter_start, filter_end))) %>% 
  bind_rows(soil_moist_F_int)

# check that correction was successful
# prelim_soil_moist <- filter(prelim_soil_moist, plot == "P304_F")
# p <- ggplot(prelim_soil_moist_test) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y= vwc_avg_30cm), color = "blue")
# ggplotly(p)

# Known Issue #5 ----------------------------------------------------------

# Description: At P304_A, 30 cm probe mis-wired during install (2022-05-05); fixed 2022-06-24 10:30
# Fix: drop data prior to 2022-06-24

# look at data
# soil_moist_A <- filter(prelim_soil_moist, plot == "P304_A")
# p <- ggplot(soil_moist_A) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y = vwc_avg_30cm), color = "green")
# ggplotly(p)

# drop 30 cm soil moisture data prior to 2022-06-24
filter_date <- ymd_hm("2022-06-24 10:15")

prelim_soil_moist <- prelim_soil_moist %>% 
  filter(!(plot == "P304_A" & timestamp <= filter_date))

# check that correction was successful
# soil_moist_A <- filter(prelim_soil_moist, plot == "P304_A")
# p <- ggplot(soil_moist_A) +
#   geom_line(aes(x = timestamp, y = vwc_avg_15cm), color = "red") +
#   geom_line(aes(x = timestamp, y = vwc_avg_30cm), color = "green")
# ggplotly(p)

# Probes moved around at P304_E -------------------------------------------

# Description: probes a and e moved around at P304_E during July visit
# Fix: drop data prior to dates in July via inspection of beta and alpha data

# set dates to drop observations
filter_date_E_a <- ymd_hm("2022-07-23 14:45")
filter_date_E_e <- ymd_hm("2022-07-28 11:30")

# drop observations
prelim_SF <- prelim_SF %>% 
  filter(!(timestamp < filter_date_E_a & plot == "P304_E" & probe == 'a')) %>% 
  filter(!(timestamp < filter_date_E_e & plot == "P304_E" & probe == 'e'))

# Site E SF probes chewed through -----------------------------------------

SF_e <- prelim_SF %>% 
  filter(plot == "P304_E")

plot <- ggplot(SF_e) +
  geom_point(aes(x = timestamp, y = Alpha_Inner)) +
  facet_wrap(~probe, scales = "free_y", ncol = 1)
ggplotly(plot)


start_a <- ymd_hm("2023-02-22 00:45")
end_a <- ymd_hm("2023-05-27 18:45")

start_c <- ymd_hm("2023-04-28 11:15")
end_c <- ymd_hm("2023-05-27 15:00")

start_d <- ymd_hm("2023-05-27 14:45")
end_d <- ymd_hm("2023-05-27 17:30")

e_1 <- ymd_hm("2023-05-08 22:30")
e_2 <- ymd_hm("2023-05-09 23:00")

prelim_SF <- prelim_SF %>% 
  mutate(Alpha_Inner = case_when((plot == "P304_E" & probe == "a" & between(timestamp, start_a, end_a)) ~ NA,
                                 (plot == "P304_E" & probe == "c" & between(timestamp, start_c, end_c)) ~ NA,
                                 (plot == "P304_E" & probe == "d" & between(timestamp, start_d, end_d)) ~ NA,
                                 (plot == "P304_E" & probe == "e" & (timestamp %in% c(e_1,e_2))) ~ NA,
                                 .default = as.numeric(Alpha_Inner)
                                 )
         )

# Site B: probe b pulled out ----------------------------------------------

prelim_SF <- prelim_SF %>% 
  mutate(Alpha_Inner = case_when(!(plot == "P304_B" 
                                   & probe == "b" 
                                   & between(timestamp, 
                                             ymd_hm("2022-12-08 07:45"), 
                                             ymd_hm("2023-06-23 08:45")))
                                 ~ Alpha_Inner))  


# Site D filling gap in MET ----------------------------------------------

# large amount of missing MET data between 2023-03-31 05:15 to 2023-06-25 08:30
# and after 2023-12-18 20:15. Use P304_D data for P304_A

prelim_rh <- prelim_rh %>% 
  distinct(timestamp,plot, .keep_all = TRUE)

rh_D <- prelim_rh %>%  #data we need to grab for P304_A
  filter(plot == "P304_D") %>% 
  filter((timestamp > "2023-03-31 05:15" & timestamp < "2023-06-25 08:30") | (timestamp > "2023-12-18 20:15")) %>% 
  #filter(timestamp > "2023-03-31 05:15") %>% 
  mutate(plot = "P304_A")


prelim_rh <- prelim_rh %>% 
  #filter(!(plot == "P304_A" & timestamp > ymd_hm("2023-03-31 05:15"))) %>% 
  filter(!(plot == "P304_A" & ((timestamp > ymd_hm("2023-03-31 05:15") & timestamp < ymd_hm("2023-06-25 08:30")) | timestamp > ymd_hm("2023-12-18 20:15")))) %>% 
  bind_rows(rh_D) #add in data from P304_D

prelim_rh <- prelim_rh %>% 
  filter(!(plot == "P304_A" & ((timestamp > ymd_hm("2023-03-31 05:15") & timestamp < ymd_hm("2023-06-25 08:30")) | timestamp > ymd_hm("2023-12-18 20:15")))) %>% 
  bind_rows(rh_D) #add in data from P304_D

# gap-filling soil moisture ------------------------------------------------

# add missing observations in VWC data, values will be filled with NA
timestamp_start <- slice_min(prelim_soil_moist, timestamp)$timestamp[[1]]
timestamp_end <- slice_max(prelim_soil_moist, timestamp)$timestamp[[1]]

timestamp_tibble_temp <- tibble(
  timestamp = seq(timestamp_start, timestamp_end, by = "15 min"),
  # ref variable will be used as a reference during the full join
  ref = "ref") %>% 
  mutate(plot = "P304_A")

timestamp_tibble <- timestamp_tibble_temp

timestamp_tibble <- timestamp_tibble %>% 
  bind_rows(mutate(timestamp_tibble_temp, plot = "P304_B")) %>% 
  bind_rows(mutate(timestamp_tibble_temp, plot = "P304_C")) %>% 
  bind_rows(mutate(timestamp_tibble_temp, plot = "P304_D")) %>% 
  bind_rows(mutate(timestamp_tibble_temp, plot = "P304_E")) %>% 
  bind_rows(mutate(timestamp_tibble_temp, plot = "P304_F"))

prelim_soil_moist <- timestamp_tibble %>% 
  left_join(prelim_soil_moist, by = c("plot","timestamp"))

# duplicate VWC observations
prelim_soil_moist <- prelim_soil_moist %>% 
  select(timestamp, plot, contains("avg")) %>% 
  pivot_longer(cols = 3:4, values_to = "vwc", names_to = "depth_cm")

# Site D Soil moisture issue -----------------------------------------------

# Site D station fell over pulling soil moisture probes out. Remove data
# from 2023-06-12 11:45 to 2023-06-22 09:15
filter_start <- ymd_hm("2023-06-12 11:45")
filter_end <- ymd_hm("2023-06-22 09:15")

#make the column vwc = vwc where case is false, else vwc=NaN
prelim_soil_moist <- prelim_soil_moist %>% 
  mutate(vwc = case_when(!(plot == "P304_D" & between(timestamp, filter_start, filter_end)) ~ vwc))

# Site C ------------------------------------------------------------------

# remove data from Site C from before 6/26/22
prelim_SF <- prelim_SF %>% 
  filter(!(plot == "P304_C" & timestamp < ymd("2022-06-26")))

# Read Out Corrected Files ------------------------------------------------

write_csv(prelim_soil_moist, "data/processed/soil_moisture_master.csv")

write_csv(prelim_SF, "data/processed/sap_flow_fixed.csv")

write_csv(prelim_rh, "data/processed/rh_master.csv")

write_csv(prelim_MET, "data/processed/MET_master.csv")

write_csv(prelim_snowdepth, "data/processed/snowdepth_master.csv")