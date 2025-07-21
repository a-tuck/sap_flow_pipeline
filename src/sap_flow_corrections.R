# Purpose -----------------------------------------------------------------
# This script has 3 parts:
# 
# 1 - CALCULATE HEAT VELOCITY FROM RAW DATA
# This script calculates heat velocity (Vh) from the Heat Ratio Method (HRM) and Tmax methods. 
# The HRM script uses both the inner and outer thermistor measurements.
# The Tmax portion of the code has been commented out because the HRM was selected.
# The script includes the dual method approach (DMA) using beta to combine the HRM & Tmax methods. 
# The script is setup so Thermal Dispersivity (k) can be varied with empirical data or not. 
#
# 2 - ADJUST FOR PROBE MISALIGNMENT 
# This script can correct for probe misalignment in 3 ways:
#       1. Calculate theoretical distances (x) between the heater probe and temperature probes using Tmax measurements
#       2. Use singular "zero" flow event to find an offset and apply this offset across the entire probe series
#       3. Use multiple "zero" flow events and interpolate the offset between these events.
# The 3rd correction method was selected and the other 2 are commented out. This method
# involves selecting zeroing events based on low VPD values (set a threshhold based on 
# your data) and then finds the nearest lowest Vh to become the zeroing event. Then, 
# we take all the zeroing events and interpolate the offset needed for the Vh values
# between each zeroing point. 
# All of these methods require making a "zero" flow event assumption.
#
# 3 - CALCULATE SAP VELOCITY FROM HEAT VELOCITY
# This calculation is from Barrett et al. (1995) and requires tree dry wood density, 
# specific heat capacity of dry wood, and tree moisture content which can either be 
# found from coring the tree or from literature.


# Load Libraries ----------------------------------------------------------

library(tidyverse) #provides a powerful syntax and tools to work with data
library(lubridate) #addition to tidyverse for working with date-times
library(plotly) #makes interactive plots
library(zoo) # provides functions for ordered indexed objects like regular and irregular time series data
library(dplyr) # provides tools for working with data frames
library(ggplot2)

# Load function file
source("src/sap_flow_corrections_functions.R")

# Select parameters ------------------------------------------------------

run_number <- 13 # for tracking misalignment correction outputs

# thresholds define the zeroing events used for the misalignment correction
vpd_min_threshold <- 0.2 #kPa, set threshold to define "max" RH
# vwc_max_threshold <- 0.16 # volumetric water content as fraction (0-1)
filter_interval_min_vh <- 5 #time (days) on either end of a max RH event to look for a Vh_min,
# the entire interval will be twice the length of the value set here

t_0 <-  3.0 # heat pulse duration in seconds
t <-  60.0 # time between heat pulse and measurement in seconds 
B <-  1 # wounding correction (set to 1 if not accounting for wounding)

# Thermal Dispersivity, k  ------------------------------------------------

# data collected from tree core samples from May 2024
# tree_core_data.csv has 6 columns: plot, probe, wf, wd, length, radius
# k_master <- read_csv("data/tree_core_data.csv")
# 
# k_master <- k_master %>%
#   mutate(
#     # calculate the below new columns for K_master
#     
#     # lengths in mm (radius, length)
#     # weights in g (wf, wd)
#     
#     vf = length * 3.1415926 * (radius)^2 * (10^-3)^3,   # Volume of fresh wood (m3)
#     mc = (wf - wd) / wd,                                # Sapwood moisture content (unitless)
#     ddw = wd / vf * (10^-3),                            # Density dry weight (kg/m3)
#     d = wf / vf * (10^-3),                              # Density of fresh sapwood (kg/m3)
#     fv_fsp = 1 - ddw / 1000 * (1000 / 1530 + 0.26),     # Fiber saturation point
#     K = (0.5985 * (mc - 0.26) * d / 1000                # Thermal conductivity (J/kg/°C)
#          ) + (0.04186 * (21 - 20 * fv_fsp)), 
#     c = (wd * 1200 + 4182 * (wf - wd)) / wf,            # Heat capacity (J/kg/°C)
#     k_empirical = K / (d * c) * 100^2                   # Thermal dispersivity (cm^2/s)
#   )

# alternatively, if no tree core data is available, use nominal thermal diffusivity, k
k_nominal <-  0.0025 # cm^2/s


# Read in data file(s) ----------------------------------------------------

SF_tibble <- read_csv("data/processed/sap_flow_QA.csv") # read sap flow (SF) data

VPD_tibble <- read_csv("data/processed/rh_master.csv")  # read relative humidity (RH) data

# VWC_tibble <- read_csv("data/processed/soil_moisture_master.csv") # read soil moisture data

# CALCULATE HEAT VELOCITY --------------------------------------------------
# Compute Vh without probe correction (Vhr: r = raw)

# set function x parameter to factory Implexx Sap Flow probe x distance
x_d_i <-  0.6 #cm
x_d_o <-  0.6
x_u_i <-  0.6
x_u_o <-  0.6

# NOTE: if below inputs to mutate include "smooth" and "cleaned" QA'd data is being used

# For each observation, compute both HRM and Tmax method with ideal distances
SF_tibble <- SF_tibble %>%
  mutate(
    # Find the corresponding empirical k value
    # k = k_master$k_empirical[match(paste(plot, probe), paste(k_master$plot, k_master$probe))],
    k = k_nominal, # cm^2/s # alternatively, use nominal value
    
    # Apply the HRM function
    Vhr_HRM_inner = na_if(HRM(Alpha_Inner_QA, x_d_i, x_u_i, k), NaN),
    
    Alpha_Both = rowMeans(cbind(Alpha_Inner_QA, Alpha_Outer_QA), na.rm = FALSE),
                           
    # Apply the HRM function using both inner and outer measurements
    Vhr_HRM_both = na_if(HRM(Alpha_Both, x_d_i, x_u_i, k), NaN)
    
    # Apply the Tmax function
    #mutate(Vhr_Tmx_outer = na_if(Tmax(dTm_outer_cleaned, x_d_o), NaN)) %>%
    #mutate(Vhr_Tmx_inner = na_if(Tmax(dTm_inner_cleaned, x_d_i), NaN))
  )

# calculate peclet's number to ensure HRM is applicable
# HRM is accurate if peclet's number < 1 (Forster 2020, Ma et al. 2024)
# NOTE: this calculation may be erroneous; extremely small numbers were calculated (?)
p <- peclet(SF_tibble$Vhr_HRM_both, SF_tibble$k, x_d_o)
p <- p[!is.na(p)]
cat("Max. Peclet Num:", max(p), "\n")

# plot
# plot <- ggplot(SF_tibble) +
#  geom_line(aes(x = timestamp, y = Vhr_HRM_both, color = probe)) +
#  facet_wrap(~plot)
# ggplotly(plot, dynamicTicks = TRUE)

# CORRECT FOR PROBE MISALIGNMENT (3 methods below) --------------------------
# # Correction Method 1: Vhx Tmax method ------------------------------------
# 
# # Find day of highest RH for each plot, make a filtering interval to find Vh min
# VPD_min_plot <- VPD_tibble %>%
#   group_by(plot) %>% 
#   slice_min(vpd_kpa) %>% 
#   distinct(plot, .keep_all = TRUE) %>% 
#   mutate(filter_interval = interval(date(timestamp) - filter_interval_min_vh, date(timestamp) + filter_interval_min_vh))
# 
# # initialize empty table to store Vh_min data in
# SF_min_tibble <- tibble(timestamp = as.POSIXct(character()),
#                         plot = character(),
#                         probe = character())
# 
# # Find the Vh min of this interval and average the Tmax values from an hour before and an hour after this Vh min occurrence.
# # NOTE: using Vhr_HRM_inner as proxy for heat velocity min
# for (i in seq_along(VPD_min_plot$plot)) {
#   plot_ <- VPD_min_plot$plot[[i]]
#   Vh_min_plot <- filter(SF_tibble, plot == plot_, timestamp %within% VPD_min_plot$filter_interval[[i]]) %>% 
#     group_by(probe) %>%
#     slice_min(Vhr_HRM_inner) %>%
#     select(timestamp, plot, probe) %>%
#     distinct(plot, probe, .keep_all = TRUE)
#   SF_min_tibble <- add_row(SF_min_tibble, Vh_min_plot)
# }
# 
# # set averaging interval size
# interval_size <- hours(1) 
# 
# # add averaging interval for each probe at each site
# SF_min_tibble <- SF_min_tibble %>%
#   mutate(interval = interval(timestamp - interval_size, timestamp + interval_size))
# 
# # initialize an empty tibble to store the averaged Tmax values for each probe
# Tm_zero <- tibble(plot = character(), probe = character(), dTm_inner = numeric(), dTm_outer = numeric(), uTm_inner = numeric(), uTm_outer = numeric())
# 
# # run a for loop that takes the Tmax averaging interval for every probe, filters the SF data to this interval averages the Tmax and stores the data
# for (i in seq_along(SF_min_tibble$timestamp)) {
#   plot_ <- SF_min_tibble$plot[[i]]
#   probe_ <- SF_min_tibble$probe[[i]]
#   SF_i <- filter(SF_tibble, plot == plot_, probe == probe_, timestamp %within% SF_min_tibble$interval[[i]]) %>% 
#     summarize(
#       dTm_inner = mean(dTm_inner_cleaned, na.rm = TRUE),
#       dTm_outer = mean(dTm_outer_cleaned, na.rm = TRUE),
#       uTm_inner = mean(uTm_inner_cleaned, na.rm = TRUE),
#       uTm_outer = mean(uTm_outer_cleaned, na.rm = TRUE)) %>%
#      mutate(plot = plot_, probe = probe_) %>%
#      select(plot, probe, everything())
#    Tm_zero <- add_row(Tm_zero, SF_i)
# }
# 
# # calculate probe distances based on the average Tmax values 
# misalignment <- Tm_zero %>% 
#   distinct() %>% 
#   rename( #get column names ready
#     x_d_inner = dTm_inner, 
#     x_d_outer = dTm_outer,
#     x_u_inner = uTm_inner,
#     x_u_outer = uTm_outer) %>% 
#   # pivot longer so mutate can be used to calculate x
#   pivot_longer(cols = 3:6, names_to = "sensor", values_to = "Tmax") %>% 
#   #calculate x
#   mutate(x = na_if(probe_distance(Tmax),NaN),
#          error = x - 0.6)
# 
# # export csv with misalignment data
# write_csv(misalignment, paste("./data/processed/xValues_run",run_number,".csv", sep = ""))
# 
# # get x data in format that can be added to SF data
# x_distance <- misalignment %>%
#   select(plot,probe,sensor,x) %>% 
#   pivot_wider(names_from = sensor, values_from = x)
# 
# # add x data to SF
# SF_tibble <- SF_tibble %>% 
#   left_join(x_distance, by = c("plot","probe"))
# 
# # Calculate corrected Vh: Vhx (x = corrected for x)
# SF_tibble <- SF_tibble %>% 
#   mutate(Vhx_HRM_outer = na_if(HRM(Alpha_Outer_smooth,x_d_outer,x_u_outer),NaN)) %>% 
#   mutate(Vhx_HRM_inner = na_if(HRM(Alpha_Inner_smooth, x_d_inner,x_u_inner), NaN)) %>% 
#   mutate(Vhx_Tmx_outer = na_if(Tmax(dTm_outer_cleaned,x_d_outer), NaN)) %>% 
#   mutate(Vhx_Tmx_inner = na_if(Tmax(dTm_inner_cleaned,x_d_inner), NaN))
# 

# # Implement Dual Method Approach 
# 
# # using beta to switch between methods
# SF_tibble <- SF_tibble %>% 
#   mutate(Vhr_DMA_outer = case_when(Beta_Outer_cleaned <= 1 ~ Vhr_HRM_outer,
#                                    Beta_Outer_cleaned > 1 ~ Vhr_Tmx_outer)) %>% 
#   mutate(Vhr_DMA_inner = case_when(Beta_Inner_cleaned <= 1 ~ Vhr_HRM_inner,
#                                    Beta_Inner_cleaned > 1 ~ Vhr_Tmx_inner)) %>% 
#   mutate(Vhx_DMA_outer = case_when(Beta_Outer_cleaned <= 1 ~ Vhx_HRM_outer,
#                                    Beta_Outer_cleaned > 1 ~ Vhx_Tmx_outer)) %>%
#   mutate(Vhx_DMA_inner = case_when(Beta_Inner_cleaned <= 1 ~ Vhx_HRM_inner,
#                                    Beta_Inner_cleaned > 1 ~ Vhx_Tmx_inner)) 

# # Correction method 2: Vhz (z = zero offset) ------------------------------
# 
# # create offset based on min Vhr_HRM during lowest VPD event
# zero_flow <- SF_tibble %>%
#   # select raw HRM (Tmax does not always exist at low flows in the data)
#   select(timestamp, plot, probe, contains(c("Vhr_HRM"))) %>%
#   left_join(select(VPD_min_plot,plot, filter_interval), by = c("plot")) %>% 
#   filter(timestamp %within% filter_interval) %>% 
#   relocate(filter_interval, .after = timestamp) %>% 
#   pivot_longer(cols = 5:6, 
#                names_to = c("corrected","method"),
#                names_sep = -9,
#                values_to = "Vh") %>% 
#   group_by(plot, probe, method) %>%
#   slice_min(Vh) %>%
#   distinct(probe, method, .keep_all = TRUE)
# 
# # add offset corrected Vh to SF tibble
# offset <- zero_flow %>%
#   select(plot, probe, method, Vh) %>%
#   rename(offset = Vh) %>%
#   # pivot wider so the offsets can be added as columns to the SF tibble
#   pivot_wider(names_from = method, values_from = offset) %>%
#   rename(offset_HRM_inner = HRM_inner, offset_HRM_outer = HRM_outer)
# 
# SF_tibble <- SF_tibble %>%
#   left_join(offset, by = c("plot","probe")) %>%
#   # calculate offsets
#   mutate(Vhz_HRM_inner = Vhr_HRM_inner - offset_HRM_inner,
#          Vhz_HRM_outer = Vhr_HRM_outer - offset_HRM_outer)
# 

# Correction method 3: Vhl (l = linear interpolation) ---------------------

# Look at VPD for each site
# VPD_plot <- ggplot(VPD_tibble) +
#   geom_line(aes(x = timestamp, y = vpd_kpa)) +
#   facet_wrap(~plot, ncol = 1)
# ggplotly(VPD_plot)

# # look at VWC for each site
# VWC_tibble <- VWC_tibble %>% 
#   # use 30 cm probe data as it is more responsive
#   filter(depth_cm == "vwc_avg_30cm")

# Find zero flow events
zero_flow_events <- VPD_tibble%>% #left_join(VWC_tibble, VPD_tibble, by = c("plot","timestamp")) %>% 
  select(timestamp, plot, vpd_kpa) %>% 
  # filter(vwc >= vwc_max_threshold) %>% 
  filter(vpd_kpa <= vpd_min_threshold) %>% 
  group_by(plot, month = month(timestamp)) %>% 
  slice_min(vpd_kpa) %>% 
  # slice_min(vwc) %>% 
  distinct(month, plot, .keep_all = TRUE) %>% 
  mutate(filter_interval = interval(date(timestamp) - filter_interval_min_vh, date(timestamp) + filter_interval_min_vh))

# Use dates of RH monthly max to find corresponding Vh min (V) and time (t)
Vh_min_events <- tibble(timestamp = as.POSIXct(character()),
                        plot = character(),
                        probe = character(),
                        method = character(),
                        Vh = numeric())

for (i in seq_along(zero_flow_events$timestamp)) {
  plot_ <- zero_flow_events$plot[[i]]
  Vh_min_i <- filter(SF_tibble, plot == plot_, timestamp %within% zero_flow_events$filter_interval[[i]])
  Vh_min_i <- Vh_min_i %>%
    select(timestamp, plot, probe, contains(c("Vhr_HRM"))) %>%
    Pivot_Vh() %>%
    group_by(plot, probe, method) %>%
    slice_min(Vh) %>%
    select(timestamp, plot, probe, method, Vh) %>% 
    distinct(plot, probe, .keep_all = TRUE)
  Vh_min_events <- add_row(Vh_min_events, Vh_min_i)
}

initial_time <- SF_tibble$timestamp[1] 

Vh_min_events <- Vh_min_events %>% 
  distinct() %>% 
  drop_na() %>% 
  mutate(t = abs(int_length(interval(initial_time, timestamp))))

# Calculate m (slope) and b (intercept) for each line (1 line per 2 vpd low events)

# initialize empty tibble
# lines indicate new zeroing baseline
lines <- tibble(plot = character(), 
                probe = character(),
                method = character(),
                start = as_datetime(character()),
                end = as_datetime(character()),
                line = numeric(),
                m = numeric(),
                b = numeric())

plots <- distinct(Vh_min_events, plot)
positions <- distinct(Vh_min_events, method)

for(plt in plots$plot){
  df_plot <- filter(Vh_min_events, plot == plt)
  probes <- distinct(df_plot,probe)
  print(paste("plot:",plt))
  for(prb in probes$probe){
    print(paste("  probe:",prb))
    df_probe <- filter(df_plot, probe == prb)
    for(pos in positions$method){
      print(paste("    position:",pos))
      df_pos <- df_probe %>% 
        filter(method == pos) %>% 
        arrange(timestamp)
      df_length <- nrow(df_pos)
      print(paste("      length:",df_length))
      index <- seq(1,df_length - 1)
      for(i in index){
        # this if statement addresses single point VPD min resulting in a horizontal line
        if((df_length - 1) != 0){
          V_1 <- df_pos$Vh[[i]]
          V_2 <- df_pos$Vh[[i+1]]
          t_1 <- df_pos$t[[i]]
          t_2 <- df_pos$t[[i+1]]
          start_ <- df_pos$timestamp[[i]]
          end_ <- df_pos$timestamp[[i+1]]
          m_ <- (V_2 - V_1)/(t_2 - t_1)
          b_ <- V_1 - m_*t_1
        } else if(i == 1){
          start_ <- NA
          end_ <-  NA
          m_ = 0
          b_ = df_pos$Vh[[i]]
        }
        
        if (i > 0){
          print(paste("      line",i,"; m =",m_,"; b =",b_))
          
          lines <- add_row(lines, tibble_row(
            plot = df_pos$plot[[1]],
            probe = df_pos$probe[[1]],
            method = df_pos$method[[1]],
            start = start_,
            end = end_,
            line = i,
            m = m_,
            b = b_
          ))
          print("      >line added to data")
        }
      }
    }
  }
}

lines <- drop_na(lines)

#use the time of the first data point as the datum for time
start_date <- SF_tibble$timestamp[[1]]

# Create empty tibble
SF_pivot <- SF_tibble %>% 
  select(timestamp, plot, probe, contains(c("Vhr_HRM"))) %>% 
  Pivot_Vh() %>% 
  mutate(t = abs(int_length(interval(timestamp, start_date))))
SF_interpolated <- SF_pivot %>% 
  filter(FALSE) %>% 
  add_column(Vhl = numeric())

# calculate new Vh (Vhl) using lines for linear interpolation
for(plt in plots$plot){
  df_plot <- filter(SF_pivot, plot == plt)
  probes <- distinct(df_plot, probe)
  print(paste("plot:",plt))
  for(prb in probes$probe) {
    df_prob <- filter(df_plot, probe == prb)
    print(paste("  probe:",prb))
    for(pos in positions$method){
      line_filtered <- filter(lines, plot == plt, probe == prb, method == pos)
      df_pos <- filter(df_prob, method == pos)
      print(paste("    position:",pos))
      for (i in seq(1,nrow(line_filtered))){
        if(nrow(line_filtered) == 1){
          if (is.na(line_filtered$start[[i]])) {print("        point case")} 
          else {print("        single line case")}
          print(paste("        applying line",i,":",df_pos$timestamp[[1]],"-",df_pos$timestamp[[nrow(df_pos)]]))
          df_interpo <- df_pos %>% 
            mutate(Vhl = Vh - (line_filtered$m[[i]]*t + line_filtered$b[[i]]))
          SF_interpolated <- add_row(SF_interpolated, df_interpo)
          print("        data appended")
          }
        else{
          print("        entering multi line case")
          if(i == 1){
            print("        first line")
            
            print(paste("        shifting data from:",df_pos$timestamp[[1]],"-",line_filtered$start[[i]]))
            df_start <- df_pos %>% 
              filter(timestamp <= line_filtered$start[[i]])
            Vh_shift <- filter(df_start, timestamp == line_filtered$start[[i]])$Vh[[1]]
            print(paste("        shifting by:", Vh_shift))
            df_interpo <- df_start %>% 
              mutate(Vhl = Vh - Vh_shift)
            SF_interpolated <- add_row(SF_interpolated, df_interpo)
            
            print(paste("        applying line",i,":",line_filtered$start[[i]],"-",line_filtered$end[[i]]))
            df_interpo <- df_pos %>% 
              filter(timestamp > line_filtered$start[[i]] & timestamp <= line_filtered$end[[i]]) %>% 
              mutate(Vhl = Vh - (line_filtered$m[[i]]*t + line_filtered$b[[i]]))
            SF_interpolated <- add_row(SF_interpolated, df_interpo)
            print("        data appended")
          }
          else if(i == nrow(line_filtered)) {
            print("        last line")
            #print(paste("        applying line",i,":",line_filtered$start[[i]],"-",line_filtered$end[[i]]))
            print(paste("        applying line",i,":",line_filtered$start[[i]],"-",df_pos$timestamp[[nrow(df_pos)]]))
            df_interpo <- df_pos %>% 
              filter(timestamp > line_filtered$start[[i]] & timestamp <= df_pos$timestamp[[nrow(df_pos)]]) %>% 
              mutate(Vhl = Vh - (line_filtered$m[[i]]*t + line_filtered$b[[i]]))
            SF_interpolated <- add_row(SF_interpolated, df_interpo)
            
            # print(paste("        shifting data from:",line_filtered$end[[i]],"-",df_pos$timestamp[[nrow(df_pos)]]))
            # df_end <- df_pos %>% 
            #   filter(timestamp > line_filtered$end[[i]])
            # Vh_shift <- filter(df_pos, timestamp == line_filtered$end[[i]])$Vh[[1]]
            # print(paste("        shifting by:", Vh_shift))
            # df_interpo <- df_end %>% 
            #   mutate(Vhl = Vh - Vh_shift)
            # SF_interpolated <- add_row(SF_interpolated, df_interpo)
            
            print("        data appended")
          }
          else{
            print(paste("        line",i))
            print(paste("        applying line",i,":",line_filtered$start[[i]],"-",line_filtered$end[[i]]))
            df_interpo <- df_pos %>% 
              filter(timestamp > line_filtered$start[[i]] & timestamp <= line_filtered$end[[i]]) %>% 
              mutate(Vhl = Vh - (line_filtered$m[[i]]*t + line_filtered$b[[i]]))
            SF_interpolated <- add_row(SF_interpolated, df_interpo)
            print("        data appended")
          }
        }
      }
    }
  }
}

# double-check that length of interpolated data matches original data before joining 
print(paste("interpolated data length = original data length:",nrow(SF_interpolated) == nrow(SF_pivot)))

test <- SF_interpolated %>% 
  group_by(plot, probe,timestamp) %>% 
  summarise(n = n())

#pivot interpolated values back to be joined with SF_tibble
SF_interpolated <- SF_interpolated %>% 
  select(!c("corrected","Vh","t")) %>% 
  pivot_wider(names_from = method, values_from = Vhl) %>% 
  rename(Vhl_HRM_both = "_HRM_both")

SF_tibble <- left_join(SF_tibble, SF_interpolated, by = c("timestamp", "plot", "probe"))

# CALCULATE SAP VELOCITY FROM HEAT VELOCITY --------------------=-----------

# load tree data
tree_data <- read_csv("data/tree_core_data.csv")

# run heat velocity to sap velocity function
SF_tibble$Vs_HRM_both <- heat_to_sap(tree_data, SF_tibble$Vhl)

# Record run metadata and save ----------------------------------------------

metadata <- c("Metadata for misalignment correction",
              paste("Run #",run_number),
              paste("k =", k),
              paste("RH max threshold:",vpd_min_threshold),
              paste("Vh min filter interval:",filter_interval_min_vh),
              "",
              "Vhr_HRM_both = Raw heat velocity; all x distances assumed to be 0.6 cm",
              "Vhl_HRM_both = heat velocity corrected for misalignment using linear interpolation method",
              "Vs_HRM_both = sap velocity",
              "method: HRM",
              "location: both inner & outer thermisters used")

# write csv file with results
write_lines(metadata, paste("./data/processed/Metadata_run",run_number,".txt", sep = ""))

write_csv(Vh_min_events, paste("./data/processed/Vh_min_events.csv"))

write_csv(zero_flow_events, paste("./data/processed/zero_flow_events.csv"))

write_csv(SF_tibble, paste("./data/processed/sap_flow_master.csv",run_number,".csv", sep = ""))

# write_csv(k_master, paste("./data/processed/k_values.csv"))

