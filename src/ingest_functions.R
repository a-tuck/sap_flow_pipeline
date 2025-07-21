# ==== ITERATIVE FOLDER ACCESS ====

iterative_access <- function(time_periods, file_type, extension_name=".dat") {
  for (period in time_periods) { # iterate over time periods in data folder
    cat("\n")
    print(paste("====== Beginning: ", period, "======"))
    StationData_path = paste("data/Sierra_BCZN/", period, "/StationData", sep="")
    StationData_folder = list.files(path = StationData_path)
    
    for (plot in StationData_folder) { # iterate over PLOT folders in StationData folder
      plot_name = plot
      path_to_plot = paste(StationData_path, "/", plot, sep='')
      plot_folder = list.files(path = path_to_plot)
      cat("\n")
      print(paste("NEW DATA FROM PLOT: ", plot))
      
      for (subfolder in plot_folder) { # iterate over PLOT_DATE folders in PLOT folder
        path_to_plot_and_date = paste(path_to_plot, "/", subfolder, sep='') # set path to PLOT_DATE folder
        plot_and_dates = list.files(path = path_to_plot_and_date)
        print(paste("Entering new subfolder: ", subfolder))
        
        for (file in plot_and_dates) { # iterate over FILES in PLOT folder
          if (grepl(extension_name, file, fixed = TRUE) & grepl(file_type, file, fixed = TRUE)) {
            
            filepath = paste(path_to_plot_and_date, "/", file, sep="") # set path to FILE
            print(paste("Accessing: ", file))
            
            if (file_type == "_Sap_Flow_") {
              current_tibble <- ingest_sap_flow(filepath) %>% # ingest
                mutate(plot = plot_name)
            }
            
            if (file_type == "_EC5_") {
              current_tibble <- ingest_soil_moisture(filepath) %>% # ingest
                mutate(plot = plot_name)
            }
            
            if (file_type == "_SnowDepth_") {
              current_tibble <- ingest_snowdepth(filepath) %>% # ingest
                mutate(plot = plot_name)
            }
            
            if (file_type == "_RH_") {
              current_tibble <- ingest_relative_humidity(filepath) %>% 
                mutate(plot = plot_name)
            }
            
            if (file_type == "_Diagnostic_") {
              current_tibble <- ingest_sap_flow_uTmax(filepath) %>% # ingest
                mutate(plot = plot_name)
            }
            
            if (file_type == "_MET_") {
              current_tibble <- ingest_MET_datalogger(filepath) %>% 
                mutate(plot = "MET")
            }
            
            ongoing_tibble <- rbind(ongoing_tibble, current_tibble)
            }
          }
        }
    }
  }
  cat("\n")
  print("Done :)")
  
  return(ongoing_tibble)
}

# ==== SAP FLOW INGESTION - heat ratio method ====

ingest_sap_flow <- function(dat_name) {
  
  col_names <- read_csv(dat_name, skip = 1, n_max = 1, col_names = FALSE)
  
  ingest_tibble <- read_csv(dat_name, skip = 4, col_names = FALSE)
  
  colnames(ingest_tibble) <- col_names
  
  ingest_tibble <- ingest_tibble %>% 
    pivot_longer(
      cols = -(1:4), 
      names_to = c(".value", "probe"), 
      names_sep = -1) %>% 
    select(!TotalSapFlow_) %>% 
    mutate(across(6:ncol(.), ~ifelse(is.nan(.), NA, .)))
  
  cleaning_column_names <- colnames(ingest_tibble)
  cleaned <- stri_replace_last(cleaning_column_names, regex="_", replacement="")
  colnames(ingest_tibble) <- cleaned
  
  ingest_tibble <- ingest_tibble %>% 
    rename(timestamp = TIMESTAMP, 
           record = RECORD)
  
  return(ingest_tibble)
  
}

# ==== SAP FLOW INGESTION - Tmax method ====

ingest_sap_flow_uTmax <- function(dat_name) {
  
  col_names <- read_csv(dat_name, skip = 1, n_max = 1, col_names = FALSE)
  
  ingest_tibble <- read_csv(dat_name, skip = 4, col_names = FALSE)
  
  colnames(ingest_tibble) <- col_names
  
  ingest_tibble <- ingest_tibble %>% 
    select(TIMESTAMP, Air_TempF_Avg, Depth) %>% 
    pivot_longer(
      cols = contains("_"), 
      names_to = c(".value", "probe"), 
      names_sep = -1) %>% 
    mutate(across(!TIMESTAMP, ~ifelse(is.nan(.), NA, .)))
  
  cleaning_column_names <- colnames(ingest_tibble)
  cleaned <- stri_replace_last(cleaning_column_names, regex="_", replacement="")
  colnames(ingest_tibble) <- cleaned
  
  ingest_tibble <- ingest_tibble %>% 
    rename(timestamp = TIMESTAMP)
  
  return(ingest_tibble)
}


# ==== SNOW DEPTH INGESTION ====
ingest_snowdepth <- function(dat_name) {
  
  col_names <- read_csv(dat_name, skip = 1, n_max = 1, col_names = FALSE)
  
  ingest_tibble <- read_csv(dat_name, skip = 4, col_names = FALSE)
  
  colnames(ingest_tibble) <- col_names
  
  ingest_tibble <- ingest_tibble %>% 
    select(TIMESTAMP, Air_TempF_Avg, Depth)
  
  return(ingest_tibble)
  
}


# ==== OLD FUNCTION for sap flow ingestion, HRM method  ====

# ingest_sap_flow <- function(dat_name, downstream = TRUE) {
#   
#   # ---- Ingest ----
#   
#   ingest_vector <- readLines(dat_name, n=2)
#   header_vector <- str_split(ingest_vector[2], pattern = '\",\"')
#   
#   for (value in header_vector) {
#     column_vector <- value
#   }
#   
#   ingest_tibble <- read.table(dat_name, sep = ",", header=FALSE, na.strings = "NAN", skip = 5, col.names = column_vector)
#   
#   # ---- Pivot and Renaming ----
#   if (downstream == TRUE) { # checking to see if it's downstream / normal
#     ingest_tibble <- ingest_tibble %>% 
#       rename(TIMESTAMP = X.TIMESTAMP, BattVolt = Batt_volt) %>%
#       rename_with(~gsub(".", "", .x, fixed = TRUE)) %>% 
#       pivot_longer (
#         cols = -c(TIMESTAMP, RECORD, BattVolt, PTemp), 
#         names_to = c(".value", "Probe"), 
#         names_sep = -1
#       ) %>% 
#       mutate(across(TIMESTAMP, as.POSIXct))
#   }
#   
#   if (downstream == FALSE) { # trying to get uTmax
#     upstream_vector <- grep("uTmax", column_vector, value = TRUE)
#     upstream_vector <- append(upstream_vector, "X.TIMESTAMP")
#     upstream_vector <- upstream_vector %>% str_replace('\"', '.') 
#     ingest_tibble <- ingest_tibble %>% 
#       select(all_of(upstream_vector)) %>% 
#       rename(TIMESTAMP = X.TIMESTAMP) %>%
#       rename_with(~gsub(".", "", .x, fixed = TRUE)) %>% 
#       pivot_longer (
#         cols = -c(TIMESTAMP), 
#         names_to = c(".value", "Probe"), 
#         names_sep = -1
#       ) %>% 
#       mutate(across(TIMESTAMP, as.POSIXct))
#   }
#     
#     # rename columns by stripping last _ 
#     cleaning_column_names <- colnames(ingest_tibble)
#     cleaned <- stri_replace_last(cleaning_column_names, regex="_", replacement="")
#     colnames(ingest_tibble) <- cleaned
#     
#     if (downstream == TRUE) { 
#       ingest_tibble <- ingest_tibble %>% 
#         rename(., timestamp = TIMESTAMP, record = RECORD, probe = Probe)
#     } else {
#       ingest_tibble <- ingest_tibble %>% 
#         rename(., timestamp = TIMESTAMP, probe = Probe)
#     }
# 
#     
#   return(ingest_tibble)
# }

# ==== SOIL MOISTURE (EC5) INGESTION ====

ingest_soil_moisture <- function(dat_name) {
  
  column_tibble <- read.csv(dat_name, skip = 1, nrows = 1, header=FALSE)
  
  # in the case that the system doesn't record an average measurement
  if (ncol(column_tibble) == 6) {
    column_vector <- c("timestamp", "record_vwc", "VW1_Min",	"VW1_Max", "VW2_Min",	"VW2_Max")
    
    ingest_tibble <- read.table(dat_name, sep = ",", header=FALSE, na.strings = "NAN", skip = 4, col.names = column_vector) %>% 
      mutate(vwc_avg_15cm = (VW1_Min + VW1_Max)/2, vwc_avg_30cm = (VW2_Min + VW2_Max)/2) %>% 
      select("timestamp", "record_vwc", "vwc_avg_15cm", "vwc_avg_30cm")
    
  } else {
    column_vector <- c("timestamp", "record_vwc", "VW1_Min",	"VW1_Max", "vwc_avg_15cm",	"VW2_Min",	"VW2_Max",	"vwc_avg_30cm")
    
    ingest_tibble <- read.table(dat_name, sep = ",", header=FALSE, na.strings = "NAN", skip = 4, col.names = column_vector) %>%
      select("timestamp", "record_vwc", "vwc_avg_15cm", "vwc_avg_30cm")
  }
  
  return(ingest_tibble)
  
}

# ==== RELATIVE HUMIDITY (RH) INGESTION ====

ingest_relative_humidity <- function(dat_name) {
  
  # set variable names
  variable_names <- c("record_rh", "timestamp", "air_temp_2m_C", "rh_percent")
  
  # read excel file, depending on the version of HOBOware, .hobo files are exported with 
  # a different number of datetime columns
  ingest_tibble <- readxl::read_excel(dat_name, col_names = TRUE, skip = 1)
  
  # older version exports a date and time column, but both contain full timestamps
  # checking to see if extra column needs to be dropped
  if (ncol(ingest_tibble) == 9) {
    ingest_tibble <- ingest_tibble %>% 
      select(1:2,4:5)
    
  } else {
    ingest_tibble <- ingest_tibble %>% 
      select(1:4)
  }
  
  # change columns  names
  colnames(ingest_tibble) <- variable_names
  
  
  # calculate sat. vapor pressure (e_sat) and VPD
  ingest_tibble <- ingest_tibble %>% 
    mutate(e_sat_Pa = 611*exp((17.27*air_temp_2m_C)/(air_temp_2m_C +237.3)),
           vpd_kpa = e_sat_Pa*(1- (rh_percent/100))/1000)
  
  return(ingest_tibble)
  
}

# ==== MET STATION DATA LOGGER INGESTION ====

ingest_MET_datalogger <- function(dat_name) {
  
  # set column names and data type
  column_names <- c("timestamp","rh","air_temp_2m_C","air_pressure_kpa","none","wind_speed_ms", "wind_gust_ms","wind_direction_deg","solar_rad_Wm2")
  column_types <- c("date","numeric","numeric","numeric","skip","numeric","numeric","numeric","numeric")
  
  # read file
  ingest_tibble <- readxl::read_excel(dat_name,
                                    col_names = column_names,
                                    col_types = column_types,
                                    skip = 3,
                                    na = "***")
  
  # convert RH to percent and add e_sat and VPD
  ingest_tibble <- ingest_tibble %>% 
    mutate(rh_percent = rh*100,
           e_sat_Pa = 611*exp((17.27*air_temp_2m_C)/(air_temp_2m_C +237.3)),
           vpd_kpa = e_sat_Pa*(1- (rh_percent/100))/1000) %>% 
    select(!rh)
  
  return(ingest_tibble)
}