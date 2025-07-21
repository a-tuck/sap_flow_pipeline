# this function replaces -99 values with linearly interpolated values for 
# the given variables
remove_error_values <- function(df, variables){
  
  # makes tibble of plots and probes to loop through
  combinations <- df %>% 
    group_by(plot, probe) %>% 
    distinct(plot, probe)
 
  # for each plot, look at each probe 
  for (i in seq(1:nrow(combinations))) {
    plot_i <- combinations$plot[[i]]
    probe_i <- combinations$probe[[i]]
    
    print(paste("Looking at:",plot_i,"probe",probe_i))
    
    # create tibble of just data for the selected plot and probe
    df_filtered <- filter(df, plot == plot_i, probe == probe_i)
    
    # for the selected plot and probe, clean each variable
    for (var in variables) {
      # create name for cleaned varibale to perserve original variable
      var_name <- paste(var,"_cleaned",sep = "")
      
      df_filtered <- df_filtered %>%
        # this code will return NA whenever -99 is encountered
        mutate("{var_name}" := case_when(.data[[var]] > -99.0 ~ .data[[var]]))
      
      # this code will approximate the values of the NA using linear interpolation
      # of neighboring values,
      # rule = 2 addresses NA values at the start/end of the data set by filling them in 
      # with the last non-NA values
      df_filtered[[var_name]] <- na.approx(df_filtered[[var_name]], rule = 2, na.rm = FALSE)
    }
    # first time through, make new tibble to append next run to
    if (i == 1){
      result_tibble <- df_filtered
    } else{
      # after first time through, add results to tibble
      result_tibble <- bind_rows(result_tibble, df_filtered)
    }
  }
  return (result_tibble)
}

# This function applies the R smoothing function using defualt setting to the 
# supplied variables
smooth_variable <- function(df, variables){
  
  # makes tibble of plots and probes to loop through
  combinations <- df %>% 
    group_by(plot, probe) %>% 
    distinct(plot, probe)
  
  # for each plot, look at each probe 
  for (i in seq(1:nrow(combinations))) {
    plot_i <- combinations$plot[[i]]
    probe_i <- combinations$probe[[i]]
    
    print(paste("Smoothing for plot:",plot_i,"probe",probe_i))
    
    # create tibble of just data for the selected plot and probe
    df_filtered <- filter(df, plot == plot_i, probe == probe_i)
    
    # for the selected plot and probe, smooth each variable
    for (var in variables) {
      var_name <- paste(var,"_smooth",sep = "")
      
      # first, replace NA values with linear approximation (NA values will 
      # break the smoothing function)
      
      df_filtered <- df_filtered %>%
        # this code will return NA whenever -99 is encountered
        mutate("{var_name}" := case_when(.data[[var]] > -99.0 ~ .data[[var]]))
      
      # this code will approximate the values of the NA using linear interpolation
      # of neighboring values,
      # rule = 2 addresses NA values at the start/end of the data set by filling them in 
      # with the last non-NA values
      df_filtered[[var_name]] <- na.approx(df_filtered[[var_name]], rule = 2, na.rm = FALSE)
      
      print(paste("applying smoothing",var))
      
      # this code smooths the data
      df_filtered <- mutate(df_filtered, "{var_name}" := as.numeric(smooth(.data[[var_name]])))
    
    }
    # first time through, make new tibble to append next run to
    if (i == 1){
      result_tibble <- df_filtered
    } else{
      # after first time through, add results to tibble
      result_tibble <- bind_rows(result_tibble, df_filtered)
    }
  }
  return (result_tibble)
}

