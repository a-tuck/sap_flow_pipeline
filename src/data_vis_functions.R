
zero_flow_plot <- function(site, vpd_data, vwc_data){
  vpd_data <- vpd_data %>% 
    filter(plot == site) %>% 
    select(timestamp, vpd_kpa) 
    
  
  vwc_data <- vwc_data %>% 
    filter(plot == site) %>% 
    filter(depth_cm == "vwc_avg_30cm") %>% 
    select(timestamp, vwc)
  
  zero <- zero_flow %>% 
    filter(plot == site) %>% 
    select(timestamp)
  
  zero_data <- full_join(vpd_data, vwc_data, by = "timestamp") %>% 
    pivot_longer(cols = 2:3, names_to = "measure", values_to = "value")
  
  threshold_values <- tibble(measure = as.character(c("vpd_kpa","vwc")),
                             value = as.numeric(c(0.2,0.17)))
  
  plot <- ggplot() +
    geom_line(data = zero_data, aes(x = timestamp, y = value)) +
    geom_vline(data = zero, aes(xintercept = timestamp)) +
    geom_hline(data = threshold_values, aes(yintercept = value)) +
    facet_wrap(~measure, ncol = 1, scales = "free_y") +
    labs(
      title = paste(site, "parameters for zero flow conditions"),
      subtitle = paste("VPD threshold =",threshold_values$value[[1]],"kPa;",
                       "VWC threshold =",threshold_values$value[[2]], "fractional"),
      y = "VPD kPa and VWC (fraction)"
    ) +
    theme_bw()
    
  
  return(plot)
}

timeseries_Vh_plot <- function(site){
  Vh_data <- Vh_tibble %>% 
    select(timestamp, plot, probe, Vhr_HRM_inner, Vhl_HRM_inner) %>% 
    filter(plot == site) %>% 
    pivot_longer(cols = 4:5, names_to = "method", values_to = "Vh")
  zero_data <- Vh_min_events %>% 
    filter(plot == site) %>% 
    select(timestamp, probe)
  
  fig <- ggplot() +
    geom_line(data = Vh_data, aes(x = timestamp, y = Vh, color = method)) +
    geom_vline(data = zero_data, aes(xintercept = timestamp)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~probe, ncol = 1) +
    labs(
      title = paste(site, ": Vhr_HRM_inner;", "zeroing points shown as vertical lines"),
      subtitle = ,
      y = "Vh cm/hr"
    ) +
    theme_bw()
  
  return(fig)
}

timeseries_Vh_corrected <- function(site){
  data <- Vh_tibble %>% 
    select(timestamp, plot, probe, Vhr_HRM_inner, Vhl_HRM_inner) %>%
    pivot_longer(4:5, names_to = "correction", values_to = "Vh") %>% 
    filter(plot == site)
  
  fig <- ggplot(data) +
    geom_line(aes(x = timestamp, y = Vh, color = correction)) +
    facet_wrap(~probe, ncol = 1) +
    labs(
      title = paste(site, ": Vhr_HRM_inner"),
      subtitle = "Zeroing points shown as vertical lines",
      y = "Vh cm/hr"
    ) +
    theme_bw()
}

timeseries_basic <- function(data, variable){
  ggplot(data) + 
    geom_line(aes(x = timestamp, y = {{variable}})) +
    theme_bw()
}

timeseries_vwc <- function(site) {
  data <- soil_vwc_tibble %>% 
    select(timestamp, plot, contains("vwc_avg")) %>% 
    filter(plot == site) %>% 
    pivot_longer(cols = 3:4, names_to = "depth", values_to = "vwc")
  
  ggplot(data) +
    geom_line(aes(x = timestamp, y = vwc, color = depth)) +
    theme_bw()
}


subplot_title <- function(title){
  list(
    x = 0.2,
    y = 1.0,
    text = title,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE
  )
}

MET_per_site <- function(site){
  data <- filter(RH_tibble, plot == site)
  fig_temp <- timeseries_basic(data, air_temp_2m_C) + ggtitle(paste(site,": air temp"))
  fig_rh <- timeseries_basic(data, rh_percent) + ggtitle(paste(site,": RH"))
  fig_vpd <- timeseries_basic(data, vpd_kpa) + ggtitle(paste(site,": VPD"))
  return(list(fig_temp,fig_rh,fig_vpd))
}
