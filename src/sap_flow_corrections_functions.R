# this functions calculates theoretical probe distances based on Tm data during a 
# chosen "zero" flow event. Equation from Implexx documentation
probe_distance <- function(t_m){
  
  # t_m ------------- time to max temperature rise following heat pulse
  # a --------------- 
  # k --------------- thermal diffusivity, cm^2/s
  # t_0 ------------- heat pulse duration in seconds
  # x --------------- theoretical probe distance
  
  a <- k*4*t_m*log((t_m)/(t_m-t_0))*(t_m-t_0)
  x <- sqrt(a/t_0)
  return(x)
}



# this function calculates Vh using the Heat Ratio Method
HRM <- function(alpha,xd,xu,k){
  
  # alpha ----------- raw data output from Implexx Sap Flow Sensor Gen 2
  # xd -------------- probe distance (set as 0.6, per manufacturer), cm
  # xu -------------- probe distance (set as 0.6, per manufacturer), cm
  # k --------------- thermal diffusivity, cm^2/s
  # t --------------- time between heat pulse and measurement, seconds 
  # t_0 ------------- heat pulse duration, seconds
  # Vh -------------- heat velocity, cm/hr
  # B --------------- wounding correction coefficient (set as 1)
  
  Vh <- (2*k*alpha/(xd+xu)) + ((xd-xu)/(2*(t-(t_0/2)))) # cm/s
  Vh <- Vh*3600*B # with time conversion
  return(Vh)
}



# this function calculates Vh using the Tmax method
Tmax <- function(tm,xd){
  
  # tm -------------- time to max temperature rise following heat pulse, seconds
  # xd -------------- probe distance (set as 0.6, per manufacturer), cm
  # k --------------- thermal diffusivity, cm^2/s
  # t --------------- time between heat pulse and measurement, seconds 
  # t_0 ------------- heat pulse duration, seconds
  # Vh -------------- heat velocity, cm/hr
  
  Vh <- sqrt(((4*k/t_0)*log(1-(t_0/tm)))+((xd^2)/(tm*(tm-t_0))))
  Vh <- Vh*3600*B # with time conversion
  return(Vh)
}



# this function pivots heat velocity data
Pivot_Vh <- function(df){
  df <- select(df,timestamp, plot, probe, matches("Vh") & matches("both"))
  df <- df %>% 
    pivot_longer(
      cols = 4:ncol(df),
      names_to = c("corrected","method"),
      names_sep = -9,
      values_to = "Vh")
}


# this function calculates the peclet number
# equation from Forster (2020). Also in Ma et al. (2024)
peclet <- function(Vh, xd, k){
  
  # Vh ------------ heat velocity (convection) (cm/hr)
  # xd ------------ probe distance (set as 0.6, per manufacturer) (cm)
  # k ------------- axial thermal diffusivity of the sapwood (conduction) (cm2/s)
  
  k <- k * 3600 # conversion to cm2/hr
  peclet_num <- Vh * xd / k
  
  peclet_num <- abs(peclet_num) # take absolute value
    
  return(peclet_num)
}


# this function calculates sap velocity from heat velocity
heat_to_sap <- function(tree_data, Vh) {
  # Equation from Barrett et al. (1995)
  
  density_wood <- tree_data$ddw[1] # (kg/m3) Dry wood density
  density_sap <- 1000              # (kg/m3) Estimated to be the density of water
  c_wood <- tree_data$c[1]         # (J/kg/C) Specific heat capacity of dry wood
  c_sap <- 4182                    # (J/kg/C) Specific heat capacity of sap
  mc <- tree_data$mc[1]            # (unitless) Moisture content
  
  # Copy the Vh dataframe
  Vs <- Vh
  
  # Calculate Vs
  Vs$Vs <- Vh$Vhr_HRM_corrected * density_wood * (c_wood + (mc * c_sap)) / (density_sap * c_sap)
  
  # Optionally print adjustment factor
  # factor <- density_wood * (c_wood + (mc * c_sap)) / (density_sap * c_sap)
  # print(paste("Adjustment factor was:", factor))
  
  return(Vs)
}
