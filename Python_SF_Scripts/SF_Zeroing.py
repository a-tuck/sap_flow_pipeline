"""
Created on Fri Nov 3 09:20:00 2023

Code written by Annie Tucker
Purpose: Correct sap flow Vh for probe misalignment via zeroing

Inputs: Vh, MET, rh

This code corrects for misalignment based on Zeppel et al. with adjustments:
    - select zeroing events based on (3) criteria
        - windspeed < 0.3m/s,
        - vpd < 0.2 kpa
        - nighttime
    - find minimum Vh values within 5-day window
        - via 'min_vh_locator' function
    - correct Vh values based on rolling linear correction
        - via 'apply_linear_correction' function

Output: Vh_corrected file



"""
# %% Prep
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import pickle
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")
import winsound

os.chdir("C:/Users/m10921492/OneDrive - Colorado School of Mines/Sierra_BCZN_Annie/Sierra_python")

from SF_Filtering import filter_data, filter_data_growing_season, additional_filter_for_inclusion_of_outer
from SF_Zeroing_Functions import apply_linear_correction, min_vh_locator

# import data
# Vh_import = pd.read_csv("data/Vh_corrected_run13_k0-0025_OriginalZeroing.csv",
                        # parse_dates=["timestamp"])
# Vh_raw = Vh_import[["timestamp", "plot", "probe", "Vhr_HRM_inner"]]
# May2024 new SF data

# Vh_import_May24 = pd.read_csv("data/Vh_corrected_withnewKvalues3_bothInnerOuter13.csv",
#                         parse_dates=["timestamp"]) # from R code
Vh_import_May24 = pd.read_csv("data/Vh_corrected_withnewKvalues313.csv",
                        parse_dates=["timestamp"]) # from R code
var = 'Vhr_HRM_inner' # or both!
Vh_raw_May24 = Vh_import_May24[["timestamp", "plot", "probe", var]]#, "Vhr_HRM_both"]]
MET = pd.read_csv("data/MET_master_update.csv", parse_dates=["timestamp"])
rh = pd.read_csv("data/rh_master_update.csv", parse_dates=["timestamp"])
# vwc = pd.read_csv("processed/vwc_processed.csv", parse_dates=["timestamp"])
# vwc = pd.read_csv("processed/vwc_processed_may24update.csv", parse_dates=["timestamp"])
# rain_hr = pd.read_csv("processed/rain_hr.csv", parse_dates=["timestamp"])
# radiation_hr = pd.read_csv("processed/radiation_hr.csv", parse_dates=["timestamp"])

# var
plots = ['P304_A', 'P304_B', 'P304_C', 'P304_D', 'P304_E', 'P304_F']
probes = ['a', 'b', 'c', 'd']
stations = plots
sensors = probes

# with open('data/Vh_corr_60hrwind_2vpd_night_filterfirst.pkl', 'rb') as f:
#     Vh_corr = pickle.load(f)
    
# with open('data/Zeroing_2vpd_60hr_night_May24_update2_bothinnerouter.pkl', 'rb') as f:
#     zeroing_events = pickle.load(f)

Vh_raw_filtered = filter_data(Vh_raw_May24, var)
Vh_raw_filtered = additional_filter_for_inclusion_of_outer(Vh_raw_filtered, var)

#%% Interpolation
# Vh_raw_May24_interpolated = pd.DataFrame()

# for station in stations:  # iterate for every station
#     for sensor in sensors:  # iterate for every probe
#         Vh_raw_May24_subset = Vh_raw_May24[(Vh_raw_May24['plot'] == station) &
#                                             (Vh_raw_May24['probe'] == sensor)]
        
#         # Perform polynomial interpolation on the 'Vhr_HRM_inner' column of the subset
#         Group = Vh_raw_May24_subset.copy()
#         Group['Vhr_HRM_inner'] = Vh_raw_May24_subset['Vhr_HRM_inner'].interpolate(
#             method='polynomial', order=3, limit_direction='both', limit=5)
        
#         # Append the interpolated data to the main DataFrame
#         Vh_raw_May24_interpolated = Vh_raw_May24_interpolated.append(Group, ignore_index=True)

# %% Locate Zeroing Events

# (1) VPD CRITERIA  ######
# Extract the date (without hours) from the "timestamp" column
rh["date"] = rh["timestamp"].dt.date

# Create a boolean condition to filter rows where "vpd_kpa" < 0.2
condition_vpd_time = (rh["vpd_kpa"] < 0.2)

# Apply the condition and group by date to find the minimum "low value" for each day
low_VPD = rh[condition_vpd_time].groupby(["date", "plot"])["vpd_kpa"].min().reset_index()

# plot the low_VPD to check for accuracy
# for station in stations:
#     vpd_min_plot = low_VPD[(low_VPD['plot'] == station)]
#     rh_plot = rh[(rh['plot'] == station)]
#     plt.plot(rh_plot['date'], rh_plot['vpd_kpa'], color='b',linewidth=.1)
#     plt.scatter(vpd_min_plot['date'],vpd_min_plot['vpd_kpa'],
#                 color='r', marker='o', s=4)
#     plt.show()



# (2) WINDSPEED CRITERIA  ######
# locate the avg value of wind for each low_VPD event
# initialize wind (u) column
low_VPD['avg_u'] = 0
# Extract the date (without hours) from the "timestamp" column
MET["date"] = MET["timestamp"].dt.date
# Group the MET DataFrame by "date" and find the avg wind speed for each day
max_wind_speed_per_day = MET.groupby('date')['wind_speed_ms'].mean()
# Populate wind column based on matching dates
for date, avg_u in max_wind_speed_per_day.items():
    low_VPD.loc[low_VPD['date'] == date, 'avg_u'] = avg_u

# drop rows in low_VPD if u > 0.3
mask = low_VPD['avg_u'] > 0.3
# Filter "low_VPD" using the mask to keep rows where "avg_u" is not greater than 0.3
low_VPD_wind = low_VPD[~mask]

# assume vpd_min events happen at 3am. Precision is not needed
low_VPD_wind['date'] = pd.to_datetime(low_VPD_wind['date'])
low_VPD_wind['timestamp'] = low_VPD_wind['date'] + pd.to_timedelta('03:00:00')
low_VPD_wind['timestamp'] = low_VPD_wind['timestamp'].dt.tz_localize('UTC')



# (3) NIGHTTIME CRITERIA  ######
# night range to be used in function below
night_start_hour = 22
night_end_hour = 5


# %% Locate minimum Vh 

# filter Vh based on misbehaving data, tree species, sensor installation
Vh_raw_filtered = filter_data(Vh_raw_May24, var)

zeroing_events = min_vh_locator(low_VPD_wind, Vh_raw_filtered, night_start_hour,
                                night_end_hour, var, window_hours=60)

# plot the min sap flow to check for accuracy
# for station in stations:
#     plt.figure()
#     zeroing_plot = zeroing_events[(zeroing_events['plot'] == station)]
#     zeroing_plot = zeroing_plot[(zeroing_plot['probe'] == 'b')]
#     Vh_raw_plot = Vh_raw_filtered[(Vh_raw_filtered['plot'] == station)]
#     Vh_raw_plot = Vh_raw_plot[(Vh_raw_plot['probe'] == 'b')]
#     plt.title(station)
#     plt.plot(Vh_raw_plot['timestamp'], Vh_raw_plot['Vhr_HRM_inner'], color='b', linewidth=.1)
#     plt.scatter(zeroing_plot['timestamp'],zeroing_plot['Vh_HRM_inner_z'],
#                 color='r', marker='o', s=4)
#     plt.show()

# alert me when it's done
winsound.Beep(1000, 500) 


# %% Apply linear correction  

corrected_dfs_VPDu = []
for plot in plots:  # iterate for every station
    for probe in probes:  # iterate for every probe
        print(f"CODE at {plot}, {probe}")
        raw_df_plot = Vh_raw_filtered[(Vh_raw_filtered['plot'] == plot)]
        raw_df = raw_df_plot[raw_df_plot['probe'] == probe]
        
        zeroing_df_plot = zeroing_events[zeroing_events['plot'] == plot]
        zeroing_df = zeroing_df_plot[zeroing_df_plot['probe'] == probe]

        # Apply linear correction and collect the corrected dataframes
        if not (plot == 'P304_E' and probe == 'a'): # because station E, sensor a has no data
            corrected_df = apply_linear_correction(raw_df.copy(), zeroing_df, var)
            corrected_dfs_VPDu.append(corrected_df)
            
# Concatenate all the corrected group dataframes into a single dataframe
Vh_corrected = pd.concat(corrected_dfs_VPDu, ignore_index=True)

# alert me when it's done
winsound.Beep(1000, 500) 


#%% test plot
Vh_corrected = filter_data_growing_season(Vh_corrected)
Vh_corrected = filter_data(Vh_corrected, 'Vhr_HRM_inner')
Vh_corrected = filter_data(Vh_corrected, 'Vhr_HRM_corrected')
# Vh_corrected = filter_data(Vh_corrected, 'Vhr_HRM_both')
# Vh_corrected = additional_filter_for_inclusion_of_outer(Vh_corrected, 'Vhr_HRM_both')
Vh_corrected = additional_filter_for_inclusion_of_outer(Vh_corrected, 'Vhr_HRM_corrected')
Vh_corrected = additional_filter_for_inclusion_of_outer(Vh_corrected, 'Vhr_HRM_inner')


plots = ['P304_A', 'P304_B', 'P304_C', 'P304_D', 'P304_E', 'P304_F']
# plots = ['P304_B']

for plot in plots:
    fig, axes = plt.subplots(4, 1, figsize=(40, 15), sharex=True, sharey=True)
    filtered_Vh_corrected = Vh_corrected[Vh_corrected['plot'] == plot]
    # filtered_Vh_corrected2 = Vh_corrected2[Vh_corrected2['plot'] == plot]
    zeroing_plot = zeroing_events[(zeroing_events['plot'] == plot)]
    num = 0  # start with first axis
    for probe in probes:
        ax = axes[num]
        zeroing_data = zeroing_plot[zeroing_plot['probe'] ==  probe]
        data = filtered_Vh_corrected[filtered_Vh_corrected['probe'] == probe]
                
        for date in zeroing_data['timestamp']:
            ax.axvline(x=date, color='#00FF00',linewidth='.3')
        
        ax.grid(True, linestyle='--', which='both')
        # ax.set_ylim(-10, 45)
        ax.set_ylabel('sap flow (cm/hr)', color='black')
        ax.plot(data['timestamp'], data['Vhr_HRM_inner'], linewidth=.5, color='grey', label='inner, uncorrected')
        # ax.plot(data['timestamp'], data['Vhr_HRM_both'], linewidth=.5, color='black', label='both, uncorrected')
        ax.plot(data['timestamp'], data['Vhr_HRM_corrected'], linewidth=.5, color='red', label='both, corrected')
        
        ax.set_title(f'Sensor {probe}', fontsize=10)
        ax.legend()
        num += 1
    fig.suptitle(f'Data from {plot}')
    # plt.subplots_adjust(hspace=0.306, wspace=0.380)
    # plt.subplots_adjust(top=0.930, bottom=0.113,left=0.047,right=0.877)
    plt.show()


# %% pickling
# # ## SAVING
# with open('data/Zeroing_2vpd_60hr_night_May24_update2_bothinnerouter.pkl', 'wb') as f:
#     pickle.dump(zeroing_events, f)

with open('data/Vh_corr_60hrwind_2vpd_night_May24_update2.pkl', 'wb') as f:
    pickle.dump(Vh_corrected, f)

Vh_corrected.to_csv('processed/Vh_corr_60hrwind_2vpd_night_May24_update2.csv')

# # open
# with open('data/Vh_corr_60hrwind_2vpd_night_May24_update2_bothinnerouter.pkl', 'rb') as f:
#     Vh_corrected = pickle.load(f)

# with open('data/Zeroing_2vpd_60hr_night_May24_update2_bothinnerouter.pkl', 'rb') as f:
#     zeroing_events = pickle.load(f)

