"""
Created on June 27, 2024
Code written by Annie Tucker

Purpose: Correct sap flow heat velocity (Vh) to sap flow sap velocity (Vs) 

Inputs: Vh_corrected & tree coring data

Output: Vs

"""
# %% Prep
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import warnings
import pickle
warnings.filterwarnings("ignore")
os.chdir("C:/Users/m10921492/OneDrive - Colorado School of Mines/Sierra_BCZN_Annie/Sierra_python")

from SF_Filtering import filter_data, filter_data_growing_season

# load data
Vh_corrected = pd.read_csv('processed/Vh_corr_60hrwind_2vpd_night_May24_update2.csv',
                        parse_dates=["timestamp"])
tree_data_all = pd.read_csv('processed/k_values.csv')

# define variables
plots = ['P304_A', 'P304_B', 'P304_C', 'P304_D', 'P304_E', 'P304_F']
probes = ['a', 'b', 'c', 'd']

#%% Define correction function

def heat_to_sap (tree_data, Vh):
    # equation from Barrett et al. (1995)
    
    density_wood = tree_data['ddw'].iloc[0] #(kg/m3)    # dry wood density
    density_sap = 1000 #kg/m3                           # estimated to be density of water
    c_wood = tree_data['c'].iloc[0] #J/kg/C             # specific heat capacity of dry wood
    c_sap = 4182 #J/kg/C                                # specific heat capacity of sap
    mc = tree_data['mc'].iloc[0] #unitless              # moisture content
    
    Vs = Vh.copy()
    
    Vs['Vs'] = Vh['Vhr_HRM_corrected']*density_wood*(c_wood + (mc*c_sap))/(density_sap*c_sap)
    
    # factor = density_wood*(c_wood + (mc*c_sap))/(density_sap*c_sap)
    # print("adjustment factor was: ",factor)
    
    return Vs

#%% convert heat velocity into sap velocity
Vs_appended = []

for plot in plots:
    for probe in probes:
        Vh = Vh_corrected[(Vh_corrected['plot'] == plot) & (Vh_corrected['probe'] == probe)]
        tree_data = tree_data_all[(tree_data_all['plot'] == plot) & (tree_data_all['probe'] == probe)]
        
        # implement correction
        Vs = heat_to_sap(tree_data, Vh)
        
        # append to final file
        Vs_appended.append(Vs)
        
        plt.figure()
        plt.plot(Vh['timestamp'], Vh['Vhr_HRM_corrected'], label='Heat Velocity')
        plt.plot(Vs['timestamp'], Vs['Vs'], color='red', label='Sap Velocity')
        plt.legend()
        plt.title(f"{plot} and {probe}")
        plt.show()

Vs_final = pd.concat(Vs_appended, ignore_index=True)

#%% save file
Vs_final.to_csv('processed/Vs_version2.csv', index=False)

# %% CALC % NOCTURNAL SF

#differentiate between nighttime and daytime by incoming radiation
# radiation_15min['timestamp'] = radiation_15min['timestamp'].dt.tz_localize('UTC')
# zeroing_VPD_wind_night_60_hr_window_2vpd_rad = pd.merge(zeroing_VPD_wind_night_60_hr_window_2vpd,
#                                                               radiation_15min[['timestamp', 'VALUE']], on='timestamp', how='left')
# Vh_corrected_VPD_wind_night_24hr_window_15vpd_rad['day_night'] = np.where(
#     Vh_corrected_VPD_wind_night_24hr_window_15vpd_rad['VALUE'] > 0,
#     'Day','Night')

#plotting to show what time of day zeroing events happen
# zeroing_VPD_wind_60_hr_window_2vpd_NIGHTTIME['time_hours'] = zeroing_VPD_wind_60_hr_window_2vpd_NIGHTTIME['timestamp'].dt.hour
# plt.figure(figsize=(12, 6))  # Optional: set the figure size
# plt.scatter(zeroing_VPD_wind_60_hr_window_2vpd_NIGHTTIME['time_hours'], zeroing_VPD_wind_60_hr_window_2vpd_NIGHTTIME['probe'])
# plt.xticks(range(24))
# plt.show()

def set_day_night(row):
    if (row['timestamp'].hour >= 22) or (row['timestamp'].hour < 5):
        return 'Night'
    else:
        return 'Day'
Vh_corr = Vh_corrected
Vh_corr['day_night'] = Vh_corr.apply(set_day_night, axis=1)


# filter
Vh_corr = filter_data(Vh_corr, 'Vhr_HRM_inner_corrected')
Vh_corr = filter_data_growing_season(Vh_corr)


#plot to verify & remove outliers
# for station in stations:  # iterate for every station
#     plt.figure()
#     df = Vh_corr[(Vh_corr['plot'] == station) & (Vh_corr['probe'] == 'b')]        
#     sns.scatterplot(data=df, x='timestamp', y='Vhr_HRM_inner_corrected',
#                     hue='day_night', palette={'Day': 'blue', 'Night': 'red'}, legend=False)
#     plt.show()


# sum total & nighttime sap velocity
daytime_sum = Vh_corr[
    Vh_corr['day_night'] == 'Day']['Vhr_HRM_inner_corrected'].sum()
nighttime_sum = Vh_corr[
    Vh_corr['day_night'] == 'Night']['Vhr_HRM_inner_corrected'].sum()

# percent nocturnal
perc = nighttime_sum / (nighttime_sum + daytime_sum) * 100
print(perc)

# %% CALC NOC % OF PEAK SF
all_perc_peaks = []

temp = filter_data_growing_season(Vh_corrected)
temp2 = filter_data(temp,'Vhr_HRM_inner_corrected')

for plot in plots:
    Vh_plot = temp2[temp2['plot'] == plot]
    for probe in probes:
        Vh_plot_sensor = Vh_plot[Vh_plot['probe'] == probe]
        Vh_plot_sensor.set_index('timestamp', inplace=True)
        # get min Vh / day
        vh_min = Vh_plot_sensor['Vhr_HRM_inner_corrected'].resample('D').min()
        # get max Vh / day
        vh_max = Vh_plot_sensor['Vhr_HRM_inner_corrected'].resample('D').max()
        # calc percent
        perc_peak = vh_min / vh_max * 100
        all_perc_peaks.append(perc_peak)
        
# Create a list to store mean values
mean_values = []

for item in all_perc_peaks:
    mean_value = item.mean()
    # Check if mean_value is not -inf before appending it
    if not np.isinf(mean_value) and not np.isnan(mean_value):
        print(mean_value)
        mean_values.append(mean_value)

# Calculate the overall average of mean values (excluding -inf values)
overall_average = np.mean(mean_values)
print(overall_average)

# %% PLOT EACH SENSOR'S SF
# MET = pd.read_csv("data/MET_master_update.csv", parse_dates=["timestamp"])
# rh = pd.read_csv("data/rh_master_update.csv", parse_dates=["timestamp"])
# vwc = pd.read_csv("processed/vwc_processed.csv", parse_dates=["timestamp"])
# vwc = pd.read_csv("processed/vwc_processed_may24update.csv", parse_dates=["timestamp"])
# rain_hr = pd.read_csv("processed/rain_hr.csv", parse_dates=["timestamp"])
# radiation_hr = pd.read_csv("processed/radiation_hr.csv", parse_dates=["timestamp"])

with open('data/Zeroing_2vpd_60hr_night_May24_update2.pkl', 'rb') as f:
    zeroing_events = pickle.load(f)

# vars
num2 = 0
# filtered_Vh_corrected = filter_data_growing_season(Vs_final)
# zeroing_events = filter_data_growing_season(zeroing_events)

# filtering other data
# rh_f = rh[(rh["timestamp"] < "2022-11-01 00:00:00+00:00") |
#         (rh["timestamp"] > "2023-05-01 00:00:00+00:00")]
# vwc_f = vwc[(vwc["timestamp"] < "2022-11-01 00:00:00+00:00") |
#           (vwc["timestamp"] > "2023-05-01 00:00:00+00:00")]
# rain_hr = rain_hr[(rain_hr["timestamp"] < "2022-11-01 00:00:00+00:00") |
#           (rain_hr["timestamp"] > "2023-05-01 00:00:00+00:00")]
# radiation_hr['timestamp'] = pd.to_datetime(radiation_hr['timestamp'], utc=True)
# radiation_hr = radiation_hr[(radiation_hr["timestamp"] < "2022-11-01 00:00:00") |
#           (radiation_hr["timestamp"] > "2023-05-01 00:00:00")]
# radiation_hr = radiation_hr[(radiation_hr["timestamp"] > "2022-07-01 00:00:00")]

# plots = ['P304_F']

# plotting
for plot in plots:
    fig, axes = plt.subplots(4, 1, figsize=(19, 10))
    Vh_c = Vs_final[Vs_final['plot'] == plot]
    # filtered_vpd = rh[rh['plot'] == plot]
    # filtered_vwc = vwc[vwc['plot'] == plot]
    filtered_zeroing = zeroing_events[zeroing_events['plot'] == plot]
    num = 0  # start with first axis
    for probe in probes:
        ax = axes[num]
        
        # plot sap flow sensor CORRECTED &&&&&& SAP VELOCITY
        # Vh_p = Vh_c[Vh_c['probe'] == probe]
        # y_axis = Vh_p['Vs']
        # x_axis = Vh_p['timestamp']
        # ax.grid(True, linestyle='--', which='both')
        # ax.set_ylim(-10, 45)
        # # ax.set_xlim("2022-07-01 00:00:00+00:00", "2023-10-20 00:00:00+00:00")
        # ax.set_ylabel('sap flow (cm/hr)', color='black')
        # ax.plot(x_axis, y_axis, linewidth=.5, color='black')
        
        
        # plot sap flow sensor CORRECTED (for misalignment/zeroing)
        Vh_p = Vh_c[Vh_c['probe'] == probe]
        y_axis = Vh_p['Vhr_HRM_inner_corrected']
        x_axis = Vh_p['timestamp']
        ax.grid(True, linestyle='--', which='both')
        ax.set_ylim(-10, 45)
        # ax.set_xlim("2022-07-01 00:00:00+00:00", "2023-10-20 00:00:00+00:00")
        ax.set_ylabel('Heat Velocity (cm/hr)', color='red')
        ax.plot(x_axis, y_axis, linewidth=.5, color='red')
        
        # plot sap flow sensor RAW
        Vh_r_p = Vh_c[Vh_c['probe'] == probe]
        y_axis = Vh_r_p['Vhr_HRM_inner']
        x_axis = Vh_r_p['timestamp']
        ax.grid(True, linestyle='--', which='both')
        # ax.set_ylim(-10, 45)
        # ax.set_xlim("2022-07-01 00:00:00+00:00", "2023-10-20 00:00:00+00:00")
        ax.set_ylabel('Heat Velocity (cm/hr)', color='black')
        ax.plot(x_axis, y_axis, linewidth=.2, color='black')
        
        
        ax.set_xlim(pd.to_datetime("2022-05-13 00:00:00+00:00"), pd.to_datetime("2024-05-12 00:00:00+00:00"))
        
        # # plot VPD on new axis
        # ax1 = ax.twinx()
        # y_axis2 = filtered_vpd['vpd_kpa']
        # x_axis2 = filtered_vpd['timestamp']
        # ax1.tick_params(labelcolor='orange')
        # ax1.set_ylabel('VPD (kpa)', color='orange')
        # # ax1.set_xlim("2022-07-01 00:00:00+00:00", "2023-10-20 00:00:00+00:00")
        # ax1.set_ylim(0, 4)
        # ax1.plot(x_axis2, y_axis2, linewidth=.4, color='orange')
        
        # # plot soil moisture on a new axis
        # ax2 = ax.twinx()
        # y_axis3 = filtered_vwc['30_cm']
        # y_axis3_15cm = filtered_vwc['15_cm']
        # x_axis3 = filtered_vwc['timestamp']
        # ax2.tick_params(labelcolor='green')
        # ax2.set_ylabel('VWC (m3/m3)', color='green')
        # ax2.set_ylim(0, 0.5)
        # ax.grid(True, linestyle='--', which='both')
        # # ax2.set_xlim("2022-07-01 00:00:00+00:00", "2023-10-20 00:00:00+00:00")
        # ax2.spines['right'].set_position(('outward', 40))
        # ax2.plot(x_axis3, y_axis3, linewidth=.5, color='green')
        # ax2.plot(x_axis3, y_axis3_15cm, linewidth=.5, color='green')
        
        # # plot rain
        # ax3 = ax.twinx()
        # y_axis4 = rain_hr['Rain_mm']
        # x_axis4 = rain_hr["timestamp"]
        # ax3.tick_params(labelcolor='blue')
        # ax3.set_ylabel('rain_mm',color='blue')
        # ax3.spines['right'].set_position(('outward',80))
        # ax3.plot(x_axis4, y_axis4,linewidth=.5,color='blue')
        
        # # plot radiation
        # ax4 = ax.twinx()
        # y_axis5 = radiation_hr['avg']
        # x_axis5 = radiation_hr['timestamp']
        # ax4.tick_params(labelcolor='green')
        # ax4.set_ylabel('Incoming Radiation (w/m2)', color='green')
        # # ax4.set_xlim("2022-07-01", "2023-10-20")
        # ax4.spines['right'].set_position(('outward', 120))
        # ax4.plot(x_axis5, y_axis5, linewidth=.6, color='green')

        # vert lines for zeroing points        
        ax6 = ax.twinx()
        # filtered_zeroing
        filtered_zeroing_probe = filtered_zeroing[filtered_zeroing['probe'] ==  probe]
        for date in filtered_zeroing_probe['timestamp']:
            plt.axvline(x=date, color='#00FF00',linewidth='.3')
        ax6.set_yticks([])
        
        num = num + 1  # track sensor/axes number
        
        ax.set_title(f'Sensor {probe}', fontsize=10)
        
    station_name = plots[num2]
    fig.suptitle(
        # 'Sap Velocity Data from 4 nearby trees in the Sierras')
        f'Data from {station_name}, zeroing via VPD<0.2 + u<0.3 + night + 5-day window')
    num2 = num2 + 1
    plt.subplots_adjust(hspace=0.306, wspace=0.380)
    plt.subplots_adjust(top=0.930, bottom=0.113,left=0.047,right=0.877)
    plt.show()