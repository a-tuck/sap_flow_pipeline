import pandas as pd
import numpy as np
from datetime import timedelta
import warnings
warnings.filterwarnings("ignore")


# function to select smallest SF near min vpd values
def min_vh_locator(target_df, vh_raw_df, night_start_hour, night_end_hour, var,
                   window_hours=60):
    #target_df has the timestamps where the above criteria is met
    zeroing_results = []  # List to hold dataframes to be concatenated
    
    # Ensure timestamps are in datetime format
    vh_raw_df['timestamp'] = pd.to_datetime(vh_raw_df['timestamp'])
    target_df['timestamp'] = pd.to_datetime(target_df['timestamp'])

    # iterate through each plot/station
    plots = ['P304_A', 'P304_B', 'P304_C', 'P304_D', 'P304_E', 'P304_F']
    sensors = ['a', 'b', 'c', 'd']
    for plot in plots:
        target_df_plot = target_df[target_df['plot'] == plot]
        for sensor in sensors:
            # iterate through each identified low VPD/wind instance
            for _, row in target_df_plot.iterrows(): # iterate thru zeroing points
                start_time = row['timestamp'] - pd.Timedelta(hours=window_hours)
                end_time = row['timestamp'] + pd.Timedelta(hours=window_hours)
                
                # Filter the vh_raw data for the current station and within the time window
                vh_window = vh_raw_df[(vh_raw_df['plot'] == plot) & 
                                      (vh_raw_df['probe'] == sensor) &
                                      (vh_raw_df['timestamp'] >= start_time) & 
                                      (vh_raw_df['timestamp'] <= end_time)]
    
                # Filter data for night hours
                vh_window_night = vh_window[(vh_window['timestamp'].dt.hour >= night_start_hour) |
                                                (vh_window['timestamp'].dt.hour <= night_end_hour)]
    
                if not vh_window_night.empty:
                    # Find the minimum Vhr_HRM_inner value for the current station
                    min_value = vh_window_night[var].min()
                    # Get the timestamp where the minimum Vhr_HRM_inner value occurred
                    min_timestamp_row = vh_window_night[vh_window_night[var] == min_value]
    
                    if not min_timestamp_row.empty:
                        min_timestamp = min_timestamp_row['timestamp'].iloc[0]
                        new_data = {'plot': plot,
                                    'probe': sensor,
                                    'Vh_HRM': min_value,
                                    'timestamp': min_timestamp
                                    }
                        zeroing_results.append(pd.DataFrame(new_data, index=[0]))

    # Concatenate all the dataframes in the list into one dataframe
    zeroing_result = pd.concat(zeroing_results, ignore_index=True).drop_duplicates()
    
    return zeroing_result






# linear correction  
# function to correct the sap flow based on zeroing & slope manipulation
def apply_linear_correction(group_df, zeroing_df, var):
    """
    group_df is already filtered for station & sensor
    zeroing_df is already filtered for station (no sensor filter needed)
    
    """
    
    # initialize df to house zeroing timestamps
    # zero_points is a series of only timestamps
    zero_points = zeroing_df['timestamp'].reset_index(drop=True)

    # Initialize the corrected column with NaNs
    group_df['Vhr_HRM_corrected'] = np.nan

    for i in range(len(zero_points) - 1): 
    ###### for zeroing the middle intervals (when there are 2 sequential zeroing points with Vhr values)
        start_value = group_df.loc[group_df['timestamp'] == zero_points[i], 
                                   var].values
        end_value = group_df.loc[group_df['timestamp'] == zero_points[i + 1],
                                 var].values
        if not np.isnan(start_value).all() and not np.isnan(end_value).all(): # if these have values
            # Calculate the slope (m) using the change in Vhr_HRM_inner over the total seconds between the zero points
            m = (end_value[0] - start_value[0]) / \
                (zero_points[i + 1] - zero_points[i]).total_seconds()
                
            # Calculate the intercept (b) using the start_value and accounting for the start timestamp
            b = start_value[0]
    
            # calculate diff in seconds between each timestamp in group_df & zeroing point
            t = group_df['timestamp'].astype('int64') // 1e9 - zero_points[i].timestamp()
            
            # Calculate the linear term (m*t + b) for all timestamps within the current segment
            linear_term = m * t + b

            # Correct the raw values by subtracting the linear term from the raw values
            segment_mask = (group_df['timestamp'] >= zero_points[i]) & (
                group_df['timestamp'] <= zero_points[i + 1])
            group_df.loc[segment_mask,'Vhr_HRM_corrected'] = group_df.loc[
                segment_mask,var] - linear_term[segment_mask]
            
            
    ###### for zeroing before 1st zeroing point (starting at mintime and going to 1st zero point)
            # use slope correction from the above first sequential zeroing points
        if i == 0:
            # calculate diff in seconds between each timestamp in group_df & zeroing point
            t_start = group_df['timestamp'].astype('int64') // 1e9 - group_df['timestamp'].min().timestamp()
            
            try:
                # Calculate the linear term (m*t) for all timestamps within the current segment
                linear_term_start = m * t_start
            
                # Correct the raw values by subtracting the linear term from the raw values
                # fist timestamp & 1st zeroing pt
                segment_mask = (group_df['timestamp'] >= group_df['timestamp'].iloc[0]) & (
                    group_df['timestamp'] <= zero_points[0])
                group_df.loc[segment_mask,'Vhr_HRM_corrected'] = group_df.loc[
                    segment_mask,var] - linear_term_start[segment_mask]
                
                # add another verticle correction (becuase m and b used are not for this dataset)
                # get Vhr_HRM_corrected value 1 timestep prior to the 1st zeroing point
                t_corr = zero_points[0] - timedelta(minutes=15)
                verticle_offset = group_df.loc[group_df['timestamp'] == t_corr, var].values[0]
                
                # implement final verticle offset correction
                group_df.loc[segment_mask,'Vhr_HRM_corrected'] = group_df.loc[
                    segment_mask,var] - verticle_offset      
            except: print('missing start value error')
        
        
        # if either value is nan, use prior m and b
        elif np.isnan(start_value).all() or np.isnan(end_value).all():
            # calculate diff in seconds between each timestamp in group_df & zeroing point
            t = group_df['timestamp'].astype('int64') // 1e9 - zero_points[i].timestamp()
            
            # Calculate the linear term (m*t + b) for all timestamps within the current segment
            try: 
                linear_term = m * t + b
    
                # Correct the raw values by subtracting the linear term from the raw values
                segment_mask = (group_df['timestamp'] >= zero_points[i]) & (
                    group_df['timestamp'] <= zero_points[i + 1])
                group_df.loc[segment_mask,'Vhr_HRM_corrected'] = group_df.loc[
                    segment_mask,var] - linear_term[segment_mask]
                
                try:
                    # add another verticle correction (becuase m and b used are not for this dataset)
                    # get Vhr_HRM_corrected value 1 timestep prior to the 1st zeroing point
                    existingpt = zero_points[i+1] if np.isnan(start_value).all() else zero_points[i]
                    t_corr = existingpt + timedelta(minutes=15)
                    verticle_offset = group_df.loc[group_df['timestamp'] == t_corr, var].values[0]
                    
                    # implement final verticle offset correction
                    group_df.loc[segment_mask,'Vhr_HRM_corrected'] = group_df.loc[
                        segment_mask,var] - verticle_offset      
                    
                except: print('missing end or start value error with second verticle offset')
                
            except: print('missing end or start value error')
        

    ###### for zeroing after last zero point (starting at last zero point and going to maxtime)
        final_zero_point = zero_points[i] # this will overwrite until last iteration/max i
            
    # calculate diff in seconds between each timestamp in group_df & zeroing point
    t = group_df['timestamp'].astype('int64') // 1e9 - final_zero_point.timestamp()
    
    # Calculate the linear term (m*t + b) for all timestamps within the current segment
    # linear term is correction value essentially
    linear_term_end = m * t + b

    # Correct the raw values by subtracting the linear term from the raw values
    segment_mask = group_df['timestamp'] >= final_zero_point
    group_df.loc[segment_mask,'Vhr_HRM_corrected'] = group_df.loc[
        segment_mask,var] - linear_term_end[segment_mask]



    ###### Set the Vhr_HRM_inner values to 0 at the zero points
    for zero_time in zero_points:
        group_df.loc[group_df['timestamp'] == zero_time,
                     'Vhr_HRM_corrected'] = 0


    
    # Make it obvious if none of the above code has corrected the data
    # condition = group_df['Vhr_HRM_corrected'].isna() & group_df['Vhr_HRM_both'].notna()
    # group_df.loc[condition, 'Vhr_HRM_corrected'] = -99


    return group_df