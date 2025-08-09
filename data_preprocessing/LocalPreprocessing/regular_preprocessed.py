import os
import json
import socket
import pandas as pd
import numpy as np
import glob

###############
## load data ##
HiRID = True
# Load configuration
with open(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'path.config'))) as f:
    config = json.load(f)

data_root = config[socket.gethostname()]["data_root"]
output_root = config[socket.gethostname()]["output_root"]

print(f"HiRID = ",HiRID)

if HiRID:
    suffix = "_HiRID"
    filepath_regular = glob.glob(os.path.join(data_root, "Hirid", "2025-06-29", "regular_measurements", "*.parquet"))
else:
    suffix = "_AmsterdamUMCDb"
    filepath_regular = glob.glob(os.path.join(data_root, "AmsterdamUMCDb", "2025-06-29", "regular_measurements", "*.parquet"))

dataframes_regular = [pd.read_parquet(file) for file in filepath_regular]
regular_df = pd.concat(dataframes_regular, ignore_index= True)

##correct lenght to length:
regular_df = regular_df.rename(columns={"session_lenght":"session_length"})

## set the NaNs to 0 (NaN was registered, when patient was deconnected from dialysis machine)
# and create a new column to differ between the "real" 0, and the 0 where patients were disconnected from CRRT. 
regular_df["NaN_converted_to_0"] = np.where(regular_df["vm5010"].isna(), 1, 0)
regular_df["vm5010"] = regular_df["vm5010"].fillna(0)
 
#### Create a new column where 1 is set from the start of Ultrafiltration (where vm5010 is > 0 the first time).
## For statistical analysis of duration until start of net ultrafiltration 
regular_df["net_ultrafiltration"] = regular_df.groupby("patid")["vm5010"].transform(lambda x: (x > 0).cummax().astype(int))

###### Change the RRT-type fo HIRID from 3 to 4 to match the numbers of Amsterdam  
# 3 in Hirid is CVVHDF, 4 in Amsterdam is CVVHDF, so changing the Hirid 3 to 4: 
if HiRID:
    regular_df.loc[regular_df["vm5025"] == 3, "vm5025"] = 4
    
## Change dtype of 2 columns from object to float:
if HiRID:
    regular_df["dm_vent_inv_state"] = regular_df["dm_vent_inv_state"].astype("float64")
    regular_df["dm_vent_niv_state"] = regular_df["dm_vent_niv_state"].astype("float64")

############## Add a row with the source of the data / add the suffix to the patid to make mischmasch impossible
if HiRID:
    regular_df["source"] = "HiRID"
    regular_df['patid'] =  regular_df['patid'].astype(str) + '_HiRID'  
else:
    regular_df["source"] = "AmsterdamUMCDb"
    regular_df['patid'] =  regular_df['patid'].astype(str) + '_Amsterdam'

####################
## Fluid outliers ## 

# set to NaN when absolute fluid change > 10L/h (NaN are automatically ignored in calculation of mean/median etc...)
regular_df_removed_outliers = regular_df
regular_df_removed_outliers["dm_balancerate_h"] = np.where(regular_df_removed_outliers["dm_balancerate_h"].abs() < 10000, regular_df_removed_outliers["dm_balancerate_h"], np.nan)

##Calculate rows and calculate number of patients with such outliers...
print("Removed Rows with Fluid balance outliers:", len(regular_df_removed_outliers["dm_balancerate_h"].isna()))
outlier_rows = regular_df_removed_outliers[regular_df_removed_outliers["dm_balancerate_h"].isna()]
num_patients_with_outliers = outlier_rows["patid"].nunique()
print("Number of Patients with Fluid balance outliers:", num_patients_with_outliers)

####################################################################################


####### Filter the dataframe for CRRT session_length <= 48h
regular_df_48h = regular_df_removed_outliers[regular_df_removed_outliers["session_length"]<=2880]

###### Filter the dataframe for 48h after start uf net ultrafiltration 
def from_NUF_start_48h(x):
    if (x["vm5010"] == 0).all():
        return x[x["session_length"] <= 2880]
    else:
        first_NUF = x.loc[x["vm5010"] > 0, "session_length"].iloc[0]
        return x[(x["session_length"] >= first_NUF) & (x["session_length"] <= (first_NUF + 2880))]          
regular_df_48h_NUF = regular_df_removed_outliers.groupby("patid").apply(lambda x: from_NUF_start_48h(x))

##############
######
##

if HiRID:
    regular_df_removed_outliers.to_csv(os.path.join(output_root, "regular_preprocessed_HiRID.csv"), index=False)
    regular_df_48h.to_csv(os.path.join(output_root, "regular_preprocessed_48h_HiRID.csv"), index=False)
    regular_df_48h_NUF.to_csv(os.path.join(output_root, "regular_preprocessed_48h_NUF_HiRID.csv"), index=False)
else:
    regular_df_removed_outliers.to_csv(os.path.join(output_root, "regular_preprocessed_AmsterdamUMCDb.csv"), index=False)
    regular_df_48h.to_csv(os.path.join(output_root, "regular_preprocessed_48h_AmsterdamUMCDb.csv"), index=False)
    regular_df_48h_NUF.to_csv(os.path.join(output_root, "regular_preprocessed_48h_NUF_AmsterdamUMCDb.csv"), index=False)

##



"""
onlyzero = regular_df.groupby("patid")["vm5010"].apply(lambda x: (x == 0).all())
onlyzero = onlyzero[onlyzero].index
#onlyzero = regular_df[regular_df["patid"].isin(onlyzero)]
print(onlyzero.unique())
"""
"""
missingtotalfluid = regular_df.groupby("patid")["fluid_balance_pre_crrt"].apply(lambda x: x.isna().all())
print(missingtotalfluid.sum())
missingtotalfluid = regular_df.groupby("patid")["fluid_balance_pre_crrt"].apply(lambda x: x.isna().all())
missing_patids = missingtotalfluid[missingtotalfluid].index.tolist()
print(missing_patids)
"""



