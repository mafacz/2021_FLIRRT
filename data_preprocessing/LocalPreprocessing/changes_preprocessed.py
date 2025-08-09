import os
import json
import socket
import pandas as pd
import numpy as np
import glob

###############
## Load data ##
HiRID = False

# Load configuration
with open(os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'path.config'))) as f:
    config = json.load(f)

data_root = config[socket.gethostname()]["data_root"]
output_root = config[socket.gethostname()]["output_root"]

print(f"HiRID = ",HiRID)

if HiRID:
    suffix = "_HiRID"
    filepath_changes = glob.glob(os.path.join(data_root, "Hirid", "2025-06-29", "change_measurements", "*.parquet"))
else:
    suffix = "_AmsterdamUMCDb"
    filepath_changes = glob.glob(os.path.join(data_root, "AmsterdamUMCDb", "2025-06-29", "change_measurements", "*.parquet"))

dataframes_changes = [pd.read_parquet(file) for file in filepath_changes]
changes_df = pd.concat(dataframes_changes, ignore_index= True)

##correct lenght to length:
changes_df = changes_df.rename(columns={"session_lenght":"session_length"})

##delete the fluid columns and lab_values, since these values are not reliable in this table (we use the regular_df instead)
changes_df = changes_df.drop(columns=["dm_balancerate_h"])

###set the NaNs to 0 (NaN was registered, when patient was deconnected from dialysis machine)
# and create a new column to differ between the "real" 0, and the 0 where patients were disconnected from CRRT. 
changes_df["NaN_converted_to_0"] = np.where(changes_df["vm5010"].isna(), 1, 0)
changes_df["vm5010"] = changes_df["vm5010"].fillna(0)
 
#### Create a new row where 1 is set from the start of Ultrafiltration (where vm5010 is > 0 the first time).
## For statistical analysis of duration until start of net ultrafiltration!
changes_df["net_ultrafiltration"] = changes_df.groupby("patid")["vm5010"].transform(lambda x: (x > 0).cummax().astype(int))

###### Change the RRT-type fo HIRID from 3 to 4 to match the numbers of Amsterdam  
# 3 in Hirid is CVVHDF, 4 in Amsterdam is CVVHDF, so changing the Hirid 3 to 4: 
if HiRID:
    changes_df.loc[changes_df["vm5025"] == 3, "vm5025"] = 4
    
## Change dtype of 2 columns from object to float to align with Amsterdam: 
if HiRID:
    changes_df["dm_vent_inv_state"] = changes_df["dm_vent_inv_state"].astype("float64")
    changes_df["dm_vent_niv_state"] = changes_df["dm_vent_niv_state"].astype("float64")

#######Add a column with the source of the data / add the suffix to the patid to make mischmasch impossible
if HiRID:
    changes_df["source"] = "HiRID"
    changes_df['patid'] =  changes_df['patid'].astype(str) + '_HiRID'
else:
    changes_df["source"] = "AmsterdamUMCDb"
    changes_df['patid'] =  changes_df['patid'].astype(str) + '_Amsterdam'

####### Filter the dataframe for CRRT session_length <= 48h
changes_df_48h = changes_df[changes_df["session_length"]<=2880]

####### Filter the dataframe for 48h after start of net Ultrafiltration (first time NUF > 0!)
def from_NUF_start_48h(x):
    if (x["vm5010"] == 0).all():
        return x[x["session_length"] <= 2880]
    else:
        first_NUF = x.loc[x["vm5010"] > 0, "session_length"].iloc[0]
        return x[(x["session_length"] >= first_NUF) & (x["session_length"] <= (first_NUF + 2880))]          
changes_df_48h_NUF = changes_df.groupby("patid").apply(lambda x: from_NUF_start_48h(x))

##############
######
##

if HiRID:
    changes_df.to_csv(os.path.join(output_root, "changes_preprocessed_HiRID.csv"), index=False)
    changes_df_48h.to_csv(os.path.join(output_root, "changes_preprocessed_48h_HiRID.csv"), index=False)
    changes_df_48h_NUF.to_csv(os.path.join(output_root, "changes_preprocessed_48h_NUF_HiRID.csv"), index=False)
else:
    changes_df.to_csv(os.path.join(output_root, "changes_preprocessed_AmsterdamUMCDb.csv"), index=False)
    changes_df_48h.to_csv(os.path.join(output_root, "changes_preprocessed_48h_AmsterdamUMCDb.csv"), index=False)
    changes_df_48h_NUF.to_csv(os.path.join(output_root, "changes_preprocessed_48h_NUF_AmsterdamUMCDb.csv"), index=False)

##
######    
##############




